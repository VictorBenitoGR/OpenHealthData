
# * Download email attachments

import os
import base64
import datetime
import pickle
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

# Si modificas estos SCOPES, elimina el archivo token.pickle.
SCOPES = ['https://www.googleapis.com/auth/gmail.readonly']

def main():
  creds = None
  if os.path.exists('token.pickle'):
    with open('token.pickle', 'rb') as token:
      creds = pickle.load(token)
  if not creds or not creds.valid:
    if creds and creds.expired and creds.refresh_token:
      creds.refresh(Request())
    else:
      flow = InstalledAppFlow.from_client_secrets_file(
        'google_credentials.json', SCOPES)
      creds = flow.run_local_server(port=0)
    with open('token.pickle', 'wb') as token:
      pickle.dump(creds, token)

  service = build('gmail', 'v1', credentials=creds)

  # Obtén la fecha de hace un mes
  past_month = datetime.datetime.now() - datetime.timedelta(days=30)
  past_month_str = past_month.strftime("%Y/%m/%d")

  # Filtra los mensajes para obtener solo los del último mes
  results = service.users().messages().list(userId='me', q=f'after:{past_month_str}').execute()
  messages = results.get('messages', [])

  if not messages:
    print('No new messages.')
  else:
    message_count = 0
    for message in messages:
      msg = service.users().messages().get(userId='me', id=message['id']).execute()
      payload = msg['payload']
      for part in payload.get('parts', []):
        if part['filename']:
          if 'data' in part['body']:
            data = part['body']['data']
          elif 'id' in part['body']:
            att_id = part['body']['id']
            att = service.users().messages().attachments().get(userId='me', messageId=message['id'], id=att_id).execute()
            data = att['body']['data']  # Aquí está la corrección
          else:
            print("Part body does not contain 'data' or 'id' field. Part: ", part)
            continue
          file_data = base64.urlsafe_b64decode(data)
          file_path = os.path.join('./data/gmail', part['filename'])

          if not os.path.isdir('./data/gmail'):
            os.makedirs('./data/gmail')
          with open(file_path, 'wb') as f:
            f.write(file_data)
          message_count += 1
    print(f'Downloaded {message_count} attachments')

if __name__ == '__main__':
  main()