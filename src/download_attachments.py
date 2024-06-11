# *** Download email attachments
# *** https://github.com/VictorBenitoGR/OpenHealthData

# *** Libraries ---------------------------------------------------------------

import os  # Interact with the operating system
import pickle  # Serialize and deserialize objects
import base64  # Encode and decode data
import datetime  # Work with dates and times
from googleapiclient.discovery import build  # Build the Gmail API service
from google_auth_oauthlib.flow import InstalledAppFlow  # Authenticate the app
from google.auth.transport.requests import Request  # Make HTTP requests

# *** Functions ---------------------------------------------------------------

# Define the scope for the application
SCOPES = ['https://www.googleapis.com/auth/gmail.readonly']

# Function to authenticate and get the service object


def get_authenticated_service():
    creds = None
    # Load credentials if they exist
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    # Refresh or obtain new credentials if necessary
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'google_credentials.json', SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)
    # Return the Gmail API service object
    return build('gmail', 'v1', credentials=creds)

# Function to download attachments from emails


def download_attachments(service, user_id='me'):
    # Calculate the date 30 days ago
    past_month = datetime.datetime.now() - datetime.timedelta(days=30)
    past_month_str = past_month.strftime("%Y/%m/%d")
    # List messages from the past month
    results = service.users().messages().list(
        userId=user_id, q=f'after:{past_month_str}').execute()
    messages = results.get('messages', [])

    if not messages:
        print('No new messages.')
        return

    message_count = 0
    # Process each message
    for message in messages:
        process_message(service, message, user_id)
        message_count += 1

    print(f'Downloaded {message_count} attachments')

# Function to process each message


def process_message(service, message, user_id):
    # Get the message details
    msg = service.users().messages().get(
        userId=user_id, id=message['id']).execute()
    payload = msg['payload']
    # Check each part of the message for attachments
    for part in payload.get('parts', []):
        if part['filename']:
            data = get_attachment_data(service, part, message, user_id)
            if data:
                save_attachment(part['filename'], data)

# Function to get attachment data


def get_attachment_data(service, part, message, user_id):
    if 'data' in part['body']:
        return part['body']['data']
    elif 'id' in part['body']:
        att_id = part['body']['id']
        att = service.users().messages().attachments().get(
            uuserId=user_id, messageId=message['id'], id=att_id).execute()
        return att['body']['data']
    else:
        print("Part body does not contain 'data' or 'id' field. Part: ", part)
        return None

# Function to save the attachment to a file


def save_attachment(filename, data):
    file_data = base64.urlsafe_b64decode(data)
    file_path = os.path.join('./data/gmail', filename)

    if not os.path.isdir('./data/gmail'):
        os.makedirs('./data/gmail')
    with open(file_path, 'wb') as f:
        f.write(file_data)

# Main function to start the program


def main():
    service = get_authenticated_service()
    download_attachments(service)


# Entry point of the script
if __name__ == '__main__':
    main()
