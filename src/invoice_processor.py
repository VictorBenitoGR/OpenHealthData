
# * Invoice processor

# pip install azure-ai-formrecognizer
from azure.ai.formrecognizer import FormRecognizerClient
from azure.core.credentials import AzureKeyCredential
import pandas as pd
import os
from dotenv import load_dotenv

# Cargar las variables de entorno desde el archivo .env
load_dotenv()
endpoint = os.getenv("FORM_RECOGNIZER_ENDPOINT")
api_key = os.getenv("FORM_RECOGNIZER_API_KEY")


# Crear un cliente de Form Recognizer
client = FormRecognizerClient(endpoint, AzureKeyCredential(api_key))

# Leer el archivo PDF
with open("./data/invoices/invoice1.pdf", "rb") as f:
  invoice = f.read()

# Enviar el archivo PDF a Form Recognizer para extraer la información
poller = client.begin_recognize_invoices(invoice)
result = poller.result()

# Crear un DataFrame para almacenar la información
data = []

# Extraer la información de la factura
for recognized_invoice in result:
  invoice_data = {}
  for name, field in recognized_invoice.fields.items():
    if name == "Items":
      for idx, items in enumerate(field.value):
        item_data = {f"Item_{name}_{idx}_{item_name}": item.value for item_name, item in items.value.items()}
        invoice_data.update(item_data)
    else:
      invoice_data[name] = field.value
  data.append(invoice_data)

df = pd.DataFrame(data)

print(df)

# Guardar el DataFrame como un archivo CSV
df.to_csv("./data/invoice_data.csv", index=False)