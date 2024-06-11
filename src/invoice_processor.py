# * Invoice processor

# *** Libraries ---------------------------------------------------------------

import os
import pandas as pd
from dotenv import load_dotenv
from azure.ai.formrecognizer import DocumentAnalysisClient
from azure.core.credentials import AzureKeyCredential
from datetime import datetime

# *** Environment variables ---------------------------------------------------

load_dotenv()

endpoint = os.getenv('FORM_RECOGNIZER_ENDPOINT')
key = os.getenv('FORM_RECOGNIZER_API_KEY')

# Initialize the DocumentAnalysisClient
client = DocumentAnalysisClient(
    endpoint=endpoint, credential=AzureKeyCredential(key))

# *** Functions ---------------------------------------------------------------

def process_invoice(invoice_path):
    # Open the invoice file
    with open(invoice_path, "rb") as f:
        # Analyze the document
        poller = client.begin_analyze_document("prebuilt-invoice", document=f)
        result = poller.result()

    # Extract and return the data
    extracted_data = {}
    for doc in result.documents:
        for name, field in doc.fields.items():
            if field.value_type not in ["list", "dictionary"]:
                extracted_data[name] = field.content
    return extracted_data


def process_all_invoices(invoice_directory):
    invoice_files = os.listdir(invoice_directory)
    all_data = []

    for invoice_file in invoice_files:
        invoice_path = os.path.join(invoice_directory, invoice_file)
        invoice_data = process_invoice(invoice_path)
        all_data.append(invoice_data)

    # Convert the list of data into a pandas DataFrame
    invoices_df = pd.DataFrame(all_data)

    # Drop the unnecessary columns
    invoices_df = invoices_df.drop(
        columns=['CustomerAddressRecipient', 'CustomerId', 'CustomerName'])

    # Get the current date and format it
    current_date = datetime.now()
    formatted_date = current_date.strftime('%Y-%m')

    # Build the filename with the current date
    filename = f"./data/invoices_{formatted_date}.csv"

    # Export the DataFrame to CSV
    invoices_df.to_csv(filename, index=False)

    return invoices_df

# *** Main script -------------------------------------------------------------


invoices_df = process_all_invoices("./data/invoices")

print(invoices_df)
