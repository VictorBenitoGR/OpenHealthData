# *** Invoice Generator
# *** https://github.com/VictorBenitoGR/OpenHealthData

import os
import random
from datetime import datetime, timedelta
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter
from reportlab.lib import colors

# Fixed hospital_info
hospital_info = {
    "name": "Queen Mamohato Memorial Hospital",
    "address": "456 Health Road, Maseru, Lesotho",
    "contact_person": "Dr. John Doe",
    "hospital_id": "QMMH-12345"
}


# Function to generate random date


def random_date(start, end):
    return start + timedelta(
        seconds=random.randint(0, int((end - start).total_seconds())))


# Random data generators


def random_provider_info():
    providers = [
        {
            "name": "Global Health Supplies",
            "address": "Qoaling Ha-Seoli Maseru, 100, Lesotho",
            "contact_number": "(+266) 987 654 3210",
            "email": "contact@globalhealth.com",
            "website": "www.globalhealth.com"
        },
        {
            "name": "Pharma Solutions Inc.",
            "address": "St. Monica's, Maputsoe 300, Lesotho",
            "contact_number": "(+266) 654 321 0987",
            "email": "info@pharmasolutions.com",
            "website": "www.pharmasolutions.com"
        },
        {
            "name": "MediCorp Ltd.",
            "address": "Boneschan Street, Venterstad, 9798, South Africa",
            "contact_number": "(+27) 321 654 0987",
            "email": "support@medicorp.com",
            "website": "www.medicorp.com"
        },
        {
            "name": "HealthTech Supplies",
            "address": "All Saints Mission Rd, Kalinyanga, South Africa",
            "contact_number": "(+27) 456 789 0123",
            "email": "contact@healthtech.com",
            "website": "www.healthtech.com"
        },
        {
            "name": "Lesotho Medical Supplies",
            "address": "30 Hospitaal St, Nxuba, 5880, South Africa",
            "contact_number": "(+27) 266 2231 0000",
            "email": "contact@lesothomedical.com",
            "website": "www.lesothomedical.com"
        }
    ]
    return random.choice(providers)


def random_invoice_details():
    today = datetime.today()
    invoice_date = random_date(today, today + timedelta(days=30))
    due_date = invoice_date + timedelta(days=30)
    return {
        "invoice_number":
        f"INV-{invoice_date.strftime('%Y%m%d')}-{random.randint(100, 999)}",
        "invoice_date": invoice_date.strftime('%Y-%m-%d'),
        "purchase_order_number": f"PO-{random.randint(1000, 9999)}",
        "payment_terms": random.choice(["Net 30 days", "Net 60 days"]),
        "due_date": due_date.strftime('%Y-%m-%d')
    }


def random_items():
    possible_items = [
        {"description": "N95 Masks", "product_code": "N95-100", "unit_price": 1.00},
        {"description": "Latex Gloves", "product_code": "LG-200", "unit_price": 0.15},
        {"description": "Alcohol Wipes", "product_code": "AW-300", "unit_price": 0.05},
        {"description": "Stethoscope", "product_code": "ST-400", "unit_price": 20.00},
        {"description": "X-Ray Machine",
            "product_code": "XR-5000", "unit_price": 15000.00},
        {"description": "Ultrasound Scanner",
            "product_code": "US-6000", "unit_price": 20000.00},
        {"description": "Surgical Masks",
            "product_code": "SM-700", "unit_price": 0.50},
        {"description": "Surgical Gowns",
            "product_code": "SG-800", "unit_price": 2.00},
        {"description": "Ventilator", "product_code": "VT-9000", "unit_price": 5000.00},
        {"description": "Ibuprofen Tablets",
            "product_code": "IB-100", "unit_price": 0.10},
        {"description": "Paracetamol Tablets",
            "product_code": "PA-200", "unit_price": 0.05},
        {"description": "Antibiotics", "product_code": "AB-300", "unit_price": 1.00},
        {"description": "Insulin", "product_code": "IN-400", "unit_price": 10.00},
        {"description": "Morphine", "product_code": "MO-500", "unit_price": 5.00},
        {"description": "Hydroxychloroquine",
            "product_code": "HY-600", "unit_price": 2.00},
        {"description": "PPE Kits", "product_code": "PP-700", "unit_price": 10.00},
        {"description": "Face Shields", "product_code": "FS-800", "unit_price": 2.00},
        {"description": "Thermometers", "product_code": "TH-900", "unit_price": 5.00}
    ]
    items = random.sample(possible_items, k=random.randint(5, 10))
    for item in items:
        if item["unit_price"] > 100:
            item["quantity"] = 1
        else:
            item["quantity"] = random.randint(1, 1000)
    return items


def random_additional_charges():
    return {
        "shipping_and_handling": random.uniform(50.00, 500.00),
        "taxes": random.uniform(100.00, 2000.00)
    }


def random_payment_info():
    payment_options = [
        {"method": "Wire Transfer", "account_name": "Global Health Supplies",
            "bank": "World Bank", "account_number": "987654321", "swift_code": "WB12345"},
        {"method": "Direct Deposit", "account_name": "Pharma Solutions Inc.",
            "bank": "Health Finance", "account_number": "123456789", "swift_code": "HF67890"},
    ]
    return random.choice(payment_options)


def random_style(c):
    fonts = ["Helvetica", "Courier", "Times-Roman"]
    font_sizes = [10, 12, 14]
    # Correctly reference colors from the imported module
    color_choices = [colors.black, colors.blue, colors.red]

    font = random.choice(fonts)
    font_size = random.choice(font_sizes)
    color = random.choice(color_choices)

    c.setFont(font, font_size)
    c.setFillColor(color)

    return font_size


def draw_string(c, x, y, text):
    font_size = random_style(c)
    c.drawString(x, y, text)
    # Adjust y position based on font size to prevent overlap, adding a margin
    return y - font_size - 5  # Adjust the 5 to increase or decrease the gap


def random_pastel_color():
    # Generates a random pastel color
    r = random.randint(200, 255)
    g = random.randint(200, 255)
    b = random.randint(200, 255)
    return (r / 255.0, g / 255.0, b / 255.0)


def draw_pastel_square(c, x, y, size):
    c.setFillColorRGB(*random_pastel_color())
    # Draw the square with filling and no contour
    c.rect(x, y, size, size, fill=1, stroke=0)


def generate_invoice_pdf(invoice_number):
    provider_info = random_provider_info()
    invoice_details = random_invoice_details()
    items = random_items()
    additional_charges = random_additional_charges()
    payment_info = random_payment_info()

    # Ensure the directory exists
    os.makedirs("./data/invoices", exist_ok=True)
    filename = f"./data/invoices/invoice{invoice_number}.pdf"
    c = canvas.Canvas(filename, pagesize=letter)

    # Adjusted for letter-sized document at 300 DPI
    doc_width, doc_height = 2550, 3300

    # Draw many random pastel squares in random positions and sizes
    for _ in range(1000):  # Increase the number of squares drawn
        square_size = random.randint(20, 100)  # Random size for each square
        # Ensure the square fits within the document's dimensions
        x = random.randint(0, doc_width - square_size)
        y = random.randint(0, doc_height - square_size)

        draw_pastel_square(c, x, y, square_size)

    y = 750  # Starting Y position, adjusted to add a top margin

    # Provider Information
    y = draw_string(c, 50, y, f"Provider: {provider_info['name']}")
    y = draw_string(c, 50, y, f"Address: {provider_info['address']}")
    y = draw_string(c, 50, y, f"Contact: {provider_info['contact_number']}")
    y = draw_string(c, 50, y, f"Email: {provider_info['email']}")
    y = draw_string(c, 50, y, f"Website: {provider_info['website']}")

    # Adjusting y for a new section
    y -= 20  # Add a larger gap before starting a new section

    # Invoice Details
    y = draw_string(
        c, 300, y, f"Invoice Number: {invoice_details['invoice_number']}")
    y = draw_string(
        c, 300, y, f"Invoice Date: {invoice_details['invoice_date']}")
    y = draw_string(
        c, 300, y, f"PO Number: {invoice_details['purchase_order_number']}")
    y = draw_string(
        c, 300, y, f"Payment Terms: {invoice_details['payment_terms']}")
    y = draw_string(c, 300, y, f"Due Date: {invoice_details['due_date']}")

    # Hospital Information
    y -= 20  # Adjust y position for a new section
    y = draw_string(c, 50, y, f"Hospital: {hospital_info['name']}")
    y = draw_string(c, 50, y, f"Address: {hospital_info['address']}")
    y = draw_string(
        c, 50, y, f"Contact Person: {hospital_info['contact_person']}")
    y = draw_string(c, 50, y, f"Hospital ID: {hospital_info['hospital_id']}")

    # Items
    y -= 20  # Adjust y position for a new section
    y = draw_string(
        c, 50, y, "Item Description | Product Code | Quantity | Unit Price | Total Price")
    for item in items:
        total_price = item["quantity"] * item["unit_price"]
        y = draw_string(
            c, 50, y, f"{item['description']} | {item['product_code']} | {item['quantity']} | ${item['unit_price']:.2f} | ${total_price:.2f}")

    # Additional Charges
    y = draw_string(
        c, 50, y, f"Shipping and Handling: ${additional_charges['shipping_and_handling']:.2f}")
    y = draw_string(c, 50, y, f"Taxes: ${additional_charges['taxes']:.2f}")

    # Total Amount Due
    subtotal = sum(item["quantity"] * item["unit_price"] for item in items)
    total_due = subtotal + \
        additional_charges['shipping_and_handling'] + \
        additional_charges['taxes']
    y = draw_string(c, 50, y, f"Subtotal: ${subtotal:.2f}")
    y = draw_string(c, 50, y, f"Total Amount Due: ${total_due:.2f}")

    # Payment Information
    y -= 20  # Adjust y position for a new section
    y = draw_string(c, 50, y, "Payment Information:")
    y = draw_string(c, 50, y, f"Method: {payment_info['method']}")
    y = draw_string(c, 50, y, f"Account Name: {payment_info['account_name']}")
    y = draw_string(c, 50, y, f"Bank: {payment_info['bank']}")
    y = draw_string(
        c, 50, y, f"Account Number: {payment_info['account_number']}")
    y = draw_string(c, 50, y, f"SWIFT Code: {payment_info['swift_code']}")

    c.save()


# Number of invoices to generate
n = 50

for i in range(1, n + 1):
    generate_invoice_pdf(i)
