
# * Generador de facturas médicas

from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter
import random
import os
import getpass
import subprocess

from datetime import datetime

def generar_factura(num_factura):
  # Crear un nuevo PDF
  c = canvas.Canvas(f"./data/invoices/invoice{num_factura}.pdf", pagesize=letter)

  # Variar el diseño en función del número de factura
  font_size = 12 + (num_factura % 3)
  c.setFont("Helvetica", font_size)
  text_color = ["black", "blue", "red"][num_factura % 3]
  c.setFillColor(text_color)

  # Elegir un número aleatorio de medicamentos
  num_medicamentos = random.randint(1, len(medicamentos))

  # Elegir los medicamentos y las cantidades
  factura = random.sample(medicamentos, num_medicamentos)
  cantidades = [random.randint(1, 10) for _ in range(num_medicamentos)]

  # Calcular el total de la factura
  total = sum(precio * cantidad for (_, precio), cantidad in zip(factura, cantidades))

  # Variar la posición del texto en función del número de factura
  y_start = 750 - (num_factura % 3) * 10

  # Generar la cabecera de la factura en el PDF
  c.drawString(50, y_start, f"Factura #{num_factura}")
  c.drawString(50, y_start - 20, f"Fecha: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

  # Generar la factura en el PDF
  c.drawString(50, y_start - 40, "Factura de medicamentos")
  for i, ((medicamento, precio), cantidad) in enumerate(zip(factura, cantidades)):
    c.drawString(50, y_start - (i + 3) * 20, f"{medicamento}: {cantidad} x ${precio} = ${cantidad * precio}")
  c.drawString(50, y_start - (num_medicamentos + 3) * 20, f"Total: ${total}")

  # Finalizar el PDF
  c.save()

# Pedir la contraseña de sudo al usuario
password = getpass.getpass("Ingrese su contraseña de sudo: ")

# Crear el directorio dentro de la carpeta del proyecto
command = "sudo -S mkdir -p ./data/invoices"  # -S permite a sudo leer la contraseña de la entrada estándar
subprocess.run(command, input=password, check=True, shell=True, universal_newlines=True)

# Cambiar el propietario del directorio a tu usuario
command = "sudo -S chown $USER:$USER ./data/invoices"
subprocess.run(command, input=password, check=True, shell=True, universal_newlines=True)

# Generar múltiples facturas aleatorias
for i in range(1, 101):
  generar_factura(i)