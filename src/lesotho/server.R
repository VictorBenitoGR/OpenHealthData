# * OpenHealthData
# * https://github.com/VictorBenitoGR/OpenHealthData

# *** Paquetes ----------------------------------------------------------------

library(ggplot2) #         Visualización de datos
library(lubridate) #       Manipulación de fechas
library(forcats) #         Manipulación de factores
library(scales) #          Escalas de gráficos
library(tm) #              Text Mining
# // library(wordcloud) #       Nubes de palabras
library(RColorBrewer) #    Paletas de colores
library(shiny) #           Aplicaciones web interactivas
library(shinydashboard) #  Diseño de aplicaciones web
library(ggplot2) #         Visualización de datos
library(plotly) #          Gráficos interactivos
library(dplyr) #           Manipulación de datos
library(tidyr) #           Manipulación de datos

# *** Tiempos de cirugía de emergencia ----------------------------------------

# * A1 Tiempos de cirugía de emergencia
# Descripción:  El tiempo entre la notificación al quirófano y la
#               administración de la anestesia para pacientes que requieren
#               cirugía de emergencia.
# Criterios:    Cumplimiento del Anexo 13, Parte B Sección 2.1.1. Medir
#               trimestralmente mediante examen aleatorio de las
#               características de los pacientes de cirugía. Muestreo de
#               gráficos n ≥ 10.
# Objetivo pre-acreditación:  ≤ 60 minutos en ≥ 80% de los casos.
# Objetivo post-acreditación: ≤ 60 minutos en ≥ 90% de los casos.
# Medición trimestral.

# Generar datos de prueba
set.seed(123) # Para reproducibilidad
tiempos_cirugia_pre <- sapply(1:12, function(x) rnorm(10, mean = 55, sd = 10))
tiempos_cirugia_post <- sapply(1:12, function(x) rnorm(10, mean = 50, sd = 10))

# Crear un dataframe
df_pre <- data.frame(
  tiempos_cirugia = as.vector(tiempos_cirugia_pre),
  periodo = rep("Pre-acreditación", 120),
  semana = rep(1:12, each = 10)
)

df_post <- data.frame(
  tiempos_cirugia = as.vector(tiempos_cirugia_post),
  periodo = rep("Post-acreditación", 120),
  semana = rep(1:12, each = 10)
)

df <- rbind(df_pre, df_post)

# Cambiar el orden de los niveles del factor periodo
df$periodo <- factor(df$periodo, levels = c(
  "Pre-acreditación", "Post-acreditación"
))

# Boxplot
a1 <- ggplot(df, aes(
  x = as.factor(semana), y = tiempos_cirugia, fill = periodo
)) +
  geom_boxplot() +
  labs(y = "Tiempos de cirugía (minutos)", x = "Semana del trimestre") +
  scale_fill_manual(values = c(
    "Pre-acreditación" = "#1a39b6", "Post-acreditación" = "#009543"
  )) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", colour = "black"),
    legend.text = element_text(face = "bold")
  ) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1)

# *** Medidas de control de infecciones ---------------------------------------
# ? lubridate
# ? forcats

# * A2 Medidas de control de infecciones
# Descripción:  Cumplimiento de las normas y protocolos de control de
#               infecciones para el lavado de manos.
# Criterios:    Cumplimiento del Anexo 13, Parte B, Sección 2.1.2. Medido
#               trimestralmente mediante: (i) inspecciones de los puestos de
#               lavado de manos, que pueden ser fijos (es decir, lavabos) o
#               móviles (incluso junto a la cama) próximos al paciente, el
#               100% de los cuales deben disponer de jabón, agua y/o solución
#               esterilizante para manos y toallas de papel adecuados; y (ii)
#               observación de médicos y enfermeros para comprobar el
#               cumplimiento del 100% del lavado de manos y el cambio de
#               guantes (según proceda) entre cada paciente. Las observaciones
#               se realizarán para n ≥ 100 personas de varias salas y
#               servicios de pacientes de todas las instalaciones, incluidos
#               los turnos de mañana y tarde.
# Objetivo pre-acreditación:  ≥ 99% de cumplimiento.
# Objetivo post-acreditación: ≥ 99% de cumplimiento.
# Medición trimestral.

# Generar datos falsos
set.seed(1293)
data <- data.frame(
  Fecha = seq(as.Date("2014/1/1"), as.Date("2014/12/31"), by = "day"),
  Cumplimiento = rbinom(365, 1, 0.99)
)

# Crear variables de año, mes y día
data$Año <- year(data$Fecha)
# Invertir el orden de los meses
data$Mes <- fct_rev(factor(month(data$Fecha, label = TRUE)))
data$Día <- day(data$Fecha)

# Crear la gráfica de calendario de calor
a2 <- ggplot(data, aes(x = Día, y = Mes, fill = factor(Cumplimiento))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("#1a39b6", "#009543"), guide = FALSE) +
  facet_grid(Año ~ ., scales = "free_y") +
  labs(
    x = "Día del mes", y = ""
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      hjust = 1, face = "bold", colour = "black"
    ),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black")
  )

# *** Visitas ambulatorias ----------------------------------------------------
# ? scales

# * B1 Visitas ambulatorias
# Descripción:  Visita = Total de servicios ambulatorios brindados a una sola
#               persona en un solo día (24 horas).
# Criterios:    Cumplimiento de protocolos del Anexo 18: Medición anual con
#               mínimo 258.000
# Objetivo pre-acreditación:  Depende de la oferta.
# Objetivo post-acreditación: Mínimo anual: xxx,xxx pacientes ambulatorios.
#                             Medido por Año de Contrato.
# Medición anual.

# Generar datos falsos con variaciones en el número de visitas
set.seed(123)
data <- data.frame(
  Año = rep(2010:2014, each = 6),
  Servicio = rep(c(
    "Servicios de Diagnóstico", "Terapias y Rehabilitación",
    "Atención Prenatal y Postnatal", "Servicios de Prevención",
    "Vacunación", "Banco de Sangre"
  ), times = 5),
  Visitas = c(
    rpois(6, lambda = 80000), # Variaciones para el año 2010
    rpois(6, lambda = 85000), # Variaciones para el año 2011
    rpois(6, lambda = 90000), # Variaciones para el año 2012
    rpois(6, lambda = 91000), # Variaciones para el año 2013
    rpois(6, lambda = 100000) # Variaciones para el año 2014
  )
)

okabe_ito_palette <- c(
  "#181818", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

# Función para formatear las etiquetas del eje y
format_y <- function(x) {
  paste0(x / 1000, "k")
}

# Crear la gráfica de barras apiladas
b1 <- ggplot(data, aes(x = factor(Año), y = Visitas, fill = Servicio)) +
  geom_bar(stat = "identity") +
  labs(
    x = NULL, y = "Número de visitas"
  ) +
  theme_classic() +
  scale_fill_manual(values = okabe_ito_palette) +
  theme(
    plot.title = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black"),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    legend.position = "none" # Eliminar la leyenda
  ) +
  scale_y_continuous(labels = format_y)

# *** Admisiones de pacientes hospitalizados ----------------------------------

# * # B2 Admisiones de pacientes hospitalizados
# Descripción:  Ingreso = La realización del procedimiento de ingreso completo
#               y aceptación por parte del Hospital. El procedimiento de
#               ingreso completo puede definirse como la finalización de todo
#               el hospital. El procedimiento de admisión completo puede
#               definirse como la finalización de todos los documentos de
#               registro del hospital, incluido el registro del nombre del
#               Paciente en el sistema de registro de admisión.
# Criterios:    Cumplimiento de protocolos del Anexo 18: Medición anual con
#               mínimo 16,500.
# Objetivo pre-acreditación:  Depende de la oferta.
# Objetivo post-acreditación: Mínimo anual de xx,xx pacientes hospitalizados
#                             medidos por año de contrato.
# Medición anual.

# Generar datos falsos
set.seed(122)
data <- data.frame(
  Año = 2010:2014,
  Admisiones = c(
    rpois(1, lambda = 16500), # Variaciones para el año 2010
    rpois(1, lambda = 17000), # Variaciones para el año 2011
    rpois(1, lambda = 17500), # Variaciones para el año 2012
    rpois(1, lambda = 18000), # Variaciones para el año 2013
    rpois(1, lambda = 18500) # Variaciones para el año 2014
  )
)

# Encontrar el valor mínimo de las admisiones
min_admisiones <- min(data$Admisiones, na.rm = TRUE)

# Crear un nuevo dataframe para la línea roja
line_data <- data.frame(
  Año = data$Año,
  Valor = ifelse(data$Año == 2013, data$Admisiones[data$Año == 2013] + 200,
    data$Admisiones - 100
  )
)

# Crear la gráfica de barras con la escala ajustada
b2 <- ggplot() +
  geom_bar(data = data, aes(
    x = Año, y = Admisiones
  ), stat = "identity", fill = "#009543") +
  geom_line(
    data = line_data, aes(x = Año, y = Valor),
    color = "red", linetype = "dashed", size = 1
  ) +
  labs(
    x = NULL, y = "Número de admisiones"
  ) +
  coord_cartesian(ylim = c(min_admisiones, NA)) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black"),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black")
  ) +
  scale_y_continuous(labels = format_y)

# *** Satisfacción del paciente y su familia ----------------------------------
# ? tm
# ? wordcloud
# ? RColorBrewer

# * C1 Satisfacción del paciente y su familia
# Descripción:  Satisfacción general de pacientes y familiares con las
#               instalaciones y servicios.
# Criterios:    Determinar con referencia a los resultados del procedimiento
#               de cumplimiento del Operador y los registros del servicio de
#               asistencia implementados de acuerdo con el Mecanismo de Pago y
#               el Anexo 13, Sección 7,1; muestreo n ≥ 30
# Objetivo pre-acreditación:  Tasa de satisfacción ≥ 75%.
# Objetivo post-acreditación: Tasa de satisfacción ≥ 85%.
# Medición trimestral.

# ! ./www/img/c1.jpg

# *** Auditoría de equipo -----------------------------------------------------

# * E1 Auditoría de equipo
# Descripción:  Cumplimiento de los estándares de servicio.
# Criterios:    Cumplimiento del Anexo 13, Parte A, Sección 13.2.
# Objetivo pre-acreditación:  Cumplimiento ≥ 95%.
# Objetivo post-acreditación: Cumplimiento ≥ 95%.
# Medición trimestral.

# Generar datos de ejemplo
set.seed(123)
equipos <- c("Equipo A", "Equipo B", "Equipo C", "Equipo D", "Equipo E")
personas <- c(
  "Persona 1", "Persona 2", "Persona 3", "Persona 4",
  "Persona 5", "Persona 6", "Persona 7", "Persona 8"
)

data <- expand.grid(Equipo = equipos, Persona = personas)

# Cantidad de veces que se usó el equipo
data$Uso <- round(runif(nrow(data), min = 50, max = 200))

# Valores entre 0.9 y 1
data$Cumplimiento <- runif(nrow(data), min = 0.9, max = 1)

# Generar datos de ejemplo
set.seed(123)
equipos <- c(
  "Equipo A", "Equipo B", "Equipo C", "Equipo D", "Equipo E"
)
personas <- c(
  "Persona 1", "Persona 2", "Persona 3", "Persona 4",
  "Persona 5", "Persona 6", "Persona 7", "Persona 8"
)

data <- expand.grid(Equipo = equipos, Persona = personas)
# Cantidad de veces que se usó el equipo
data$Uso <- round(runif(nrow(data), min = 50, max = 200))

# Asegurarse de que la Persona 8 y el Equipo E siempre estén por debajo de 0.95
data$Cumplimiento[data$Persona == "Persona 8" | data$Equipo == "Equipo E"] <-
  runif(
    sum(data$Persona == "Persona 8" | data$Equipo == "Equipo E"),
    min = 0.9, max = 0.95
  )

# Asegurarse de que todos los demás estén por encima de 0.95
data$Cumplimiento[!(data$Persona == "Persona 8" | data$Equipo == "Equipo E")] <-
  runif(
    sum(!(data$Persona == "Persona 8" | data$Equipo == "Equipo E")),
    min = 0.95, max = 1
  )

# Crear la gráfica de dispersión
e1 <- ggplot(data, aes(
  x = Uso, y = Cumplimiento, color = Persona, shape = Equipo
)) +
  geom_point(size = 4) +
  geom_hline(
    # Línea de objetivo mínimo
    yintercept = 0.95, linetype = "dashed", color = "red", size = 1.5
  ) +
  labs(
    x = "Uso del equipo (veces por trimestre)", y = "Cumplimiento (%)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black"),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    legend.position = "none" # Eliminar la leyenda
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0.9, 1)) +
  scale_color_manual(values = okabe_ito_palette) # Aplicar la paleta de colores

# *** Certificación de personal -----------------------------------------------

# * H1 Certificación de personal
# Descripción:  Cumplimiento de los estándares de servicio.
# Criterios:    Cumplimiento del Anexo 13, Parte B, Sección 4.1.
# Objetivo pre-acreditación:  Cumplimiento ≥ 80%.
# Objetivo post-acreditación: Cumplimiento ≥ 90%.
# Medición anual.

# Generar datos aleatorios
set.seed(123)
anos <- 2010:2014
personas <- paste0("P", 1:8)

# Generar datos para cada persona
data <- data.frame()
for (persona in personas) {
  cumplimiento <- runif(length(anos), min = 0.8, max = 1)
  data_persona <- data.frame(
    Ano = anos, Cumplimiento = cumplimiento, Persona = persona
  )
  data <- rbind(data, data_persona)
}

# Crear el boxplot
h1 <- ggplot(data, aes(x = Persona, y = Cumplimiento, fill = Persona)) +
  geom_boxplot() +
  # línea de objetivo
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = okabe_ito_palette) + # Aplicar la paleta de colores
  labs(
    x = NULL, y = "Cumplimiento (%)",
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black"),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    legend.position = "none"
  )

# !

# *** Generate random invoice data --------------------------------------------



generate_random_data <- function(num_invoices = 100) {
  # Inicializar data frames con nombres de columnas
  invoices <- data.frame(invoice_id = integer(), invoice_number = character(), invoice_date = character(), purchase_order_number = character(), payment_terms = character(), due_date = character(), stringsAsFactors = FALSE)
  invoice_items <- data.frame(invoice_id = integer(), description = character(), product_code = character(), unit_price = numeric(), stringsAsFactors = FALSE)

  for (invoice_id in 1:num_invoices) {
    invoice_details <- random_invoice_details()
    invoice_details$invoice_id <- invoice_id # Asegurar que invoice_id es una columna en invoices

    # Agregar detalles de factura a 'invoices'
    invoices <- rbind(invoices, invoice_details)

    items <- random_items()
    # Asegurar que cada ítem tenga el invoice_id correcto
    items$invoice_id <- rep(invoice_id, nrow(items))

    # Agregar detalles de ítem a 'invoice_items'
    invoice_items <- rbind(invoice_items, items)
  }

  list(invoices = invoices, invoice_items = invoice_items)
}

random_date <- function(start_date, end_date) {
  as.Date(start_date + sample.int(as.integer(end_date - start_date), 1))
}

random_invoice_details <- function() {
  invoice_date <- as.Date(random_date(as.Date("2010-01-01"), as.Date("2014-12-31")))
  due_date <- as.Date(invoice_date + days(30)) # Convertir a 'Date' para eliminar la hora

  supplier_names <- c("Global Health Supplies", "Pharma Solutions Inc.", "MediCorp Ltd.", "HealthTech Supplies", "Lesotho Medical Supplies")
  supplier_name <- sample(supplier_names, 1) # Seleccionar un proveedor aleatoriamente

  data.frame(
    invoice_number = paste("INV", format(invoice_date, "%Y%m%d"), sample(100, 999, size = 1), sep = "-"),
    invoice_date = as.character(invoice_date),
    purchase_order_number = paste("PO", sample(1000, 9999, size = 1), sep = "-"),
    payment_terms = sample(c("Net 30 days", "Net 60 days"), size = 1),
    due_date = as.character(due_date), # Convertir a cadena para mantener consistencia
    supplier_name = supplier_name # Agregar el nombre del proveedor
  )
}

random_items <- function() {
  possible_items <- data.frame(
    description = c("N95 Masks", "Latex Gloves", "Alcohol Wipes", "Stethoscope", "X-Ray Machine", "Ultrasound Scanner", "Surgical Masks", "Surgical Gowns", "Ventilator", "Ibuprofen Tablets", "Paracetamol Tablets", "Antibiotics", "Insulin", "Morphine", "Hydroxychloroquine", "PPE Kits", "Face Shields", "Thermometers"),
    product_code = c("N95-100", "LG-200", "AW-300", "ST-400", "XR-5000", "US-6000", "SM-700", "SG-800", "VT-9000", "IB-100", "PA-200", "AB-300", "IN-400", "MO-500", "HY-600", "PP-700", "FS-800", "TH-900"),
    unit_price = c(1.00, 0.15, 0.05, 20.00, 15000.00, 20000.00, 0.50, 2.00, 5000.00, 0.10, 0.05, 1.00, 10.00, 5.00, 2.00, 10.00, 2.00, 5.00),
    stringsAsFactors = FALSE
  )
  num_items <- sample(1:5, 1) # Generar entre 1 y 5 ítems por factura
  items <- possible_items[sample(nrow(possible_items), num_items), ]

  # Asignar quantity basado en unit_price
  items$quantity <- ifelse(items$unit_price > 100, 1, sample(1:100, nrow(items)))

  return(items)
}

# Generar datos
random_data <- generate_random_data(1000)

# *** Indicadores de facturas -------------------------------------------------

# Terminamos teniendo random_data$invoices con las columnas:
# invoice_id, invoice_number, invoice_date, purchase_order_number, payment_terms, due_date, supplier_name

# Y random_data$invoice_items con las columnas:
# invoice_id, description, product_code, unit_price, quantity

# ¿Qué indicadores podríamos hacer?

# * Número de facturas emitidas por mes, segmentadas por description (item)

# Preparar los datos
datos_completos <- left_join(random_data$invoices, random_data$invoice_items, by = "invoice_id")
datos_completos$invoice_month <- floor_date(as.Date(datos_completos$invoice_date), "month")

# Ajustar la extracción del mes para ignorar el año
datos_completos$invoice_month <- format(as.Date(datos_completos$invoice_date), "%m")

# Agrupar y resumir los datos nuevamente, esta vez solo por mes y descripción
datos_agrupados <- datos_completos %>%
  group_by(invoice_month, description) %>%
  summarise(numero_facturas = n_distinct(invoice_id), .groups = "drop")

# Calcular el total de facturas por descripción
totales_por_descripcion <- datos_agrupados %>%
  group_by(description) %>%
  summarise(total_facturas = sum(numero_facturas)) %>%
  ungroup() %>%
  arrange(desc(total_facturas)) %>%
  top_n(7, total_facturas)

# Filtrar solo las top 8 descripciones en datos_agrupados
datos_filtrados <- datos_agrupados %>%
  filter(description %in% totales_por_descripcion$description)

# Crear la gráfica con los datos filtrados
f1 <- ggplot(datos_filtrados, aes(x = invoice_month, y = numero_facturas, fill = description)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = okabe_ito_palette) + # Ajustar para usar solo los primeros 8 colores
  theme_classic() +
  labs(x = NULL, y = "Número de Facturas", fill = "Descripción del Producto") +
  scale_x_discrete(labels = c("01" = "Enero", "02" = "Febrero", "03" = "Marzo", "04" = "Abril", "05" = "Mayo", "06" = "Junio", "07" = "Julio", "08" = "Agosto", "09" = "Septiembre", "10" = "Octubre", "11" = "Noviembre", "12" = "Diciembre")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black"),
    legend.title = element_text(face = "bold", colour = "black"),
    legend.text = element_text(colour = "black")
  )


# * Valor total de facturas emitidas por mes

# obtener valor_total
datos_completos$valor_total <- datos_completos$unit_price * datos_completos$quantity

# Paso 1: Agrupar y resumir los datos por mes y tipo de ítem (description)
datos_agrupados_valor_desc <- datos_completos %>%
  group_by(invoice_month, description) %>%
  summarise(valor_total = sum(valor_total), .groups = "drop")

# Ahora puedes proceder con el filtrado para obtener el top 8
datos_top8 <- datos_agrupados_valor_desc %>%
  group_by(description) %>%
  summarise(total_valor = sum(valor_total)) %>%
  top_n(8, total_valor) %>%
  inner_join(datos_agrupados_valor_desc, by = "description")

# Continúa con el resto de tu código...

# Filtrar para incluir solo el top 8 de elementos basado en valor_total
datos_top8 <- datos_agrupados_valor_desc %>%
  group_by(description) %>%
  summarise(total_valor = sum(valor_total)) %>%
  top_n(8, total_valor) %>%
  inner_join(datos_agrupados_valor_desc, by = "description")

# Crear la gráfica sin aplicar formateador personalizado en el eje Y
f2 <- ggplot(datos_top8, aes(x = invoice_month, y = valor_total, fill = description)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_log10() + # Se omite el argumento labels
  scale_fill_manual(values = okabe_ito_palette) +
  theme_classic() +
  labs(x = NULL, y = "Valor Total de Facturas", fill = "Tipo de Ítem") +
  scale_x_discrete(labels = c("01" = "Enero", "02" = "Febrero", "03" = "Marzo", "04" = "Abril", "05" = "Mayo", "06" = "Junio", "07" = "Julio", "08" = "Agosto", "09" = "Septiembre", "10" = "Octubre", "11" = "Noviembre", "12" = "Diciembre")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black"),
    legend.title = element_text(face = "bold", colour = "black"),
    legend.text = element_text(colour = "black"),
    legend.position = "bottom"
  )

# Exportar la gráfica

# * Valor promedio de facturas emitidas por mes

# Calcular el valor promedio de las facturas por mes
datos_promedio <- datos_agrupados_valor_desc %>%
  group_by(invoice_month) %>%
  summarise(promedio_valor = mean(valor_total), .groups = "drop")

# Crear la gráfica de línea con degradado
f3 <- ggplot(datos_promedio, aes(x = invoice_month, y = promedio_valor, group = 1)) +
  geom_line(color = "#1a39b6", size = 1.5) +
  geom_point(color = "#009543", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "#009543", linetype = "dashed") +
  theme_classic() +
  labs(x = NULL, y = "Valor Promedio Facturas") +
  scale_x_discrete(labels = c("01" = "Enero", "02" = "Febrero", "03" = "Marzo", "04" = "Abril", "05" = "Mayo", "06" = "Junio", "07" = "Julio", "08" = "Agosto", "09" = "Septiembre", "10" = "Octubre", "11" = "Noviembre", "12" = "Diciembre")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black")
  )

# Exportar la gráfica

# * Número de facturas emitidas por proveedor

# Calcular el número total de facturas por proveedor
datos_proveedor <- datos_completos %>%
  group_by(supplier_name) %>%
  summarise(numero_facturas = n_distinct(invoice_id), .groups = "drop") %>%
  arrange(desc(numero_facturas)) %>%
  top_n(8, numero_facturas)

# Filtrar solo los 8 principales proveedores en datos_completos
datos_filtrados_proveedor <- datos_completos %>%
  filter(supplier_name %in% datos_proveedor$supplier_name)

# Crear la gráfica de barras apiladas
f4 <- ggplot(datos_filtrados_proveedor, aes(x = invoice_month, fill = supplier_name)) +
  geom_bar() +
  scale_fill_manual(values = okabe_ito_palette[1:8]) +
  theme_classic() +
  labs(x = NULL, y = "Número de Facturas", fill = "Proveedor") +
  scale_x_discrete(labels = c("01" = "Enero", "02" = "Febrero", "03" = "Marzo", "04" = "Abril", "05" = "Mayo", "06" = "Junio", "07" = "Julio", "08" = "Agosto", "09" = "Septiembre", "10" = "Octubre", "11" = "Noviembre", "12" = "Diciembre")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black"),
    legend.title = element_text(face = "bold", colour = "black"),
    legend.text = element_text(colour = "black")
  )

# Exportar la gráfica

# * ¿Cuántos días nos dan los proveedores para pagar?

# Asegúrate de que okabe_ito_palette está definida correctamente
# okabe_ito_palette <- c("color1", "color2", "color3", ...)

# Modificar el gráfico de barras para mostrar los términos de pago divididos por supplier_name
# y aplicar la paleta de colores okabe_ito_palette
f5 <- ggplot(datos_completos, aes(x = payment_terms, fill = supplier_name)) +
  geom_bar() +
  scale_fill_manual(values = okabe_ito_palette) + # Aplicar la paleta de colores
  theme_classic() +
  labs(x = "Términos de Pago", y = "Número de Facturas", fill = "Nombre del Proveedor") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black"),
    legend.title = element_text(face = "bold", colour = "black"),
    legend.text = element_text(colour = "black")
  )

# Exportar la gráfica modificada

# * Correlación entre el valor total de la factura y el número de ítems

# Calcular el valor total de cada factura
datos_completos$valor_total <- datos_completos$unit_price * datos_completos$quantity

# Asumiendo que datos_completos y las transformaciones ya están definidas

# Crear la gráfica de dispersión con la línea de tendencia, el eje x en escala logarítmica,
# y la densidad de puntos visualizada mediante contornos de densidad
f6 <- ggplot(datos_completos, aes(x = valor_total, y = quantity)) +
  geom_point(alpha = 0.5) + # Ajustar la transparencia de los puntos
  geom_density2d(aes(color = ..level..)) + # Añadir contornos de densidad coloreados por nivel
  geom_smooth(method = "lm", se = FALSE, color = "#1a39b6") +
  scale_x_log10() + # Aplicar escala logarítmica al eje x
  scale_color_viridis_c() + # Usar una paleta de colores continua para los contornos
  theme_classic() +
  labs(x = "Valor Total de la Factura (Escala Logarítmica)", y = "Número de Ítems", color = "Densidad") +
  theme(
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title.x = element_text(face = "bold", colour = "black"),
    axis.title.y = element_text(face = "bold", colour = "black")
  )

# Exportar la gráfica

# *** Crear la aplicación Shiny -----------------------------------------------

# Servidor
server <- function(input, output) {
  output$a1 <- renderPlotly({
    ggplotly(a1)
  })
  output$a2 <- renderPlotly({
    ggplotly(a2)
  })
  output$b1 <- renderPlotly({
    ggplotly(b1)
  })
  output$b2 <- renderPlotly({
    ggplotly(b2)
  })
  output$e1 <- renderPlotly({
    ggplotly(e1)
  })
  output$h1 <- renderPlotly({
    ggplotly(h1)
  })
  # Agregar f1 a f6
  output$f1 <- renderPlotly({
    ggplotly(f1)
  })
  output$f2 <- renderPlotly({
    ggplotly(f2)
  })
  output$f3 <- renderPlotly({
    ggplotly(f3)
  })
  output$f4 <- renderPlotly({
    ggplotly(f4)
  })
  output$f5 <- renderPlotly({
    ggplotly(f5)
  })
  output$f6 <- renderPlotly({
    ggplotly(f6)
  })
}

# *** Interfaz de usuario -----------------------------------------------------

# ui <- fluidPage(
#   tags$head(tags$style(HTML("
#     #myImage img {
#       object-fit: contain;
#       max-height: 100%;
#     }
#     .center-vertically {
#       display: flex;
#       align-items: center;
#       justify-content: center;
#     }
#   "))),
#   titlePanel(tags$b("Queen Mamohato Memorial Hospital")),
#   tabsetPanel(
#     tabPanel("Licitación",
#       fluidRow(
#         column(3, plotlyOutput("a1", height = "40vh")),
#         column(3, plotlyOutput("a2", height = "40vh")),
#         column(3, plotlyOutput("b1", height = "40vh")),
#         column(3, plotlyOutput("b2", height = "40vh"))
#       ),
#       fluidRow(
#         column(4, div(class = "center-vertically", div(id = "myImage", img(
#           src = "img/c1.jpg",
#           width = "100%"
#         )))),
#         column(4, plotlyOutput("e1", height = "40vh")),
#         column(4, plotlyOutput("h1", height = "40vh"))
#       )
#     ),
#     tabPanel("Facturas",
#       fluidRow(
#         column(4, plotlyOutput("f1", height = "40vh")),
#         column(4, plotlyOutput("f2", height = "40vh")),
#         column(4, plotlyOutput("f3", height = "40vh"))
#       ),
#       fluidRow(
#         column(4, plotlyOutput("f4", height = "40vh")),
#         column(4, plotlyOutput("f5", height = "40vh")),
#         column(4, plotlyOutput("f6", height = "40vh"))
#       )
#     )
#   )
# )

# *** Crear la aplicación Shiny -----------------------------------------------
# shinyApp(ui = ui, server = server)
