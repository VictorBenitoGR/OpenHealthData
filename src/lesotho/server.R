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
}
