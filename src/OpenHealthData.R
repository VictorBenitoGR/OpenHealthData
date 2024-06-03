# * OpenHealthData
# * https://github.com/VictorBenitoGR/OpenHealthData

# *** Packages ----------------------------------------------------------------

library(ggplot2)

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
  labs(y = "Tiempos de cirugía (minutos)", x = "Semana") +
  scale_fill_manual(values = c(
    "Pre-acreditación" = "#1a39b6", "Post-acreditación" = "#009543"
  )) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1)

# Exportar
ggsave("assets/a1.jpg", a1, width = 10, height = 6, units = "in", dpi = 600)

# *** Medidas de control de infecciones ---------------------------------------

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

# *** Visitas ambulatorias ----------------------------------------------------

# * B1 Visitas ambulatorias
# Descripción:  Visita = Total de servicios ambulatorios brindados a una sola
#               persona en un solo día (24 horas).
# Criterios:    Cumplimiento de protocolos del Anexo 18: Medición anual con
#               mínimo 258.000
# Objetivo pre-acreditación:  Depende de la oferta.
# Objetivo post-acreditación: Mínimo anual: xxx,xxx pacientes ambulatorios.
#                             Medido por Año de Contrato.
# Medición anual.

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

# *** Satisfacción del paciente y su familia ----------------------------------

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

# *** Auditoría de equipo -----------------------------------------------------

# * E1 Auditoría de equipo
# Descripción:  Cumplimiento de los estándares de servicio.
# Criterios:    Cumplimiento del Anexo 13, Parte A, Sección 13.2.
# Objetivo pre-acreditación:  Cumplimiento ≥ 95%.
# Objetivo post-acreditación: Cumplimiento ≥ 95%.
# Medición trimestral.

# *** Certificación de personal -----------------------------------------------

# * H1 Certificación de personal
# Descripción:  Cumplimiento de los estándares de servicio.
# Criterios:    Cumplimiento del Anexo 13, Parte B, Sección 4.1.
# Objetivo pre-acreditación:  Cumplimiento ≥ 80%.
# Objetivo post-acreditación: Cumplimiento ≥ 90%.
# Medición anual.

# *** ShinyApp interactivo ----------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)

# Crear un ShinyApp interactivo con las gráficas creadas
