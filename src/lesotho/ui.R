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

# *** Interfaz de usuario -----------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    #myImage img {
      object-fit: contain;
      max-height: 100%;
    }
    .center-vertically {
      display: flex;
      align-items: center;
      justify-content: center;
    }
  "))),
  titlePanel(tags$b("Queen Mamohato Memorial Hospital")),
  fluidRow(
    column(3, plotlyOutput("a1", height = "40vh")),
    column(3, plotlyOutput("a2", height = "40vh")),
    column(3, plotlyOutput("b1", height = "40vh")),
    column(3, plotlyOutput("b2", height = "40vh"))
  ),
  fluidRow(
    column(4, div(class = "center-vertically", div(id = "myImage", img(
      src = "img/c1.jpg",
      width = "100%"
    )))),
    column(4, plotlyOutput("e1", height = "40vh")),
    column(4, plotlyOutput("h1", height = "40vh"))
  )
)
