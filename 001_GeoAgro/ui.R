# Load the ggplot2 package which provides
# the 'mpg' dataset.

mycolors <- c("Red", "Orange", "Green", "Blue")

source("script/code001.R")
library(ggplot2)
# sample(unique(BASE$Campo), 1)
fluidPage(
  titlePanel("Campos Geolocalizados"),
  
 fluidRow(
   column(6, 
          fluidRow(leafletOutput("mymap01", width = "700", height = 400)), 
          p(),
          fluidRow(leafletOutput("mymap02", width = "700", height = 400))
          ),
   column(6,
          fluidRow(
            column(4, 
                   radioButtons('hito_elegido', 'Hito:', choices = detalle_hitos, selected = detalle_hitos[1]), br(),
                   uiOutput("selector_interno02")),
            column(4, selectizeInput('campo', 'Campo:', choices = unique(BASE$Campo), selected = unique(BASE$Campo)[2] )),
            column(4, uiOutput("selector_interno01"))
          ),
          fluidRow(DT::dataTableOutput("table02"))
   )
   ),
  p(),

 
  

br(),
br(), 
 titlePanel("Basic DataTable"),
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("campo_filtro",
                       "Campo:",
                       c("All",
                         unique(as.character(BASE$Campo))))
    ),
    column(4,
           selectInput("lote_filtro",
                       "Lotes:",
                       c("All",
                         unique(as.character(BASE$Lotes))))
    ),
    column(4,
           selectInput("hito01_filtro",
                       "Hito 01:",
                       c("All",
                         unique(as.character(BASE$"Hito 1 –Licencias"))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table01")
)