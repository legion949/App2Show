
##################
# LOAD libreries #
##################
source("lib.R")

##################
# LOAD prefiles #
##################
source("prefiles.R")

shinyUI(
  fluidPage(
    tags$head(includeHTML("www/google-analytics-ThugChem.html")),
  useShinyjs(),
  
  # 0001 - 3 de 3 - Deteccion de lenguaje por defecto....
# # "This is your browser language",
# #  textOutput('your_lang'),
  ##############################################
  
  
  # load page layout
  dashboardPage(title = "ThugChem - EasyChem", 
    skin = "green",
    
    # Encabezado
   # dashboardHeader(title="Biodiversity in National Parks", titleWidth = 300),
   # dashboardHeader(title = details[1,2], titleWidth = 300),
   dashboardHeader(title = textOutput("texto01_01") , titleWidth = 300),
    # Lateral
    dashboardSidebar(width = 300,
                     sidebarMenu( 
                       
                      # Selector de idiomas... 
                      uiOutput("nn_language"),
                      
                      # Primer item: Estequimetría
                     #  menuItem(textOutput("texto02_01"), tabName = "tab_interno01", icon = icon("home")),
                     menuItem(textOutput("texto02_01"), tabName = "tab_interno01", icon = icon("home")),
                     
                      
                     menuItem("instrucciones", tabName = "tab_interno02", icon = icon("thumbtack")),
                     menuItem("Contacto", tabName = "tab_interno03", icon = icon("thumbtack"))
                       )
                     
    ), # end dashboardSidebar
  
    
    # Cuerpo
    dashboardBody(
      
      tabItems(
        
     #   tabItem(tabName = "tab_interno01", uiOutput("chemUI_01")),
        tabItem(tabName = "tab_interno01",
              
                fluidRow(
                  # Familia Quimica
                  column(2, uiOutput("rec_chemfam")),
              
                  # NumAtom y Simbolo - Seleccion 1  y # Valencia de Seleccion 1
                  column(5, uiOutput("rec_mix1"), br(), uiOutput("rec_valencia1")),
              
                  # NumAtom y Simbolo - Seleccion 2  y # Valencia de Seleccion 2
                  column(5, uiOutput("rec_mix2"), br(), uiOutput("rec_valencia2")),
                  br()
                ),
              
              
              textOutput("texto06_01"),
        # #     tableOutput("tabla1"),
        # #     textOutput("texto07_01"),
        # #     tableOutput("tabla2"),
       
        fluidRow(
          column(8,  uiOutput("control")),
          column(4, uiOutput("my_helper"))
              ),
              plotOutput("graf1"),
  
              br(),
              br(),
              tableOutput("tabla3"),
              tableOutput("tabla4"),
      # #  tableOutput("tabla5"),
        br(), br(), br()),
        tabItem(tabName = "tab_interno02", "Seleccione un elemento químico de la lista y una valencia.", br(), 
                                            "Se desarrollará paso a paso la estequimetría química.", br(),
                                            "Podrá visualizar la resolución con diferente grando de ayuda.", br(),
                                            "Las explicaciones que se dan detallan orientan al estudiente a
                                             entender cómo se está procediento en cada paso.", br(),
                                            "La resolución finaliza con el nombre correcto en las 3 nomenclaturas
                                             químicas más utilizadsa: UIPAC, Clásica y Numeral Stock.", br(),
                                            "La página se encuentra en proceso de traducción a diferentes idiomas."),
      tabItem(tabName = "tab_interno03", "Correo: d.eliaspanigo@gmailcom") 
        
      )
    ) # end dashboardBody
    
  )# end dashboardPage
  
 ) # End fluidpage()
) # End ShinyUI