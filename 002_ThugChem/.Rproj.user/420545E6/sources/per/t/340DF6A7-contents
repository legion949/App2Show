
options(encoding="utf-8")
##################
# LOAD libreries #
##################
source("lib.R")


##################
# LOAD prefiles #
##################
source("prefiles.R")

# 0001 - 1 de 3 - Deteccion de lenguaje por defecto....
jscode <- "var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('mydata', language);
console.log(language);"


shinyServer(function(input, output ,session) {
  
  # 0001 - 2 de 3 - Deteccion de lenguaje por defecto....
  runjs(jscode)
  output$your_lang <- renderPrint(input$mydata)
  #######################################################
  
  
  # 
  default_language <- reactive({
    if (!is.null(input$mydata)) {
    
    partes <- strsplit(as.character(input$mydata), "-")
  
    unlist(partes)[1]  
    }
    })
    
  
 
  
  
  output$nn_language <- renderUI({
    
    idiomas <- c("English", "Español", "Francais")
    codificacion <- c("en", "es", "fr")
    names(codificacion) <- idiomas
    
    dt <- codificacion == default_language()
    
    selectInput(inputId = "language_selector", 
                label = "Language", 
                choices = codificacion,
                selected = codificacion[dt]
                )
    
  })
  
  
  selected_language <- reactive({ 
    
    if(is.null(input$language_selector)) mi_lenguaje <- default_language() else mi_lenguaje <- input$language_selector
    
    mi_lenguaje <- as.character(mi_lenguaje)
    mi_lenguaje
    

    
  })
  
  
  # Todos los textos
  
  # Titulo de la App 
  # idShiny01
  output$texto01_01 <- renderText({ 
    
    dt <- idShiny == "idS01_01"
    details[dt,input$language_selector]

    })  
  
  
  # Munu Lateral - Opcion: "Estequiometria"
  # idShiny02
  output$texto02_01 <- renderText({ 
    
    dt <- idShiny == "idS02_01"
    
    details[dt,input$language_selector]
    
  })  
  
  
  
  # # Cuerpo - Menu de Seleccion: Familia Quimica
  # idShiny 03
  
  # Familia Quimica
  output$rec_chemfam <- renderUI({
    
    if (!is.null(input$language_selector)) {
      
    # Texto: "Familia Quimica"
    search.me <- "idS03_01"
    dt3 <- idShiny == search.me 
    texto03_01 <- details[dt3,input$language_selector]
    remove(search.me)
    
    
    # Eleccion de Familia Quimica
    search.me <- "idS04"
    dt4 <- grep(search.me, idShiny)
    my_choices <- details[dt4,input$language_selector]
    remove(search.me)
  
    
    
    radioButtons(inputId = "chemfam", 
                 label = texto03_01,
                 choices = my_choices,
                 selected = my_choices[1])
    
    } else return(NULL)
    
    #  } else return(NULL)
  })
  
  
  
  chemfam <- reactive({
    
    # El usuario eligio una familia quimica...
    # pero esa familia puede estar en varios idiomas.
    # Entonces, para poder usarlo en otras partes de la programacion
    # de manera mas sencilla, vamos a tener en numero de la eleccion que 
    # hizo, y asi reutilizarlo.

  input$chemfam
    
    
    
  })
  
  num_chemfam <- reactive({
    
    # El usuario eligio una familia quimica...
    # pero esa familia puede estar en varios idiomas.
    # Entonces, para poder usarlo en otras partes de la programacion
    # de manera mas sencilla, vamos a tener en numero de la eleccion que 
    # hizo, y asi reutilizarlo.
    
    if (!is.null(selected_language())) {
      if (!is.null(input$chemfam)) {
        
    # Eleccion de Familia Quimica
    search.me <- "idS04"
    dt4 <- grep(search.me, idShiny)
    my_choices <- as.character(details[dt4,selected_language()])
    
    num_orden <- c(1:length(my_choices))
    
    dt <- my_choices ==  input$chemfam
    
    este_num_orden <- num_orden[dt]
    
   
    
    if(length(este_num_orden) == 0) este_num_orden <- NULL
    
    este_num_orden
    
    } else return(NULL)
    } else return(NULL)
  })
  
  
  simbolos_quimicos <- reactive({
    
    if (!is.null(input$language_selector)) {
      
      # La 2da columna de todas las tablas periodicas de todos los idiomas
      # posee el detalle del simbolo del elemeneto quimico.
      TabPeriod[[input$language_selector]][,2]
      
    } else return(NULL)
    
    
  })
  
  nombres_quimicos <- reactive({
    
    if (!is.null(input$language_selector)) {
      
      # La 3ra columna de todas las tablas periodicas de todos los idiomas
      # posee el detalle del nombre del elemento quimico
      TabPeriod[[input$language_selector]][,3]
      
    } else return(NULL)
    
    
  })
  
  numero_atomico <- reactive({
    
    if (!is.null(input$language_selector)) {
      
      # La 4ta columna de todas las tablas periodicas de todos los idiomas
      # posee el detalle del numero atomico de cada elemento quimico
      TabPeriod[[input$language_selector]][,4]
      
    } else return(NULL)
    
    
  })
  
  
  
  fusion_info <- reactive({
    
    if (!is.null(input$language_selector)) {
     
      # La 4ta columna de todas las tablas periodicas de todos los idiomas
      # posee el detalle del numero atomico de cada elemento quimico
      paste0(numero_atomico(), " - ", nombres_quimicos(), " (", simbolos_quimicos(), ")")
      
    } else return(NULL)
    
    
  })
 
  valencias <- reactive({
    
    if (!is.null(input$language_selector)) {
      
      # La 10ma columna de todas las tablas periodicas de todos los idiomas
      # posee el detalle de la valencia de cada elemento quimico separadas
      # por un punto y coma.
      strsplit(as.character(TabPeriod[[input$language_selector]][,10]), ";")
      
    } else return(NULL)
    
    
  })
  
  
  # Menu de eleccion del 1er elemento
  output$rec_mix1 <- renderUI({
    
    if (!is.null(input$language_selector)) {
      
      # Texto: "Núm Atóm - Elemento (Símbolo)"
      search.me <- "idS05_01"
      dt <- idShiny == search.me 
      texto05_01 <- details[dt,input$language_selector]
      remove(search.me)
      
      cantidad_elementos <- length(fusion_info())
      sorteo <- sample(1:cantidad_elementos, 1)
      
      
    
    # Detalle1 
    selectInput(inputId = "mix1", 
                label = texto05_01,
                choices = fusion_info(),
                selected = fusion_info()[sorteo])
    
    } else return(NULL)
  })
  
  
  # Menu eleccion valencia del 1er elemento
  output$rec_valencia1 <- renderUI({
    
    if (!is.null(input$mix1)){

      # Texto: "Valencia"
      search.me <- "idS05_02"
      dt <- idShiny == search.me 
      texto05_02 <- details[dt,input$language_selector]
      remove(search.me)
      
      # Al elemento seleccionado le detectamos
      # su posicion en la tabla periodica segun el
      # número atomico, y separamos sus valores de valencia.
      
      dt <- fusion_info() == input$mix1
      esta_pos <- numero_atomico()[dt]
      estas_valencias <- valencias()[[esta_pos]]
      
      
      # Detalle1 
      
      radioButtons(inputId = "valencia1", 
                   label = texto05_02,
                   choices = estas_valencias,
                   selected = estas_valencias[1])
    } else return(NULL)
  })
  
  
  # Menu de eleccion del 1er elemento
  output$rec_mix2 <- renderUI({
    

    if (!is.null(input$language_selector)) {
      if (!is.null(num_chemfam())) if (num_chemfam()>=5) {

      # Texto: "Núm Atóm - Elemento (Símbolo)"
      search.me <- "idS05_03"
      dt <- idShiny == search.me 
      texto05_03 <- details[dt,input$language_selector]
      remove(search.me)
      
      cantidad_elementos <- length(fusion_info())
      sorteo <- sample(1:cantidad_elementos, 1)
      
      
      
      # Detalle1 
      selectInput(inputId = "mix2", 
                  label = texto05_03,
                  choices = fusion_info(),
                  selected = fusion_info()[sorteo])
      
      } else return(NULL)
    } else return(NULL)
  })
  
  
  # Menu eleccion valencia del 1er elemento
  output$rec_valencia2 <- renderUI({
    
    if (!is.null(input$mix2)){
      if (!is.null(input$language_selector)) {
        if (!is.null(num_chemfam())) if (num_chemfam()>=5) {
      
            
      # Texto: "Valencia"
      search.me <- "idS05_04"
      dt <- idShiny == search.me 
      texto05_04 <- details[dt,input$language_selector]
      remove(search.me)
      
      # Al elemento seleccionado le detectamos
      # su posicion en la tabla periodica segun el
      # número atomico, y separamos sus valores de valencia.
      
      dt <- fusion_info() == input$mix2
      esta_pos <- numero_atomico()[dt]
      estas_valencias <- valencias()[[esta_pos]]
      
      
      # Detalle1 
      
      radioButtons(inputId = "valencia2", 
                   label = texto05_04,
                   choices = estas_valencias,
                   selected = estas_valencias[1])
    } else return(NULL)
      } else return(NULL)
    } else return(NULL)
  })
  
  # Detect para el mix1
  num_atom1 <-  reactive({  
    
    if (!is.null(input$mix1)) {
            dt <- as.numeric(as.character(strsplit(input$mix1, " ")[[1]][1]))
            dt
            
    } else return(NULL)
            })
  
  # Detect para el mix2
  num_atom2 <-  reactive({  
    if (!is.null(input$mix1)) {
    dt <-  as.numeric(as.character(strsplit(input$mix2, " ")[[1]][1]))
    dt
    } else return(NULL)
  })
  
  
  # Valencias de cada elemento
  valencia1 <-  reactive({  
    
    if (!is.null(input$valencia1)) {
      
      
      
      esta_valencia <- as.numeric(as.character(input$valencia1))
      esta_valencia
      
    } else return(NULL)
  })
  
  # Valencias de cada elemento
  valencia2 <-  reactive({  
    
    if (!is.null(input$valencia2)) {
      
 
      
      esta_valencia <- as.numeric(as.character(input$valencia2))
      esta_valencia
      
    } else return(NULL)
  })
  
  
  ### ********** SEGUIR ACAAAAAAAAAAAAAAAA

  PasosResolucion <-   reactive({

    if (!is.null(num_atom1())) 
      if (!is.null(valencia1())) 
        if (!is.null(TabPeriod)) 
          if (!is.null(num_chemfam())) # 
            ResolGeneral(input_numfam = num_chemfam(),
                         input_num_atom1 = num_atom1(), input_valencia1 = valencia1(),
                         input_num_atom2 = NULL, input_valencia2 = NULL,
                         input_language_interno =  "es", 
                         input_language_optativo = NULL,
                         input_tabla = TabPeriod)

           
          # ResolOxidos(input_num_atom1 = num_atom1(), input_valencia1 = valencia1(),
          #       input_language = "es", input_tabla = TabPeriod)


  })


  PasosLaTeX <-   reactive({
    
    if(!is.null(PasosResolucion()))
      if (dim(PasosResolucion())[1] > 1) 
        if (dim(PasosResolucion())[2] > 1) 
          LaTeXGeneral(input_numfam = num_chemfam(), 
                       input_ejemplo = PasosResolucion(),
                       input_language_interno = "es", 
                       input_language_optativo = "es",
                       input_tabla = TabPeriod)
    
    
   
  })
  
  
  CantidadesPaso <-   reactive({
    
    if(!is.null(selected_language()))
        if (!is.null(TabPeriod)) 
          if (!is.null(num_chemfam()))
            if(!is.null(PasosResolucion()))
              if (dim(PasosResolucion())[1] > 1) 
                if (dim(PasosResolucion())[2] > 1) 
                  if(!is.null(imagen_elegida())) { 
                    
                    
                    CantidadesGeneral(input_numfam = num_chemfam(),
                                      input_ejemplo = PasosResolucion(), 
                                      input_language_interno = "es", 
                                      input_language_optativo = selected_language(), 
                                      input_details = details, 
                                      input_searchme = "idS12",
                                      input_tabla = TabPeriod,
                                      input_paso = imagen_elegida())
                    
                  
                  }
                  
                  
                  
                  
                
    
    # ResolOxidos(input_num_atom1 = num_atom1(), input_valencia1 = valencia1(),
    #       input_language = "es", input_tabla = TabPeriod)
    
    
  })
  
  Nomenclatura <- reactive({

    
    if (!is.null(num_chemfam())) 
      if (is.numeric(num_chemfam())) 
        if (!is.null(TabPeriod)) 
          if(!is.null(selected_language())) 
            if (!is.null(num_atom1())) 
              if (is.numeric(num_atom1()))
                if (!is.null(valencia1())) 
                  if (is.numeric(valencia1())) 
         
            
               NomenGeneral(input_numfam = num_chemfam(), 
                           input_num_atom1 = num_atom1(),
                           input_valencia1 = valencia1() ,
                           input_num_atom2 = NULL,
                           input_valencia2 = NULL ,
                           input_language_interno = "es", 
                           input_language_optativo = "es", 
                           input_tabla = TabPeriod)
          
           
                  
        
              
         
      
                  
           
  })
  
  # Titulo de la App 
  # idShiny01
  output$texto06_01 <- renderText({ 
    
    dt <- idShiny == "idS06_01"
    details[dt,input$language_selector]
    
  })  
  
 
  output$tabla1 <- renderTable({

   PasosResolucion()
   
  
  })
  
  
  
  output$texto07_01 <- renderText({ 
    
    dt <- idShiny == "idS07_01"
    details[dt,input$language_selector]
    
  })  
  
  output$tabla2 <- renderTable({
    
    PasosLaTeX()
          
         
    
    
  })
  
  
  output$control <- renderUI({
    
    if (!is.null(input$language_selector)) {
      
    # Leyenda en botones...
    search.me <- "idS08"
    dt8 <- grep(search.me, idShiny)
    my_choices8 <- details[dt8,input$language_selector]
    remove(search.me)
    
    
    # Leyenda en slider
    search.me <- "idS11"
    dt11 <- grep(search.me, idShiny)
    my_choices11 <- details[dt11,input$language_selector]
    remove(search.me)
    
 
    
   # fluidPage(
      fluidRow(
        column(7, 
               br(),
        actionButton(inputId = "b1", label = my_choices8[1]),
        actionButton(inputId = "b2", label = my_choices8[2]),
        actionButton(inputId = "b3", label = my_choices8[3]),
        actionButton(inputId = "b4", label = my_choices8[4]),
        actionButton(inputId = "b5", label = my_choices8[5])
      ),
        column(5, sliderInput(inputId = "slider", label = my_choices11[1], min=0, max=10, value=1, step=1)
      )
    )
   # )
      
    }
    
    
  })
  
  output$my_helper <- renderUI({
   
    
    if (!is.null(input$language_selector)) { 
    # Leyenda en el helper
    search.me <- "idS13"
    dt13 <- grep(search.me, idShiny)
    my_choices13 <- details[dt13,input$language_selector]
    remove(search.me)
    
    #Opciones del helper
    search.me <- "idS14"
    dt14 <- grep(search.me, idShiny)
    my_choices14 <- details[dt14,input$language_selector]
    remove(search.me)
    
    
    fluidRow(
      column(2),
      column(6, sliderInput(inputId = "helper", label = my_choices13[1], min=0, max=(length(my_choices14)-1), value=3, step=1)
      )
    )
    
    }
  })
  
  
  
  # Inititating reactive values, these will `reset` for each session
  # These are just for counting purposes so we can step through the questions
  # Cantidad de pasos
  vector_slider <- reactive({
    
    if (!is.null(PasosResolucion())) {
      pasos_agregados <- 2
      max_paso <- nrow(PasosResolucion()) + pasos_agregados
      
      seq(1, max_paso, by = 1)
    } 

  })
  
  
  
  # Imagen elegida para mostrar "Paso a Paso Imagenes"
  imagen_elegida <- reactiveVal(1)
  
  
  
  
  
  observe({
    
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    if (!is.null(vector_slider()))  if (!is.null(imagen_elegida())){
      updateSliderInput(session, "slider", value = imagen_elegida(),
                        min = min(vector_slider()), max = max(vector_slider()), step = 1)
    }
  })
  
  if (TRUE) {
    
    
    
    # Si aprieta el boton "Inicio"
    observeEvent(input$b1, {
      imagen_elegida(1)
      
    })
    
    # Si aprieta el bon "Prev"
    observeEvent(input$b2, {
      if (imagen_elegida() > 1) imagen_elegida(imagen_elegida() - 1)
      
      
    })
    
    # Si aprieta el bon "Next"
    observeEvent(input$b3, {
      
      if (imagen_elegida() < length(vector_slider()))  imagen_elegida(imagen_elegida() + 1)
      
      
      
      
    })
    
    # Si aprieta el boton "Resolucion"
    observeEvent(input$b4, {
      imagen_elegida(max(vector_slider())-1)
      
    })
    
    # Si aprieta el boton "Nomenclatura"
    observeEvent(input$b5, {
      imagen_elegida(max(vector_slider()))
      
    })
    
    # Si cambia el Slider
    observeEvent(input$slider, {
      imagen_elegida(input$slider)
      
    })
    
    
  }
  
  

  output$graf1 <- renderPlot({
    
    # Controlador 000...
    # Este controlador 000, despues hay que fletarlo...
    # Lo puse para poder poner un grafico que diga "Proximamente..." en las
    # partes que todavia no hice...
    if(!is.null(num_chemfam()))
      if(length(num_chemfam()) > 0) {
   
        
    # Controlador 000-01
        if (num_chemfam() == 1) {
          
           
          if(!is.null(selected_language()))
            if(!is.null(PasosResolucion()))
              if (dim(PasosLaTeX())[1] > 1) 
                if (dim(PasosLaTeX())[2] > 1) 
                  if(!is.null(imagen_elegida()))
                    if(!is.null(vector_slider()))
                      if(!is.null(input$helper))
          if(imagen_elegida() < length(vector_slider())) {
            
            
            input_numfam <- num_chemfam()
            input_ejemplo <- PasosResolucion()
            input_language_interno <- "es"
            input_language_optativo <-selected_language()
            input_paso <- imagen_elegida()
            input_total <- nrow(input_ejemplo)
            input_subtitulos <- subtitulos
            input_tabla <- TabPeriod
            
            mis_subtitulos <- SubtitulosGeneral(
                                  input_numfam = input_numfam,
                                  input_ejemplo = input_ejemplo, 
                                  input_language_interno = input_language_interno, 
                                  input_language_optativo = input_language_optativo, 
                                  input_subtitulos = input_subtitulos, 
                                  input_tabla = input_tabla,
                                  input_paso = input_paso,
                                  input_total = input_total)
          
            
            # Grafico
            GrafGeneral(input_numfam = num_chemfam(), 
                        input_latex = PasosLaTeX(),
                        input_paso = imagen_elegida(),
                        input_language_interno = "es",
                        input_language_optativo = NULL,
                        input_tabla = TabPeriod,
                        input_color_fondo = "orange",
                        input_color_general = "black",
                        input_color_especifico = "blue",
                        input_color_ecuacion = "black",
                        input_color_signo = "black",
                        input_color_paso = "blue")
          
            
            
          # Paso...
          if(input$helper >= 1) text(mis_subtitulos[[1]][1], mis_subtitulos[[1]][2], mis_subtitulos[[1]][3], pos=1, cex=1.3, adj = c(0.5, 0.5)) 
          
          # Frase general...
          if(input$helper >= 2)text(mis_subtitulos[[2]][1], mis_subtitulos[[2]][2], mis_subtitulos[[2]][3], pos=1, cex=1.3, adj = c(0.5, 0.5)) 
          
          # Frase especifica...
          if(input$helper >= 3)text(mis_subtitulos[[3]][1], mis_subtitulos[[3]][2], mis_subtitulos[[3]][3], pos=1, cex=1.3, col="blue", adj = c(0.5, 0.5)) 
          
          
       #   text(gps_x2[input_paso], gps_y2[input_paso], salida_sub2, pos=1, cex=1.3, adj = c(0.5, 0.5))
          
          # Frase especifica...
       #   text(gps_x3[input_paso], gps_y3[input_paso], salida_sub3, pos=1, cex=1.3, adj = c(0.5, 0.5), col="blue")
          
          } # Fin Parte5
            ###########################################################
          
          
          # Caso 2... Si le tiramos por la cabeza la nomenclatura...
          else if(imagen_elegida() == length(vector_slider())){
            
            if (selected_language() == "es") if(!is.null(Nomenclatura())) {
            
              # Pintada de fondo
              plot(c(0:30), axes=F, col="white", xlab=" ", ylab=" ")
              rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "orange")
              
              # Detalles de nomenclatura
              nomenclatura_completa <- Nomenclatura()
              
              # Coordenadas
              x_nomen <- c( 4,  4,  4,  4, 12)
              y_nomen <- c(25, 19, 13,  7,  7)
              
              
              nombre_uipac <- paste0(names(nomenclatura_completa)[1],": ", nomenclatura_completa[1])
              nombre_clasico <- paste0(names(nomenclatura_completa)[2],": ", nomenclatura_completa[2])
              nombre_stock <- paste0(names(nomenclatura_completa)[3],": ", nomenclatura_completa[3])
              intro_fq <- "Fórmula Química: "
              fq <- nomenclatura_completa[4]
              fq_latex <- nomenclatura_completa[5]
              
              armado_fq  <-   parse(text=paste("text(x_nomen[5], y_nomen[5], 
                                            expression(",nomenclatura_completa[5],"),
                                            col='black', cex=2, pos = 4)", collapse=""))
              
              
              
#              frase4 <- paste0(names(nomenclatura_completa)[1],": ", nomenclatura_completa[1])
              text(x_nomen[1],y_nomen[1], nombre_uipac, cex=2, pos = 4)
              text(x_nomen[2],y_nomen[2], nombre_clasico, cex=2, pos = 4)
              text(x_nomen[3],y_nomen[3], nombre_stock, cex=2, pos = 4)
              text(x_nomen[4],y_nomen[4], intro_fq, cex=2, pos = 4)
              eval(armado_fq)
              
              
            }
              
              if (selected_language() != "es") {
            # Buscamos la opcion correcta...
            search.me <- "idS10_01"
            dt10 <- grep(search.me, idShiny)
            my_choices <- as.character(details[dt10,input$language_selector])
            
            # my_choices <- Nomenclatura()[1]
            
            # Creamos un grafico de aviso...
            
           
            
            
            
            plot(1,1, col="white", axes= F, xlab="", ylab="")
            text(1,1, my_choices)
            } 
          }
          
        } 
   
   
               
        if (num_chemfam() == 2) {
          
          
         
          
          # Buscamos la opcion correcta...
          search.me <- "idS09_01"
          dt10 <- grep(search.me, idShiny)
          my_choices <- as.character(details[dt10,input$language_selector])
          
          # Creamos un grafico de aviso...
          plot(1,1, col="white", axes= F, xlab="", ylab="")
          text(1,1, my_choices)
          
        }             
                   
         
        
                
                      
                    } # Fin Controlador 000
    
  })

  

  output$tabla3 <- renderTable({



    CantidadesPaso()





  })

  
 
  output$tabla4 <- render_tableHTML({
    
    if (!is.null(CantidadesPaso())) {
      
      # Leyenda en botones...
      search.me <- "idS12"
      dt12 <- grep(search.me, idShiny)
      my_choices12 <- details[dt12,input$language_selector]
      remove(search.me)
      
      .afirmativo <- as.character(as.vector(as.matrix(my_choices12[12])))
      .negativo <- as.character(as.vector(as.matrix(my_choices12[13])))
      
    # #f6f6f6
    data <- CantidadesPaso();
    # //Some operations on data
    data %>% 
      tableHTML(rownames = F) %>% 
      add_css_conditional_column(conditional = '==',
                                 value = .negativo,
                                 css = list(c('background-color'),
                                            c('red')),
                                 columns = 1:ncol(data)) %>% 
      add_css_conditional_column(conditional = '==',
                                 value = .afirmativo,
                                 css = list(c('background-color'),
                                            c('green')),
                                 columns = 1:ncol(data)) %>% 
      add_css_conditional_column(conditional = 'between',
                                 between = c(10, 20),
                                 css = list(c('background-color'),
                                            c('lightred')),
                                 columns = 1:ncol(data))
    
    
    }
    
    
  })
  
  
  
  output$tabla5 <- renderTable({
    
    if(!is.null(Nomenclatura())) {
      
      aver <- Nomenclatura()
  
  cat(is.null(aver))
  as.matrix(aver)
    
    }
    
    
  })
  
  # output$chemUI_01 <- renderUI ({ 
  #   
  #   div(
  #     uiOutput("rec_chemfam"),
  #     uiOutput("rec_mix1"),
  #     uiOutput("rec_valencia1"),
  #     uiOutput("rec_mix2"),
  #     uiOutput("rec_valencia2")
  #   
  #   )
  # })  
  
  
  
  
  
# observe(print(default_language()))
  ##################################################
  
  
  
  
  
  
}



) # End shinySever()