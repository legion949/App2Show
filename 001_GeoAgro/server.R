# Load the ggplot2 package which provides
# the 'mpg' dataset.
source("script/code001.R")

library(ggplot2)
library(DT)



function(input, output, session) {
  
  # Filter data based on selections
  output$table01 <- DT::renderDataTable(DT::datatable({
    data <- BASE[,-ncol(BASE)]
    if (input$campo_filtro != "All") {
      data <- data[data$Campo == input$campo_filtro,]
    }
    if (input$lote_filtro != "All") {
      data <- data[data$Lotes == input$lote_filtro,]
    }
    if (input$hito01_filtro != "All") {
      data <- data[data$"Hito 1 –Licencias" == input$hito01_filtro,]
    }
    data
  }) %>%
    formatStyle(columns = colnames(BASE)[-length(colnames(BASE))],
               # target = 'row',
                backgroundColor = styleEqual(
                  c(as.character(as.vector(as.matrix(BASE_COLORS[,3])))), 
                  c(as.character(as.vector(as.matrix(BASE_COLORS[,4])))))))
  
  # Filter data based on selections
  output$table02 <- DT::renderDataTable(DT::datatable({
    
    if(!is.null(input$hito_elegido)) if(!is.null(input$categorias_hito)){
    
    columnas_elegidas <- c(colnames(BASE)[c(1:5)], input$hito_elegido)
    data <- as.data.frame(as.matrix(BASE[,columnas_elegidas]))
    data <- data[data$Campo == input$campo,]
    
    
    este_hito <- as.character(input$hito_elegido)
    categorias_del_hito <- as.character(as.vector(as.matrix(BASE_REF[este_hito])))
    
 
    categorias_habilitadas <- na.omit(as.character(as.vector(input$categorias_hito)))
    
    dt_habilitadas <- rep(F, length(categorias_del_hito))
    for(k in 1:length(dt_habilitadas)) dt_habilitadas[k] <- sum(categorias_del_hito[k] == categorias_habilitadas) == 1

   dt_canceladas <- !dt_habilitadas
   
   categorias_canceladas <- categorias_del_hito[dt_canceladas]
   cat("categorias_canceladas: ", categorias_canceladas, "\n")   
   cat("dt_canceladas: ", dt_canceladas, "\n")  
   

   if (sum(dt_canceladas) > 0) {
     recorte <- unique(unlist(strsplit(categorias_canceladas, "[(]"))[c(T,F)])
     aver2 <- rep(NA, length(recorte))
   

     for (r in 1:length(recorte)){

       metralla <- strsplit(recorte[r], "")[[1]]
       metralla <- metralla[-length(metralla)]
       metralla <- paste0(metralla, collapse = "")
       aver2[r] <- metralla

       remove(metralla)
     }
     cat("aver2: ", aver2, "\n") 
     
     cat("este_hito: ", colnames(data) == este_hito, "\n") 
     for (h in 1:length(aver2)){
       
       dt_sacar <- !data[,este_hito] == aver2[h]
       cat("dt_sacar: ", dt_sacar, "\n") 
       data <- data[dt_sacar,]
       
     } 
   } 
    
  
  
    data
    
    } else return(NULL)
  }) %>%
  formatStyle(columns = input$hito_elegido,
              target = 'row',
              backgroundColor = styleEqual(
                c(as.character(as.vector(as.matrix(BASE_COLORS[,3])))), 
                c(as.character(as.vector(as.matrix(BASE_COLORS[,4])))))))
  
  
  
  # Selector de uno de los Lotes de un Campo en particular
  output$selector_interno01 <- renderUI({
    #  cities <- getNearestCities(input$lat, input$long)
    
    este_campo <- input$campo
    
    dt_lotes <- BASE$Campo == este_campo
    este_id <- BASE$id[dt_lotes][1]
    numero_orden_lotes <- BASE$ORDEN[dt_lotes]
    nombre_lotes <- BASE$Lotes[dt_lotes]
    nombre_lotes
    
    radioButtons("lote", "Lotes del Campo:", nombre_lotes, selected = nombre_lotes[1])
  })
  
  
  # Selector de las categorias de Hito, dentro del Hito seleccionado
  output$selector_interno02 <- renderUI({
    #  cities <- getNearestCities(input$lat, input$long)
    
    este_hito <- input$hito_elegido
    
    categorias_del_hito <- as.character(as.vector(as.matrix(BASE_REF[este_hito])))
    arreglo_nombre <- paste0("Categorías del ", input$hito_elegido, ":")
    checkboxGroupInput("categorias_hito", arreglo_nombre, categorias_del_hito, selected = categorias_del_hito)
  })
  
  # obs <- observe({    
  #   cat(input$lote, '\n')
  # })
  
  
  
  
  
  
  
  
  
  
  output$mymap01 <- renderLeaflet({
    
    este_campo <-  input$campo
    # este_campo <- "El Amanecer"
    dt_campo <- BASE$Campo == este_campo
    este_id <- BASE$id[dt_campo][1]
    orden_inicial <- c(1:length(dt_campo))
    
    dt_lotes <- BASE$id == este_id
    seleccion_orden <- orden_inicial[dt_lotes]

    
    estos_lotes <- seleccion_orden
    estos_nombres <- BASE$Lotes[estos_lotes]
    orden_interno <- 1:length(estos_lotes)
    
    colores <- c("orange", "green", "blue")
    
    #este_hito <- "Hito 1 –Licencias"  
    este_hito <- input$hito_elegido
    estado_campo <- as.vector(as.matrix(BASE[este_hito]))[dt_lotes]
    colores <- rep(NA, length(estado_campo))
    
    for (k in 1:length(estado_campo)){

     dt_color <- as.vector(as.matrix(BASE_COLORS[,3])) == estado_campo[k]
     colores[k] <- as.vector(as.matrix(BASE_COLORS[,2]))[dt_color]
     remove(dt_color)
    }
    
   
    
    
    Sr <- list()
    Srs <- list()
    
    
    for (h in 1:length(orden_interno)) Sr[[h]] <- Polygon(coordenadas_finales[[seleccion_orden[h]]])
    
    # Sr1 = Polygon(coordenadas_finales[[1]])
    # Sr2 = Polygon(coordenadas_finales[[2]])
    # Sr3 = Polygon(coordenadas_finales[[3]])
    
    for (h in 1:length(orden_interno)) Srs[[h]] = Polygons(list(Sr[[h]]), estos_nombres[h])
    
    
    # m <- leaflet() %>%
    #   addTiles() %>%
    
    SpP = SpatialPolygons(Srs, orden_interno)
    # leaflet(height = "300px") %>% 
    leaflet() %>% 
      addTiles() %>%
    # addPolygons(data = SpP, popup ="Un punto de todo el campo",   col =colores)
      addPolygons(data = SpP, popup = paste0("Lote: ",estos_nombres),   col = colores)
  })
  
 

  
  output$mymap02 <- renderLeaflet({

   if (!is.null(input$lote)) {
     
    nombre_este_lote <- input$lote

    dt_lote <- BASE$Lotes == nombre_este_lote
    numero_orden_lote <- BASE$ORDEN[dt_lote]
    colores <- c("orange", "green", "blue")
    orden_interno <- 1:length(nombre_este_lote)
    
    Sr <- list()
    Srs <- list()
    
    
    Sr[[1]] <- Polygon(coordenadas_finales[[numero_orden_lote]])
    
    # Sr1 = Polygon(coordenadas_finales[[1]])
    # Sr2 = Polygon(coordenadas_finales[[2]])
    # Sr3 = Polygon(coordenadas_finales[[3]])
    
    Srs[[1]] <- Polygons(list(Sr[[1]]), nombre_este_lote)
    
    
    SpP = SpatialPolygons(Srs, orden_interno)
    # leaflet(height = "300px") %>% 
    leaflet() %>% 
      addTiles() %>%
      addPolygons(data = SpP, popup =paste0("Lote: ", nombre_este_lote),   col =colores[1])
    
    } else return(NULL)
  })
  
}