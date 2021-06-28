


CantidadesOxidos <-function(input_ejemplo = NULL, 
                       input_language_interno = NULL, 
                       input_language_optativo = NULL, 
                       input_details = NULL, 
                       input_searchme = "idS12",
                       input_tabla = NULL,
                       input_paso = NULL) {
  
  
  # Especificaciones de language interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  
  # Controles
  # # 1) input_ejemplo: no nulo, data.frame o matrix, con filas y columnas
  # # 2) input_language_interno: no nulo, vector, caracter, de longitud uno , y dos caracteres 
  # # 3) input_language_optativo: no nulo, vector, caracter, de longitud uno , y dos caracteres
  # # 4) input_details: no nulo, data.frame o matrix, con filas y columnas
  # # 5) input_searchme: no nulo, vector, caracter, de longitud uno
  # # 6) input_tabla: no nulo, una lista, al menos un objeto en la lista, objeto con dos dimensiones
  # # 7) input_paso: no nulo, vector, numerico, de longitud uno  
  # # 8) Pequenio cambio sobre "input_ejemplo"
  {
    ###
    
    control_OK <- list()
    control_paso <- 0
    seguir <- TRUE
    mensaje <- list()
    
    # # 1) input_ejemplo: no nulo, data.frame o matrix, con filas y columnas
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_ejemplo))           seguir <- FALSE
      if (!is.data.frame(input_ejemplo) && !is.matrix(input_ejemplo))      seguir <- FALSE
      if(length(dim(input_ejemplo)) !=  2)    seguir <- FALSE
      if(dim(input_ejemplo)[1] < 1)    seguir <- FALSE
      if(dim(input_ejemplo)[2] < 1)    seguir <- FALSE
      
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 1 de 8) ### \n",
                                     "Problemas en el argumento: input_ejemplo \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Un data frame o una matriz \n", 
                                     "- Tener 2 dimensiones \n",
                                     "- Tener al menos 1 fila \n",
                                     "- Tener al menos 1 columna \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
      
    } # Fin Control 1)

    
    
    # # 2) input_language_interno: no nulo, vector, caracter, de longitud uno , y dos caracteres.
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_language_interno))        seguir <- FALSE
      if (!is.vector(input_language_interno))     seguir <- FALSE
      if(!is.character(input_language_interno))   seguir <- FALSE
      if(length(input_language_interno) != 1)     seguir <- FALSE
      if(length(strsplit(input_language_interno, "")[[1]]) != 2)  seguir <- FALSE
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 2 de 8) ### \n",
                                     "Problemas en el argumento: input_language_interno \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Vector \n", 
                                     "- Tipo caracter \n",
                                     "- Longitud uno \n",
                                     "- Tener dos caracteres (por ejemplo 'es') \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
    } # Fin Control 2)
    
    
    
    # # 3) input_language_optativo: no nulo, vector, caracter, de longitud uno , y dos caracteres.
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_language_optativo))        seguir <- FALSE
      if (!is.vector(input_language_optativo))     seguir <- FALSE
      if(!is.character(input_language_optativo))   seguir <- FALSE
      if(length(input_language_optativo) != 1)     seguir <- FALSE
      if(length(strsplit(input_language_optativo, "")[[1]]) != 2)  seguir <- FALSE
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 3 de 8) ### \n",
                                     "Problemas en el argumento: input_language_optativo \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Vector \n", 
                                     "- Tipo caracter \n",
                                     "- Longitud uno \n",
                                     "- Tener dos caracteres (por ejemplo 'es') \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      # # 2) input_language_interno: vector, caracter, de longitud uno , y dos caracteres 
      # # 3) input_tabla: lista, cada elemento debe ser un data frame
      
      
      
    } # Fin Control 3)
    
    
    # # 4) input_details: no nulo, data.frame o matrix, con filas y columnas
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_details))           seguir <- FALSE
        if (!is.data.frame(input_details) && !is.matrix(input_details))      seguir <- FALSE
          if(length(dim(input_details)) !=  2)    seguir <- FALSE
            if(dim(input_details)[1] < 1)    seguir <- FALSE
              if(dim(input_details)[2] < 1)    seguir <- FALSE
      
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 4 de 8) ### \n",
                                     "Problemas en el argumento: input_details \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Un data frame o una matriz \n", 
                                     "- Tener 2 dimensiones \n",
                                     "- Tener al menos 1 fila \n",
                                     "- Tener al menos 1 columna \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
      
    } # Fin Control 4)
   
    
    # # 5) input_searchme: no nulo, vector, caracter, de longitud uno
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_searchme))        seguir <- FALSE
        if (!is.vector(input_searchme))     seguir <- FALSE
          if(!is.character(input_searchme))   seguir <- FALSE
            if(length(input_searchme) != 1)     seguir <- FALSE

      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 5 de 8) ### \n",
                                     "Problemas en el argumento: input_searchme \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Vector \n", 
                                     "- Tipo caracter \n",
                                     "- Longitud uno \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
    } # Fin Control 5)
    
    
    # # 6) input_tabla: no nulo, lista, al menos un objeto en la lista, 
    # #                 al menos 1 data frame, con filas y columnas.
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_tabla))        seguir <- FALSE
      if (!is.list(input_tabla))     seguir <- FALSE
      if(length(input_tabla) == 0) seguir <- FALSE
      if(!is.data.frame(input_tabla[[1]]))   seguir <- FALSE
      if(ncol(input_tabla[[1]]) == 0) seguir <- FALSE
      if(nrow(input_tabla[[1]]) == 0)  seguir <- FALSE
      
      # Desicion
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 6 de 8) ### \n",
                                     "Problemas en el argumento: input_tabla \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Lista \n", 
                                     "- Al menos un objeto dentro de la lista \n",
                                     "- Los objetos contenidos deben ser data.frames \n",
                                     "- Cada data frame debe contener al menos una columna \n",
                                     "- Cada data frame debe contener al menos una fila \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
    } # Fin Control 6)
    
    
    
    # # 7) input_paso: no nulo, vector, caracter, de longitud uno
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_paso))        seguir <- FALSE
        if (!is.vector(input_paso))     seguir <- FALSE
          if(!is.numeric(input_paso))   seguir <- FALSE
            if(length(input_paso) != 1)     seguir <- FALSE
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 7 de 8) ### \n",
                                     "Problemas en el argumento: input_paso \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Vector \n", 
                                     "- Numerico \n",
                                     "- Longitud uno \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
    } # Fin Control 7)
    
    # # 8) Pequenio cambio sobre "input_ejemplo".
    {
      ###
      # Sucede que el elemneto Fluor, que tiene simbolo "F" como toma
      # como si fuera un "FALSE". Entonces, hay que ver como lo tomo 
      # y en tal caso asignar un cambio para que no lo tome como valor
      # logico sino como tipo caracter.
      # Del objeto "input_tabla", es la tercer columna la que posee el 
      # simbolo quimico del elemento que participa en el oxido.
      # Tomaremos esa 3er columna y veremos si lo ha comado como "FALSE".
      
      # Hacemos una parada tecnica...
      # Tomamos de la primera fila, la columna 3. Alli esta el simbolo quimico
      # Si hay anotado un "FALSE"... lo cambiamos por "F" tipo caracter.
      
      # Parada tecnica...
      paradita <- input_ejemplo[1,3]
      
      # Si hace falta el cambio...
      if (!is.na(paradita)) if (paradita == "FALSE"){
        
        # Se va a fijar en cada elemento de la matrix para hacer el cambio...
        for (v1 in 1:nrow(input_ejemplo)) for (v2 in 1:ncol(input_ejemplo)){
          
          # Si hace falta, implementa el cambio...
          if (!is.na(input_ejemplo[v1,v2])) if (input_ejemplo[v1,v2] == "FALSE") input_ejemplo[v1,v2] <- "F"
          
        } # Fin doble for...
      } # Fin if paradita == "FALSE"
      
      
      
      
      
      ###  
    } # Fin Control 8)
   
    
    
    ### 
  } # Fin Controles
  ##################################################
  
  
  # #
  
  # Ejecucion de Cantidades en Oxidos
  # # Realizamos la conversion al formato LaTeX para oxidos
  {
    ###  
    
    # Si paso todos los controles se lleva a cabo el ejercicio...
    if (seguir) {
  
      # Parte1: Rotulos y temas de idioma
      {
      ###
        
        # Leyenda en slider
        idShiny_interno <- as.character(as.vector(as.matrix(input_details[,5])))
        dt12 <- grep(input_searchme, idShiny_interno )
        my_choices12 <- as.character(as.vector(as.matrix(details[dt12,input_language_optativo])))
        
        afirmativo <- as.character(as.vector(as.matrix(my_choices12[12])))
        negativo <- as.character(as.vector(as.matrix(my_choices12[13])))
        
        
      ###  
      } # Fin Parte1
      ##################################################
  
      
      # Parte 2: Informacion necesaria
      {
        ###
        
        # General de la Tabla...
        simbolos <- as.character(input_tabla[["es"]][,2])
        tipo <- as.character(input_tabla[["es"]][,7])
        subtipo <- as.character(input_tabla[["es"]][,8])
        num_atom <- input_tabla[["es"]][,4]
        nombres <- as.character(input_tabla[["es"]][,3])
        
        # Especifica del ejercicio...
        # Simbolos quimicos...
        simbolo1 <- as.character(input_ejemplo[1,3])
        simbolo2 <- as.character(input_ejemplo[1,6])
        dt1 <- simbolos == simbolo1
        num_atom1 <- num_atom[dt1]
        nombre1 <- nombres[dt1]
        tipo1 <- tipo[dt1]
        ###  
      } # Fin Parte2
      ##############################################
      
      
      # Parte3: Creacion matrix_cantidades
      {
        ###
        
        
        # Creamos la tabla de cantidades
        #mis_columnas <- c("Elemento", "Cantidades Reactivos", "Cantidades Productos", "Balanceado", "Balanceado Final", "Ecuación Final")
        mis_columnas <- my_choices12[c(1:6)]
        mis_filas <- my_choices12[c(7:9)]
        matrix_cantidades <- as.data.frame(matrix(NA, length(mis_filas), length(mis_columnas)))
        rownames(matrix_cantidades) <- mis_filas
        colnames(matrix_cantidades) <- mis_columnas
        
        ###    
      } # Fin Parte3
      #######################################
      
      
      
      # Parte4: Correccion sobre el paso a desarrollar
      {
        ###
        
        # Estrictamente son 8 los pasos estequimetricos.
        # Pero sucede que tal vez agregue algunos pasos mas.
        # Entonces... Por mas que venga una orden de afuera con
        # pasos por encima de 8, yo muestor el paso 8 que es el ultimo 
        # paso estequimetrico
        n <- input_paso
        if (n > 8) n <- 8
        
        ###    
      } # Fin Parte4
      ################################################
      
      
      
      # Parte5: Ejecucion de matrix_cantidades
      {
        ###
        
        
        # 1) Si el elemento no es un gas noble... y no es el oxigeno
        if (tipo1 != "Gas Noble" && simbolo1 != "O") {
          
          # Coeficientes y subindices...
          coef1 <- as.numeric(as.character(input_ejemplo[n, 2]))
          sub1 <-  as.numeric(as.character(input_ejemplo[n, 4]))
          coef2 <- as.numeric(as.character(input_ejemplo[n, 5]))
          sub2 <-  as.numeric(as.character(input_ejemplo[n, 7]))
          coef3 <- as.numeric(as.character(input_ejemplo[n, 8]))
          sub3 <-  as.numeric(as.character(input_ejemplo[n,10]))
          sub4 <-  as.numeric(as.character(input_ejemplo[n,12]))
          
          # Nuevos detalles...
          suma_elemento_reactivos <- "---"
          suma_elemento_productos <- "---"
          suma_oxigeno_reactivos <- "---"
          suma_oxigeno_productos <- "---"
          nota_elemento_reactivos <- "---"
          nota_elemento_productos <- "---"
          nota_oxigeno_reactivos <- "---"
          nota_oxigeno_productos <- "---"
          
          # Operamos si corresponde... y sumamos
          if (!is.na(coef1) | !is.na(sub1)) suma_elemento_reactivos <- coef1*sub1
          if (!is.na(coef2) | !is.na(sub2)) suma_oxigeno_reactivos  <- coef2*sub2
          if (!is.na(coef3) | !is.na(sub3)) suma_elemento_productos <- coef3*sub3
          if (!is.na(coef3) | !is.na(sub4)) suma_oxigeno_productos  <- coef3*sub4
          
          
          
          # Operamos si corresponde...
          if (!is.na(coef1) | !is.na(sub1)) nota_elemento_reactivos <- paste0(coef1, "*", sub1, " = ", coef1*sub1)
          if (!is.na(coef2) | !is.na(sub2)) nota_oxigeno_reactivos  <- paste0(coef2, "*", sub2, " = ", coef2*sub2)
          if (!is.na(coef3) | !is.na(sub3)) nota_elemento_productos <- paste0(coef3, "*", sub3, " = ", coef3*sub3)
          if (!is.na(coef3) | !is.na(sub4)) nota_oxigeno_productos  <- paste0(coef3, "*", sub4, " = ", coef3*sub4)
          
          # Decision de balanceado del elemento1
          dt1_1 <- identical(suma_elemento_reactivos, suma_elemento_productos)
          if(dt1_1) decision1_1 <- afirmativo else decision1_1 <- negativo
          
          
          # Decision de balanceado del oxigeno
          dt1_2 <- identical(suma_oxigeno_reactivos, suma_oxigeno_productos)
          if(dt1_2) decision1_2 <- afirmativo else decision1_2 <- negativo
          
          # Decision de balanceado general
          dt1_3 <- sum(dt1_1, dt1_2) == 2
          if(dt1_3) decision1_3 <- afirmativo else decision1_3 <- negativo
          
          # Decision de ecuacion final
          esta_ecuacion <- as.character(as.vector(as.matrix(input_ejemplo[n,c(2:ncol(input_ejemplo))])))
          ecuacion_final <- as.character(as.vector(as.matrix(input_ejemplo[nrow(input_ejemplo),c(2:ncol(input_ejemplo))])))
          dt2 <- identical(esta_ecuacion, ecuacion_final) 
          if (dt2) decision2 <- afirmativo else decision2 <- negativo
          
          # Guardamos los calculos del elemento1...
          matrix_cantidades[1,] <- c(simbolo1, nota_elemento_reactivos, 
                                     nota_elemento_productos, decision1_1, decision2, decision2)
          
          matrix_cantidades[2,] <- c(simbolo2, nota_oxigeno_reactivos, 
                                     nota_oxigeno_productos, decision1_2, decision2, decision2)
          
          matrix_cantidades[3,] <- c(my_choices12[10], my_choices12[11], my_choices12[11],
                                     decision1_3, decision2, decision2)
          
        } # Fin 1)
        #####################################################################
        
        
        # 2) Si el elemento es el oxigeno
        if (simbolo1 == "O") {
          
          # Coeficientes y subindices...
          coef1 <- as.numeric(as.character(input_ejemplo[n, 2]))
          sub1 <-  as.numeric(as.character(input_ejemplo[n, 4]))
          coef2 <- as.numeric(as.character(input_ejemplo[n, 5]))
          sub2 <-  as.numeric(as.character(input_ejemplo[n, 7]))
          coef3 <- as.numeric(as.character(input_ejemplo[n, 8]))
          sub3 <-  as.numeric(as.character(input_ejemplo[n,10]))
          sub4 <-  as.numeric(as.character(input_ejemplo[n,12]))
          
          # Nuevos detalles...
          suma_elemento_reactivos <- "---"
          suma_elemento_productos <- "---"
          suma_oxigeno_reactivos <- "---"
          suma_oxigeno_productos <- "---"
          nota_elemento_reactivos <- "---"
          nota_elemento_productos <- "---"
          nota_oxigeno_reactivos <- "---"
          nota_oxigeno_productos <- "---"
          
          # Operamos si corresponde... y sumamos en reactivos...
          if (!is.na(coef1) && !is.na(sub1)) 
            if (!is.na(coef2) && !is.na(sub2)) 
              suma_oxigeno_reactivos <- coef1*sub1 + coef2*sub2
          
          # Operamos si corresponde... y sumamos en reactivos...
          if (!is.na(coef3) && !is.na(sub3)) suma_oxigeno_productos <- coef3*sub3
          
          
          
          # Operamos si corresponde... y anotamos en reactivos...
          if (!is.na(coef1) && !is.na(sub1)) 
            if (!is.na(coef2) && !is.na(sub2)) 
              nota_oxigeno_reactivos  <- paste0("(",coef1, "*", sub1, ") + (", coef2, "*", sub2, ") = ", suma_oxigeno_reactivos)
          
          # Operamos si corresponde... y anotamos en productos...
          if (!is.na(coef3) && !is.na(sub3)) nota_oxigeno_productos <- paste0(coef3, "*", sub3," = ", suma_oxigeno_productos)
          
          
          
          # Decision de balanceado del elemento1
          dt1_1 <- identical(suma_oxigeno_reactivos, suma_oxigeno_productos)
          if(dt1_1) decision1_1 <- afirmativo else decision1_1 <- negativo
          
          
          
          # Decision de balanceado general
          dt1_3 <- dt1_1 
          #dt1_3 <- sum(dt1_1, dt1_2) == 2
          if(dt1_3) decision1_3 <- afirmativo else decision1_3 <- negativo
          
          # Decision de ecuacion final
          esta_ecuacion <- as.character(as.vector(as.matrix(input_ejemplo[n,c(2:ncol(input_ejemplo))])))
          ecuacion_final <- as.character(as.vector(as.matrix(input_ejemplo[nrow(input_ejemplo),c(2:ncol(input_ejemplo))])))
          dt2 <- identical(esta_ecuacion, ecuacion_final) 
          if (dt2) decision2 <- afirmativo else decision2 <- negativo
          
          # Guardamos los calculos del elemento1...
          matrix_cantidades[1,] <- c(simbolo1, nota_oxigeno_reactivos, 
                                     nota_oxigeno_productos, decision1_1, decision2, decision2)
          
          matrix_cantidades[2,] <- matrix_cantidades[2,]
          
          matrix_cantidades[3,] <- c(my_choices12[10], my_choices12[11], my_choices12[11],
                                     decision1_3, decision2, decision2)
          
          # Serruchada...
          matrix_cantidades <- matrix_cantidades[c(1,3), ]
        } # Fin 1)
        #####################################################################
        
        
        
        # 3) Si el elementoes un gas noble... 
        if (tipo1 == "Gas Noble") {
          
          # Coeficientes y subindices...
          coef1 <- as.numeric(as.character(input_ejemplo[n, 2]))
          sub1 <-  as.numeric(as.character(input_ejemplo[n, 4]))
          coef2 <- as.numeric(as.character(input_ejemplo[n, 5]))
          sub2 <-  as.numeric(as.character(input_ejemplo[n, 7]))
          coef3 <- as.numeric(as.character(input_ejemplo[n, 8]))
          sub3 <-  as.numeric(as.character(input_ejemplo[n,10]))
          sub4 <-  as.numeric(as.character(input_ejemplo[n,12]))
          
          # Nuevos detalles...
          suma_elemento_reactivos <- "---"
          suma_elemento_productos <- "---"
          suma_oxigeno_reactivos <- "---"
          suma_oxigeno_productos <- "---"
          nota_elemento_reactivos <- "---"
          nota_elemento_productos <- "---"
          nota_oxigeno_reactivos <- "---"
          nota_oxigeno_productos <- "---"
          
          # Operamos si corresponde... y sumamos
          if (!is.na(coef1) | !is.na(sub1)) suma_elemento_reactivos <- coef1*sub1
          if (!is.na(coef2) | !is.na(sub2)) suma_oxigeno_reactivos  <- coef2*sub2
          if (!is.na(coef3) | !is.na(sub3)) suma_elemento_productos <- coef3*sub3
          if (!is.na(coef3) | !is.na(sub4)) suma_oxigeno_productos  <- coef3*sub4
          
          
          
          # Operamos si corresponde...
          if (!is.na(coef1) | !is.na(sub1)) nota_elemento_reactivos <- paste0(coef1, "*", sub1, " = ", coef1*sub1)
          if (!is.na(coef2) | !is.na(sub2)) nota_oxigeno_reactivos  <- paste0(coef2, "*", sub2, " = ", coef2*sub2)
          if (!is.na(coef3) | !is.na(sub3)) nota_elemento_productos <- paste0(coef3, "*", sub3, " = ", coef3*sub3)
          if (!is.na(coef3) | !is.na(sub4)) nota_oxigeno_productos  <- paste0(coef3, "*", sub4, " = ", coef3*sub4)
          
          # Decision de balanceado del elemento1
          dt1_1 <- identical(suma_elemento_reactivos, suma_elemento_productos)
          if(dt1_1) decision1_1 <- afirmativo else decision1_1 <- negativo
          
          
          # Decision de balanceado del oxigeno
          dt1_2 <- identical(suma_oxigeno_reactivos, suma_oxigeno_productos)
          if(dt1_2) decision1_2 <- afirmativo else decision1_2 <- negativo
          
          # Decision de balanceado general
          dt1_3 <- sum(dt1_1, dt1_2) == 2
          if(dt1_3) decision1_3 <- afirmativo else decision1_3 <- negativo
          
          # Decision de ecuacion final
          esta_ecuacion <- as.character(as.vector(as.matrix(input_ejemplo[n,c(2:ncol(input_ejemplo))])))
          ecuacion_final <- as.character(as.vector(as.matrix(input_ejemplo[nrow(input_ejemplo),c(2:ncol(input_ejemplo))])))
          dt2 <- identical(esta_ecuacion, ecuacion_final) 
          if (dt2) decision2 <- afirmativo else decision2 <- negativo
          
          # Guardamos los calculos del elemento1...
          matrix_cantidades[1,] <- c(simbolo1, nota_elemento_reactivos, 
                                     nota_elemento_productos, decision1_1, decision2, decision2)
          
          matrix_cantidades[2,] <- c(simbolo2, nota_oxigeno_reactivos, 
                                     nota_oxigeno_productos, decision1_2, decision2, decision2)
          
          matrix_cantidades[3,] <- c(my_choices12[10], my_choices12[11], my_choices12[11],
                                     decision1_3, decision2, decision2)
          
        } # Fin 3)
        #####################################################################
        
        ###
      } # Fin Parte5
      ########################################
      
      
      # Parte 6: Salida de matrix_cantidades
      {
      ###
        return(matrix_cantidades)
      ###  
      } # Fin Parte 6
      ################################################
      
      
      
    } # Fin seguir
 
    
    # Si paso algun problema...
    if (!seguir) { return("Problemas en CantidadesOxidos()")}
  ###   
  } # Fin Realizamos
  #########################################################
  
 
  
  
  
  
  
  
  
  
}
  
  



CantidadesGeneral <- function(input_numfam = NULL, 
                        input_ejemplo = NULL, 
                        input_language_interno = "es", 
                        input_language_optativo = NULL, 
                        input_details = NULL, 
                        input_searchme = "idS12",
                        input_tabla = NULL,
                        input_paso = NULL) {
  
  # Especificaciones de lenguaje interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  
  
  
  # Resoluciones
  
  if (input_numfam == 1)     CantidadesOxidos(input_ejemplo = input_ejemplo, 
                                              input_language_interno = input_language_interno, 
                                              input_language_optativo = input_language_optativo, 
                                              input_details = input_details, 
                                              input_searchme = input_searchme,
                                              input_tabla = input_tabla,
                                              input_paso = input_paso)
  
  
} # End Function***


