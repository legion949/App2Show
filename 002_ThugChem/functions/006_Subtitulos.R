

SubtitulosOxidos <-function(input_ejemplo = NULL, 
                            input_language_interno = NULL, 
                            input_language_optativo = NULL, 
                            input_subtitulos = NULL, 
                            input_tabla = NULL,
                            input_paso = NULL,
                            input_total = NULL) {
  
  
  # Especificaciones de language interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  
  # Controles
  # # 1) input_ejemplo: no nulo, data.frame o matrix, con filas y columnas
  # # 2) input_language_interno: no nulo, vector, caracter, de longitud uno , y dos caracteres 
  # # 3) input_language_optativo: no nulo, vector, caracter, de longitud uno , y dos caracteres
  # # 4) input_subtitulos: no nulo, lista dentro de lista, dataframe o matrix, con filas y columnas
  # # 5) input_tabla: no nulo, una lista, al menos un objeto en la lista, objeto con dos dimensiones
  # # 6) input_paso: no nulo, vector, numerico, de longitud uno  
  # # 7) input_total: no nulo, vector, numerico, de longitud uno  
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
    
    
    # # 4) input_subtitulos: no nulo, lista dentro de lista, dataframe o matrix, con filas y columnas
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_subtitulos))     seguir <- FALSE
        if(!is.list(input_subtitulos))   seguir <- FALSE
          if(!is.list(input_subtitulos[[1]][[1]]))   seguir <- FALSE
            if (!is.data.frame(input_subtitulos[[1]][[1]]) && !is.matrix(input_subtitulos[[1]][[1]]))      seguir <- FALSE
              if(length(dim(input_subtitulos[[1]][[1]])) !=  2)    seguir <- FALSE
                if(dim(input_subtitulos[[1]][[1]])[1] < 1)    seguir <- FALSE
                  if(dim(input_subtitulos[[1]][[1]])[2] < 1)    seguir <- FALSE
      
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 4 de 8) ### \n",
                                     "Problemas en el argumento: input_subtitulos \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Es una lista... \n",
                                     "- Una lista dentro de otra lista... \n",
                                     "- Un data frame o una matriz \n", 
                                     "- Tener 2 dimensiones \n",
                                     "- Tener al menos 1 fila \n",
                                     "- Tener al menos 1 columna \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
      
    } # Fin Control 4)
    
    
  
    
    
    # # 5) input_tabla: no nulo, lista, al menos un objeto en la lista, 
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
      
      
      
      
    } # Fin Control 5)
    
    
    
    # # 6) input_paso: no nulo, vector, numerico, de longitud uno
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
      
      
      
      
    } # Fin Control 6)
    
    
    # # 7) input_total: no nulo, vector, numerico, de longitud uno
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_total))        seguir <- FALSE
        if (!is.vector(input_total))     seguir <- FALSE
          if(!is.numeric(input_total))   seguir <- FALSE
            if(length(input_total) != 1)     seguir <- FALSE
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 7 de 8) ### \n",
                                     "Problemas en el argumento: input_total \n",
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
  
  
  
  # Ejecucion de Subtitulos en Oxidos
  # # Realizamos todos los subtitulos y comentarios a los pasos estequimetricos
  {
    ###  
    
    # Si paso todos los controles se lleva a cabo el ejercicio...
    if (seguir) {
      
  
      
      # Parte 2: Informacion necesaria
      {
        ###
        
        # General de la Tabla...
        simbolos <- as.character(input_tabla[["es"]][,2])
        tipo <- as.character(input_tabla[["es"]][,7])
        subtipo <- as.character(input_tabla[["es"]][,8])
        num_atom <- input_tabla[["es"]][,4]
        nombres <- as.character(input_tabla[["es"]][,3])
        estado <- as.character(input_tabla[["es"]][,9])
        #valencia <- strsplit(as.character(input_tabla[["es"]][,10]), ";")
        
        
        # Toma la valencia a partir de la info del simbolo y la vaencia
        # que hay en el input_ejemplo
        simbolo1 <- as.character(input_ejemplo[1,3])
        valencia1 <- input_ejemplo[4,12]
        
        # Con lo anterior obtenemos el detalle de todo lo que puede hacer falta
        dt1 <- simbolos == simbolo1
        num_atom1 <- num_atom[dt1]
        nombre1 <- nombres[dt1]
        tipo1 <- tipo[dt1]
        estado1 <- estado[dt1]
       
        
        ###  
      } # Fin Parte2
      ##############################################
      
      # Parte 3: GPS
      {
        ###
        
        
        gps_x1 <- c(15, 15, 15, 15, 15, 15, 15, 15)
        gps_y1 <- c( 2,  2,  2,  2,  2,  2,  2,  2)    
        
        gps_x2 <- c(15, 15, 15, 15, 15, 15, 15, 15)
        gps_y2 <- c(30, 30, 30, 30, 30, 30, 30, 30)    
        
        gps_x3 <- c(15, 15, 15, 15, 15, 15, 15, 15)
        gps_y3 <- c(20, 20, 20, 20, 20, 20, 20, 20)    
        
        
        ###
      } # Fin Parte3
      ##############################################
      
      
      # Parte4: Subtitulos por default...
      {
        ###
        # Elementos por default de subtitulos...
        default_sub1 <- as.character(as.vector(as.matrix(subtitulos[[1]][[1]][,input_language_optativo])))    
        default_sub2 <- as.character(as.vector(as.matrix(subtitulos[[1]][[2]][input_paso,input_language_optativo])))  
        default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][input_paso,input_language_optativo])))
        
        
        ###  
      } # Fin Parte4
      #############################################
      
      
      # Parte5: Subtitulos3 por casos especiales en oxidos
      {
        ###
        # Solo cambian los subtitulos3... que es el detalle especifico
        # para cada paso de cada ejercicio.
        # Todos los cambios dependen de caracteristicas especiales
        # de los elementos que intervienen o los numeros que se van obteniendo
        # en el desarrollo de la resolucion del ejercicio.
        
        # Evaluacion general... 
        dt1 <- simbolo1 != "O"  # No es el oxigeno...
        dt2 <- tipo1 != "Gas Noble" # No es un gas noble...
        dt3 <- sum(dt1, dt2) == 2  # Es un elemento que no es ni el oxigeno ni un gas noble
        dt4 <- estado1 == "Gas"   # Es un gas
        dt5 <- sum(dt2, dt4) == 2 # Es un gas, pero no es gas noble...
        # Caso 1) Casos para cuando el elemento no es el oxigeno ni un gas noble
        if(dt3) {
          
          
          # Caso 1.1) En productos, paso 5... simplificacion de los subindices
          if(input_paso == 5) {
            # La frase que esta puesta por defecto es para cuando los dos subindices son
            # igual a uno. Si los subindices no son ambos igual a 1, hay dos casos:
            # Caso 1.1.1) El mcd es igual a 1. No se puede simplificar.
            # Caso 1.1.2) El mcd es diferente de 1. Se puede simplificar.
            # Veamos que pasa...
            
            # Paso en cuestion y paso anterior
            paso_interno <- 5
            paso_anterior <- 4
            
            # Estos son los subindices
            sub3 <- as.numeric(as.character(input_ejemplo[paso_anterior, 10]))
            sub4 <- as.numeric(as.character(input_ejemplo[paso_anterior, 12]))
            
            # Veamos si ambos son iguales a 1...
            magic1 <- FALSE
            if(sub3 == 1) if(sub4 == 1) magic1 <- TRUE
            
            # Vemos el minumo comun divisor, que es el coeficiente3 del paso 5
            coef3 <- as.numeric(as.character(input_ejemplo[paso_interno, 8]))
            
            
            magic2 <- FALSE
            if(coef3 == 1) magic2 <- TRUE
            
            
            # Y ahora... con eso... planteamos dos textos para el subtitulo 3.
            
            # Caso 1.1.1) No era factible realizar una simplificacion
            if(magic1 == FALSE) if(magic2 == TRUE) {
              
              id_especial <- "caso1.1.1"
              rotulos_especiales <-  subtitulos[[1]][[3]][,5]
              dt_especial <- rotulos_especiales == id_especial
              default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
              remove(id_especial, rotulos_especiales, dt_especial)
              
              # El nuevo texto, no tiene nada que deba ser reemplazado  
            }
            
            # Caso 1.1.2) Efectivamente se llevo a cabo una simplificacion
            if(magic1 == FALSE) if(magic2 == FALSE) {
              
              id_especial <- "caso1.1.2"
              rotulos_especiales <-  subtitulos[[1]][[3]][,5]
              dt_especial <- rotulos_especiales == id_especial
              default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
              remove(id_especial, rotulos_especiales, dt_especial)
              
              # Hay que hacer un reemplazo
              default_sub3 <- gsub("MY_COEF3_PASO5", coef3, default_sub3)
            }
            
            # Borramos todo lo que hemos creado aqui...
            remove(sub3, sub4, coef3, magic1, magic2, paso_interno, paso_anterior)
            
            
            
            
            
            
          } # Fin Caso 1.1)
          ####################################################################
          
          
          # Caso 1.2) En productos, paso 6... balanceo del oxigeno
          if(input_paso == 6) {
            # La frase que esta puesta por defecto es para cuando el oxigeno ya
            # esta balanceado
            
            
            # Paso en cuestion y paso anterior
            paso_interno <- 6
            
            
            # Estos son los subindices
            coef2 <- as.numeric(as.character(input_ejemplo[paso_interno,  5]))
            coef3 <- as.numeric(as.character(input_ejemplo[paso_interno,  8]))
            
            # Deteccion interna   
            id_especial <- "caso1.2.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            remove(id_especial, rotulos_especiales, dt_especial)
            
            # Hay que hacer dos reemplazos
            default_sub3 <- gsub("MY_COEF2_PASO6", coef2, default_sub3)
            default_sub3 <- gsub("MY_COEF3_PASO6", coef3, default_sub3)
            
            
            # Borramos todo lo que hemos creado aqui...
            remove(coef2, coef3, paso_interno)
            
          } # Fin Caso 1.2)
          ####################################################################
          
          
          # Caso 1.3) En productos, paso 7... balanceo del elemento
          if(input_paso == 7) {
            # La frase que esta puesta por defecto es el elemento
            # ya esta balanceado.
            
            
            # Paso en cuestion y paso anterior
            paso_interno <- 7
            
            
            # Estos son los subindices
            coef1 <- as.numeric(as.character(input_ejemplo[paso_interno,  2]))
            
            # Deteccion interna   
            id_especial <- "caso1.3.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            remove(id_especial, rotulos_especiales, dt_especial)
            
            # Hay que hacer dos reemplazos
            default_sub3 <- gsub("MY_COEF1_PASO7", coef1, default_sub3)
            default_sub3 <- gsub("MY_NAME", nombre1, default_sub3)
            
            
            # Borramos todo lo que hemos creado aqui...
            remove(coef1, paso_interno)
            
          } # Fin Caso 1.3)
          #####################################################################
          
          
          
          # Caso 1.4) En productos, paso 8... simplificacion de coeficientes
          if(input_paso == 8) {
            # La frase que esta puesta por defecto es que no es necesario
            # simplificar los coeficientes por que todos los coeficientes
            # son iguales a 1.
            # Hay dos casos mas...
            # Caso 1.4.1) Cuando los coeficientes no pueden simplificarse
            #             por que si bien no todos son iguales entre si,
            #             no hay un comund divisor distinto de 1.
            # Caso 1.4.2) Cuando es posible simplificar coeficientes.
            
            # Paso en cuestion y paso anterior
            paso_interno <- 8
            paso_anterior <- 7
            
            # Subindice del oxido en pasos 7 y 8
            coef1_7 <- as.numeric(as.character(input_ejemplo[paso_anterior,  2]))
            coef2_7 <- as.numeric(as.character(input_ejemplo[paso_anterior,  5]))
            coef3_7 <- as.numeric(as.character(input_ejemplo[paso_anterior,  8]))
            
            # Veamos si la suma de los 3 coeficientes es igual a 3.
            suma_coef <- sum(coef1_7, coef2_7, coef3_7)
            magic8 <- suma_coef == 3
            
            library(numbers)
            mcd <- mGCD(c(coef1_7, coef2_7 , coef3_7 ))
            
            
            # Vamos al Caso 1.4.1
            # # No era posible simplificar...
            if (!magic8) if(mcd == 1) {
              # Deteccion interna   
              id_especial <- "caso1.4.1"
              rotulos_especiales <-  subtitulos[[1]][[3]][,5]
              dt_especial <- rotulos_especiales == id_especial
              default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
              remove(id_especial, rotulos_especiales, dt_especial)
              
              
            } # Fin Vamso al Caso 1.4.1
            
            # Vamos al Caso 1.4.2
            # # Se pudo simplificar...
            if (mcd != 1) {
              # Deteccion interna   
              id_especial <- "caso1.4.2"
              rotulos_especiales <-  subtitulos[[1]][[3]][,5]
              dt_especial <- rotulos_especiales == id_especial
              default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
              remove(id_especial, rotulos_especiales, dt_especial)
              
              # Hay que hacer un reemplazo
              default_sub3 <- gsub("MY_MCD_PASO8", mcd, default_sub3)
              
            } # Fin Vamso al Caso 1.4.2
            
            # Borramos todo lo que hemos creado aqui...
            remove(coef1_7, coef2_7, coef3_7, mcd, paso_interno, paso_anterior)
            
          } # Fin Caso 1.4)
          #####################################################################
          
          
          # Caso 1.5) El reactivos, el paso 2... el reactivo lleva subindice
          #           2 por que tambien es un gas.
          if(input_paso == 2) if(dt5){
            # La frase que esta puesta por defecto es que el oxigeno
            # lleva un subindice por que es un gas. Hay que agrega
            # a esa nota, una explicacion para cuando el elemento
            # que va a reaccionar es un gas.
            
            
            # Paso en cuestion
            paso_interno <- 2
            
            # Deteccion interna   
            id_especial <- "caso1.5.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y Parte2
            default_sub3_1 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][2,input_language_optativo])))
            default_sub3_2 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            #Fusion
            default_sub3 <- paste0(default_sub3_1, "\n", default_sub3_2)
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, default_sub3_1, default_sub3_2)
            
            # Hay que hacer un reemplazo
            default_sub3 <- gsub("MY_NAME", nombre1, default_sub3)
            
            
            # Borramos todo lo que hemos creado aqui...
            remove(paso_interno)
            
          } # Fin Caso 1.5)
          #####################################################################
          
          
          ###
        } # Fin Caso 1)
        ########################################################################
        
        
        # Caso 2) Para cuando el elemento es el "Oxigeno".
        if(!dt1) {
          ###    
          
          # Caso 2.1) En productos, paso 3... presentamos solo al oxigeno
          if(input_paso == 3) {
            # La frase que esta puesta por defecto es para cuando se forma un oxido.
            # Aca solo hay oxigeno
            # Veamos que pasa...
            
            # Paso en cuestion
            paso_interno <- 3
            
            id_especial <- "caso2.1.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            remove(id_especial, rotulos_especiales, dt_especial)
            
            # El nuevo texto, no tiene nada que deba ser reemplazado  
          }
          
          
          # Caso 2.2) En productos, paso 4... subindice del oxigeno en productos
          if(input_paso == 4) {
            # La frase que esta puesta por defecto es para cuando se forma un oxido
            # e intercambiar las valencias de los elementos.
            # En este caso, solo hay oxigeno, y le va un dos como subindice 
            # en productos por que es un gas.
            # Aca solo hay oxigeno
            # Veamos que pasa...
            
            # Paso en cuestion
            paso_interno <- 4
            
            id_especial <- "caso2.2.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            remove(id_especial, rotulos_especiales, dt_especial)
            
            # El nuevo texto, no tiene nada que deba ser reemplazado  
          }  
          
          # Caso 2.3) En productos, paso 6... coeficiente del oxigeno en productos
          if(input_paso == 6) {
            # La frase que esta puesta por defecto es para cuando se forma un oxido
            # y balancear globlamente al oxigeno.
            # En este caso, solo hay oxigeno, y le va un dos como coeficiente al producto
            # y listo, se balanceo todo.
            # Veamos que pasa...
            
            # Paso en cuestion
            paso_interno <- 6
            
            id_especial <- "caso2.3.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            remove(id_especial, rotulos_especiales, dt_especial)
            
            # El nuevo texto, no tiene nada que deba ser reemplazado  
          }     
          
          
          
          
          ###      
        } # Fin Caso 2)
        #######################################################
        
        
        # Caso 3) Para cuando el elemento es un gas noble
        if(!dt2) {
          ###  
          
          # Caso 3.1) En reactivos... paso 1... presentacion de los elementos.
          if(input_paso == 1) {
            # La frase que esta puesta por defecto es para presentar la valencia
            # de cada uno de los elementos de reactivos. 
            # A esa frase hay que agregarle que el elemento seleccionado es
            # tiene valencia cero por ser un gas noble.
            # Paso en cuestion
            paso_interno <- 1
            
            id_especial <- "caso3.1.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y 2
            default_sub3_1 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][paso_interno,input_language_optativo])))
            default_sub3_2 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            # Fusion
            default_sub3 <- paste0(default_sub3_1, "\n", default_sub3_2)
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, default_sub3_1, default_sub3_2)
            
            
            # Hay que hacer un reemplazo
            default_sub3 <- gsub("MY_NAME", nombre1, default_sub3)
          }
          
          
          # Caso 3.2) En reactivos... paso 2... subindices de gases.
          if(input_paso == 2) {
            # La frase que esta puesta por defecto es para al subindice de gases.
            # Pero los gases nobles no llevan subindice.
            # Paso en cuestion
            paso_interno <- 2
            
            id_especial <- "caso3.2.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y 2
            default_sub3_1 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][paso_interno,input_language_optativo])))
            default_sub3_2 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            # Fusion
            default_sub3 <- paste0(default_sub3_1, "\n", default_sub3_2)
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, default_sub3_1, default_sub3_2, paso_interno)
            
            # No hay elementos para reemplazar
          }
          
          
          # Caso 3.3) En reactivos... paso 3... presentacion del oxido.
          if(input_paso == 3) {
            # La frase que esta puesta por defecto es presentar al oxido.
            # Pero los gases nobles no reaccionan con oxigeno y no se 
            # formara oxido.
            
            paso_interno <- 3
            
            id_especial <- "caso3.3.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y 2
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, paso_interno)
            
            # No hay elementos para reemplazar
          }
          
          
          # Caso 3.4) En reactivos... paso 4... intercambio de valencias.
          if(input_paso == 4) {
            # La frase que esta puesta por defecto para detallar el
            # intercambio de valencais. En este caso eso no ocurre ya que no
            # ocurre reaccion quimica.
            
            paso_interno <- 4
            
            id_especial <- "caso3.4.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y 2
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, paso_interno)
            
            # No hay elementos para reemplazar
          }
          
          
          # Caso 3.5) En reactivos... paso 5... simplificacion de subindices.
          if(input_paso == 5) {
            # La frase que esta puesta por defecto para detallar la
            # simplificación de valencias. En este caso eso no ocurre ya que no
            # ocurre reaccion quimica.
            
            paso_interno <- 5
            
            id_especial <- "caso3.5.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y 2
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, paso_interno)
            
            # No hay elementos para reemplazar
          }
          
          
          # Caso 3.6) En reactivos... paso 6... balance del oxigeno.
          if(input_paso == 6) {
            # La frase que esta puesta por defecto para detallar el balanceo del oxigeno.
            # En este caso eso no ocurre ya que no
            # ocurre reaccion quimica.
            
            paso_interno <- 6
            
            id_especial <- "caso3.6.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y 2
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, paso_interno)
            
            # No hay elementos para reemplazar
          }
          
          
          # Caso 3.7) En reactivos... paso 7... balance del elemento.
          if(input_paso == 7) {
            # La frase que esta puesta por defecto para detallar el balanceo 
            # del elemento. En este caso eso no ocurre ya que no
            # ocurre reaccion quimica.
            
            paso_interno <- 7
            
            id_especial <- "caso3.7.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y 2
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, paso_interno)
            
            # No hay elementos para reemplazar
          }
          
          
          # Caso 3.8) En reactivos... paso 8... balance del elemento.
          if(input_paso == 8) {
            # La frase que esta puesta por defecto para detallar la
            # simplificacion de los coeficientes.
            # En este caso eso no ocurre ya que no
            # ocurre reaccion quimica.
            
            paso_interno <- 8
            
            id_especial <- "caso3.8.1"
            rotulos_especiales <-  subtitulos[[1]][[3]][,5]
            dt_especial <- rotulos_especiales == id_especial
            
            # Parte1 y 2
            default_sub3 <- as.character(as.vector(as.matrix(subtitulos[[1]][[3]][dt_especial,input_language_optativo])))
            
            
            
            # Borramos algunas cosas...
            remove(id_especial, rotulos_especiales, dt_especial, paso_interno)
            
            # No hay elementos para reemplazar
          }
          
          
          ###  
        } # Fin Caso 3)
        ###############################################################
        
        ###  
      } # Fin  Parte5
      ########################################################
      
      
      # Parte 6: Cambios finales para presentar los subtitulos
      {
        ###
        # Cambios necesarios dentro del default...
        # # Sub2
        mod_sub2 <- gsub("\\\\n", "\n", default_sub2)
        
        # # Sub3
        mod_sub3 <- gsub("\\\\n", "\n", default_sub3)
        mod_sub3 <- gsub("MY_NAME", nombre1, mod_sub3)
        mod_sub3 <- gsub("MY_VALENCIA", valencia1, mod_sub3)
        
        # Subtitulos de salida...
        salida_sub1 <- paste0(default_sub1[1], " - ", default_sub1[2] ," ", input_paso," ", default_sub1[3]," ", input_total)
        salida_sub2 <- paste0(default_sub1[2], " ", input_paso, ") ", mod_sub2, collapse="")
        salida_sub3 <- mod_sub3
        
        
        
        ###  
      } # Fin Parte6
      ####################################################################
      
      
      # Parte 7: Salida
      {
      ###
        
        #Posicion x, posicion y, texto...
        armado1 <- list(gps_x1[input_paso], gps_y1[input_paso], salida_sub1)
        armado2 <- list(gps_x2[input_paso], gps_y2[input_paso], salida_sub2)
        armado3 <- list(gps_x3[input_paso], gps_y3[input_paso], salida_sub3)
        
        #Rejunte...
        rejunte <- list(armado1, armado2, armado3)
        
        # Suerte...
        return(rejunte)
        
      ###  
      } # Fin Parte 7
      #################################################
      
    } # Fin seguir
    ############################################
  
  ###  
  } # Fin Ejecucion de subtitulos en oxidos
  #####################################################################
  
  
###  
} # Fin Fincion SubtitulosOxidos()***
############################################################################################


SubtitulosGeneral <- function(input_numfam = NULL, 
                        input_ejemplo = NULL, 
                        input_language_interno = NULL, 
                        input_language_optativo = NULL, 
                        input_subtitulos = NULL, 
                        input_tabla = NULL,
                        input_paso = NULL,
                        input_total = NULL) {
  
  # Especificaciones de lenguaje interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  
  
  
  # Resoluciones
  
  if (input_numfam == 1)     SubtitulosOxidos(input_ejemplo = input_ejemplo, 
                                              input_language_interno = input_language_interno, 
                                              input_language_optativo = input_language_optativo, 
                                              input_subtitulos = input_subtitulos, 
                                              input_tabla = input_tabla,
                                              input_paso = input_paso,
                                              input_total = input_total)
  
  
  
  
  
  
  
} # End Function***



