
input_num_atom1 = 8
input_valencia1 = 2
input_language_interno ="es" 
input_language_optativo ="es" 
input_tabla = TabPeriod

NomenOxidos <-function( input_num_atom1 = NULL,
                        input_valencia1 = NULL,
                        input_language_interno = NULL, 
                        input_language_optativo = NULL, 
                        input_tabla = NULL) {
  
  
  
  
  # Especificaciones de language interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  
  
  
  
  # Controles
  # # 1) input_num_atom1: no nulo, vector, tipo numerico, longitud 1.
  # # 2) input_valencia1: no nulo, vector, tipo numerico, longitud 1.
  # # 3) input_language_interno: no nulo, vector, caracter, de longitud uno , y dos caracteres 
  # # 4) input_language_optativo: no nulo, vector, caracter, de longitud uno , y dos caracteres
  # # 5) input_tabla: no nulo, una lista, al menos un objeto en la lista, objeto con dos dimensiones
  {
    ###
    
    control_OK <- list()
    control_paso <- 0
    seguir <- TRUE
    mensaje <- list()
    
    # # 1) input_num_atom1: no nulo, vector, tipo numerico, de longitud uno
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_num_atom1))        seguir <- FALSE
        if (!is.vector(input_num_atom1))     seguir <- FALSE
          if(!is.numeric(input_num_atom1))   seguir <- FALSE
            if(length(input_num_atom1) != 1)     seguir <- FALSE
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 1 de 8) ### \n",
                                     "Problemas en el argumento: input_num_atom1 \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Vector \n", 
                                     "- Tipo numerico \n",
                                     "- Longitud uno \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
    } # Fin Control 2)
    
    
    
    # # 2) input_valencia1: no nulo, vector, tipo numerico, de longitud uno
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_valencia1))        seguir <- FALSE
        if (!is.vector(input_valencia1))     seguir <- FALSE
          if(!is.numeric(input_valencia1))   seguir <- FALSE
            if(length(input_valencia1) != 1)     seguir <- FALSE
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 1 de 8) ### \n",
                                     "Problemas en el argumento: input_valencia1 \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Vector \n", 
                                     "- Tipo numerico \n",
                                     "- Longitud uno \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      
      
      
    } # Fin Control 2)
    
    
    # # 3) input_language_interno: no nulo, vector, caracter, de longitud uno , y dos caracteres.
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
      
      
      
      
    } # Fin Control 3)
    
    
    
    # # 4) input_language_optativo: no nulo, vector, caracter, de longitud uno , y dos caracteres.
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
    
    
    
    
   
    
   
    
    
    
    ### 
  } # Fin Controles
  #####################################################################################################
  
  
  # Parte 1: Informacion necesaria muy particular
  {
    ###
    
    # General de la Tabla...
    simbolos <- as.character(input_tabla[["es"]][,2])
    tipo <- as.character(input_tabla[["es"]][,7])
    subtipo <- as.character(input_tabla[["es"]][,8])
    num_atom <- input_tabla[["es"]][,4]
    nombres <- as.character(input_tabla[["es"]][,3])
    estado <- as.character(input_tabla[["es"]][,9])
    valencias <- strsplit(as.character(input_tabla[["es"]][,10]), ";")
    #valencia <- strsplit(as.character(input_tabla[["es"]][,10]), ";")
    
    # Detalles muy especiales...
    cantidad_valencias <- as.numeric(as.character(input_tabla[["es"]][,16]))
    nombres_oxidos <- as.character(input_tabla[["es"]][,18])
    # Toma el numero atomico del elemento y la valencia que utiliza
    # de los input de la funcion
    # que hay en el input_ejemplo
    num_atom1 <- input_num_atom1
    valencia1 <- input_valencia1
    
    # Con lo anterior obtenemos el detalle de todo lo que puede hacer falta
    simbolo1 <- simbolos[num_atom1]
    nombre1 <- nombres[num_atom1]
    tipo1 <- tipo[num_atom1]
    estado1 <- estado[num_atom1]
    dt1 <- simbolo1 == simbolos

    
    # Detalle especial para nomenclatura
    cantidad_valencias1 <- cantidad_valencias[dt1]
    orden_valencias1 <- c(1:cantidad_valencias1)
    grupo_valencias1 <- as.numeric(as.character(valencias[[num_atom1]]))
    nombre_oxido1 <- nombres_oxidos[dt1]
    dt_orden_valencias1 <- orden_valencias1[grupo_valencias1 == valencia1]
    
    ###  
  } # Fin Parte1
  ##############################################
  
  # Parte 2: Otros objetos necesarios
  {
  ###
    
    # Numeros romanos
    romanos <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "")
    
    # Detalles de cantidades
    detalle_cantidad1 <- c("Mon", "Di", "Tri", "Tetra", "Penta", "Hexa", "Hepta", "Octo")
    detalle_cantidad2 <- c("mon", "di", "tri", "tetra", "penta", "hexa", "hepta", "octo")
    
  ###  
  } # Parte 2
  ############################################
  
  # Parte3: Resolucion clasica
  {
  ###  
    # Generacion de input_ejemplo
    input_ejemplo <- ResolGeneral(input_numfam = 1, # El 1 es para Oxidos...
                          input_num_atom1 = num_atom1, input_valencia1 = valencia1,
                          input_num_atom2 = NULL, input_valencia2 = NULL,
                          input_language_interno =  input_language_interno, 
                          input_language_optativo = input_language_optativo,
                          input_tabla = input_tabla)
    
    
    # # Pequenio cambio sobre "input_ejemplo".
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
    } # Fin Pequenio cambio
    
  ###  
  } # Fin Parte3
  ##################################
  
  
  # Parte 4: Resolucion LaTeX
  {
  ###
    
    
    input_latex <- LaTeXGeneral(input_numfam = 1,  # El 1 es para Oxidos...
                        input_ejemplo = input_ejemplo,
                        input_language_interno = input_language_interno, 
                        input_language_optativo = input_language_optativo,
                        input_tabla = input_tabla)
    
   
    
  ###  
  } # Fin Parte4
  ##########################################
  
  
  # Parte5: Creacion de objetos intermedios
  {
  ###
    # Tomamos el ultimo detalle del oxido en si.
    cantidad_pasos <- nrow(input_ejemplo)
    coef3 <- as.numeric(as.character(input_ejemplo[cantidad_pasos,8]))
    elemento3 <- as.character(input_ejemplo[cantidad_pasos,9])
    sub3 <- as.numeric(as.character(input_ejemplo[cantidad_pasos,10]))
    elemento4 <- as.character(input_ejemplo[cantidad_pasos,11])
    sub4 <- as.numeric(as.character(input_ejemplo[cantidad_pasos,12]))
  
  ###    
  } # Fin Parte5
  ################################################################
  
  
  # Parte 6: Nomenclatura
  {
  ###
    
    # Evaluacion general del caso... 
    dt1 <- simbolo1 != "O"  # No es el oxigeno...
    dt2 <- tipo1 != "Gas Noble" # No es un gas noble...
    dt3 <- sum(dt1, dt2) == 2  # Es un elemento que no es ni el oxigeno ni un gas noble
    dt4 <- estado1 == "Gas"   # Es un gas
    dt5 <- sum(dt2, dt4) == 2 # Es un gas, pero no es gas noble...
    
    # Generacion de 'salida_nomenclaturas'
    {
    ###
      
      
    # Para resumir, hay 3 nomenclaturas diferentes.
    # Dos de ellas cambian su forma de nombrar a los compuestos quimicos
    # segun la cantidad de valencias que tiene el elemento quimico que participa.
    # Estas son "IUPAC" y "Clasica". 
    # La 3ra no depende de la cantidad de valencias, es la "Numeral Stock".
    
    
    # Objetos generales...
    {
      ###
      
      r1 <- romanos[valencia1]
      r2 <- romanos[2]
      
      nombre_fijo <- nombre1
      mi_nombre <- nombre1
      
      ###    
    } # Fin Objetos Generales...
    
    nombres_nomenclaturas <- c("IUPAC", "Clásica", "Numeral Stock", "FQ", "FQ LaTeX")
    salida_nomenclaturas <- rep(NA, length(nombres_nomenclaturas))
    names(salida_nomenclaturas) <- nombres_nomenclaturas
    
    ###
    } # fin Generacion
    ###########################################################
    
    
    
    # Caso 1) Casos para cuando el elemento no es el oxigeno ni un gas noble
    if(dt3){
      
     
    
        
        # Forma 1) IUPAC
        salida_nomenclaturas[1] <- paste0("Óxido de ", nombre_fijo, " (", r1, ")")
      
      
        # Forma 2) Clasica
        {
        ###
          # Depende de la cantidad de valencias del elemento
          # y la valencia en si mismo que usa de ese total de valencias
          
            # Si tiene una sola valencia...
            if (cantidad_valencias1 == 1) {
        
            # En este caos el nombre es muy facil...
            salida_nomenclaturas[2] <- paste0("Óxido de ", nombre_fijo)
      
      
     
      
      
      
            } # Fin Si tiene solo una valencia...
      
      
            # Si tiene dos valencias...
            if (cantidad_valencias1 == 2) {
                nombres_nomenclatura <- strsplit(nombre_oxido1, "; ")[[1]]
                este_nombre_nomenclatura <- nombres_nomenclatura[dt_orden_valencias1]
                salida_nomenclaturas[2] <- paste0("Óxido ", este_nombre_nomenclatura)
              
            
              
            } # Fin Si tiene dos valencia...
      
          
            # Si tiene tres valencias...
            if (cantidad_valencias1 == 3) {
              
              # Prefijos y pool de nombres
              prefijo <- c("Hipo", "", "")
              nombres_nomenclatura <- strsplit(nombre_oxido1, "; ")[[1]][c(1,1,2)]
              
              # Prefijo y nombre seleccionado...
              este_prefijo <- prefijo[dt_orden_valencias1]
              este_nombre_nomenclatura <- nombres_nomenclatura[dt_orden_valencias1]
              
              # Armado final
              salida_nomenclaturas[2] <- paste0("Óxido ", este_prefijo, este_nombre_nomenclatura)
              
        ###
        } # Fin Si tiene tres valencias...
        #########################################################################
      
          # Si tiene tres valencias...
          if (cantidad_valencias1 == 4) {
            
            # Prefijos y pool de nombres
            prefijo <- c("Hipo", "", "", "Per")
            nombres_nomenclatura <- strsplit(nombre_oxido1, "; ")[[1]][c(1,1,2,2)]
            
            # Prefijo y nombre seleccionado...
            este_prefijo <- prefijo[dt_orden_valencias1]
            este_nombre_nomenclatura <- nombres_nomenclatura[dt_orden_valencias1]
            
   
            # Armado final
            salida_nomenclaturas[2] <- paste0("Óxido ", este_prefijo, este_nombre_nomenclatura)
            
            ###
          } # Fin Si tiene cuatro valencias...
          #########################################################################
     
          
          # Si tiene tres valencias...
          if (cantidad_valencias1 == 5) {
            
            # Prefijos y pool de nombres
            prefijo <- c("Hipo", "", "", "Per", "Super")
            nombres_nomenclatura <- strsplit(nombre_oxido1, "; ")[[1]][c(1,1,2,2,2)]
            
            # Prefijo y nombre seleccionado...
            este_prefijo <- prefijo[dt_orden_valencias1]
            este_nombre_nomenclatura <- nombres_nomenclatura[dt_orden_valencias1]
            
            
            # Armado final
            salida_nomenclaturas[2] <- paste0("Óxido ", este_prefijo, este_nombre_nomenclatura)
            
            ###
          } # Fin Si tiene cinco valencias...
          #########################################################################
      
      
    } # Fin Forma 2)
        ###################################    
        
        
        # Forma 3: Numeral Stock
        {
          ###
          
          # No depende de la cantidad de valencias. Por eso es mas corto el script.
          # Elegimos el detalle de cantidades correcto 
          dc1 <- detalle_cantidad1[sub4]
          dc2 <- detalle_cantidad1[sub3]
          
          # Hacemos una correccion particular
          if (sub3 == 1) dc2 <- ""
          
          
          
          # Stock
          salida_nomenclaturas[3] <- paste0(dc1, "óxido de ", dc2, mi_nombre)
          
          
          
          
          ###  
        } # Fin Forma 3: Numeral Stock
        ###########################################  
      
        
    } # Fin Caso 1)
    
    # Caso 2) Para cuando el elemento es el "Oxigeno".
    if(!dt1) {
      
      salida_nomenclaturas[1] <- "Oxígeno"
      salida_nomenclaturas[2] <- "Oxígeno" 
      salida_nomenclaturas[3] <- "Oxígeno"

      
    } # Fin Caso 2)
    
    
    # Caso 3) Para cuando el elemento es un gas noble
    if(!dt2) {
      
      salida_nomenclaturas[1] <- paste0(nombre1, " es un Gas Noble")
      salida_nomenclaturas[2] <- paste0("El ", nombre1, " NO reacciona con Oxígeno")
      salida_nomenclaturas[3] <- paste0("No se forma un Óxido")
      
    } # Fin Caso 3)
    
   
    
    
    # Caso 4: Formula Quimica
    {
      ###
      fq <-  paste0(input_ejemplo[nrow(input_ejemplo), c(9:12)], collapse="")
      fq_latex <- input_latex[nrow(input_latex), 7] 
      
      salida_nomenclaturas[4] <- fq
      salida_nomenclaturas[5] <- fq_latex
      ###  
    } # Forma 4
    #########################################################
    
  ###  
  } # Fin Parte6
  #######################################
  
  
  # Parte7: Salida
  {
  ###
    
    return(salida_nomenclaturas)
  ###  
  } # Fin parte 7
  ######################################
  
} # Fun Function NomenOxidos***


# Nomenclatura General
NomenGeneral <- function(input_numfam = NULL, 
                         input_num_atom1 = NULL,
                         input_valencia1 = NULL,
                         input_num_atom2 = NULL,
                         input_valencia2 = NULL,
                         input_language_interno = "es", 
                         input_language_optativo = "es", 
                         input_tabla = NULL) {
  
  # Especificaciones de lenguaje interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  
  
  
  # Resoluciones
  
  if (input_numfam == 1)     NomenOxidos( input_num_atom1 = input_num_atom1,
                                          input_valencia1 = input_valencia1 ,
                                          input_language_interno = input_language_interno, 
                                          input_language_optativo = input_language_optativo, 
                                          input_tabla = input_tabla)
  
  
  
  
  
  
  
} # End Function***

# 
# NomenOxidos( input_num_atom1 = input_num_atom1,
#                         input_valencia1 = input_valencia1 ,
#                         input_language_interno = input_language_interno, 
#                         input_language_optativo = input_language_optativo, 
#                         input_tabla = input_tabla)
#   
#   
# 
# aver <- NomenGeneral(input_numfam = 1,
#               input_num_atom1 = input_num_atom1,
#              input_valencia1 = input_valencia1 ,
#              input_num_atom2 = NULL,
#              input_valencia2 = NULL ,
#              input_language_interno = input_language_interno,
#              input_language_optativo = input_language_optativo,
#              input_tabla = input_tabla)
# 
# 
# 
# 
