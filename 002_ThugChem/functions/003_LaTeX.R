# LaTeX Oxidos
LaTeXOxidos <-function(input_ejemplo = NULL, 
                       input_language_interno = "es", 
                       input_language_optativo = NULL,  
                       input_tabla = NULL) {
  
 
  # Especificaciones de lenguaje interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  
  # Controles
  # # 1) input_ejemplo: no nulo, data.frame o matrix, con filas y columnas
  # # 2) input_language_interno: no nulo, vector, caracter, de longitud uno , y dos caracteres 
  # # 3) input_language_optativo: no nulo, vector, caracter, de longitud uno , y dos caracteres
  # # 4) input_tabla: no nulo, una lista, al menos un objeto en la lista, objeto con dos dimensiones
  # # 5) Pequenio cambio sobre "input_ejemplo"
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
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 1 de 5) ### \n",
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
    ###########################################################################
    
    
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
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 2 de 5) ### \n",
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
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 3 de 5) ### \n",
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
    
    
    # # 4) input_tabla: no nulo, lista, al menos un objeto en la lista, 
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
        
        mensaje[[control_paso]] <- c("### LaTeXOxidos (Control de argumentos - Paso 4 de 5) ### \n",
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
      
      
      
      
    } # Fin Control 4)
    
    
    # # 5) Pequenio cambio sobre "input_ejemplo".
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
    } # Fin Control 5)
    ####################################################
    
   
    ### 
  } # Fin Controles
  ##################################################

  
  # #
  
  # Ejecucion Oxidos
  # # Realizamos la conversion al formato LaTeX para oxidos
  {
    ###  
    
    # Si paso todos los controles se lleva a cabo el ejercicio...
    if (seguir) {
      
      
      # Parte 1: Informacion necesaria
      {
      ###
        
        # General de la Tabla...
        simbolos <- as.character(input_tabla[["es"]][,2])
        tipo <- as.character(input_tabla[["es"]][,7])
        subtipo <- as.character(input_tabla[["es"]][,8])
        num_atom <- input_tabla[["es"]][,4]
        nombres <- as.character(input_tabla[["es"]][,3])
        
        # Especifica del ejercicio...
        simbolo1 <- as.character(input_ejemplo[1,3])
        dt1 <- simbolos == simbolo1
        num_atom1 <- num_atom[dt1]
        nombre1 <- nombres[dt1]
        tipo1 <- tipo[dt1]
      ###  
      }
      ##############################################
      
      
      # Parte 2: Estructura general de "matrix_solucion"
      {
        ###
        
        # 1.0) Explicacion de la Parte 1 (por si la queres leer)
        {
          ###
          # Ya tenemos una matriz o dataframe que tiene la solucion
          # de una ecuacion quimica de oxido paso a paso.
          # Ahora, convertimos esa matrix de resolucion en una matriz
          # con detalles de la ecuacion en formato latex para utilizar 
          # mas adelante.
          # La matrix_latex tendra 7 columnas
          #
          # Cada "matrix_solucion" tendra las siguientes columnas en oxidos:
          #    1) Orden de paso
          #    2) Coeficiente del elemento en reactivos
          #    3) Elemento y subindice de reactivos juntos en latex
          #    4) Coeficiente del oxigeno en reactivos
          #    5) Oxigeno y subindice de reactivos juntos en latex
          #    6) Coeficiente del oxido
          #    7) Formula del oxido con subindices juntos en latex
          ###
        } # Fin explicacion
        ########################################################
        
        
        # 1.1) Creacion del "matrix_latex"
        {
          ###
          
         
          # Nombre de las columnas...
          mis_nombres <- c("Orden", 
                           "Coef1", "Elemento",
                           "Coef2", "Oxigeno",
                           "Coef3", "Oxido")
          
          matrix_latex <- matrix(NA, nrow(input_ejemplo), length(mis_nombres))
          colnames(matrix_latex) <- mis_nombres
          ###  
        }
        ########################################################
        
        
        ###
      } # Fin Parte 2
      ##################################################
  
      
      # Parte 3: Conversion a LaTeX
      {
        ###
        
        # Determinamos la cantidad de pasos que existen en el objeto...
        cantidad_pasos <- nrow(input_ejemplo)
        
        # Para cada paso estequimetrico que se tiene...
        for (escudo in 1:cantidad_pasos) {

          # Participantes Necesarios
          {
          ###
            
              # Separamos el detalle del elemento en reactivos...
              coef1 <- as.numeric(as.character(input_ejemplo[escudo,2]))
              elemento1 <- as.character(input_ejemplo[escudo,3]) 
              sub1 <- as.numeric(as.character(input_ejemplo[escudo,4]))
              
              # Separamos el detalle del oxigeno en reactivos
              coef2 <- as.numeric(as.character(input_ejemplo[escudo,5]))
              elemento2 <- as.character(input_ejemplo[escudo,6])
              sub2 <- as.numeric(as.character(input_ejemplo[escudo,7]))
              
              # Separamos el detalle del oxido en productos
              coef3 <- as.numeric(as.character(input_ejemplo[escudo,8]))
              elemento3 <- as.character(input_ejemplo[escudo,9])
              sub3 <- as.numeric(as.character(input_ejemplo[escudo,10]))
              elemento4 <- as.character(input_ejemplo[escudo,11])
              sub4 <- as.numeric(as.character(input_ejemplo[escudo,12]))
          
          ###
          } # Fin Participantes necesarios...
          ##################################################################
          
          
          
          # Nuevas creaciones
          armado_elemento <- empaquetadora1(elemento1, sub1) 
          armado_oxigeno <- empaquetadora1(elemento2, sub2) 
          armado_oxido <- empaquetadora_oxido(elemento3, sub3, tipo1, elemento4, sub4) 
          
          
          
          
        # Guardamos en nuevo detalle en LaTeX  
        matrix_latex[escudo, 1] <- escudo
        matrix_latex[escudo, 2] <- coef1
        matrix_latex[escudo, 3] <- armado_elemento
        matrix_latex[escudo, 4] <- coef2
        matrix_latex[escudo, 5] <- armado_oxigeno
        matrix_latex[escudo, 6] <- coef3
        matrix_latex[escudo, 7] <- armado_oxido
        
        # Hacemos un cambio, para que no queden "NA"
        matrix_latex[c(1,2), c(6,7)] <- ""
          
        } # Fin for escudo
        
        
        
          
          # if (escudo == 8){
          #   fq <- armado_oxido
          #   if (is.na(fq)) fq <- "Sin Formula" 
          # }
          # 
        } # Fin for Ahora...
      ###########################################################################################
        
      
      # Parte 4: Salida
      {
      ###
        
        return(matrix_latex)
        
      ###  
      } # Fin Parte 4: Salida
      #######################################
      
      
      ###
      } # Fin if seguir
      ###########################################
                 

}  


} # Fin LaTeXOxidos()






LaTeXGeneral <- function(input_numfam = NULL, 
                         input_ejemplo = NULL,
                         input_language_interno = "es", 
                         input_language_optativo = NULL,
                         input_tabla = NULL) {
  
  # Especificaciones de lenguaje interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  # Participes necesarios generales
  {
    ###
    
    
    # General de la Tabla...
    simbolos <- as.character(input_tabla[["es"]][,2])
    tipo <- as.character(input_tabla[["es"]][,7])
    subtipo <- as.character(input_tabla[["es"]][,8])
    num_atom <- input_tabla[["es"]][,4]
    nombres <- as.character(input_tabla[["es"]][,3])
    
  ###  
  }
  ###################################################
  
  # Participantes necesario especifico 1 (solo primer elemento)
  {
  ###
    
    # Especifica del 1er elemento...
    input_simbolo1 <- as.character(input_ejemplo[1,3])
    dt1 <- simbolos ==  input_simbolo1
    input_num_atom1 <- num_atom[dt1]
    input_nombre1 <- nombres[dt1]
    input_tipo1 <- tipo[dt1]
  ###  
  }
  #############################################################
  
  
  # Participantes necesario especifico 2 (solo 2do elemento)
  {
  ###
    
    # Si se eligio Oxosal(5) u Oxosal(6)
    if (input_numfam > 5) {
      
    # Especifica del 2do elemento...
    input_simbolo2 <- as.character(input_ejemplo[1,3])
    dt2 <- simbolos ==  input_simbolo2
    input_num_atom2 <- num_atom[dt2]
    nombre2 <- nombres[dt2]
    tipo2 <- tipo[dt2]
    } # Fin if
    
  ###  
  }
  ##############################################
  
  
  
  # Resoluciones
  
  if (input_numfam == 1)    LaTeXOxidos(input_ejemplo = input_ejemplo, 
                                     input_language_interno = "es", 
                                     input_language_optativo = "es",  input_tabla = input_tabla)
  
  
} # End Function***