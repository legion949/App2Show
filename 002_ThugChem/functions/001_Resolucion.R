

# input_num_atom1 = 7
# input_valencia1 = 5
# input_language_interno = "es"
# input_language_optativo = NULL
# input_tabla = input_tabla


# Resol Oxidos
ResolOxidos <-function( input_num_atom1 = NULL, input_valencia1 = NULL, 
                        input_language_interno = "es", input_language_optativo = NULL,
                        input_tabla = NULL) {
  
  
  
  # Aclaracion de los input!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # Se le pide el tema de el idioma con el argumento "input_language_interno"
  # por si hiciera falta tener en cuenta algo del idioma, peeeeeeeeerooo...
  # en realidad esto solo funciona con una tabla en espanol.
  # Internamente, se utilizara la Tabla Periodica en Espanol, ya 
  # que por ejemplo, hay que detectar si el elemento seleccionado es un gas o no,
  # y si es un gas noble o no, y esa deteccion es con palabras especificas que
  # no pueden ser variables. Internamente todo debe ser manejado en un solo idioma, y sera
  # en espanol.
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  # Controles
  # # 1) input_num_atom1: no nulo,  vector, numerico y de longitud uno.
  # # 2) input_valencia1: no nulo,  vector, numerico y de longitud uno.
  # # 3) input_language_interno: no nulo, vector, caracter, de longitud uno , y dos caracteres 
  # # 4) input_language_optativo: no nulo, vector, caracter, de longitud uno , y dos caracteres 
  # # 5) input_tabla: no nulo, lista, cada elemento debe ser un data frame
  {
    ###
    
    control_OK <- list()
    control_paso <- 0
    seguir <- TRUE
    mensaje <- list()
    
    # # 1) input_num_atom1: vector, numerico y de longitud uno.
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_num_atom1))           seguir <- FALSE
      if (!is.vector(input_num_atom1))      seguir <- FALSE
      if(!is.numeric(input_num_atom1))    seguir <- FALSE
      if(length(input_num_atom1) != 1)  seguir <- FALSE
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### ResolOxidos (Control de argumentos - Paso 1 de 5) ### \n",
                                     "Problemas en el argumento: input_num_atom1 \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Vector \n", 
                                     "- Numerico \n",
                                     "- Longitud uno \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      # # 2) input_language_interno: vector, caracter, de longitud uno , y dos caracteres 
      # # 3) input_tabla: lista, cada elemento debe ser un data frame
      
      
      
    }
    
    
    # # 2) input_valencia1: vector, numerico y de longitud uno.
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_valencia1))           seguir <- FALSE
      if (!is.vector(input_valencia1))      seguir <- FALSE
      if(!is.numeric(input_valencia1))    seguir <- FALSE
      if(length(input_valencia1) != 1)  seguir <- FALSE
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### ResolOxidos (Control de argumentos - Paso 2 de 5) ### \n",
                                     "Problemas en el argumento: input_valencia1 \n",
                                     "Este argumento debe ser: \n",
                                     "- No nulo \n",
                                     "- Vector \n", 
                                     "- Numerico \n",
                                     "- Longitud uno \n")
        
        # Salida del cartel de aviso
        cat(mensaje[[control_paso]])
        
      }
      
      # # 2) input_language_interno: vector, caracter, de longitud uno , y dos caracteres 
      # # 3) input_tabla: lista, cada elemento debe ser un data frame
      
      
      
    }
    
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
        
        mensaje[[control_paso]] <- c("### ResolOxidos (Control de argumentos - Paso 3 de 5) ### \n",
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
      
      # # 2) input_language_interno: vector, caracter, de longitud uno , y dos caracteres 
      # # 3) input_tabla: lista, cada elemento debe ser un data frame
      
      
      
    }
    
    
    
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
        
        mensaje[[control_paso]] <- c("### ResolOxidos (Control de argumentos - Paso 4 de 5) ### \n",
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
      
      
      
    }
    
    
    
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
        
        mensaje[[control_paso]] <- c("### ResolOxidos (Control de argumentos - Paso 5 de 5) ### \n",
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
      
      # # 2) input_language_interno: vector, caracter, de longitud uno , y dos caracteres 
      # # 3) input_tabla: lista, cada elemento debe ser un data frame
      
      
      
    }
    
    ### 
  } # Fin Controles
  ##################################################
  
  
  
  # Ejecucion Oxidos
  # # Realizamos todos los pasos estequimetricos para que llevan a la
  # # resolucion correcta de un oxido
  {
    ###  
    
    # Si paso todos los controles se lleva a cabo el ejercicio...
    if (seguir) {
      
      
      # Participantes Necesarios
      # Reunimos toda la informacion que necesitamos para
      # llevar a cabo el ejercicio
      {
        ###
        
        # nombre1 <- "Sodio"
        # simbolo1 <- "Na"
        # valencia1 <-  1
        # estado1 <- "Sólido"
        # tipo1 <- "Metal"
        
        # Por defecto...
        num_atom1 <- as.numeric(as.character(input_num_atom1))
        valencia1 <-  as.numeric(as.character(input_valencia1))
        
        # Reclutados
        simbolo1 <- as.character(input_tabla[[input_language_interno]][num_atom1, 2])
        nombre1  <- as.character(input_tabla[[input_language_interno]][num_atom1, 3])
        tipo1 <- as.character(input_tabla[[input_language_interno]][num_atom1, 7])
        estado1 <- as.character(input_tabla[[input_language_interno]][num_atom1, 9])
        
        
        
        
        ###  
      } # Fin participantes Necesarios
      ####################################################
      
      
      
      # Resolucion Estequimetrica Paso a Paso
      # # Aqui se lleva a cabo la resolucion...
      # # Devolvera un objeto llamado "RESOLUCION"
      {
        ###  
        
        # Parte 1: Estructura general de "matrix_solucion"
        {
        ###
          
          # 1.0) Explicacion de la Parte 1 (por si la queres leer)
          {
            ###
            # Se da la estructura para una matriz o dataframe que tendra 
            # 8 filas y 12 columnas.
            # Cada fila es un paso estequimetrico (8 pasos para oxidos).
            # Cada columna es un detalle que va dentro de la ecuacion en cada paso.
            #
            # Cada "matrix_solucion" tendra las siguientes columnas en oxidos:
            #    1) Orden de paso
            #    2) Coeficiente del elemento en reactivos
            #    3) Abreviacion del elemento en reactivos
            #    4) Subindice del elemento en reactivos
            #    5) Coeficiente del Oxigeno en reactivos
            #    6) Simbolo quimico del Oxigeno en reactivos
            #    7) Subindice del Oxigeno en reactivos
            #    8) Coeficiente del Oxido en productos
            #    9) Simbolo quimico del elemento en productos
            #   10) Subindice del elemento en el oxido en productos
            #   11) Simbolo quimico del elemento en productos
            #   12) Subindice del oxigeno en el oxido en productos
            ###
          } # Fin explicacion
          ########################################################
          
          
          # 1.1) Creacion del "matrix_solucion"
          {
          ###
            
            # Cantidad Pasos Estequimetricos Oxidos (CPEO)
            CPEO <- 8
            
            mis_nombres <- c("Orden", 
                             "Coef1", "elementoReac", "Sub1",   # Coeficiente y subindice del elemento 
                             "Coef2", "OxigenoReac",  "Sub2",    # Coeficiente y subindice del Oxigeno 
                             "Coef3", "elementoProd", "Sub3", "OxigenoProd", "Sub4") # Coeficiente del oxido, subindice del elemento y el oxigeno del oxido
            
            
            matrix_solucion <- data.frame(matrix(NA, CPEO, length(mis_nombres)))
            colnames(matrix_solucion) <- mis_nombres
          ###  
          }
          ########################################################
          
          
          ###
        } # Fin Parte 1
        ##################################################
        
        
        
        # Parte 2: Resolucion Estequimetrica para Oxidos
        {
        ###
          
          
          # 2.0) Explicacion de la Parte 2 (por si la queres leer)
          {
            ###
            
            # Para el caso de los Oxidos...
            # La estequimetria se realiza de la siguiente forma:
            #
            # 1)  Se coloca el primero elemento y luego el Oxigeno separados ambos
            #     con un signo "+" siendo ambos reactivos. 
            #     No se agregan subindices ni coeficientes, todavía.
            #     ---- En lo informatico, coeficientes y subindices igual a 1 ---- 
            #     ---- y solo en reactivos. Nada en productos. ---- 
            #     A la derecha del oxigeno se coloca una flecha llamada "Flecha de Reaccion".
            #     Luego de la flecha de reaccion de detallaran los productos.
            #     
            #
            #  
            # 2)  El Oxigeno de los reactivos lleva un subindice 2 por ser un gas.
            #     Si el elemento que reaccionara con el oxigeno tambien es un gas
            #     y no es un gas noble, llevara tambien un dos como subindice.
            #     
            #
            #  
            # 3)  A la derecha de la flecha, se detallara el producto de la reaccion
            #     que en este caso es un OXIDO. La estructura del OXIDO es el 
            #     elemento y el Oxigeno juntos (sin espacio entre ellos) indicando
            #     que son parte de una misma molecula. En principio el subdice
            #     del elemento y del oxigeno en productos sera 1 en ambos casos.
            # 
            #
            # 4)  Se cambian los subindices en el Oxido.
            #     En el Oxido... el subindice del elemento sera la valencia del oxigeno.
            #     En el Oxido... el subindice del oxigeno sera la valencia del elemento.
            #
            # 5)  Si se puede, en el oxido, se simplifican los subindices por el 
            #     minimo comun divisor (mcd). Si ha sido posible simplificar los
            #     subindices, se coloca el mcd como coeficiente del oxido.
            #
            # 6)  Teniendo en cuenta las cantidades del elemento en productos
            #     se balancean las cantidades del elemento en reactivos.
            #     Para determinar las cantidades del elemento en reactivos 
            #     se multiplica el subindice por el coeficiente que posee el elemento
            #     en reactivos.
            #     Para determinar las cantidades del elemento en productos se
            #     multiplica su indice y su coeficiente en reactivos.
            #     Si las cantidades del elemento en reactivos y productos no son
            #     iguales se procede a modificar un coeficiente al elemento en reactivos
            #     de tal forma de igualar la cantidad que hay en producto.
            #
            #  
            # 7)  Se procede a balancear la ecuacion por segunda vez.
            #     Ya tenemos balanceado al oxigeno de manera global, ahora
            #     balanceamos al elemento.
            #     Este paso7 balancea solo al elemento.
            #     Con este fin se calcula la cantidad del elemento en reactivos y en
            #     productos. 
            #     Para calcular la cantidad del elemento en reactivos
            #     se multiplican entre si el coeficiente y el subindice correspondiente
            #     al elemento en reactivos.
            #     Para calcular la cantidad de elemento en productos
            #     se multiplican entre si el coeficiente del oxido y el subindice
            #     del elemento dentro del oxido, correspondiente a producto.
            #     Si las cantidad del elemento en reactivos y productos no son iguales
            #     solo debe modificarse el coeficiente del elemento en reativos, y de
            #     esta forma quedara balanceada finalmente toda la ecuacion.
            #     Este nuevo coeficiente se calculara como la cantidad del elemento
            #     en productos dividido la cantidad del elemento en reactivos.
            #     El resultado de esta operacion es el nuevo coeficiente para el 
            #     elemento en reactivos, y otorgara el balance general de toda la 
            #     ecuacion quimica.
            # 
            # 8)  Si es posible, se simplifican todos los coeficientes por el
            #     mismo minimo comun divisor (mcd).
            
            
            
            
            ###  
          } # Fin 2.0)
          #########################################################
          
          
          
          
          # 2.1) Algotirmo estequimetrico para Oxidos
          #      cuando el elemento no es el oxigeno
          #      ni un gas noble...
          # 1) Si el elemento no es un gas noble... y no es el oxigeno
          if (tipo1 != "Gas Noble" && simbolo1 != "O") {
          
          
          
            ###
            
            
            # Plantamos una semilla "cero" de pasos estequiometricos.
            escalon <- 0
            
            
            # Paso 1 de 8 - Oxidos
            {
              ###
              
              # 1)  Se coloca el primero elemento y luego el Oxigeno separados ambos
              #     con un signo "+" siendo ambos reactivos. 
              #     No se agregan subindices ni coeficientes, todavía.
              #     ---- En lo informatico, coeficientes y subindices igual a 1 ---- 
              #     ---- y solo en reactivos. Nada en productos. ---- 
              #     A la derecha del oxigeno se coloca una flecha llamada "Flecha de Reaccion".
              #     Luego de la flecha de reaccion de detallaran los productos.
              ##################################################################
              
              # Un paso mas...
              escalon <- escalon + 1
              
              # Creacion del paso 1...
              paso1 <- rep(NA, ncol(matrix_solucion))
              names(paso1) <- colnames(matrix_solucion)
              
              # Implementamos el paso1...
              paso1["Orden"] <-  escalon
              paso1["Coef1"] <- 1
              paso1["elementoReac"] <- simbolo1
              paso1["Sub1"] <- 1
              paso1["Coef2"] <- 1
              paso1["OxigenoReac"] <- "O"
              paso1["Sub2"] <- 1
              paso1["Coef3"] <- ""
              paso1["elementoProd"] <- ""
              paso1["Sub3"] <- ""
              paso1["OxigenoProd"] <- ""
              paso1["Sub4"] <- ""

              
              # Cargamos el paso1  
              matrix_solucion[escalon, ] <- paso1
              
              ###
            } # Fin Paso 1 de 8 - Oxidos
            ####################################################################
            
            
            # Paso 2 de 8 - Oxidos
            {
              ###
              
              # 2)  El Oxigeno de los reactivos lleva un subindice 2 por ser un gas.
              #     Si el elemento que reaccionara con el oxigeno tambien es un gas
              #     y no es un gas noble, llevara tambien un dos como subindice.
              
              # Un paso mas...
              escalon <- escalon + 1
              
              # Creacion del paso2
              paso2 <- paso1
              
              # Implementacion de cambios de escalon
              paso2["Orden"] <-  escalon
              
              # Subindice del elemento
              sub_ind1 <- 1   # Por defecto va un 1
              # Si es gaseoso y no es gas noble...  va un 2 en vez de un 1...
              if (estado1 == "Gas" & tipo1 != "Gas Noble") sub_ind1 <- 2
              
              # Subindice del Oxigeno
              sub_ind2 <- 2   # Va un dos por ser el Oxigeno un gas... por defecto
              
              
              # Implementamos las modificacion del paso2...
              paso2["Sub1"] <- sub_ind1
              paso2["Sub2"] <- sub_ind2
              
              
              # Cargamos el paso2  
              matrix_solucion[escalon, ] <- paso2
              
              # Eliminamos lo que ya no hace falta...
              remove(sub_ind1, sub_ind2)
              
              ###
            } # Fin Paso 2 de 8 - Oxidos
            ####################################################################
            
            
            # Paso 3 de 8 - Oxidos
            {
              ###
              
              # 3)  A la derecha de la flecha, se detallara el producto de la reaccion
              #     que en este caso es un OXIDO. La estructura del OXIDO es el 
              #     elemento y el Oxigeno juntos (sin espacio entre ellos) indicando
              #     que son parte de una misma molecula. En principio el subdice
              #     del elemento y del oxigeno en productos sera 1 en ambos casos.
              #     El coeficiente del producto es 1 tambien.
              
              # Un paso mas...
              escalon <- escalon + 1
              
              
              # Creacion del paso 3...
              paso3 <- paso2
              
              # Implementacion de cambios de escalon
              paso3["Orden"] <-  escalon
              
              # Detalle de los participantes en el paso 3...
              elemento_prod <- simbolo1
              oxigeno_prod <- "O"
              
              # Coeficiente inical para el oxido
              coef3 <- 1    # Por defecto va un 1
              
              # Subindice dentro del Oxido para el elemento y el oxigeno
              sub_ind3 <- 1   # Por defecto va un 1
              sub_ind4 <- 1   # Por defecto va un 1
              
              
              # Implementamos las modificacion del paso3...
              paso3["Coef3"] <- coef3
              paso3["elementoProd"] <- elemento_prod
              paso3["Sub3"] <- sub_ind3
              paso3["OxigenoProd"] <- oxigeno_prod
              paso3["Sub4"] <- sub_ind4
              
              
              # Cargamos el paso3  
              matrix_solucion[escalon, ] <- paso3
              
              # Eliminamos lo que ya no hace falta...
              remove(coef3, elemento_prod, sub_ind3, oxigeno_prod, sub_ind4)
              
              ###
            } # Fin Paso 3 de 8 - Oxidos
            ####################################################################
            
            
            # Paso 4 de 8 - Oxidos
            {
              ###
              
              # 4)  Se cambian los subindices en el Oxido.
              #     En el Oxido... el subindice del elemento sera la valencia del oxigeno.
              #     En el Oxido... el subindice del oxigeno sera la valencia del elemento.
              
              # Un paso mas...
              escalon <- escalon + 1
              
              # Creacion del paso4
              paso4 <- paso3
              
              # Implementacion de cambios de escalon
              paso4["Orden"] <-  escalon
              
              # Subindices del elemento y el oxigeno en productos...
              sub_ind3 <- 2   # Aqui va la valencia del oxigeno
              sub_ind4 <- valencia1    # Aqui va la valencia del elemento
              
              # Implementamos las modificacion del paso4...
              paso4["Sub3"] <- sub_ind3
              paso4["Sub4"] <- sub_ind4
              
              
              # Cargamos el paso4  
              matrix_solucion[escalon, ] <- paso4
              
              # Eliminamos lo que ya no hace falta...
              remove(sub_ind3, sub_ind4)
              
              ###
            } # Fin Paso 4 de 8 - Oxidos
            ####################################################################
            
            
            # Paso 5 de 8 - Oxidos
            {
              ###
              
              # 5)  Si se puede, en el oxido, se simplifican los subindices por el 
              #     minimo comun divisor (mcd). Si ha sido posible simplificar los
              #     subindices, se coloca el mcd como coeficiente del oxido.
              
              # Un paso mas...
              escalon <- escalon + 1
              
              # Creacion del paso 5
              paso5 <- paso4
              
              # Implementacion de cambios de escalon
              paso5["Orden"] <-  escalon
              
              # Recuperamos los subindices del paso 4...
              sub_ind3 <- as.numeric(as.character(paso4["Sub3"]))
              sub_ind4 <- as.numeric(as.character(paso4["Sub4"])) 
              
              # Calculamos el minimo comun divisor.
              # Encontre una libreria que lo hace directamente!
              library(numbers)
              mcd <- mGCD(c(sub_ind3, sub_ind4))
              
              # Creamos los nuevos subindices en el oxido...
              mod_sub_ind3 <- sub_ind3/mcd
              mod_sub_ind4 <- sub_ind4/mcd
              
              # Implementamos las modificacion del paso5...
              paso5["Sub3"] <- mod_sub_ind3
              paso5["Sub4"] <- mod_sub_ind4
              paso5["Coef3"] <- mcd
              
              # Cargamos el paso5  
              matrix_solucion[escalon, ] <- paso5
              
              # Eliminamos lo que ya no hace falta...
              remove(sub_ind3, sub_ind4, mod_sub_ind3, mod_sub_ind4, mcd)
              
              ###
            } # Fin Paso 5 de 8 - Oxidos
            ##################################################################################################################
            
            
            # Paso 6 de 8 - Oxidos
            {
              ###
              
              # 6)  Se procede a balancear la ecuacion por primera vez.
              #     Primero se balancea la cantidad de oxigeno y luego la cantidad
              #     del elemento. 
              #     Este paso 6 balancea solo al oxigeno.
              #     Con este fin se calcula la cantidad de oxigeno en reactivos y en
              #     productos. 
              #     Para calcular la cantidad de oxigeno en reactivos
              #     se multiplican entre si el coeficiente y el subindice correspondiente
              #     al oxigeno en reactivos.
              #     Para calcular la cantidad de oxigeno en productos
              #     se multiplican entre si el coeficiente del oxido y el subindice
              #     del oxigeno dentro del oxido, correspondiente a producto.
              #     Si las cantidad de oxigeno en reactivos y productos no son iguales
              #     se colocara el subindice del oxigeno en el oxido como coeficiente
              #     del oxigeno en reactivos, y se colocara el subindice del oxigeno 
              #     en reacitivos como coeficiente del oxido en productos. Este procedi
              #     miento balancea la cantidad de oxigeno de la ecuacion.
              
              
              
              # Un paso mas...
              escalon <- escalon + 1
              
              # Creacion del paso6...
              paso6 <- paso5
              
              # Implementacion de cambios de escalon
              paso6["Orden"] <-  escalon
              
              # Recuperamos subindices y coeficiente del paso5 para el oxigeno y asi
              # balancear al oxigeno de manera global...
              coef2 <- as.numeric(as.character(paso5["Coef2"]))
              sub_ind2 <- as.numeric(as.character(paso5["Sub2"]))
              coef3 <- as.numeric(as.character(paso5["Coef3"]))
              sub_ind4 <- as.numeric(as.character(paso5["Sub4"]))
              
              
              # Calculamos la cantidad de oxigeno, en reactivos y en productos
              cantidad_oxigeno_reactivos <- coef2*sub_ind2
              cantidad_oxigeno_productos <- coef3*sub_ind4
              
              # Si las cantidades de oxigeno son iguales, el paso6 es igual a paso5
              # y eso ya esta armado por defecto al crearse el paso6.
              # Ahora... si las cantidades de oxigeno son diferentes, hay que implementar
              # los cambios correspondientes.
              if (cantidad_oxigeno_reactivos != cantidad_oxigeno_productos){
                
                # Generamos los cambios para el paso6...
                mod_coef2 <- sub_ind4
                mod_coef3 <- sub_ind2
                
                # Implementamos las modificacion del paso6...
                paso6["Coef2"] <- mod_coef2
                paso6["Coef3"] <- mod_coef3
                
                # Eliminamos lo que ya no hace falta de este paso interno...
                remove(mod_coef2, mod_coef3)
                
              } # Fin if
              
              
              # Cargamos el paso6
              matrix_solucion[escalon, ] <- paso6
              
              # Eliminamos lo que ya no hace falta...
              remove(coef2, sub_ind2, coef3, sub_ind4,
                     cantidad_oxigeno_reactivos, cantidad_oxigeno_productos)
              
              ###
            } # Fin Paso 6 de 8 - Oxidos
            ##################################################################################################################
            
            
            
            # Paso 7 de 8 - Oxidos
            {
              ###
              
              # 7)  Se procede a balancear la ecuacion por segunda vez.
              #     Ya tenemos balanceado al oxigeno de manera global, ahora
              #     balanceamos al elemento.
              #     Este paso7 balancea solo al elemento.
              #     Con este fin se calcula la cantidad del elemento en reactivos y en
              #     productos. 
              #     Para calcular la cantidad del elemento en reactivos
              #     se multiplican entre si el coeficiente y el subindice correspondiente
              #     al elemento en reactivos.
              #     Para calcular la cantidad de elemento en productos
              #     se multiplican entre si el coeficiente del oxido y el subindice
              #     del elemento dentro del oxido, correspondiente a producto.
              #     Si las cantidad del elemento en reactivos y productos no son iguales
              #     solo debe modificarse el coeficiente del elemento en reativos, y de
              #     esta forma quedara balanceada finalmente toda la ecuacion.
              #     Este nuevo coeficiente se calculara como la cantidad del elemento
              #     en productos dividido la cantidad del elemento en reactivos.
              #     El resultado de esta operacion es el nuevo coeficiente para el 
              #     elemento en reactivos, y otorgara el balance general de toda la 
              #     ecuacion quimica.
              
              
              
              # Un paso mas...
              escalon <- escalon + 1
              
              # Creacion del paso7...
              paso7 <- paso6
              
              # Implementacion de cambios de escalon
              paso7["Orden"] <-  escalon
              
              # Recuperamos subindices y coeficiente del paso6 para el elemento
              # y asi balancear al oxigeno de manera global...
              coef1 <- as.numeric(as.character(paso6["Coef1"]))
              sub_ind1 <- as.numeric(as.character(paso6["Sub1"]))
              coef3 <- as.numeric(as.character(paso6["Coef3"]))
              sub_ind3 <- as.numeric(as.character(paso6["Sub3"]))
              
              
              # Calculamos la cantidad del elemento en reactivos y en productos
              cantidad_elemento_reactivos <- coef1*sub_ind1
              cantidad_elemento_productos <- coef3*sub_ind3
              
              
              
              
              # Si las cantidades del elemento son iguales, el paso7 es igual a paso6
              # y eso ya esta armado por defecto al crearse el paso7.
              # Ahora... si las cantidades de oxigeno son diferentes, hay que implementar
              # los cambios correspondientes.
              if (cantidad_elemento_reactivos != cantidad_elemento_productos){
                
                # Calculamos el nuevo coeficiente por si hace falta luego...
                # Este seria el nuevo coeficiente para el elemento en reactivos            
                mod_coef1 <- cantidad_elemento_productos / cantidad_elemento_reactivos  
                
                
                # Implementamos las modificacion del paso7...
                paso7["Coef1"] <- mod_coef1
                
                # Eliminamos lo que ya no hace falta de este paso interno...
                remove(mod_coef1)
                
              } # Fin if
              
              
              
              
              # Cargamos el paso7
              matrix_solucion[escalon, ] <- paso7
              
              # Eliminamos lo que ya no hace falta...
              remove(coef1, sub_ind1, coef3, sub_ind3,
                     cantidad_elemento_reactivos, cantidad_elemento_productos)
              
              ###
            } # Fin Paso 7 de 8 - Oxidos
            ##################################################################################################################
            
            
            # Paso 8 de 8 - Oxidos
            {
              ###
              
              # 8)  Si es posible, se simplifican todos los coeficientes por el
              #     mismo minimo comun divisor (mcd).
              
              # Un paso mas...
              escalon <- escalon + 1
              
              # Creacion del paso8
              paso8 <- paso7
              
              # Implementacion de cambios de escalon
              paso8["Orden"] <-  escalon
              
              # Recuperamos los tres coeficientes del paso7...
              coef1 <- as.numeric(as.character(paso7["Coef1"]))
              coef2 <- as.numeric(as.character(paso7["Coef2"]))
              coef3 <- as.numeric(as.character(paso7["Coef3"]))
              
              
              # Calculamos el minimo comun divisor.
              # Encontre una libreria que lo hace directamente!
              library(numbers)
              mcd <- mGCD(c(coef1, coef2, coef3))
              
              # Creamos los nuevos subindices en el oxido...
              mod_coef1 <- coef1/mcd
              mod_coef2 <- coef2/mcd
              mod_coef3 <- coef3/mcd
              
              
              # Si el minimo comun divisor es igual a 1, no hay que hacer
              # ningun cambio, por que es asi por defecto.
              # Pero si el minimo comun dividor es diferente a 1, hay que 
              # implementar cambios...
              if (mcd != 1){
                
                # Implementamos las modificacion del paso5...
                paso8["Coef1"] <- mod_coef1
                paso8["Coef2"] <- mod_coef2
                paso8["Coef3"] <- mod_coef3
                
                # Eliminamos lo que ya no hace falta de este paso interno...
                remove(mod_coef1, mod_coef2, mod_coef3)
                
              } # Fin if
              
              
              
              # Cargamos el paso8  
              matrix_solucion[escalon, ] <- paso8
              
              # Eliminamos lo que ya no hace falta...
              remove(coef1, coef2, coef3, mcd)
              
              ###
            } # Fin Paso 8 de 8 - Oxidos
            ##################################################################################################################
            
            
            
            ###
          } #  Fin 2.1)
          ########################################################
          
          
          
          # 2.2) Casos especiales para ecuaciones de Oxidos
          {
            ###
            # Hay varios casos particulares que hay que tener en cuenta...
            # Caso 1) El elemento es el Oxigeno.
            # Caso 2) El elemento es un gas noble.
            # En estos casos, la resolucion del ejercicio es completamente 
            # diferente, y conforma un caso particular que debe ser detallado
            # especificamente en un lugar diferente de la programacion.
            
            
            # Caso 1) El elemento es el Oxigeno
            {
              ###
              if (simbolo1 == "O") {
                
                # Algotirmo estequimetrico para el "OXIGENO" solamente
                {
                  ###  
                  
                  # Plantamos una semilla "cero" de pasos estequiometricos.
                  escalon <- 0
                  
                  
                  # Paso 1 de 8 - Caso 1 - Oxigeno 
                  {
                    ###
                    
                    # 1)  Se coloca el oxigeno un signo "+" y de nuevo el oxigeno en reactivos.
                    #     No se agregan subindices ni coeficientes, todavía.
                    #     ---- En lo informatico, coeficientes y subindices igual a 1 ---- 
                    #     ---- y solo en reactivos. Nada en productos. ---- 
                    #     A la derecha del segundo oxigeno se coloca una flecha llamada "Flecha de Reaccion".
                    #     Luego de la flecha de reaccion de detallaran los productos.
                    ##################################################################
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso 1...
                    paso1 <- rep(NA, ncol(matrix_solucion))
                    names(paso1) <- colnames(matrix_solucion)
                    
                    # Implementamos el paso1...
                    paso1["Orden"] <-  escalon
                    paso1["Coef1"] <- 1
                    paso1["elementoReac"] <- simbolo1
                    paso1["Sub1"] <- 1
                    paso1["Coef2"] <- 1
                    paso1["OxigenoReac"] <- "O"
                    paso1["Sub2"] <- 1
                    paso1["Coef3"] <- ""
                    paso1["elementoProd"] <- ""
                    paso1["Sub3"] <- ""
                    paso1["OxigenoProd"] <- ""
                    paso1["Sub4"] <- ""

                    
                    # Cargamos el paso1  
                    matrix_solucion[escalon, ] <- paso1
                    
                    ###
                  } # Fin Paso 1 de 8 - Caso 1 - Oxigeno 
                  ####################################################################
                  
                  
                  # Paso 2 de 8 - Caso 1 - Oxigeno 
                  {
                    ###
                    
                    # 2)  Ambos oxigenos de reactivos llevan un subindice un 2 por ser un gas.
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso2
                    paso2 <- paso1
                    
                    # Implementacion de cambios de escalon
                    paso2["Orden"] <-  escalon
                    
                    # Subindice para ambos oxigenos...
                    sub_ind1 <- 2   # Va un dos por ser el Oxigeno un gas... por defecto
                    sub_ind2 <- 2   # Va un dos por ser el Oxigeno un gas... por defecto
                    
                    
                    # Implementamos las modificacion del paso2...
                    paso2["Sub1"] <- sub_ind1
                    paso2["Sub2"] <- sub_ind2
                    
                    
                    # Cargamos el paso2  
                    matrix_solucion[escalon, ] <- paso2
                    
                    # Eliminamos lo que ya no hace falta...
                    remove(sub_ind1, sub_ind2)
                    
                    ###
                  } # Fin Paso 2 de 8 - Caso 1 - Oxigeno 
                  ####################################################################
                  
                  
                  # Paso 3 de 8 - Caso 1 - Oxigeno 
                  {
                    ###
                    
                    # 3)  A la derecha de la flecha, se detallara el producto de la reaccion
                    #     que en este caso particular es solo el oxigeno.
                    #     Todavia no le ponemos el subindice por ser gaseoso. Solo
                    #     Presentamos el oxigeno como producto.
                    #     Asignamos al coeficiente3 el valor de 1.
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    
                    # Creacion del paso 3...
                    paso3 <- paso2
                    
                    # Implementacion de cambios de escalon
                    paso3["Orden"] <-  escalon
                    
                    # Detalle de los participantes en el paso 3...
                    elemento_prod <- simbolo1
          
                    
                    # Coeficiente de productos por defecto para paso3...
                    coef3 <- 1  # Por defecto va un 1
                    
                    # Subindice dentro del oxigeno en productos
                    sub_ind3 <- 1   # Por defecto va un 1
                    
                    
                    
                    # Implementamos las modificacion del paso3...
                    paso3["elementoProd"] <- elemento_prod
                    paso3["Sub3"] <- sub_ind3
                    paso3["Coef3"] <- coef3
                    
                    
                    # Cargamos el paso3  
                    matrix_solucion[escalon, ] <- paso3
                    
                    # Eliminamos lo que ya no hace falta...
                    remove(coef3, sub_ind3, elemento_prod)
                    
                    ###
                  } # Fin Paso 3 de 8 - Caso 1 - Oxigeno 
                  ####################################################################
                  
                  
                  # Paso 4 de 8 - Caso 1 - Oxigeno 
                  {
                    ###
                    
                    # 4)  Se cambian los subindices en productos en el Oxido.
                    #     En este caso solo esta el oxigeno en productos.
                    #     Como es un gas, debe llevar subindice 2
                    
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso4
                    paso4 <- paso3
                    
                    # Implementacion de cambios de escalon
                    paso4["Orden"] <-  escalon
                    
                    # Subindices del elemento y el oxigeno en productos...
                    sub_ind3 <- 2   # Aqui va un 2 por ser el oxigeno biatomico
                    
                    # Implementamos las modificacion del paso4...
                    paso4["Sub3"] <- sub_ind3
                    
                    
                    # Cargamos el paso4  
                    matrix_solucion[escalon, ] <- paso4
                    
                    # Eliminamos lo que ya no hace falta...
                    remove(sub_ind3)
                    
                    ###
                  } # Fin Paso 4 de 8 - Caso 1 - Oxigeno 
                  ####################################################################
                  
                  
                  # Paso 5 de 8 - Caso 1 - Oxigeno 
                  {
                    ###
                    
                    # 5)  Este paso era para la simplificacion de subindices en el
                    #     oxido. Aca no hay nada que hacer. El paso5 es igual al paso4.
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso 5
                    paso5 <- paso4
                    
                    # Implementacion de cambios
                    paso5["Orden"] <-  escalon
                    
                    # Cargamos el paso4  
                    matrix_solucion[escalon, ] <- paso5
                    
                    ###
                  } # Fin Paso 5 de 8 - Caso 1 - Oxigeno 
                  ##################################################################################################################
                  
                  
                  # Paso 6 de 8 - Caso 1 - Oxigeno 
                  {
                    ###
                    
                    # 6)  Se procede a balancear la ecuacion por primera vez.
                    #     En la estequimetria normal, se hace en dos pasos, y este
                    #     es el primero. Para este caso, aqui ya se balancea
                    #     todo el oxigeno de la ecuacion quimica.
                    #     Sumaremos todo lo de reactivos y sumaremos todo lo de
                    #     productos y haremos el balanceo que haga falta.
                    #     Lo unico que cambiara sera el coeficente 3, que es
                    #     el coeficente en productos.
                    
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso6...
                    paso6 <- paso5
                    
                    # Implementacion de cambios de escalon
                    paso6["Orden"] <-  escalon
                    
                    # Recuperamos subindices y coeficiente del paso5 para el oxigeno y asi
                    # balancear al oxigeno de manera global...
                    coef1 <- as.numeric(as.character(paso5["Coef1"]))
                    sub_ind1 <- as.numeric(as.character(paso5["Sub1"]))
                    coef2 <- as.numeric(as.character(paso5["Coef2"]))
                    sub_ind2 <- as.numeric(as.character(paso5["Sub2"]))
                    coef3 <- as.numeric(as.character(paso5["Coef3"]))
                    sub_ind3 <- as.numeric(as.character(paso5["Sub3"]))
                    
                    
                    # Calculamos la cantidad de oxigeno, en reactivos y en productos
                    cantidad_oxigeno_reactivos <- coef1*sub_ind1 + coef2*sub_ind2
                    cantidad_oxigeno_productos <- coef3*sub_ind3
                    
                    # Si las cantidades de oxigeno son iguales, el paso6 es igual a paso5
                    # y eso ya esta armado por defecto al crearse el paso6.
                    # Ahora... si las cantidades de oxigeno son diferentes, hay 
                    # que implementar los cambios correspondientes sobre 
                    # el coeficiente 3 en este caso particular.
                    if (cantidad_oxigeno_reactivos != cantidad_oxigeno_productos){
                      
                      # Generamos los cambios para el paso6...
                      mod_coef3 <- cantidad_oxigeno_reactivos / cantidad_oxigeno_productos
                      
                      
                      # Implementamos las modificacion del paso6...
                      paso6["Coef3"] <- mod_coef3
                      
                      # Eliminamos lo que ya no hace falta de este paso interno...
                      remove(mod_coef3)
                      
                    } # Fin if
                    
                    

                    # Cargamos el paso6
                    matrix_solucion[escalon, ] <- paso6
                    
                    # Eliminamos lo que ya no hace falta...
                    remove(coef1, sub_ind1, coef2, sub_ind2, coef3, sub_ind3, 
                           cantidad_oxigeno_reactivos, cantidad_oxigeno_productos)
                    
                    ###
                  } # Fin Paso 6 de 8 - Oxidos
                  ##################################################################################################################
                  
                  
                  # Paso 7 de 8 - Caso 1 - Oxigeno
                  {
                    ###
                    
                    # 7)  Normalmente, se procede a balancear la ecuacion por segunda vez,
                    #     pero nosotros ya tenemos todo balanceado y listo para este
                    #     caso especial. Por lo tanto el paso 7 es igual al paso 6.
                    
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso7...
                    paso7 <- paso6
                    
                    # Implementacion de cambios de escalon
                    paso7["Orden"] <-  escalon
                    
                    # Cargamos el paso7
                    matrix_solucion[escalon, ] <- paso7
                    
                    
                    
                    ###
                  } # Fin Paso 7 de 8 - Caso 1 - Oxigeno
                  ##################################################################################################################
                  
                  
                  # Paso 8 de 8 - Caso 1 - Oxigeno
                  {
                    ###
                    
                    # 8)  Normalmente, se procede simplificar los coeficiente
                    #     de toda la ecuacion pero nosotros ya tenemos 
                    #     todo balanceado y listo para este caso especial. 
                    #     Por lo tanto el paso 8 es igual al paso 7.
                    
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso7...
                    paso8 <- paso7
                    
                    # Implementacion de cambios de escalon
                    paso8["Orden"] <-  escalon
                    
                    # Cargamos el paso8
                    matrix_solucion[escalon, ] <- paso8
                    
                    
                    
                    ###
                  } # Fin Paso 8 de 8 - Caso 1 - Oxigeno
                  ##################################################################################################################
                  
                  
                  ###
                } # Fin Algotirmo estequimetrico para el "OXIGENO" solamente
                #################################################################
                
                
              } # Fin if "O"
              ###############################################################    
              
              
              ###  
            } # Caso 1)
            ################################################
            
            
            # Caso 2) El elemento es un gas noble
            {
              ###
              if (tipo1 == "Gas Noble") {
                
                # Algotirmo estequimetrico para gases nobles solamente
                {
                  ###  
                  
                  # Plantamos una semilla "cero" de pasos estequiometricos.
                  escalon <- 0
                  
                  
                  # Paso 1 de 8 - Caso 2 - Gas Noble
                  {
                    ###
                    
                    # 1)  Se coloca el oxigeno un signo "+" y de gas noble en reactivos.
                    #     No se agregan subindices ni coeficientes, todavía.
                    #     ---- En lo informatico, coeficientes y subindices igual a 1 ---- 
                    #     ---- y solo en reactivos. Nada en productos. ---- 
                    #     A la derecha del gas noble se coloca una flecha llamada 
                    #     "Flecha de Reaccion".
                    #     Luego de la flecha de reaccion de detallaran los productos.
                    ##################################################################
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso 1...
                    paso1 <- rep(NA, ncol(matrix_solucion))
                    names(paso1) <- colnames(matrix_solucion)
                    
                    # Implementamos el paso1...
                    paso1["Orden"] <-  escalon
                    paso1["Coef1"] <- 1
                    paso1["elementoReac"] <- simbolo1
                    paso1["Sub1"] <- 1
                    paso1["Coef2"] <- 1
                    paso1["OxigenoReac"] <- "O"
                    paso1["Sub2"] <- 1
                    paso1["Coef3"] <- ""
                    paso1["elementoProd"] <- ""
                    paso1["Sub3"] <- ""
                    paso1["OxigenoProd"] <- ""
                    paso1["Sub4"] <- ""

                    
                    # Cargamos el paso1  
                    matrix_solucion[escalon, ] <- paso1
                    
                    ###
                  } # Fin Paso 1 de 8 - Caso 2 - Gas Noble
                  ####################################################################
                  
                  
                  # Paso 2 de 8 - Caso 2 - Gas Noble
                  {
                    ###
                    
                    # 2)  El oxigeno lleva un 2 como subindice por ser
                    #     un gas. El gas noble no lleva subindice 2 sino
                    #     que lleva un 1 de subindice.
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso2
                    paso2 <- paso1
                    
                    # Implementacion de cambios de escalon
                    paso2["Orden"] <-  escalon
                    
                    # Subindices para el gas noble y el oxigeno en reactivos...
                    sub_ind1 <- 1   # Va un uno para el gas noble, por ser gas noble... por defecto
                    sub_ind2 <- 2   # Va un dos por ser el oxigeno un gas... por defecto
                    
                    
                    # Implementamos las modificacion del paso2...
                    paso2["Sub1"] <- sub_ind1
                    paso2["Sub2"] <- sub_ind2
                    
                    
                    # Cargamos el paso2  
                    matrix_solucion[escalon, ] <- paso2
                    
                    # Eliminamos lo que ya no hace falta...
                    remove(sub_ind1, sub_ind2)
                    
                    ###
                  } # Fin Paso 2 de 8 -  Caso 2 - Gas Noble
                  ####################################################################
                  
                  
                  # Paso 3 de 8 -  Caso 2 - Gas Noble
                  {
                    ###
                    
                    # 3)  A la derecha de la flecha, se detallara el producto de la reaccion
                    #     que son el oxigeno y el gas noble separados por el signo "+",
                    #     pero no se agrega el sub indice del oxigeno todavia.
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    
                    # Creacion del paso 3...
                    paso3 <- paso2
                    
                    # Implementacion de cambios de escalon
                    paso3["Orden"] <-  escalon
                    
                    # Detalle de los participantes en el paso 3...
                    elemento_prod <- simbolo1
                    oxigeno_prod <- "O"
                    
                    # Coeficientes y subindice en productos
                    coef3 <- 1
                    sub_ind3 <- 1   # Por defecto va un 1 para el gas noble
                    sub_ind4 <- 2   # Por defecto va un 2 para el oxigeno
                    
                    # Coeficiente en productos
                    
                    
                    # Implementamos las modificacion del paso3...
                    paso3["Coef3"] <- coef3
                    paso3["elementoProd"] <- elemento_prod
                    paso3["Sub3"] <- sub_ind3
                    paso3["OxigenoProd"] <- oxigeno_prod
                    paso3["Sub4"] <- sub_ind4
                    
                    
                    
                    # Cargamos el paso3  
                    matrix_solucion[escalon, ] <- paso3
                    
                    # Eliminamos lo que ya no hace falta...
                    remove(coef3, elemento_prod, sub_ind3, oxigeno_prod, sub_ind4)
                    
                    ###
                  } # Fin Paso 3 de 8 -  Caso 2 - Gas Noble
                  ####################################################################
                  
                  
                  # Paso 4 de 8 -  Caso 2 - Gas Noble
                  {
                    ###
                    
                    # 4)  Se cambian los subindices en productos.
                    #     Si bien en este caso no hay reaccion , se le coloca 
                    #     al oxigeno un 2 como subindice en productos por ser un gas.
                    
                    
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso4
                    paso4 <- paso3
                    
                    # Implementacion de cambios de escalon
                    paso4["Orden"] <-  escalon
                    
                    # Subindices del elemento y el oxigeno en productos...
                    sub_ind4 <- 2   # Aqui va un 2 por ser el oxigeno biatomico
                    
                    # Implementamos las modificacion del paso4...
                    paso4["Sub4"] <- sub_ind4
                    
                    
                    # Cargamos el paso4  
                    matrix_solucion[escalon, ] <- paso4
                    
                    # Eliminamos lo que ya no hace falta...
                    remove(sub_ind4)
                    
                    ###
                  } # Fin Paso 4 de 8 -  Caso 2 - Gas Noble
                  ####################################################################
                  
                  
                  
                  # Paso 5 de 8 -  Caso 2 - Gas Noble
                  {
                    ###
                    
                    # 5)  Este paso era para la simplificacion de subindices en el
                    #     oxido. Aca no hay nada que hacer. El paso5 es igual al paso4.
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso 5
                    paso5 <- paso4
                    
                    # Implementacion de cambios de escalon
                    paso5["Orden"] <-  escalon
                    
                    # Cargamos el paso5  
                    matrix_solucion[escalon, ] <- paso5
                    
                    ###
                  } # Fin Paso 5 de 8 -  Caso 2 - Gas Noble
                  ##################################################################################################################
                  
                  
                  # Paso 6 de 8 -  Caso 2 - Gas Noble
                  {
                    ###
                    
                    # 6)  Supuestamente, se procede a balancear la ecuacion por primera vez.
                    #     En la estequimetria normal, se hace en dos pasos, y este
                    #     es el primero. 
                    #     Para este caso ya esta todo balanceado y no hay nada
                    #     mas que hacer. El paso 6 es igual que el 5.
                    
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso6...
                    paso6 <- paso5
                    
                    # Implementacion de cambios de escalon
                    paso6["Orden"] <-  escalon
                    
                    # Cargamos el paso6  
                    matrix_solucion[escalon, ] <- paso6
                    
                    ###
                  } # Fin Paso 6 de 8 -  Caso 2 - Gas Noble
                  ##################################################################################################################
                  
                  
                  
                  # Paso 7 de 8 -  Caso 2 - Gas Noble
                  {
                    ###
                    
                    # 7)  Normalmente, se procede a balancear la ecuacion por segunda vez,
                    #     pero nosotros ya tenemos todo balanceado y listo para este
                    #     caso especial. Por lo tanto el paso 7 es igual al paso 6.
                    
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso7...
                    paso7 <- paso6
                    
                    # Implementacion de cambios de escalon
                    paso7["Orden"] <-  escalon
                    
                    # Cargamos el paso7
                    matrix_solucion[escalon, ] <- paso7
                    
                    
                    
                    ###
                  } # Fin Paso 7 de 8 -  Caso 2 - Gas Noble
                  ##################################################################################################################
                  
                  
                  # Paso 8 de 8 -  Caso 2 - Gas Noble
                  {
                    ###
                    
                    # 8)  Normalmente, se procede simplificar los coeficiente
                    #     de toda la ecuacion pero nosotros ya tenemos 
                    #     todo balanceado y listo para este caso especial. 
                    #     Por lo tanto el paso 8 es igual al paso 7.
                    
                    
                    # Un paso mas...
                    escalon <- escalon + 1
                    
                    # Creacion del paso7...
                    paso8 <- paso7
                    
                    # Implementacion de cambios de escalon
                    paso8["Orden"] <-  escalon
                    
                    # Cargamos el paso8
                    matrix_solucion[escalon, ] <- paso8
                    
                    
                    
                    ###
                  } # Fin Paso 8 de 8 -  Caso 2 - Gas Noble
                  ##################################################################################################################
                  
                  
                  ###
                } # Fin Algotirmo estequimetrico para gases nobles solamente
                #################################################################
                
                
              } # Fin if "Gas Noble"
              ###############################################################    
              
              
              ###  
            } # Caso 2)
            ################################################
            
            
            ###  
          } # Fin 2.2)
          ###############################################
          
          
          
          
          
          
         
          
          
          
          
          
          
          
          
          ###  
        } # Fin  Resolucion Estequimetrica Paso a Paso
        ###################################################
        
        
        
        # Parte 3: Objetos de salida de la funcion
        {
        ###
          
          
          return(matrix_solucion)
          
          
        ###  
        } # Fin Parte 3
        ############################################
        
        
        ### 
      } # Fin Si paso todos los controles
      #####################################################
      
      
      
      ###    
    } # Fin Ejecucion Oxidos
    ###################################################################
    
    
    # nombre1 <- "Sodio"
    # simbolo1 <- "Na"
    # valencia1 <-  1
    # estado1 <- "Sólido"
    # tipo1 <- "Metal"
    
    
    
    
    
    
  }     
  
  
} # End Function***




# Resolucion General de todo (CAPO!)
ResolGeneral <- function(input_numfam = NULL, 
                         input_num_atom1 = NULL, input_valencia1 = NULL,
                         input_num_atom2 = NULL, input_valencia2 = NULL,
                         input_language_interno = "es", 
                         input_language_optativo = NULL,
                         input_tabla = NULL) {
  
  # Especificaciones de lenguaje interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  
  # Resoluciones
  
  if (input_numfam == 1) ResolOxidos(input_num_atom1 = input_num_atom1, 
                                     input_valencia1 = input_valencia1,
                                     input_language_interno = input_language_interno,
                                     input_language_optativo = input_language_optativo,
                                     input_tabla = input_tabla)
  
  
} # End Function***