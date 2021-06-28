

grafOxidos <- function(input_latex = NULL, 
                       input_paso = NULL,
                       input_language_interno = "es", 
                       input_language_optativo = NULL,
                       input_tabla = NULL,
                       input_color_fondo = "orange", 
                       input_color_general = "black",
                       input_color_especifico = "blue",
                       input_color_ecuacion = "black",
                       input_color_signo = "black",
                       input_color_paso = "blue") {
  
  
  
  # Especificaciones de lenguaje interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
  # Controles
  # # 1) input_latex: no nulo, data.frame o matrix, con filas y columnas
  # # 2) input_paso: no nulo, vector, numerico, longitud uno.
  # # 3) input_language_interno: no nulo, vector, caracter, de longitud uno , y dos caracteres 
  # # 4) input_language_optativo: no nulo, vector, caracter, de longitud uno , y dos caracteres 
  # # 5) input_tabla: no nulo, una lista, al menos un objeto en la lista, objeto con dos dimensiones
  # # 6) Pequenio cambio sobre "input_latex"
  {
    ###
    
    control_OK <- list()
    control_paso <- 0
    seguir <- TRUE
    mensaje <- list()
  
    # # 1) input_latex: no nulo, data.frame o matrix, con filas y columnas
    {
      # Numero de paso
      control_paso <- control_paso + 1
      
      # Controles
      if (is.null(input_latex))           seguir <- FALSE
        if (!is.data.frame(input_latex) && !is.matrix(input_latex))      seguir <- FALSE
          if(length(dim(input_latex)) !=  2)    seguir <- FALSE
            if(dim(input_latex)[1] < 1)    seguir <- FALSE
              if(dim(input_latex)[2] < 1)    seguir <- FALSE
      
      
      
      # Decision
      if (seguir == TRUE) control_OK[[control_paso]] <- TRUE else
      {
        
        control_OK[[control_paso]] <- FALSE
        
        mensaje[[control_paso]] <- c("### grafOxidos (Control de argumentos - Paso 1 de 6) ### \n",
                                     "Problemas en el argumento: input_latex \n",
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
    
    
    
    # # 2) input_paso: no nulo, vector, numerico, longitud uno.
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
        
        mensaje[[control_paso]] <- c("### grafOxidos (Control de argumentos - Paso 2 de 6) ### \n",
                                     "Problemas en el argumento: input_paso \n",
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
        
        mensaje[[control_paso]] <- c("### grafOxidos (Control de argumentos - Paso 2 de 6) ### \n",
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
        
        mensaje[[control_paso]] <- c("### grafOxidos (Control de argumentos - Paso 4 de 6) ### \n",
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
        
        mensaje[[control_paso]] <- c("### grafOxidos (Control de argumentos - Paso 5 de 6) ### \n",
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
    
    
    # # 6) Pequenio cambio sobre "input_latex".
    {
      ###
      # Sucede que el elemneto Fluor, que tiene simbolo "F" como toma
      # como si fuera un "FALSE". Entonces, hay que ver como lo tomo 
      # y en tal caso asignar un cambio para que no lo tome como valor
      # logico sino como tipo caracter.
      # Del objeto "input_latex", es la tercer columna la que posee el 
      # simbolo quimico del elemento que participa en el oxido.
      # Tomaremos esa 3er columna y veremos si lo ha comado como "FALSE".
      
      # Hacemos una parada tecnica...
      # Tomamos de la primera fila, la columna 3. Alli esta el simbolo quimico
      # Si hay anotado un "FALSE"... lo cambiamos por "F" tipo caracter.
      
      # Parada tecnica...
      paradita <- input_latex[1,3]
      
      # Si hace falta el cambio...
      if (!is.na(paradita)) if (paradita == "FALSE"){
        
        # Se va a fijar en cada elemento de la matrix para hacer el cambio...
        for (v1 in 1:nrow(input_latex)) for (v2 in 1:ncol(input_latex)){
          
          # Si hace falta, implementa el cambio...
          if (!is.na(input_latex[v1,v2])) if (input_latex[v1,v2] == "FALSE") input_latex[v1,v2] <- "F"
          
        } # Fin doble for...
      } # Fin if paradita == "FALSE"
      
      
      
      
      
      ###  
    } # Fin Control 6)

  ###
  }
  ##############################################################################
  
    
  
  
  
  # #
  
 
  # # Capitulo 1: Estequimetria...
  {
    ###  
    
    # Si paso todos los controles se lleva a cabo el ejercicio...
    if (seguir) {
      
        
      # Parte 1) Explicacion de lo que hace cada paso estequimetrico
      #          y que se va a graficar
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
      } # Fin Parte1)
      #########################################################
      
      
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
        
        
        # Especifica del ejercicio...
        simbolo1 <- as.character(input_latex[1,3])
        dt1 <- simbolos == simbolo1
        num_atom1 <- num_atom[dt1]
        nombre1 <- nombres[dt1]
        tipo1 <- tipo[dt1]
        estado1 <- estado[dt1]
        
        # Toma la valencia a partir de la info que hay en LaTeX
        
        # Le cargamos un valor por defecto...
        valencia1 <- 1
        
        # Ahora... vemos si en realidad es otr...
        metralla1 <- strsplit(as.character(input_latex[4,7]), "O")[[1]]
        if (length(metralla1) == 2) valencia1 <- as.numeric(as.character(strsplit(metralla1, "")[[2]][2]))
          
        
        # Si es el oxigeno...
        if (simbolo1 == "O") valencia1 <- 2
        
        # Si es un gas noble... Le ponemos un 9!
        if (tipo1 == "Gas Noble") valencia1 <- 9
        
        # Nota: esto de ponerle un 9, es para que en los numeros romanos, 
        #       a la posicion ocho le puse un espacio.
        
        
        
        ###  
      } # Fin Parte2
      ##############################################
      
      
      # Parte 3: Pared de fondo
      {
      ###
        plot(c(0:30), axes=F, col="white", xlab=" ", ylab=" ")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = input_color_fondo)
      ###  
      } # Fin Parte3
      #######################################################
      
      
      # Parte 4: Pintada
      {
      ###
        # Lo que sigue a continuacion es todo el detalle sucesivo para cada 
        # paso estequimetrico. Cada paso sucesivo va acumulando detalles.

        
        # Un pequenio cambio, pero con aviso...
        {
        ###  
          # En principio hay una cantidad de pasos definida
          # para realizar la estequimetria de oxidos.
          # En este caso son 8 (se obtienen por nrow(input_latex) por ejemplo).
          # Peeeero... yo quiero agregar dos diapositivas mas...
          # Diapo Entra 9: tiene la ecuacion final y dice "Estequimetria Final" como
          #                 cartel, para avisarle al waso que efectivamente esa es
          #                 la ecuacion quimica.
          # Diapo Extra 10: Nomenclatura.
          # El tema es que... input_latex llega hasta el 8, y no hasta el 10.
          # Entonces, se me clava la funcion cuando trata de buscar elementos de
          # input_latex de una fila que no existe.
          
          # Entonces.. si input_paso es 9... Va a guardar esa info, y a input_paso
          # le va a restar una unidad.
          # Y la diapo con nomenclatura, decidi que va a ser una funcion a parte a lo mejor.
          

          if (input_paso <=8) pintor_extra <- F else
            if (input_paso == 9) {pintor_extra <- T; input_paso <- 8}
          
        ###    
        }
        #####################################################
        
        
        # Participes necesarios
        {
        ###
          
          # Cantidad de pasos totales
          cantidad_pasos <- nrow(input_latex)
          
          # Detalles a ingresar
          coef1 <- as.numeric(as.character(input_latex[input_paso,2]))
          elemento_reactivo <- as.character(input_latex[input_paso,3]) 
          
          coef2 <- as.numeric(as.character(input_latex[input_paso,4]))
          oxigeno_reactivo <- as.character(input_latex[input_paso,5]) 

          
          coef3 <- as.numeric(as.character(input_latex[input_paso,6]))
          oxido_producto <- as.character(input_latex[input_paso,7]) 
          if (oxido_producto == "") oxido_producto <- NA
          
          
        ###  
        } # Fin Participes necesarios
        ##############################################
        
        
        # Detecciones para condicionales
        {
        ###
          
          # Deteccion General de caracteristicas
          dt_gn1 <- dt1                       # Detectar la posicion del elemento elegido
          dt_gn2 <- tipo1 == "Gas Noble"      # Detectar si es un "Gas Noble"
          dt_gn3 <- estado1 == "Gas"          # Detectar si es un "Gas"
          dt_gn4 <- simbolo1 == "O"           # Detectar si es "Oxigeno"
          dt_gn5 <- simbolo1 == "H"           # Detectar si es "Hidrogeno"
          dt_gn6 <- sum(dt_gn2, dt_gn4) == 0  # Detectar si esta todo OK... esto es que No es "O" ni "Gas Noble"
          
          
        ###
        } # Fin Detecciones
        ##################################################
     
           
        # Marcas en la pared
        {
          ###
          # Esto le da la altura de referencia en el eje Y para todo
          # lo que se va a graficar en cada paso.
          
          # Coordenadas "y" de los elementos
          gps_y <- c(7, 7, 7, 7, 7, 7, 7, 7, 7, 20) 
          
          
          # Coordenadas "x" de los elementos
          gps_x <- c( 5, 7, 11, 14, 15.3, 17, 20, 24, 27, 18.5) -3
          
          ###  
        } # Fin Marcas en la pared
        ##############################################
        

        # Prefabricados 
        {
        ###
          
          # Valencias en numeros romanos
          romanos <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", " ")
          r1 <- romanos[valencia1]
          r2 <- romanos[2]
        
          
          # Armado del elemento en reactivos...
          armado_elemento <-   parse(text=paste("text(gps_x[2], gps_y[2], 
                                            expression(",elemento_reactivo,"),
                                            col='", input_color_ecuacion, "', cex=2)", collapse=""))
          
          # Armado del oxigeno en reactivos...
          armado_oxigeno  <-   parse(text=paste("text(gps_x[5], gps_y[5], 
                                            expression(", oxigeno_reactivo,"),
                                            col='", input_color_ecuacion,"', cex=2)", collapse=""))
          
          
          
          # Armado del oxido en productos
          armado_oxido    <-   parse(text=paste("text(gps_x[9], gps_y[9], 
                                            expression(", oxido_producto,"),
                                            col='",input_color_ecuacion,"', cex=2)", collapse=""))
          
          
          
          
        ###
        } # Fin Prefabricado 
        ########################################################
          
          
        # Implementacion del grafico...
        {
        ###  
        
          # Reactivos...
          eval(armado_elemento)
          eval(armado_oxigeno)
          
          # Signo "+"  
          text(gps_x[3], gps_y[3], "+", pos=2, offset=0.1, cex=3, col=input_color_signo)
          
          # Flecha
          arrows(gps_x[6],gps_y[6],gps_x[7], gps_y[7], col=input_color_signo)
          
          # Productos...
          eval(armado_oxido)
          
          # Simbolos
          
          # 1.1) Coeficiente del Elemento
          if(!is.na(coef1)) if (coef1 > 1) text(gps_x[1], gps_y[1], paste(coef1, sep=""), pos=2, offset=0.2, cex=6, col=input_color_ecuacion)
          if(!is.na(coef2)) if (coef2 > 1) text(gps_x[4], gps_y[4], paste(coef2, sep=""), pos=2, offset=0.2, cex=6, col=input_color_ecuacion)
          if(!is.na(coef3)) if (coef3 > 1) text(gps_x[8], gps_y[8], paste(coef3, sep=""), pos=2, offset=0.2, cex=6, col=input_color_ecuacion)
          
          
          # Correciones segun la cantidad de letras y numeros romanos
          cantidad_letras <- length(strsplit(simbolo1, "")[[1]])
          correccion1 <- 0
          correccion2 <- 0
          
          # Si tiene mas de una letra... 
          #if (cantidad_letras > 1) correccion1 <- 0.15
          correccion2 <- 0.17
          
          # Si NO es un Gas Noble, pero es un Gas
          if (input_paso == 1) if (dt_gn2 == FALSE && dt_gn3 == TRUE) correccion1 <- 0
          
          if (input_paso == 1) correccion2 <- 0
          text(gps_x[2] - correccion1, gps_y[2] + 4, r1,  cex=2, col=input_color_signo)
          text(gps_x[5]    - correccion2, gps_y[5] + 4, r2,  cex=2, col=input_color_signo)
          
          
          # Cartal de super final todo OK!
          if (pintor_extra)text(gps_x[10], gps_y[10], "Estequiometría Final",  cex=3, col=input_color_general)
          
          
        ###    
        }
        #############################################
        
        
       
        
       
        
        
        
        
        
      ###  
      } # Fin Parte 3
      ###########################################
      
    } # Fin seguir
    ##############################
    
    
  ###  
  } # Fin Capitulo 1...
  ######################################################  
  
  

  # # Capitulo 3: Nomenclatura
  {
  ###
    if (1 == 2) {
      
      # Nomenclatura
      if (escudo == 10) {
        
        # Buscamos la fila en donde esta el Oxido
        # dentro de la lista de nomenclatura
        
        # Recambio de valencia si es un gas noble    
        if (valencia1 == 9) valencia1 <- 0
        
        aver1 <- Nomenclatura[,4] == simbolo1
        aver2 <- Nomenclatura[,5] == valencia1
        
        suma <- as.numeric(aver1) + as.numeric(aver2)
        
        dt_suma <- suma == 2
        
        # Hay que corregir esto... para uqe no haga falta sumarle 1
        espada <- as.numeric(as.character(Nomenclatura[dt_suma, 1])) 
        
        oxido_producto <- as.character(ejemplo_latex[8,7]) 
        
        nomen_iupac <- Nomenclatura[espada,  9]
        nomen_clasica   <- Nomenclatura[espada, 10]
        nomen_stock   <- Nomenclatura[espada, 11]
        
        nomen_iupac   <- paste0("IUPAC: ", nomen_iupac)
        nomen_clasica <- paste0("Clásica: ", nomen_clasica)
        nomen_stock   <- paste0("Stock: ", nomen_stock)
        
        
        ###############################################################################
        
        # Coordenadas "y" de los elementos
        gps_y <- c(7, 7, 7, 7, 7, 7, 7, 7) 
        
        
        
        # Salida General de Nomenclatura
        if (TRUE){
          
          
          
          armado_oxido <-   parse(text=paste("text(15, 5, 
                                             expression(", oxido_producto,"), 
                                             col=color_ecuacion, cex=3)", collapse=""))
          
          
          eval(armado_oxido)
          
          text(15, 25, nomen_iupac, cex=3, col=color_general)
          text(15, 17, nomen_clasica, cex=3, col=color_general)
          text(15, 10, nomen_stock, cex=3, col=color_general)
          
          
          
        } # Fin Salida General Nomenclatura
        #############################################################
        
        
        
        
        
        
      } # Fin 3) Nomenclatura
      #########################################################
      
    }
  ###  
  } # Fin Capitulo 3...
  #################################################################
 
  
  
  

  
  
  
  
}




GrafGeneral <- function(input_numfam = NULL, 
                        input_latex = NULL,
                        input_paso = NULL,
                        input_language_interno = "es", 
                        input_language_optativo = NULL,
                        input_tabla = NULL,
                        input_color_fondo = "orange", 
                        input_color_general = "black",
                        input_color_especifico = "blue",
                        input_color_ecuacion = "black",
                        input_color_signo = "black",
                        input_color_paso = "blue") {
  
  # Especificaciones de lenguaje interno por defecto y optativo si es null
  input_language_interno <- "es"
  if (is.null(input_language_optativo)) input_language_optativo <- "es"
  
 
  
  
  # Resoluciones
  
  if (input_numfam == 1)     grafOxidos(input_latex = input_latex,
                                        input_paso = input_paso,
                                        input_language_interno = "es", 
                                        input_language_optativo = NULL,
                                        input_tabla = input_tabla,
                                        input_color_fondo = input_color_fondo,
                                        input_color_general = input_color_general,
                                        input_color_especifico = input_color_especifico,
                                        input_color_ecuacion = input_color_ecuacion,
                                        input_color_signo = input_color_signo,
                                        input_color_paso = input_color_paso)
  
  
  
  
  
  
  
} # End Function***


