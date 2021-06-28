
# este_simbolo = 2, este_sub = 2

# Funcion "empaquetadora1"
empaquetadora1 <- function(este_simbolo, este_sub){
  
  # Si ambos son informacion correcta...
  todo_OK <- TRUE
  if (is.na(este_simbolo) | este_simbolo == "") todo_OK <- FALSE
    if (is.na(este_sub) | este_sub == "") todo_OK <- FALSE
  
  # Si esta todo OK seguimos...
  if (todo_OK) {
    
    # Armamos las partes generales...
    partes <- paste(c(este_simbolo, "[", este_sub, "]"), collapse="")
    
    # Pero si tiene valencia 1 lo serruchamos un poco...
    if (este_sub == 1) partes <- paste(este_simbolo, collapse="")
    
    
    # Juntamos las partes...
    paquete_armado <- paste(partes, sep="")
    
    return(paquete_armado)
  } # Fin armado_hidroxido. 
  else return("Algo mal en la empaquetadora de oxidos... \n")
  
} # Fin function empaquetadora***






# Funcion "empaquetadora_oxido"
empaquetadora_oxido <- function(este_simbolo1, este_sub1, este_tipo1, 
                                este_simbolo2, este_sub2){
  
  # Si ambos son informacion correcta...
  todo_OK <- TRUE
  if (is.na(este_simbolo1) | este_simbolo1 == "") todo_OK <- FALSE
    if (is.na(este_sub1) | este_sub1 == "") todo_OK <- FALSE
      if (is.na(este_tipo1) | este_tipo1 == "") todo_OK <- FALSE
        if (is.na(este_simbolo2) | este_simbolo2 == "") todo_OK <- FALSE
          if (is.na(este_sub2) | este_sub2 == "") todo_OK <- FALSE
  
  # Peeero, hay una excepcion: con el oxigeno en una formula de oxido...
  if (todo_OK == FALSE) {
    
    
    if (este_simbolo1 =="O") # Si el primer elmento es el oxigeno...
      if (!is.na(este_sub1) | este_sub1 != "")  # Y tengo el subindice1
        if (!is.na(este_tipo1) | este_tipo1 != "") # Y tengo el tipo de elemento que es element1...
          if (is.na(este_simbolo2) | este_simbolo2 == "") # Y no tengo el 2do simbolo...
            todo_OK <- TRUE # Esta todo OK!
  }        
  # Si esta todo OK seguimos...
  if (todo_OK) {
    
    # 1) Si el elemento no es un gas noble... y no es el oxigeno
    if (este_tipo1 != "Gas Noble" && este_simbolo1 != "O") {
      
        # Parte 1 del oxido...
        p1_oxido <- paste(c(este_simbolo1, "[", este_sub1, "]"), collapse="")
        if (este_sub1 == 1) p1_oxido <- paste(este_simbolo1, collapse="")
        
        # Parte 2 del oxido...
        p2_oxido <- paste(c(este_simbolo2, "[", este_sub2, "]"), collapse="")
        if (este_sub2 == 1) p2_oxido <- paste(este_simbolo2, collapse="")
    
        # Oxido Armado LaTeX
        armado_oxido <- paste(p1_oxido, p2_oxido, sep="*")
        
    } # Fin if 1)
    ########################################################################
    
    
    # 2) Si es el oxigeno...
    if (este_simbolo1 == "O") {
      
      # Parte 1 del oxido...
      p1_oxido <- paste(c(este_simbolo1, "[", este_sub1, "]"), collapse="")
      if (este_sub1 == 1) p1_oxido <- paste(este_simbolo1, collapse="")
      
      # Parte 2 del oxido...
      p2_oxido <- ""
      
      # Oxido Armado LaTeX
      armado_oxido <- paste(p1_oxido, sep="")
      
    } # Fin if 2)
    ########################################################################
    
    
    # 3) Si es un gas noble...
    if (este_tipo1 == "Gas Noble") {
      
      # Parte 1 del oxido...
      p1_oxido <- paste(c(este_simbolo1, "[", este_sub1, "]"), collapse="")
      if (este_sub1 == 1) p1_oxido <- paste(este_simbolo1, collapse="")
      
      # Parte 2 del oxido..
      p2_oxido <- paste(c(este_simbolo2, "[", este_sub2, "]"), collapse="")
      if (este_sub2 == 1) p2_oxido <- paste(este_simbolo2, collapse="")
      
      # Oxido Armado LaTeX
      armado_oxido <- paste(p1_oxido, " + ", p2_oxido, sep="")
      
    } # Fin if 3)
    ########################################################################
    
    return(armado_oxido)
  } # Fin armado_hidroxido. 
  else return("Algo mal en la empaquetadora_oxido()... \n")
  
} # Fin function empaquetadora_oxido***
