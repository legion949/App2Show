

# 1) Detalles de Lenguaje
details <- read.csv("details.csv", sep=";", header = T, stringsAsFactors = F)
idShiny <- details$idShiny
################################################################################


# 2) Tablas periodicas
TabPeriod <- list()

# # English...
TabPeriod[[1]] <- read.csv("data/TabPeriod_en.csv", sep=";", dec=".", header = T)
names(TabPeriod)[1] <- "en"
      

# # Español...
TabPeriod[[2]] <- read.csv("data/TabPeriod_es.csv", sep=";", dec=".", header = T)
names(TabPeriod)[2] <- "es"


# # Frances...
TabPeriod[[3]] <- read.csv("data/TabPeriod_fr.csv", sep=";", dec=".", header = T)
names(TabPeriod)[3] <- "fr"
################################################################################


# 3) Funciones
mis_archivos <- list.files("functions")
for (k in 1:length(mis_archivos)) source(paste0("functions", "/", mis_archivos[k] ))
remove(k, mis_archivos)


# 4) Subtitulos
mis_archivos <- list.files("subtitulos")
mis_subarchivos <- list()
subtitulos <- list()

for (k in 1:length(mis_archivos)) mis_subarchivos[[k]] <- list.files(paste0("subtitulos", "/", mis_archivos[k] ))

#for (k in 1:length(mis_archivos)) subtitulos[[k]] <- list()

for (k in 1:length(mis_archivos)) for (j in 1:length(mis_subarchivos[[k]])) {

  if (j == 1) subtitulos[[k]] <- list()
  
  este_archivo <- paste0("subtitulos", "/", mis_archivos[k], "/",  mis_subarchivos[[k]][j])
  subtitulos[[k]][[j]] <- read.csv(file = este_archivo, sep=";", header = T)
    
}
remove(k, j, mis_archivos, mis_subarchivos, este_archivo)
