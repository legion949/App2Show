# extract long-lat coordinates from a bunch of kmz files
# first unzips the KMZ-s then reads coordinates from each KML with getKMLcoordinates {maptools}
# (worth checking this as well: https://gist.github.com/holstius/6631918 )

# Librerias
library(maptools)
library(leaflet)
library(readxl)
library(sp)
library(stringr)

# Base de referencia
archivo_lotes <- "bases/Lotes2.xlsx"
BASE <- read_excel(path= archivo_lotes, sheet = 1)
BASE_REF <- read_excel(path= archivo_lotes, sheet = 2)
BASE_COLORS <- read_excel(path= archivo_lotes, sheet = 3)
# BASE <- as.data.frame(BASE)

# Hitos
detalle_hitos <- colnames(BASE)[grep("Hito", colnames(BASE))]


# Coordenada codificadas
coordenadas_codificadas <- as.character(as.vector(BASE$Coordenadas))
nombre_lote <- BASE$Lotes

# Decodificacion de Coordenadas
coordenadas_finales <- list()

for (n in 1:length(coordenadas_codificadas)) {
#  (cat(n, "\n"))
  paso01 <- str_replace_all(coordenadas_codificadas[n], fixed(" "), "")
  paso02 <- strsplit(paso01, "hhh")[[1]]
  paso03 <- strsplit(paso02, "ppp")
  paso04 <- unlist(paso03)
  paso05 <- matrix(NA, length(paso04)/2, 2)
  paso05[,1] <- as.numeric(paso04[c(T,F)])
  paso05[,2] <- as.numeric(paso04[c(F,T)])
  coordenadas_finales[[n]] <- paso05
}




# Nombre del lote para las coordenadas
names(coordenadas_finales) <- BASE$Lotes  


# 
# # # list the kmz files in a given folder path
# # KMZs <- list.files(path="GeoData/kmz-files", pattern="*.kmz", full.names=FALSE)
# # 
# # # Data/kmz-files
# # 
# # # unzip each KMZ file and read coordinates with getKMLcoordinates()
# # # select only the matrix element of the list returned by getKMLcoordinates(), 
# # # therefore mention the index [[1]]
# # LonLat <- sapply(KMZs, 
# #                  function(x) 
# #                    getKMLcoordinates(kmlfile        = unzip(zipfile = paste0("GeoData/kmz-files/", x),
# #                                                             exdir   = "Data/kml-files"), 
# #                                      ignoreAltitude = TRUE)[[1]])
# # 
# 
# 
# este_id <- 2
# dt_lotes <- BASE$id == este_id
# estos_lotes <- c(1:length(dt_lotes))[dt_lotes]
# estos_nombres <- BASE$Lotes[estos_lotes]
# orden_interno <- 1:length(estos_lotes)
# colores <- c("orange", "green", "blue")
# 
# 
# Sr <- list()
# Srs <- list()
# 
# 
# for (h in 1:length(orden)) Sr[[h]] <- Polygon(coordenadas_finales[[h]])
# 
# # Sr1 = Polygon(coordenadas_finales[[1]])
# # Sr2 = Polygon(coordenadas_finales[[2]])
# # Sr3 = Polygon(coordenadas_finales[[3]])
# 
# for (h in 1:length(orden)) Srs[[h]] = Polygons(list(Sr[[h]]), estos_nombres[h])
# 
# 
# # m <- leaflet() %>%
# #   addTiles() %>%
# 
# SpP = SpatialPolygons(Srs, orden)
# # leaflet(height = "300px") %>% 
# leaflet() %>% 
#   addTiles() %>%
#   addPolygons(data = SpP, , popup ="Un punto de todo el campo",   col =colores)
# 
