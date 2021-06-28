# extract long-lat coordinates from a bunch of kmz files
# first unzips the KMZ-s then reads coordinates from each KML with getKMLcoordinates {maptools}
# (worth checking this as well: https://gist.github.com/holstius/6631918 )

# Librerias
library(maptools)
library(leaflet)
library(readxl)

archivo_lotes <- "bases/Lotes.xlsx"
BASE <- read_excel(path= archivo_lotes, sheet = 1)
BASE <- as.data.frame(BASE)


# list the kmz files in a given folder path
KMZs <- list.files(path="GeoData/kmz-files", pattern="*.kmz", full.names=FALSE)

# Data/kmz-files

# unzip each KMZ file and read coordinates with getKMLcoordinates()
# select only the matrix element of the list returned by getKMLcoordinates(), 
# therefore mention the index [[1]]
LonLat <- sapply(KMZs, 
                 function(x) 
                   getKMLcoordinates(kmlfile        = unzip(zipfile = paste0("GeoData/kmz-files/", x),
                                                            exdir   = "Data/kml-files"), 
                                     ignoreAltitude = TRUE)[[1]])

coordenadas_guardadas <- rep(NA, length(LonLat))

for (n in 1:length(LonLat))
    coordenadas_guardadas[n] <- paste0(paste0(LonLat[[n]][,1], "ppp", LonLat[[n]][,2], rep(" hhh ", nrow(LonLat[[n]]))), collapse="")

# dim(coordenadas_guardadas) <- c(length(coordenadas_guardadas), 1)

# salida <- as.data.frame(as.matrix(coordenadas_guardadas))

BASE$Coordenadas <- coordenadas_guardadas


wb_tablas <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb = wb_tablas, sheetName = "Hoja 1")
openxlsx::writeData(wb = wb_tablas,x = coordenadas_guardadas,  sheet = "Hoja 1", startRow = 1, startCol = 1)
openxlsx::saveWorkbook(wb_tablas, "bases/Lotes2.xlsx", overwrite = T)
