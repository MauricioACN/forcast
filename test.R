# load libraries
library(rgeos)
library(sf)
library(leaflet)

library(readxl)
library(dplyr)

# deptos
deptos <- sf::read_sf("data/depto.shp")
mpio <- sf::read_sf("data/mpio.shp")

deptos2 <- deptos %>% st_transform(3116)
deptos3  <- sf::st_simplify(deptos2, preserveTopology = TRUE, dTolerance = 1000)
deptos4 <- deptos3 %>% st_transform(4326)

# data
df = readxl::read_excel("data/BaseColocacionesVictimas.xlsx")
df$COD_DPT = formatC(df$COD_DPT,flag=0,width=2)
df$COD_MPIO_INVERSION = sprintf("%05d",df$COD_MPIO_INVERSION)

df_full_agg = df %>% group_by(FECCORTE,COD_DPT,COD_MPIO_INVERSION,SECTOR,TIPO_PRODUCTOR,SUBTIPO_PRODUCTOR,Género,DES_PROGRAMA_CREDITO) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T))

qpal <- colorQuantile("YlOrRd", deptos4$AREA)(deptos4$AREA)

mapa <- leaflet(deptos4)
mapa <- addTiles(mapa)
mapa <- addPolygons(mapa, color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,
                      fillColor = qpal,
                      highlightOptions = highlightOptions(color = "red", weight = 2,
                                                          bringToFront = TRUE))







