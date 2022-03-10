# load libraries
library(rgeos)
library(sf)
library(leaflet)
library(readxl)
library(tidyverse)

# Mapas
deptos <- sf::read_sf("data/depto.shp")
mpio <- sf::read_sf("data/mpio.shp")

df_depto = df_depto %>% select(-Departamento)
deptos4 = left_join(deptos,df_depto, by = c("DPTO"="COD_DPT"))
deptos4 = left_join(deptos4,df_full_agg, by = c("DPTO"="COD_DPT"))

deptos2 <- deptos4 %>% st_transform(3116)
deptos3  <- sf::st_simplify(deptos2, preserveTopology = TRUE, dTolerance = 1000)
deptos4 <- deptos3 %>% st_transform(4326)

qpal <- colorQuantile("YlOrRd", deptos4$prom_subsidio)(deptos4$prom_subsidio)

#create label texts
LabelText <- paste0(
  "<b>Departamento:</b> ", deptos4$NOMBRE_DPT,"<br>",
  "<b>Valor Subsidio:</b> ", format(deptos4$prom_subsidio, nsmall=0, big.mark=","),"<br>",
  "<b>Cantidad Créditos:</b> ", format(deptos4$n_creditos, nsmall=0, big.mark=","),"<br>",
  "<b>Cantidad Operaciones:</b> ", format(deptos4$prom_operaciones, nsmall=0, big.mark=","),"<br>",
  "<b>Valor Créditos:</b> ", format(deptos4$prom_millones, nsmall=0, big.mark=","),"<br>",
  "<b>Participación subsidiada:</b> ", format(round((deptos4$prom_subsidio/deptos4$prom_millones)*100,2), nsmall=0, big.mark=","))

paletteBins <- c(0, 50000, 100000, 250000, 500000, 1000000, 2500000, 5000000, 10000000)
colorPalette <- colorBin(palette = "YlOrBr", domain = deptos4$prom_subsidio, na.color = "transparent", bins = paletteBins)

mapa <- leaflet(deptos4)
mapa <- addTiles(mapa)
mapa <- addPolygons(mapa, color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,
                      fillColor = qpal,
                      highlightOptions = highlightOptions(color = "red", weight = 2,
                                                          bringToFront = TRUE),
                    label = lapply(LabelText, htmltools::HTML))

mapa <- mapa %>% leaflet::addLegend(pal = colorPalette,
                  values = ~deptos4$prom_subsidio,
                  opacity=0.9,
                  title = "Promedio de Subsidio",
                  position = "bottomleft")





