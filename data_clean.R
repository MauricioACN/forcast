library(rgeos)
library(sf)
library(leaflet)
library(readxl)
library(tidyverse)

source("functions.R",encoding = "utf-8")
################################## Lectura bases ##################################
# datos de creditos
df = readxl::read_excel("data/BaseColocacionesVictimas.xlsx")
df$COD_DPT = formatC(df$COD_DPT,flag=0,width=2)
df$COD_MPIO_INVERSION = sprintf("%05d",df$COD_MPIO_INVERSION)
df$FECCORTE = as.Date(as.character(df$FECCORTE),"%Y%m%d")

# mapas
deptos <- sf::read_sf("data/depto.shp")
deptos = deptos[!deptos$DPTO %in% c("91","88"),]
deptos = modify_coords(deptos)
# for (i in 1:length(deptos@polygons)){
#   deptos@polygons[[i]]@ID = deptos$DPTO[i]
#   names(deptos@polygons)[i] = deptos$DPTO[i]
# }

mpio <- sf::read_sf("data/mpio.shp")
mpio = mpio[!mpio$DPTO %in% c("91","88"),]
mpio = modify_coords(mpio)

################################## agregacion de variables ##################################

df = data_clean(df)

################################## Construccion de df final ##################################

df_depto_agg = df %>% group_by(FECCORTE,COD_DPT,SECTOR_F,TIPO_PRODUCTOR_F) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T)) %>%
  rename(#FECCORTE=FECCORTE_F,
         SECTOR = SECTOR_F,
         TIPO_PRODUCTOR = TIPO_PRODUCTOR_F)

df_mpio_agg = df %>% group_by(FECCORTE_F,COD_MPIO_INVERSION,SECTOR_F,TIPO_PRODUCTOR_F) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T)) %>%
  rename(FECCORTE=FECCORTE_F,
         SECTOR = SECTOR_F,
         TIPO_PRODUCTOR = TIPO_PRODUCTOR_F)

df_full_agg = df %>% group_by(FECCORTE_F,COD_DPT) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T)) %>%
  rename(FECCORTE=FECCORTE_F)

mynewspdf <- merge(deptos,df_depto_agg,by.x="DPTO",by.y="COD_DPT", duplicateGeoms = T)

write_rds(mynewspdf,"forcast_col/data_clean/deptos.rds")
write_rds(mpio,"forcast_col/data_clean/mpio.rds")
write_rds(df_depto_agg,"forcast_col/data_clean/df_depto_agg.rds")
write_rds(df_mpio_agg,"forcast_col/data_clean/df_mpio_agg.rds")



