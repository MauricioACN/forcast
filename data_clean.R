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

mpio <- sf::read_sf("data/mpio.shp")
mpio = mpio[!mpio$DPTO %in% c("91","88"),]
mpio = modify_coords(mpio)

################################## agregacion de variables ##################################
df = data_clean(df)

################################## Construccion de df final ##################################
data = df

## data agregada a nivel de depto
# Agrgar dato por sector y por subsidio
data_1 = data %>% group_by(FECCORTE_F,COD_DPT,SECTOR_F) %>%
  summarise(part_sect = sum(Subsidio,na.rm=T)) %>%
  arrange(desc(part_sect)) %>% slice(1) %>%
  rename(SECTOR_influyente =SECTOR_F,
         FECCORTE = FECCORTE_F)

# Agrgar dato por productor y por subsidio
data_2 = data %>% group_by(FECCORTE_F,COD_DPT,TIPO_PRODUCTOR_F) %>%
  summarise(part_prod = sum(Subsidio,na.rm=T)) %>%
  arrange(desc(part_prod)) %>% slice(1) %>%
  rename(productor_influyente =TIPO_PRODUCTOR_F,
         FECCORTE = FECCORTE_F)

# Agrgar dato por sector y por valor del credito
data_3 = data %>% group_by(FECCORTE_F,COD_DPT,SECTOR_F) %>%
  summarise(part_sect_cred = sum(Millones,na.rm=T)) %>%
  arrange(desc(part_sect_cred)) %>% slice(1) %>%
  rename(SECTOR_influyente_cred =SECTOR_F,
         FECCORTE = FECCORTE_F)

# Agrgar dato por productor y por valor del credito
data_4 = data %>% group_by(FECCORTE_F,COD_DPT,TIPO_PRODUCTOR_F) %>%
  summarise(part_prod_cred = sum(Millones,na.rm=T)) %>%
  arrange(desc(part_prod_cred)) %>% slice(1) %>%
  rename(productor_influyente_cred =TIPO_PRODUCTOR_F,
         FECCORTE = FECCORTE_F)

# Agrgar dato por depto y fecha
data_full = data %>% group_by(FECCORTE_F,COD_DPT) %>%
  summarise(total_subsidio = sum(Subsidio,na.rm=T),
            no_creditos = n(),
            total_creditos = sum(Millones,na.rm=T),
            total_ope = sum(No_OPERACIONES,na.rm=T),
            prom_part_sub = mean(prom_porc_sub,na.rm=T)) %>%
  rename(FECCORTE = FECCORTE_F)

# join todos los subsets
data_full = left_join(data_full,data_1, by=c("COD_DPT"="COD_DPT","FECCORTE"="FECCORTE")) %>%
  left_join(.,data_2,by=c("COD_DPT"="COD_DPT","FECCORTE"="FECCORTE")) %>%
  left_join(.,data_3,by=c("COD_DPT"="COD_DPT","FECCORTE"="FECCORTE")) %>%
  left_join(.,data_4,by=c("COD_DPT"="COD_DPT","FECCORTE"="FECCORTE"))

# crear nuevas variables y modificar antiguas
data_full = data_full %>% mutate(porce_prod_sub = round(part_prod/total_subsidio,2),
                                 porce_sect_sub = round(part_sect/total_subsidio,2),
                                 porce_prod_cred = round(part_prod_cred/total_creditos,2),
                                 porce_sect_cred = round(part_sect_cred/total_creditos,2),
                                 part_sub_cre = round(total_subsidio/total_creditos,2),
                                 total_subsidio = round(total_subsidio/1e3,0),
                                 total_creditos = round(total_creditos/1e3,0),
                                 part_prod = round(part_prod/1e3,0),
                                 part_sect = round(part_sect/1e3,0),
                                 part_prod_cred = round(part_prod_cred/1e3,0),
                                 part_sect_cred = round(part_sect_cred/1e3,0)
)

# imputar nas con 0
data_full[is.na(data_full)]= 0

# Agregacion inical a todos los niveles a nivel de depto
df_depto_agg = df %>% group_by(FECCORTE_F,COD_DPT,SECTOR_F,TIPO_PRODUCTOR_F) %>%
  summarise(n_creditos = n(),
            prom_operaciones = sum(No_OPERACIONES,na.rm=T),
            prom_millones = sum(Millones,na.rm=T),
            prom_subsidio = sum(Subsidio,na.rm=T)) %>%
  rename(FECCORTE=FECCORTE_F,
         SECTOR = SECTOR_F,
         TIPO_PRODUCTOR = TIPO_PRODUCTOR_F)

# Agregacion inical a todos los niveles a nivel de municipio
df_mpio_agg = df %>% group_by(FECCORTE_F,COD_MPIO_INVERSION,SECTOR_F,TIPO_PRODUCTOR_F) %>%
  summarise(n_creditos = n(),
            prom_operaciones = sum(No_OPERACIONES,na.rm=T),
            prom_millones = sum(Millones,na.rm=T),
            prom_subsidio = sum(Subsidio,na.rm=T)) %>%
  rename(FECCORTE=FECCORTE_F,
         SECTOR = SECTOR_F,
         TIPO_PRODUCTOR = TIPO_PRODUCTOR_F)

df_full_agg = df %>% group_by(FECCORTE_F,COD_DPT) %>%
  summarise(n_creditos = n(),
            prom_operaciones = sum(No_OPERACIONES,na.rm=T),
            prom_millones = sum(Millones,na.rm=T),
            prom_subsidio = sum(Subsidio,na.rm=T)) %>%
  rename(FECCORTE=FECCORTE_F)

################################## Construccion de df como SpatialDataFrame ##################################
mynewspdf <- merge(deptos,df_depto_agg,by.x="DPTO",by.y="COD_DPT", duplicateGeoms = T)
deptos_agg_mapa <- merge(deptos,data_full,by.x="DPTO",by.y="COD_DPT", duplicateGeoms = T)

# clean final df
deptos_agg_mapa@data <- deptos_agg_mapa@data[,-c(3:5)]
mynewspdf@data <- mynewspdf@data[,-c(3:5)]

################################## Escritura de df ##################################
write_rds(mynewspdf,"forcast_col/data_clean/deptos.rds")
write_rds(deptos_agg_mapa,"forcast_col/data_clean/deptos_mapa.rds")
# write_rds(mpio,"forcast_col/data_clean/mpio.rds")
# write_rds(df_depto_agg,"forcast_col/data_clean/df_depto_agg.rds")
# write_rds(df_mpio_agg,"forcast_col/data_clean/df_mpio_agg.rds")



