library(rgeos)
library(sf)
library(leaflet)
library(readxl)
library(tidyverse)

source("functions.R")
################################## Lectura bases ##################################
# datos de creditos
df = readxl::read_excel("data/BaseColocacionesVictimas.xlsx")
df$COD_DPT = formatC(df$COD_DPT,flag=0,width=2)
df$COD_MPIO_INVERSION = sprintf("%05d",df$COD_MPIO_INVERSION)
df$FECCORTE = as.Date(as.character(df$FECCORTE),"%Y%m%d")

# mapas
deptos <- sf::read_sf("data/depto.shp")
deptos = deptos[!deptos$DPTO %in% c("91","88"),]
deptos_simple = deptos %>% select(DPTO,NOMBRE_DPT,geometry)

mpio <- sf::read_sf("data/mpio.shp")
mpio = mpio[!mpio$DPTO %in% c("91","88"),]
mpio_simple = mpio %>% select(DPTO,MPIO,MPIOS,NOMBRE_MPI)

################################## agregacion de variables ##################################

prueba_1 = df %>% group_by(SECTOR) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T),
            proporcion = round(prom_subsidio/prom_millones*100,2)) %>%
  mutate(prop_n = round(n_creditos/sum(n_creditos)*100,2),
         prop_credit = round(prom_millones/sum(prom_millones)*100,2),
         prop_subs = round(prom_subsidio/sum(prom_subsidio)*100,2)) %>% arrange(desc(prop_subs))

prueba_1 = prueba_1$SECTOR[1:10]

df$val_sector = df$SECTOR %in% prueba_1
df$SECTOR_F = ifelse(df$val_sector,df$SECTOR,"Otros Sectores")

prueba_2 = df %>% group_by(TIPO_PRODUCTOR) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T),
            proporcion = round(prom_subsidio/prom_millones*100,2)) %>%
  mutate(prop_n = round(n_creditos/sum(n_creditos)*100,2),
         prop_credit = round(prom_millones/sum(prom_millones)*100,2),
         prop_subs = round(prom_subsidio/sum(prom_subsidio)*100,2)) %>% arrange(desc(prop_subs))

df$TIPO_PRODUCTOR_F = ifelse(df$TIPO_PRODUCTOR=="GRANDE","GRANDE","PEQUEÑO Y MEDIANO")

prueba_3 = df %>% group_by(Género) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T),
            proporcion = round(prom_subsidio/prom_millones*100,2)) %>%
  mutate(prop_n = round(n_creditos/sum(n_creditos)*100,2),
         prop_credit = round(prom_millones/sum(prom_millones)*100,2),
         prop_subs = round(prom_subsidio/sum(prom_subsidio)*100,2)) %>% arrange(desc(prop_subs))

df = df %>% filter(Género!="PERSONA JURÍDICA")

prueba_4 = df %>% group_by(DES_PROGRAMA_CREDITO) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T),
            proporcion = round(prom_subsidio/prom_millones*100,2)) %>%
  mutate(prop_n = round(n_creditos/sum(n_creditos)*100,2),
         prop_credit = round(prom_millones/sum(prom_millones)*100,2),
         prop_subs = round(prom_subsidio/sum(prom_subsidio)*100,2)) %>% arrange(desc(prop_subs))

prueba_5 = df %>% group_by(SUBTIPO_PRODUCTOR) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T),
            proporcion = round(prom_subsidio/prom_millones*100,2)) %>%
  mutate(prop_n = round(n_creditos/sum(n_creditos)*100,2),
         prop_credit = round(prom_millones/sum(prom_millones)*100,2),
         prop_subs = round(prom_subsidio/sum(prom_subsidio)*100,2)) %>% arrange(desc(prop_subs))

prueba_6 = df %>% group_by(Departamento) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T),
            proporcion = round(prom_subsidio/prom_millones*100,2)) %>%
  mutate(prop_n = round(n_creditos/sum(n_creditos)*100,2),
         prop_credit = round(prom_millones/sum(prom_millones)*100,2),
         prop_subs = round(prom_subsidio/sum(prom_subsidio)*100,2)) %>% arrange(desc(prop_subs))


### agrupacion por trimestre
df = df %>% mutate(FECCORTE_F = lubridate::quarter(FECCORTE, with_year = TRUE),
                   y = str_sub(FECCORTE_F,start = 1,end = 4),
                   m = as.numeric(str_sub(FECCORTE_F,start = 6,end = 6)),
                   m = m*3,
                   FECCORTE_F = as.Date(paste(y,m,"01",sep="-")))


################################## Construccion de df final ##################################

df_depto_agg = df %>% group_by(FECCORTE_F,COD_DPT,SECTOR_F,TIPO_PRODUCTOR_F) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T)) %>%
  rename(FECCORTE=FECCORTE_F,
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

df_map_depto = left_join(df_depto_agg,deptos_simple,by = c("COD_DPT"="DPTO"))
df_map_depto = df_map_depto %>% st_as_sf()
df_map_depto = reduction_size(df_map_depto)

df_map_mpio = left_join(df_mpio_agg,mpio_simple,by = c("COD_MPIO_INVERSION"="MPIO"))
df_map_mpio = df_map_mpio %>% st_as_sf()
df_map_mpio = reduction_size(df_map_mpio)

write_rds(df_map_depto,"forcast_col/data_clean/df_map_depto.rds")
write_rds(df_map_mpio,"forcast_col/data_clean/df_map_mpio.rds")


