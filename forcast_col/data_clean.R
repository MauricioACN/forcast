
df = readxl::read_excel("data/BaseColocacionesVictimas.xlsx")
df$COD_DPT = formatC(df$COD_DPT,flag=0,width=2)
df$COD_MPIO_INVERSION = sprintf("%05d",df$COD_MPIO_INVERSION)
df$FECCORTE = as.Date(as.character(df$FECCORTE),"%Y%m%d")

df_full_agg = df %>% group_by(FECCORTE,COD_DPT,COD_MPIO_INVERSION,SECTOR,TIPO_PRODUCTOR,SUBTIPO_PRODUCTOR,Género,DES_PROGRAMA_CREDITO) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T))

df_depto = df %>% group_by(COD_DPT,Departamento) %>%
  summarise(n_creditos = n(),
            prom_operaciones = mean(No_OPERACIONES,na.rm=T),
            prom_millones = mean(Millones,na.rm=T),
            prom_subsidio = mean(Subsidio,na.rm=T))

deptos <- sf::read_sf("data/depto.shp")
deptos = deptos[!deptos$DPTO %in% c("91","88"),]
mpio <- sf::read_sf("data/mpio.shp")

df_full_agg = df_full_agg %>% st_as_sf()

