library(tictoc)

reduction_size = function(df){
  tictoc::tic()
  deptos2 <- df %>% st_transform(3116)
  deptos3  <- sf::st_simplify(deptos2, preserveTopology = TRUE, dTolerance = 1000)
  deptos4 <- deptos3 %>% st_transform(4326)
  tictoc::toc()
  peso_in = object.size(df)
  peso_fin = object.size(deptos4)
  messa = paste("peso inicial de ",peso_in," , peso final de ",peso_fin, "reduccion del ",round(peso_fin/peso_in*100,2))
  message(messa)
  return(deptos4)
}

modify_coords = function(df){
  deptos2 <- df %>% st_transform(3116)
  deptos3  <- sf::st_simplify(deptos2, preserveTopology = TRUE, dTolerance = 1000)
  deptos4 <- deptos3 %>% st_transform(4326)
  deptos = as_Spatial(deptos4)
  deptos
}

data_clean = function(df){

  old_names = colnames(df)
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

  ## agrupacion por trimestre
  df = df %>% mutate(FECCORTE_F = lubridate::quarter(FECCORTE, with_year = TRUE),
                     y = str_sub(FECCORTE_F,start = 1,end = 4),
                     m = as.numeric(str_sub(FECCORTE_F,start = 6,end = 6)),
                     m = m*3,
                     FECCORTE_F = as.Date(paste(y,m,"01",sep="-")))

  df = df %>% mutate(prom_porc_sub = round(Subsidio/Millones,2))

  df
}
