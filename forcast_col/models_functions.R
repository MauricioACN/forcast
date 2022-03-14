
lista_modelos = data.frame("modelos"=c("Pronóstico Naive",
                                       "Modelo Exponencial Suavizado",
                                       "Pronóstico de Holt-Winters",
                                       "Pronóstico TBATS",
                                       "Auto Arima",
                                       "Sarima"),
                           "funciones" = c("naive",
                                           "ets",
                                           "dshw",
                                           "tbats",
                                           "arima",
                                           "sarima"),stringsAsFactors = F)

naive_fun <- function(df,time){
  tryCatch({
    naive = snaive(df, h=time)
    snaive_forecast = forecast(naive, h=time)
    c(snaive_forecast$mean)
  },error=function(e)e
  )
}

ets_fun <- function(df,time){
  tryCatch({
    ets_model = ets(df, allow.multiplicative.trend = TRUE)
    ets_forecast = forecast(ets_model, h=time)
    ets_forecast = as.data.frame(ets_forecast)
    ets_forecast$`Point Forecast`
  },error=function(e)e
  )
}


dshw_fun <- function(df,time){
  tryCatch({
    dshw_model = dshw(df, period1=4, period2 = 12, h=time)
    dshw_model_f = forecast(dshw_model, h=time)
    c(dshw_model_f$mean)
  },error=function(e)e
  )
}

tbats_fun <- function(df,time){
  tryCatch({
    tbats_model = tbats(df)
    tbats_forecast = forecast(tbats_model, h=time)
    tbats_forecast = tbats_forecast %>% as.data.frame()
    tbats_forecast$`Point Forecast`
  },error=function(e)e
  )
}

arima_fun <- function(df,time){
  tryCatch({
    arima_optimal = auto.arima(df)
    arima_forcast = forecast(arima_optimal,h = time)
    arima_forcast = arima_forcast %>% as.data.frame()

    list(result = arima_forcast$`Point Forecast`,
         model = arima_optimal)

  },error=function(e)e
  )
}

sarima_fun <- function(df,time){
  tryCatch({
    salida_arima = arima_fun(df,time)

    p = arimaorder(salida_arima$model)[1]
    d = arimaorder(salida_arima$model)[2]
    q = arimaorder(salida_arima$model)[3]

    sarima_forecast = sarima.for(df,
                                 n.ahead=time,
                                 p=p,d=d,q=q,P=p,D=d,Q=q,S=12)
    sarima_forecast = as.data.frame(sarima_forecast)
    sarima_forecast$pred

  },error=function(e)e
  )
}


model_proccesing <- function(df,model,time_imput=1,prom_movil){

  datasets = list(depto_ts_test = df %>% filter(FECCORTE>="2021-07-01") %>% as.data.frame(),
                  depto_ts_train = df %>% filter(FECCORTE<="2021-07-01")%>% as.data.frame(),
                  depto_ts = df %>% as.data.frame())

  if(prom_movil=="Promedio Movil"){
    datasets$depto_ts$Prom_movil = rollmean(x = datasets$depto_ts$Prop_subs,
                                            k = time_imput,
                                            fill = NA,
                                            align = "right")
  }else{
    hist_percent_subs = mean(datasets$depto_ts$Prop_subs)
  }

  time_serie = ts(datasets$depto_ts_train$Total_millones,
                  frequency = 12,
                  start=lubridate::year(min(datasets$depto_ts_train$FECCORTE))
  )

  time_serie_completed = ts(datasets$depto_ts$Total_millones,
                  frequency = 12,
                  start=lubridate::year(min(datasets$depto_ts$FECCORTE))
  )

  time = nrow(datasets$depto_ts_test)

  salida_modelos = data.frame(id = seq(1,time))
  salida_pronostico = data.frame(id = seq(1,time_imput))

  for (variable in model) {

    if (variable=="Pronóstico Naive") {
      salida_modelos$naive <- naive_fun(df = time_serie,time = time)
      salida_pronostico$naive <- naive_fun(df = time_serie_completed,time = time_imput)
    }
    else if (variable=="Modelo Exponencial Suavizado"){
      salida_modelos$ets <- ets_fun(df = time_serie,time = time)
      salida_pronostico$ets <- ets_fun(df = time_serie_completed,time = time_imput)
    }
    else if (variable=="Pronóstico de Holt-Winters"){
      salida_modelos$dshw <- dshw_fun(df = time_serie,time = time)
      salida_pronostico$dshw <- dshw_fun(df = time_serie_completed,time = time_imput)
    }
    else if (variable=="Pronóstico TBATS"){
      salida_modelos$tbats <- tbats_fun(df = time_serie,time = time)
      salida_pronostico$tbats <- tbats_fun(df = time_serie_completed,time = time_imput)
    }
    else if (variable=="Auto Arima"){
      tmp = arima_fun(df = time_serie,time = time)
      salida_modelos$arima <- tmp$result

      tmp_pr = arima_fun(df = time_serie_completed,time = time_imput)
      salida_pronostico$arima <- tmp_pr$result
    }
    else if (variable=="Sarima"){
      salida_modelos$sarima <- sarima_fun(df = time_serie,time = time)
      salida_pronostico$sarima <- sarima_fun(df = time_serie_completed,time = time_imput)
    }

  }

  salida_modelos$Total_millones = datasets$depto_ts_test$Total_millones

  mape_modelos=data.frame(Metrica="MAPE")

  cols_final = lista_modelos$funciones[lista_modelos$modelos %in% model]

  if(length(model)>1){
    salida_modelos$Prom_Models = rowMeans(salida_modelos[,-1],na.rm = T)
    cols_final = c(cols_final,"Prom_Models")

    salida_pronostico$Prom_Models = rowMeans(salida_pronostico[,-1],na.rm = T)
  }

  for (columna in cols_final) {
    mape_modelos[,columna] = MLmetrics::MAPE(y_pred = salida_modelos[,columna],
                                             y_true = salida_modelos$Total_millones)
  }

  #### procesamiento sobre predicciones finales
  fecha_pron = max(datasets$depto_ts$FECCORTE)
  fecha_pron = fecha_pron+31
  fecha_pron = as.Date(format(fecha_pron,"%Y-%m-01"))
  salida_pronostico$FECCORTE = seq.Date(from = fecha_pron,
                                        to = fecha_pron+months(time_imput-1),
                                        by = "months")

  if(length(model)>1){

    salida_pronostico = salida_pronostico %>% select(FECCORTE,Prom_Models)

  }
  else{

    salida_pronostico = salida_pronostico %>% select(FECCORTE,cols_final)

  }

  salida_pronostico = full_join(salida_pronostico,datasets$depto_ts) %>% arrange(FECCORTE)

  if(prom_movil=="Promedio Movil"&length(model)>1){
    salida_subsidios = data.frame(Subsidios=salida_pronostico$Prom_Models*datasets$depto_ts$Prom_movil[nrow(datasets$depto_ts)])
  }
  else if (prom_movil=="Promedio Movil"&length(model)==1){
    salida_subsidios = data.frame(Subsidios=salida_pronostico[,cols_final]*datasets$depto_ts$Prom_movil[nrow(datasets$depto_ts)])
  }
  else if (prom_movil=="Promedio Histórico"&length(model)>1){
    salida_subsidios = data.frame(Subsidios=salida_pronostico[,Prom_Models]*hist_percent_subs)
  }
  else if (prom_movil=="Promedio Histórico"&length(model)==1){
    salida_subsidios = data.frame(Subsidios=salida_pronostico[,cols_final]*hist_percent_subs)
  }



  salida_subsidios$FECCORTE = salida_pronostico$FECCORTE
  salida_subsidios = full_join(salida_subsidios,datasets$depto_ts) %>% arrange(FECCORTE)

  #### Salida de la función
  list(pronosticos = salida_modelos,
       mape_modelos = mape_modelos,
       time_serie = time_serie,
       datasets = datasets,
       salida_pronostico = salida_pronostico,
       salida_subsidios = salida_subsidios)

}

