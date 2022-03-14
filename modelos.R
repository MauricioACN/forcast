library(xts)
library(forecast)
library(TTR)
library(MLmetrics)
library(tidyverse)
library(astsa)
library(tseries)
source("functions.R",encoding = "utf-8")

df = readxl::read_excel("data/BaseColocacionesVictimas.xlsx")
df$COD_DPT = formatC(df$COD_DPT,flag=0,width=2)
df$COD_MPIO_INVERSION = sprintf("%05d",df$COD_MPIO_INVERSION)
df$FECCORTE = as.Date(as.character(df$FECCORTE),"%Y%m%d")

df = data_clean(df = df,type = "mensual")
depto_ts = df %>% group_by(FECCORTE,COD_DPT) %>%
  summarise(Total_millones = sum(Millones,na.rm=T),
            Total_subsidio = sum(Subsidio,na.rm=T))


plot_df = full_join(lista[[1]]$depto_ts_train,lista[[1]]$depto_ts_test)

plot_df$promedio_models = rowMeans(plot_df[,c(5:9)],na.rm = T)

p = ggplot(plot_df)+
  geom_line(aes(x=FECCORTE,y = Total_millones),color="red")+
  geom_line(aes(x=FECCORTE,y = naive,colour = "naive"))+
  geom_line(aes(x=FECCORTE,y = ets_forecast,colour = "ets_forecast"))+
  geom_line(aes(x=FECCORTE,y = tbats_forecast,colour = "tbats_forecast"))+
  geom_line(aes(x=FECCORTE,y = arima_forcast,colour = "arima_forcast"))+
  geom_line(aes(x=FECCORTE,y = sarima_forcast,colour = "sarima_forcast"))+
  scale_color_manual(name = "Modelos",
                     values = c("naive" = "darkblue",
                                "ets_forecast" = "blue",
                                "tbats_forecast" = "green",
                                "arima_forcast" = "pink",
                                "sarima_forcast" = "#4287f5",
                                "promedio_models" = "#42f5f2"))+
  ggtitle("Proyecciones para el Departamento Antioquia")+
  theme_classic()

plotly::ggplotly(p)



######
naive = snaive(time_series_base[[1]][[1]], h=24)
snaive_forecast = forecast(naive, h=24)
snaive_forecast = as.data.frame(snaive_forecast)
new_df_forecast = snaive_forecast$`Point Forecast`
new_df_forecast = data.frame(naive=ets_forecast$`Point Forecast`)

ets_model = ets(time_series_base[[1]][[1]], allow.multiplicative.trend = TRUE)
ets_forecast = forecast(ets_model, h=24)
ets_forecast = as.data.frame(ets_forecast)
new_df_forecast$ets_forecast = ets_forecast$`Point Forecast`

dshw_model = dshw(time_series_base[[1]][[1]], period1=4, period2 = 12, h=24)
dshw_model_f = forecast(dshw_model, h=24)
dshw_model_f = as.data.frame(dshw_model_f)
new_df_forecast$dshw_model_f = dshw_model_f$`Point Forecast`

tbats_model = tbats(time_series_base[[1]][[1]])
tbats_forecast = forecast(tbats_model, h=24)
tbats_forecast = tbats_forecast %>% as.data.frame()
new_df_forecast$tbats_forecast = tbats_forecast$`Point Forecast`

arima_optimal = auto.arima(time_series_base[[1]][[1]])
arima_forcast = forecast(arima_optimal,h = 24)
arima_forcast = arima_forcast %>% as.data.frame()
new_df_forecast$arima_forcast = arima_forcast$`Point Forecast`

library(astsa)
sarima_forecast = sarima.for(time_series_base[[1]][[1]],
                             n.ahead=24,
                             p=0,d=1,q=0,P=0,D=1,Q=0,S=12)
sarima_forecast = as.data.frame(sarima_forecast)
new_df_forecast$depto_ts_test$sarima_forcast = sarima_forecast$pred
new_df_forecast$FECCORTE = seq.Date(from = as.Date("2021-06-01"),to = as.Date("2023-06-01"),length.out = 12)

plot_df = full_join(lista[[1]]$depto_ts_train,new_df_forecast)

plot_df$promedio_models = rowMeans(plot_df[,c(5:8)],na.rm = T)

p = ggplot(plot_df)+
  geom_line(aes(x=FECCORTE,y = Total_millones),color="red")+
  geom_line(aes(x=FECCORTE,y = promedio_models,colour = "Modelo"))+
  scale_color_manual(name = "Modelos",
                     values = c("Modelo" = "darkblue"))+
  ggtitle("Proyecciones para el Departamento Antioquia")+
  theme_classic()

plotly::ggplotly(p)



#####
######
naive = snaive(time_series_base[[7]][[1]], h=24)
snaive_forecast = forecast(naive, h=24)
snaive_forecast = as.data.frame(snaive_forecast)
new_df_forecast = snaive_forecast$`Point Forecast`
new_df_forecast = data.frame(naive=ets_forecast$`Point Forecast`)

ets_model = ets(time_series_base[[7]][[1]], allow.multiplicative.trend = TRUE)
ets_forecast = forecast(ets_model, h=24)
ets_forecast = as.data.frame(ets_forecast)
new_df_forecast$ets_forecast = ets_forecast$`Point Forecast`

dshw_model = dshw(time_series_base[[7]][[1]], period1=4, period2 = 12, h=24)
dshw_model_f = forecast(dshw_model, h=24)
dshw_model_f = as.data.frame(dshw_model_f)
new_df_forecast$dshw_model_f = dshw_model_f$`Point Forecast`

tbats_model = tbats(time_series_base[[7]][[1]])
tbats_forecast = forecast(tbats_model, h=24)
tbats_forecast = tbats_forecast %>% as.data.frame()
new_df_forecast$tbats_forecast = tbats_forecast$`Point Forecast`

arima_optimal = auto.arima(time_series_base[[7]][[1]])
arima_forcast = forecast(arima_optimal,h = 24)
arima_forcast = arima_forcast %>% as.data.frame()
new_df_forecast$arima_forcast = arima_forcast$`Point Forecast`

library(astsa)
sarima_forecast = sarima.for(time_series_base[[5]][[1]],
                             n.ahead=24,
                             p=0,d=1,q=0,P=0,D=1,Q=0,S=12)
sarima_forecast = as.data.frame(sarima_forecast)
new_df_forecast$depto_ts_test$sarima_forcast = sarima_forecast$pred
new_df_forecast$FECCORTE = seq.Date(from = as.Date("2021-06-01"),to = as.Date("2023-06-01"),length.out = 12)

plot_df = full_join(lista[[2]]$depto_ts_train,new_df_forecast)

plot_df$promedio_models = rowMeans(plot_df[,c(5:8)],na.rm = T)

p = ggplot(plot_df)+
  geom_line(aes(x=FECCORTE,y = Total_millones),color="red")+
  geom_line(aes(x=FECCORTE,y = arima_forcast,colour = "Modelo"))+
  scale_color_manual(name = "Modelos",
                     values = c("Modelo" = "darkblue"))+
  ggtitle("Proyecciones para el Departamento Chocó")+
  theme_classic()

plotly::ggplotly(p)

