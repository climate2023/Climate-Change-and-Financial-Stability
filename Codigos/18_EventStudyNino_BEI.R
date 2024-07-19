##########################################################
# Event_Study_BEI se rerfiere al analisis hecho por Tommaso et al (2023), aumentado con un 
# GARCH proveniente de ...
# Importante mencionar que es para las series del BEI unicamente, ya que se van a proponer tres modelos distintos
#  1) Autorregresivo unicamente
#  2) Aumentado por medida de desanclaje
#  3) Aumentado tanto por medida de desanclaje como por expectativas de produccion
# Autores: Juan Pablo Bermudez.
##########################################################

rm(list = ls())
# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))

# Lectura de datos --------------------------------------------------------
# Lectura base BEI
load(paste0(Dir,'Procesado/Base_BEI.RData'))

# Parametros --------------------------------------------------------------
numero.lags          <- NULL             # Parametro con el cual se elige el numero de rezagos para la serie de BEI
estimation_start     <- c(500)   #<<<--- No. de dias antes del evento para comenzar la estimacion (media)
estimation_end       <- 1                #<<<--- No. dias antes del evento para finalizar la estimacion (media)
max_abnormal_returns <- 15               #<<<--- No. dias maximos despues del evento para calcular retorno anormal (media)
length_car_window    <- 15               #<<<--- Ventana para calcular el CAR (por ejemplo 5 significa [0,+5], donde 0 es el dia del evento) (mean)
date_col_name        <- "Start.Date"     #<<<--- Parametro que indica el nombre de la columna clase <Date>, la cual contiene la fecha de eventos
geo_col_name         <- "Country"        #<<<--- Parametro que indica el nombre de la columna que contiene los paises, o puede ser cualquier otra variable 
#       que se quiera estudiar, como regiones, ciudades, etc
umbral.evento        <- c(0)    #<<<--- Umbrales evento para el estudio sobre la media. Ya no se van a filtrar los eventos
plazos.bei           <- c(1,2,3,4,5)     #<<<--- Parametro que indica cuales seran las series de BEI analizadas, por ejemplo a 1, 2 3 4 o 5 anos. 
estimation_vol_start <- c(500)  #<<<--- No. de dias antes del evento para comenzar la estimacion (media)
vol_ev_window        <- 15               #<<<--- Tamaño de la ventana de evento (varianza)
umbral.evento.vol    <- c(0)    #<<<--- Umbrales evento para estudio de volatilidad
bei.sinprima         <- T                #<<<--- Parametro que indica cuales son los BEI de los que se obtendran resultados, T significa que seran los BEI sin prima,
#       F seran los BEI originales
bool.media           <- T                #<<<--- bool para estudio en media
bool.varianza        <- F                #<<<--- bool para estudio en varianza

# Tratamiento de datos ----------------------------------------------------
# En primer lugar, es relevante mencionar que solamente tenemos datos del BEI de Colombia, por lo cual solamente
# necesitamos los desastres de Colombia. Actualmente estamos utilizando los desastres que salen de la base de datos 
# EMDAT, pero ya que contamos con solamente un pais, se podria utilizar una fuente nacional, anteriormente se comento
# que se podrian ver anuncios de los desastres 

# Debido a lo anterior se filtra la base <emdat_base> para solo los desastres de Colombia
emdat_base_colombia <- read_xlsx(paste0(getwd(),'/Bases/BEI/Fechas_Nino.xlsx'))

emdat_base_colombia <- emdat_base_colombia %>% 
  pivot_longer(cols = everything(),names_to = 'Type_announcement', values_to = 'Start.Date') %>% 
  dplyr::filter(!is.na(Start.Date)) %>% 
  dplyr::mutate('Country' = 'Colombia',
                'na.start' = 0) %>% 
  mutate(Start.Date = as.Date(Start.Date))

# Como se tienen 5 elementos en <plazos.bei> se puede correr usando un for loop
for(plazo.bei in plazos.bei){
  # Filtrar exclusivamente las series que correspondan al plazo de interes, establecido por <plazo.bei>
  base.bei.originales.final <- base_bei_originales_final[, grep(plazo.bei, colnames(base_bei_originales_final), value = TRUE)]
  base.bei.sin.prima.final  <- base_bei_sin_prima_final[, grep(plazo.bei, colnames(base_bei_sin_prima_final), value = TRUE)]
  
  # Como todas las variables son I(1), es necesario diferenciarlas para asi poder hacer la regresion entre series I(0)
  base.bei.originales.final <- diff(base.bei.originales.final)[2:nrow(base.bei.originales.final)]
  base.bei.sin.prima.final  <- diff(base.bei.sin.prima.final)[2:nrow(base.bei.sin.prima.final)]
  
  # Como todos los modelos van a contar con un componente autoregresivo de las variables dependientes, es necesario generar
  # una base con las series rezagadas
  base.bei.original.lag <- create.lags(base = base.bei.originales.final, interest.vars = colnames(base.bei.originales.final)[grepl('BEI',colnames(base.bei.originales.final))],
                                       no.lags = numero.lags, AR.m = 20, complete.base = F)
  base.bei.sinprima.lag <- create.lags(base = base.bei.sin.prima.final, interest.vars = colnames(base.bei.sin.prima.final)[grepl('BEI',colnames(base.bei.sin.prima.final))],
                                       no.lags = numero.lags, AR.m = 20, complete.base = F)
  # Establecer la base de interes para realizar el event study. Puede realizarse un loop para realizar las dos al tiempo, pero faltaria simplificar el codigo.
  if(bei.sinprima){
    base.interes <- base.bei.sinprima.lag
  }else{
    base.interes <- base.bei.original.lag
  }
  
  # Event Study Media -------------------------------------------------------
  if(bool.media) {
    length_estimation_window <- estimation_start - estimation_end + 1 # Tamaño de la ventana de estimacion
    length_event_window      <- length_car_window + 1 # Longitud ventana de evento es 1 mas <length_car_window>
    # <length_car_window> no puede ser mayor a <max_abnormal_returns>, ya que implica una ventana de evento mayor al numero de retornos
    # anormales estimados.
    # Asegurar que la ventana de evento no sea mayor que los retornos anormales estimados. 
    if(length_car_window > max_abnormal_returns) length_car_window <- max_abnormal_returns
    
    # Se eliminan los eventos que no cuentan con la ventana minima de estimacion ni con la ventana minima de evento usando la funcion <drop.events>
    eventos_filtrado <- drop.events(data.events = emdat_base_colombia, base = base.interes, estimation.start = estimation_start, max.ar=max_abnormal_returns, 
                                    date_col_name, geo_col_name)
    
    # -------------------------- Regresion estimation window ---------------------------------------------
    
    # <estimation.event.study> realiza la estimacion por OLS para cada evento en <data.events>. Retorna una lista para cada evento que incluye:
    #     Dataframe      : retornos observados, estimados y anormales para la ventana de estimacion y ventana de evento. 
    #     Standard_error : error estandar de los errores de la estimacion por OLS
    # El objeto de salida de esta funcion sera la base para las pruebas de Wilcoxon y bootstrap
    
    # Se van a correr tres modelos distintos para probar distintas especificaciones
    # Primer modelo: autorregresivo
    all_events_list <- estimation.event.study.bei(base = base.interes, data.events = eventos_filtrado, max.ar = 15, es.start = estimation_start, 
                                                  es.end = estimation_end, var.endogena = paste0('BEI_',plazo.bei,'Y') , vars.exo = NULL, 
                                                  GARCH = "sGARCH", overlap.events = eventos_filtrado, overlap.max = umbral.evento, 
                                                  date.col.name = date_col_name, var.col.name = geo_col_name, bool.nino = T)
    # Segundo modelo: autorregresivo + grado desanclaje
    var.exo1 <- colnames(base.interes)[grep('grado', colnames(base.interes))]
    all_events_list_desanclaje <- estimation.event.study.bei(base = base.interes, data.events = eventos_filtrado, max.ar = 15, es.start = estimation_start, 
                                                             es.end = estimation_end, var.endogena = paste0('BEI_',plazo.bei,'Y') , vars.exo = var.exo1, 
                                                             GARCH = "sGARCH", overlap.events = eventos_filtrado, overlap.max = umbral.evento,
                                                             date.col.name = date_col_name, var.col.name = geo_col_name, bool.nino = T)
    # Tercer modelo: autorregresivo + grado desanclaje + expectativas PIB
    var.exo2 <- colnames(base.interes)[vgrep(c('grado','Forecast'), colnames(base.interes))]
    all_events_list_expectativas <- estimation.event.study.bei(base = base.interes, data.events = eventos_filtrado, max.ar = 15, es.start = estimation_start, 
                                                               es.end = estimation_end, var.endogena = paste0('BEI_',plazo.bei,'Y') , vars.exo = var.exo2, 
                                                               GARCH = "sGARCH", overlap.events = eventos_filtrado, overlap.max = umbral.evento,
                                                               date.col.name = date_col_name, var.col.name = geo_col_name, bool.nino = T)
    
    # Guardar los resultados de las regresiones
    tipo.bei <- ifelse(bei.sinprima, 'sinprima','originales')
    save(all_events_list, all_events_list_desanclaje, all_events_list_expectativas, 
         file=paste0(getwd(),'/Resultados_regresion/Nino/BEI','_tra',umbral.evento,'_est',estimation_start,'_',tipo.bei,'_',plazo.bei,'_year','.RData'))
  }
  
  # Volatility event study --------------------------------------------------
  if(bool.varianza) {
    # Filtrar los eventos para que solo queden aquellos que cumplan con una ventana minima de estimacion y una ventana minima de 
    # evento
    eventos.filtrado.volatilidad <- drop.events(data.events = emdat_base_colombia, base = base.interes, estimation.start = estimation_vol_start, max.ar=vol_ev_window, 
                                                date_col_name, geo_col_name)
    
    # Se corren tres modelos distintos para probar distintas especificaciones
    # Primer modelo: autorregresivo
    volatility_results <- volatility_event_study_bei(base = base.interes, data.events = eventos.filtrado.volatilidad, max.ar = vol_ev_window, 
                                                     es.start = estimation_vol_start, es.end = estimation_end, 
                                                     var.endogena = paste0('BEI_',plazo.bei,'Y'), vars.exo = NULL, GARCH = 'sGARCH', 
                                                     overlap.events = eventos.filtrado.volatilidad, overlap.max = umbral.evento.vol, 
                                                     date.col.name = date_col_name, var.col.name = geo_col_name, bool.nino = T)
    
    # Segundo modelo: autorregresivo + grado desanclaje
    var.exo1 <- colnames(base.interes)[grep('grado', colnames(base.interes))]
    volatility_results_desanclaje <- volatility_event_study_bei(base = base.interes, data.events = eventos.filtrado.volatilidad, max.ar = vol_ev_window, 
                                                                es.start = estimation_vol_start, es.end = estimation_end, 
                                                                var.endogena = paste0('BEI_',plazo.bei,'Y'), vars.exo = var.exo1, GARCH = 'sGARCH', 
                                                                overlap.events = eventos.filtrado.volatilidad, overlap.max = umbral.evento.vol, 
                                                                date.col.name = date_col_name, var.col.name = geo_col_name, bool.nino = T)
    # Tercer modelo: autorregresivo + grado desanclaje + expectativas PIB
    var.exo2 <- colnames(base.interes)[vgrep(c('grado','Forecast'), colnames(base.interes))]
    volatility_results_expectativas <- volatility_event_study_bei(base = base.interes, data.events = eventos.filtrado.volatilidad, max.ar = vol_ev_window, 
                                                                  es.start = estimation_vol_start, es.end = estimation_end, 
                                                                  var.endogena = paste0('BEI_',plazo.bei,'Y'), vars.exo = var.exo2, GARCH = 'sGARCH', 
                                                                  overlap.events = eventos.filtrado.volatilidad, overlap.max = umbral.evento.vol, 
                                                                  date.col.name = date_col_name, var.col.name = geo_col_name, bool.nino = T)
    
    # Guardar los resultados de las regresiones
    tipo.bei <- ifelse(bei.sinprima, 'sinprima','originales')
    save(volatility_results, volatility_results_desanclaje, volatility_results_expectativas, 
         file=paste0(getwd(),'/Resultados_regresion/Nino/BEI','_tra',umbral.evento.vol,'_est',estimation_vol_start,'_',tipo.bei,'_',plazo.bei,'_year_Varianza','.RData'))
  }
}