##########################################################
# Formacion base de datos para Estudio de Eventos con el BEI
# Autores: Juan Pablo Bermudez.
##########################################################

rm(list = ls())
# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))

# Parametros del codigo ---------------------------------------------------
original.date.columns <- c('Date','...1') # <<<--- nombre de las columnas que incluyen la fecha en la base original
meses.estimacion      <- 37 #<<<--- parametro para determinar el numero de meses que se utilizaran para la estimacion 
                            # del ARIMA que servira para pronosticar la serie
meses.forecast        <- c(12, 24, 36, 48, 60) #<<<--- numero de meses para hacer el pronostico de la serie
save                  <- T #<--- para guardar los datos en alta frecuencia, F para cargarlos

# Cargar datos ------------------------------------------------------------
# Obtener los nombres de las hojas del excel para poder leerlas todas
names.excel <- excel_sheets(path = paste0(Dir,'BEI/Series_BEI_LFM_ISE_desanclaje.xlsx'))
# Iterar a traves de los nombres para obtener todas las bases
dataframes  <- lapply(names.excel, function(name){
  df.temp <- read_excel(paste0(Dir,'BEI/Series_BEI_LFM_ISE_desanclaje.xlsx'), sheet = name)
  # Ahora es necesario convertir todas las series a xts, siendo el indice la columna de la fecha
  colname.fecha <- colnames(df.temp)[colnames(df.temp) %in% original.date.columns]
  df.temp.datos <- df.temp %>% dplyr::select(!all_of(colname.fecha))
  df.xts  <- xts(df.temp.datos, order.by = as.Date(df.temp[[colname.fecha]])) 
  return(df.xts)
})
names(dataframes) <- names.excel

# Generación serie mensual expectativa ISE --------------------------------
# La serie recibida ya esta desestacionalizada entonces no es de preocupacion modelar el comportamiento
# estacional dentro del arima

# Hacemos un ciclo <for> para obtener las cinco series de forecast
forecast.ise <- list()
for(i in 1:(nrow(dataframes[['ISE']]) - meses.estimacion +1)){
  datos.estimacion <- dataframes[['ISE']][i:(meses.estimacion+i-1)]
  # Se coloca d = 1 porque la serie es no estacionaria, I(1)
  arima.estimado <- auto.arima(datos.estimacion, d = 1, max.p = 20, max.q = 20)
  # Ya con el arima estimado, podemos hacer el forecast para todos los meses en <meses.forecast>
  # Por ejemplo, cuando <meses.estimacion> es 37, se esta estimando el arima hasta enero de 2003, y 12 meses despues
  # es enero del 2004, 24 meses enero del 2005, ... Al final obtendremos entonces 5 series, donde la primera sera una proxy
  # de las expectativas del PIB a un ano, la segunda expectativas a dos anos y asi sucesivamente
  forecast.arima <- forecast(arima.estimado, h = tail(meses.forecast,1))
  # Obtenemos el pronostico puntual para los <meses.forecast>
  forecast.meses.interes        <- forecast.arima$mean[meses.forecast]
  # Colocarle la fecha que se esta pronosticando
  names(forecast.meses.interes) <- tail(index(datos.estimacion),1) %m+% months(meses.forecast)
  forecast.ise[[i]] <- forecast.meses.interes
}

xts.forecast <- lapply(seq_along(meses.forecast), function(index){
  # Obtenemos un vector solamente para la serie que corresponde a <index>
  temp.list <- unlist(purrr::map(forecast.ise, ~.x[index]))
  # Tenemos que convertir en xts la serie, recordando que las fechas son los nombres del vector
  temp.xts           <- xts(as.numeric(temp.list), order.by = as.Date(names(temp.list)))
  colnames(temp.xts) <- paste0('ISEForecast_',index,'_Year')
  return(temp.xts)
})
names(xts.forecast) <- paste0('ISEForecast_',1:5,'_Year')

# Volver serie a alta frecuencia ------------------------------------------
if(save){
  xts.daily.forecast <- lapply(xts.forecast, function(xts) td(xts~1, to ='day',method = 'fast', conversion = 'mean'))
  save(xts.daily.forecast, file=paste0(paste0(Dir,'BEI/daily_forecast.RData')))
}else{
  load(paste0(paste0(Dir,'BEI/daily_forecast.RData')))
}

# <if(0)> porque se pretendia buscar si algun metodo generaba desagregaciones no estacionales con una restriccion de suma
if(0){
  # Intento de desagregar usando solamente los primeros 72 datos en vez de la serie mensual completa
  methods <- c('chow-lin-maxlog', 'chow-lin-minrss-ecotrim', 'chow-lin-minrss-quilis', "chow-lin-fixed", "dynamic-maxlog", "dynamic-minrss",
                     "dynamic-fixed", "fernandez", "litterman-maxlog", "litterman-minrss", "litterman-fixed", "denton-cholette", "denton", "fast", 
                     "uniform", "ols")
  for(method.chosen in methods){
    xts.daily.prueba.serietruncada <- td(xts.forecast[[1]][1:72] ~ 1, to = 'day', method = method.chosen)
    p <- autoplot(xts.daily.prueba.serietruncada$values, col = 'red') + theme_bw() +ggtitle(paste0('Desagregacion diaria usando ', method.chosen))
    ggsave(paste0('C:/Users/jpber/OneDrive/Documents/BanRep/BEI/Prueba_Desagregacion/',method.chosen,'.png'), 
           plot = p,width = 800, height = 600, units = 'px', scale = 2)
    print(paste0('Done: ' ,method.chosen))
  }
}

# Las series diarias deben quedar solamente hasta la fecha en que se tienen datos de BEI y desanclaje
fecha.max <- as.Date(min(unlist(lapply(dataframes[2:5], function(x) tail(index(x),1)))))

# Reducir el indice de las series en <xts.daily.forecast>
xts.daily.forecast.reduced <- lapply(xts.daily.forecast, function(list){
  list$values <- list$values[index(list$values) <= fecha.max]
  return(list)
})

lapply(names(xts.daily.forecast.reduced), function(name){
  data <- xts.daily.forecast.reduced[[name]]$values
  p <- autoplot(data, col='red') + theme_bw() + ggtitle(paste0(name, 'series'))
  ggsave(filename = paste0(cd.graficos,'Forecast_ISE/', name,'.png'),width = 800, height = 600, units = 'px', scale = 2)
})

xts.daily.forecast.values <- purrr::map(xts.daily.forecast.reduced, ~.x$values)

# Juntar todas las series en una sola base de datos
base_forecast <- do.call(cbind, xts.daily.forecast.values)
colnames(base_forecast) <- names(xts.daily.forecast.values)
  
# Creacion base de datos --------------------------------------------------
# Por ultimo creamos dos bases de datos principales: una basada en los BEI originales, y la segunda basada en los BEI sin prima.
# Ambas bases tendrán los calculos del BEI, su respectivo grado de desanclaje, y las estimaciones diarias de las expectativas del PIB
# <xts.daily.forecast.reduced>
base_bei_originales <- do.call(cbind,dataframes[grep('originales', str_to_lower(names(dataframes)))])
base_bei_originales <- cbind(base_bei_originales, base_forecast)

base_bei_sin_prima  <- do.call(cbind,dataframes[grep('sin_prima', str_to_lower(names(dataframes)))])
base_bei_sin_prima  <- cbind(base_bei_sin_prima, base_forecast)

# Sin embargo, por construccion de la serie diaria de expectativas del PIB, esa serie tiene datos en fines de semana y festivos, debido a que en 
# esos dias sigue habiendo produccion, o al menos no seria logico desagregar la serie sin contar los fines de semana y festivos. El problema es que de este
# modo salen valores NA en las series del BEI, por lo que lo que el tratamiento adecuado seria restringir el indice de <base_bei_originales> al indice de los bei
# originales, y de manera similar con los bei sin prima.
base_bei_originales_final <- base_bei_originales[index(dataframes$BEI_originales)]
base_bei_sin_prima_final  <- base_bei_sin_prima[index(dataframes$BEI_sin_prima)]

# Guardar los datos en .RData ---------------------------------------------
save(base_bei_originales_final, base_bei_sin_prima_final, file = paste0(Dir,'Procesado/Base_BEI.RData'))
