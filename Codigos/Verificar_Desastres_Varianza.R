# El siguiente codigo busca encontrar cuales combinaciones de  pais-tipodesastre no tienen desastres en el analisis de la varianza, 
# y cuales tienen solamente uno. Se hara principalmente para las ventanas de estimacion y traslape de interes, es decir 500 E - 50 T y 750 E - 50 T


if(1){
  # Generar la clase ESVolatility, para poder manejar los resultados de la estimacion para la varianza
  setClass("ESVolatility",slots=list(coefficients = "numeric",goodness_of_fit = "numeric",res_estandar_estimacion="xts",
                                     res_no_estandar_estimacion="xts",variance_forecast="xts",residuales_evento="xts",
                                     info.evento = 'data.frame'))
  
  if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")
  
  rm(list = ls())
  if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
  if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')
  
  cat("\014")
  
  # Cargar librerias --------------------------------------------------------
  
  library(tidyverse)
  library(xts)
  library(timeDate)
  library(zoo)
  library(tempdisagg)
  library(tsbox)
  library(quantmod)
  library(timeSeries)
  library(forecast)
  library(nlme)
  library(seasonal)   
  library(openxlsx)
  library(urca)
  library(fable)
  library(lmtest)
  library(moments)
  library(stargazer)
  library(Hmisc)
  library(scales)
  library(vars)
  library(smoots)
  library(dynlm)
  library(systemfit)
  library(ks)
  library(knitr)
  library(gridExtra)
  library(stringr)
  library(maps)
  library(mapproj)
  library(ggthemes)
  library(tmap)
  library(sf)
  library(ggsci)
  library(classInt)
  library(gnFit)
  library(rugarch)
  library(knitr)
  library(kableExtra)
  library(janitor) # Para manejo de tablas descriptivas
  library(xtable)  # Para exportar tablas a latex 
  library(RColorBrewer)
  library(tools)
  library(writexl)  # Para crear excel
  library(readxl)
  
  # Cargar funciones --------------------------------------------------------
  source(paste0(getwd(),'/Codigos/Functions_Climate_Change.r')) # Source de las funciones
  
  countries   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                   'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice
}

# Se crea un tipo de objeto S4 para guardar la tabla de la media, y aparte el numero de eventos para cada pais
setClass('Tabla.varianza', slots = list(dataframe = 'data.frame', no.eventos = 'numeric'))

# Prueba de filtro  -------------------------------------------------------
directorio.saved        <- paste0(getwd(),'/Resultados_regresion/')
# directorio.guardar      <- paste0(directorio.saved,'Tablas/') # con cambio de bootstrap las tablas se guardan en otra carpeta
directorio.guardar      <- paste0(directorio.saved,'Tablas/') 

# Parametros --------------------------------------------------------------
tipo.serie              <- 'Indices'   #<<<--- Puede ser 'CDS' o 'Indices'  
tipo.estudio            <- 'varianza' #<<<--- Puede ser de 'media' o 'varianza'
regresor.mercado        <- 'PM'    #<<<--- Retornos de mercado 'PM' es promedio movil y 'MSCI' es el retorno MSCI Emerging Markets
paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
# de todos los paises de los que se tiene informacion
columna.agrupar         <- 'Ambas'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
# 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
# 'Ambas' implica que se va a analizar por ambas columbas, por ejemplo: brazil - hidrologico, brazil - geofisico, ...
vol_ev_window           <- 15  #<<<--- Tamaño de la ventana de evento

ventanas.estimacion <- c(500, 750)
ventanas.traslape   <- c('50')   #<<<--- Puede ser 50, 100 o 150   (Importante que sea string)
for(ventana.estimacion in ventanas.estimacion){
  for(ventana.traslape in ventanas.traslape){
    # Volatility event study --------------------------------------------------
    # load de los objetos de la varianza
    load(paste0(directorio.saved,tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.estudio,'_',regresor.mercado,'.RData'))
    # Eliminar objetos NA
    volatility_results <- suppressWarnings(purrr::discard(volatility_results,is.na))
    
    # Filtracion de los resultados -------------------------------------------
    # Dejar solo los eventos que hayan sucedido en algun pais de <paises.resultados>
    volatility_results <- volatility_results[purrr::map(volatility_results,~.x@info.evento$Country) %in% paises.resultados]
    # Ver cuantos desastres hay por cada valor de <columna.agrupar>. Por ejemplo: 1 biological, 3 climatological
    # 37 geophysical, 110 hydrological, 31 meteorological. De este modo se observa que hay muy pocos eventos biologicos
    # y climatologicos para poder realizar cualquier analisis
    table(unlist(purrr::map(volatility_results,~.x@info.evento$Disaster.Subgroup)))
    tipos.desastres <- unique(unlist(purrr::map(volatility_results, ~.x@info.evento$Disaster.Subgroup)))
    
    # Separar la lista dependiendo de una columna en especifico introducida por el usuario 
    if(columna.agrupar != 'Ambas') v.lista.separada <- split(volatility_results, sapply(volatility_results, function(x) x@info.evento[[columna.agrupar]]))
    # Generar listas distintas para cada valor de la <columna.agrupar>, en caso de querer utilizarlas mas adelante
    # for (i in seq_along(v.lista.separada)) assign(paste0("v.list.", names(v.lista.separada)[i]), v.lista.separada[[i]])
    # Cuando <columna.agrupar> == <'Ambas'> toca tener un trato especial
    if(columna.agrupar == 'Ambas'){
      # En primer lugar por cada desastre se necesita una combinacion del pais y el tipo de desastre
      # Para eso creamos una funcion, solo para mejor lectura
      crear.columna <- function(df) {
        df <- df %>%
          mutate(Desastre.Pais = paste0(Country, Disaster.Subgroup))
        return(df)
      }
      volatility_results <- purrr::map(volatility_results, function(s4_object) {
        s4_object@info.evento <- crear.columna(s4_object@info.evento)
        return(s4_object)
      })
      # Ahora si podemos separar <volatility_results> en listas dependiendo del desastre y el pais donde sucedio
      v.lista.separada <- split(volatility_results, sapply(volatility_results, function(x) x@info.evento[['Desastre.Pais']]))
      # Buscamos la combinacion de paises-tiposdesastres que generan solamente un desastre
      lista.un.evento <- purrr::keep(v.lista.separada, ~(length(.x) == 1))
      # Por otro lado, como la especificacion de Bialkowski (2008) ecuacion 5 esta hecha para mas de un evento, solamente se van
      # a guardar los elementos de <v.lista.separada> que contengan mas de un desastre
      v.lista.separada <- purrr::keep(v.lista.separada, ~(length(.x) > 1))
      
      # Ya tenemos las combinaciones de pais y tipo desastre con un evento, falta buscar aquellas que no tienen ningun evento en absoluto
      posibles.combinaciones <- apply(expand.grid(countries, tipos.desastres),1, function(x) paste0(x[1],x[2]))
      # Concatenamos los valores de los que si tenemos datos
      nombres.listas <- c(names(lista.un.evento),names(v.lista.separada))
      # Combinaciones sin evento
      nombres.sin.eventos <- setdiff(posibles.combinaciones, nombres.listas)
    }
    print(paste0('Estimacion ', ventana.estimacion,'. Traslape ', ventana.traslape,' no hay desastres para: ',paste0(nombres.sin.eventos, collapse=' - ')))
    print(paste0('Estimacion ', ventana.estimacion,'. Traslape ', ventana.traslape,' hay solamente un desastre para: ',paste0(names(lista.un.evento),collapse= ' - ')))
  }
}