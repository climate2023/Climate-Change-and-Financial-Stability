if(1){
  # Generar la clase ESVolatility, para poder manejar los resultados de la estimacion para la varianza
  setClass("ESVolatility",slots=list(coefficients = "numeric",goodness_of_fit = "numeric",res_estandar_estimacion="xts",
                                     res_no_estandar_estimacion="xts",variance_forecast="xts",residuales_evento="xts",
                                     info.evento = 'data.frame'))
  
  if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")
  
  rm(list = ls())
  if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Repositorio original/Climate-Change-and-Financial-Stability')
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

# Prueba de filtro  -------------------------------------------------------
directorio.saved        <- paste0(getwd(),'/Resultados_regresion/')
# directorio.guardar      <- paste0(directorio.saved,'Tablas/') # con cambio de bootstrap las tablas se guardan en otra carpeta
directorio.guardar      <- paste0(directorio.saved,'Tablas/Nuevas_Tablas_Varianza/') 

ventanas.estimacion <- c(500, 750, 1000)
for(ventana.estimacion in ventanas.estimacion){
  tipo.serie              <- 'CDS'   #<<<--- Puede ser 'CDS' o 'Indices'  
  # ventana.estimacion      <- '750'   #<<<--- Puede ser 200, 300 o 500   (Importante que sea string)
  ventana.traslape        <- '50'   #<<<--- Puede ser 50, 100 o 200   (Importante que sea string)
  tipo.estudio            <- 'varianza' #<<<--- Puede ser de 'media' o 'varianza'
  regresor.mercado        <- 'PM'    #<<<--- Retornos de mercado 'PM' es promedio movil y 'MSCI' es el retorno MSCI Emerging Markets
  
  tipos.desastre.eliminar <- c('Biological','Climatological') #<<<--- NULL si no se desea eliminar ningun tipo de desastre 
  paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
  # de todos los paises de los que se tiene informacion
  eventos.fecha.exac      <- T  #<<<--- booleano para indicar si se quieren usar solamente los eventos que tengan una fecha exacta
  # <T> en caso de querer solo los que tienen fecha exacta.<F>si se quieren usar tambien aquellos eventos de
  # los que se asumio el dia  
  columna.agrupar         <- 'Ambas'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
  # 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
  # 'Ambas' implica que se va a analizar por ambas columbas, por ejemplo: brazil - hidrologico, brazil - geofisico, ...
  vol_ev_window           <- 15  #<<<--- Tamaño de la ventana de evento
  
  # Volatility event study --------------------------------------------------
  # load de los objetos de la varianza
  load(paste0(directorio.saved,tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.estudio,'_',regresor.mercado,'.RData'))
  # Eliminar objetos NA
  volatility_results <- purrr::discard(volatility_results,is.na)
  
  # Filtracion de los resultados -------------------------------------------
  # Tipo de desastre
  round(table(unlist(purrr::map(volatility_results, ~.x@info.evento$Disaster.Subgroup)))/length(volatility_results),3)
  # Dejar solamente los eventos que tengan fecha exacta, para el caso en que eventos.fecha.exac == T
  if(eventos.fecha.exac) volatility_results <- purrr::keep(volatility_results, ~ .x@info.evento$na_start == 0)
  # Conservar solamente los eventos geofisicos, meteorologicos e hidrologicos 
  if(!is.null(tipos.desastre.eliminar)) volatility_results <- volatility_results[!(purrr::map(volatility_results, ~.x@info.evento$Disaster.Subgroup) %in% tipos.desastre.eliminar)]
  # Dejar solo los eventos que hayan sucedido en algun pais de <paises.resultados>
  volatility_results <- volatility_results[purrr::map(volatility_results,~.x@info.evento$Country) %in% paises.resultados]
  # Ver cuantos desastres hay por cada valor de <columna.agrupar>. Por ejemplo: 1 biological, 3 climatological
  # 37 geophysical, 110 hydrological, 31 meteorological. De este modo se observa que hay muy pocos eventos biologicos
  # y climatologicos para poder realizar cualquier analisis
  table(unlist(purrr::map(volatility_results,~.x@info.evento$Disaster.Subgroup)))
  
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
    # Por otro lado, como la especificacion de Bialkowski (2008) ecuacion 5 esta hecha para mas de un evento, solamente se van
    # a guardar los elementos de <v.lista.separada> que contengan mas de un desastre
    v.lista.separada <- purrr::keep(v.lista.separada, ~(length(.x) > 1))
  }
  
  # Graficas CAV ------------------------------------------------------------
  for(i in seq_along(v.lista.separada)){
    element <- v.lista.separada[[i]]
    name    <- names(v.lista.separada)[i]  
    grafico_cav(element,as.numeric(ventana.estimacion),vol_ev_window)
    title(name,line=0.75)
  }
  
  # Tabla CAV/significancia ------------------------------------------------
  
  # Dataframe con muchas ventanas
  matrix.volatilidad <- matrix(nrow=(vol_ev_window),ncol=(length(v.lista.separada)+1))
  iteraciones.bool  <- 5000
  for(i in seq_along(v.lista.separada)){
    for(j in (1:(vol_ev_window))){
      prueba <- bootstrap.volatility2(v.lista.separada[[i]],as.numeric(ventana.estimacion),j,bootstrap_vol_iterations = iteraciones.bool)
      matrix.volatilidad[j,i] <- paste(prueba$CAV,prueba$Significancia)
    }
  }
  
  k <- length(v.lista.separada)+1
  for(j in (1:(vol_ev_window))){ 
    prueba.cav <- bootstrap.volatility2(volatility_results,as.numeric(ventana.estimacion),j,bootstrap_vol_iterations = iteraciones.bool)
    matrix.volatilidad[j,k] <- paste(prueba.cav$CAV,prueba.cav$Significancia)
  }
  colnames(matrix.volatilidad) <- c(names(v.lista.separada),'Todos')
  Ventana                      <- 1:(vol_ev_window)
  matrix.volatilidad           <- cbind(Ventana,matrix.volatilidad)
  
  dataframe.volatilidad        <- data.frame(matrix.volatilidad)
  
  # Guardar las tablas de significancia. No es necesario agregar el tipo de test ya que podemos guardar ambas tablas
  if(columna.agrupar=='Disaster.Subgroup') agrupacion <- 'tipodesastre'
  if(columna.agrupar=='Country') agrupacion <- 'pais'
  if(columna.agrupar=='Ambas')   agrupacion <- 'paistipodesastre'
  save(dataframe.volatilidad,
       file=paste0(directorio.guardar,'Tablas_',tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.estudio,'_',regresor.mercado,'_',agrupacion,'.RData'))
}