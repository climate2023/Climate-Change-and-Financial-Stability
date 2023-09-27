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

# Los siguientes argumentos van a filtrar los resultados y tablas
serie             <- 'Indices'      #<<<--- puede ser 'Indices' o 'CDS'
tipo.estudio      <- 'media'     #<<<--- puede ser 'media' o 'varianza'
regresor.mercado  <- 'benchmark'    #<<<--- puede ser 'PM' o 'benchmark', para CDS todavia no hay benchmark
umbrales.evento   <- c(50,100,200)  #<<<--- puede ser 50 100 o 200
if(tipo.estudio=='media') es.windows <- c(200,300,500) #<<<--- Para media puede ser 200, 300 o 500. Para varianza solamente 500
if(tipo.estudio=='varianza') es.windows <- c(500,750,1000)
columnas.tabla    <- 'paistipodesastre' #<<<--- Las tablas de la media estan guardadas tanto por tipo de desastre como por pais
# <columnas.tabla> toma el valor de 'tipodesastre' o 'pais'. Tambien estan guardadas por 'ambas, en cuyo caso <columnas.tabla> toma el valor
# de 'paistipodesastre'

# Organizacion tablas -----------------------------------------------------

lista.wilcoxon  <- list()
numero.eventos.wilcoxon <- list()
lista.bmp       <- list()
lista.bootstrap <- list()
indice.lista   <- 0
for(i in seq_along(umbrales.evento)){
  umbral.del.evento <- umbrales.evento[i]
  for(j in seq_along(es.windows)){
    indice.lista <- indice.lista +1
    estimation.window <- es.windows[j]
    if(tipo.estudio == 'media') load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Tablas_',serie,'_tra',umbral.del.evento,'_est',
                       estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
    # Cargamos las tablas con el nuevo bootstrap
    if(tipo.estudio == 'varianza') load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Nuevas_Tablas_Varianza/Tablas_',serie,'_tra',umbral.del.evento,'_est',
                     estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
    if(tipo.estudio=='media'){
      lista.wilcoxon[[indice.lista]]         <- tabla.wilcoxon@dataframe
      names(lista.wilcoxon)[[indice.lista]]  <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
      numero.eventos.wilcoxon[[indice.lista]] <- tabla.wilcoxon@no.eventos
      names(numero.eventos.wilcoxon)[[indice.lista]]  <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
      
      lista.bmp[[indice.lista]] <- tabla.bmp@dataframe
      names(lista.bmp)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
    }
    if(tipo.estudio=='varianza'){
      lista.bootstrap[[indice.lista]] <- dataframe.volatilidad
      names(lista.bootstrap)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
    }
  }
}

# Tablas para la media ----------------------------------------------------

if(tipo.estudio == 'media'){
  # Con <setdiff(unique(unlist(lapply(lista.wilcoxon, colnames))), c('Ventana'))> encontramos todos los nombres de columnas de las tablas, eliminando 'Ventana'
  # Lo anterior debido a que se va a crear una tabla para cada tipo de desastre, o para cada pais, o para cada pais-tipo de desastre
  # Por otro lado, se necesita que hayan datos para cada elemento en <lista.wilcoxon>, por lo que es necesario hallar la union de los nombres 
  # de columnas de todos los dataframes en <lista.wilcoxon>
  columnas.union <- setdiff(unique(unlist(lapply(lista.wilcoxon, colnames))), c('Ventana'))
  for (nombre.columna in columnas.union){
    lista.interes <- lista.wilcoxon
    # <lista.interes> es una lista de dataframes, donde las columnas son:
    # <Ventana>, que se refiere a la ventana de evento
    # <Todos> que se refiere a la totalidad de los desastres
    # El resto de columnas es la division de los desastres, ya sea por tipo de desastre, pais, o por una combinacion
    # entre pais y tipo de desastre.
    # Es posible que no existan ciertas columnas para la combinacion entre pais y tipo de desastres
    # Por ejmplo: el primer elemento de <lista.interes> hace referencia a la estimacion a 500 dias de estimacion y 50 de traslape,
    # y puede que no hayan eventos geofisicos en Brazil con estas ventanas. En este caso, para un correcto funcionamiento del codigo
    # se tienen que completar los dataframes, porque si no, mas adelante <purrr::map_dfc> no va a funcionar
    # ¿Como se van a completar los dataframes? Veamos que cada uno de ellos debe tener length(columnas.union) + 1 columnas,
    # donde +1 hace referencia a la columna 'Ventana', por lo que para aquellos que tengan menos columnas, se va a buscar cual es 
    # la(s) columna(s) faltantes y escribir '' (espacios en blanco) en ellas
    no.columnas.dataframes <- unlist(lapply(lista.interes, ncol))
    for(p in seq_along(no.columnas.dataframes)) {
      no.columnas <- no.columnas.dataframes[p]
      # Encontramos los dataframes que les falta al menos una columna
      if(no.columnas != (length(columnas.union) + 1)) {
        nombres.columnas   <- colnames(lista.interes[[p]])
        # Encontramos cuales son las columnas que le faltan
        columnas.faltantes <- setdiff(columnas.union, nombres.columnas)
        lista.interes[[p]][columnas.faltantes] <- ''
      }
    }
    dataframe.wil     <- purrr::map_dfc(lista.interes, ~.x[,nombre.columna])
    numero.eventos    <- purrr::map(numero.eventos.wilcoxon, ~.x[nombre.columna])
    numero.eventos    <- unlist(lapply(numero.eventos, function(x) as.character(ifelse(is.na(x), '', x))))
    dataframe.wil200  <- dataframe.wil[,grep('Estimacion_200',colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 200 dias
    dataframe.wil300  <- dataframe.wil[,grep('Estimacion_300',colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 300 dias
    dataframe.wil500  <- dataframe.wil[,grep('Estimacion_500',colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 500 dias
    
    # Eventos para cada dataframe
    eventos.wil200 <- paste('Eventos: ', numero.eventos[grep('Estimacion_200', names(numero.eventos))])
    eventos.wil300 <- paste('Eventos: ', numero.eventos[grep('Estimacion_300', names(numero.eventos))])
    eventos.wil500 <- paste('Eventos: ', numero.eventos[grep('Estimacion_500', names(numero.eventos))])
    # Retirar nombres de columnas para hacer rbind 
    colnames(dataframe.wil200) <- NA
    colnames(dataframe.wil300) <- NA
    colnames(dataframe.wil500) <- NA
    
    # Agregar el numero de eventos a cada dataframe
    dataframe.wil200 <- rbind(eventos.wil200, dataframe.wil200)
    dataframe.wil300 <- rbind(eventos.wil300, dataframe.wil300)
    dataframe.wil500 <- rbind(eventos.wil500, dataframe.wil500)
    
    # Juntarlos en un gran dataframe
    dataframe.wil.organizado <- rbind(dataframe.wil200,dataframe.wil300, dataframe.wil500)
    # Nombres de columnas
    colnames(dataframe.wil.organizado) <- c('est50','est100','est200')
    # Añadir columna de dias de estimacion
    dataframe.wil.organizado$`Estimacion` <- c(rep('',8),200,rep('',15),300,rep('',15),500,rep('',7))
    # Mutar las columnas <50>, <100> y <200> para agregar un '/', para poder colocar la significancia de BMP en la misma tabla
    dataframe.wil.organizado <- dataframe.wil.organizado %>% 
      mutate('50' = paste(est50,'/'), '100' = paste(est100,'/'),'200' = paste(est200,'/'))
    # Seleccionar solamente las columnas de interes
    dataframe.wil.organizado <- dataframe.wil.organizado %>% dplyr::select(Estimacion,`50`,`100`,`200`)
    
    lista.interes <- lista.bmp 
    # Se realiza el mismo procedimiento que para lista.wilcoxon
    no.columnas.dataframes <- unlist(lapply(lista.interes, ncol))
    for(m in seq_along(no.columnas.dataframes)) {
      no.columnas <- no.columnas.dataframes[m]
      # Encontramos los dataframes que les falta al menos una columna
      if(no.columnas != (length(columnas.union) + 1)) {
        nombres.columnas   <- colnames(lista.interes[[m]])
        # Encontramos cuales son las columnas que le faltan
        columnas.faltantes <- setdiff(columnas.union, nombres.columnas)
        lista.interes[[m]][columnas.faltantes] <- ''
      }
    }
    data.bmp         <- purrr::map_dfc(lista.interes, ~.x[,nombre.columna])
    data.bmp200      <- data.bmp[,grep('Estimacion_200',colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 200 dias
    data.bmp300      <- data.bmp[,grep('Estimacion_300',colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 300 dias
    data.bmp500      <- data.bmp[,grep('Estimacion_500',colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 500 dias
    
    # Retirar nombres de columnas para hacer rbind 
    colnames(data.bmp200) <- NA
    colnames(data.bmp300) <- NA
    colnames(data.bmp500) <- NA
    # Agregar el numero de eventos a cada dataframe . Los mismos eventos de wilcoxon sirven
    data.bmp200 <- rbind(eventos.wil200, data.bmp200)
    data.bmp300 <- rbind(eventos.wil300, data.bmp300)
    data.bmp500 <- rbind(eventos.wil500, data.bmp500)
    
    # Juntarlos en un gran dataframe
    data.bmp.organizado <- rbind(data.bmp200,data.bmp300, data.bmp500)
    # Nombres de filas
    colnames(data.bmp.organizado) <- c('est50','est100','est200')
    # Mutar para solamente tener los * de significancia
    data.bmp.organizado <- data.bmp.organizado %>% 
      mutate('50bmp' = gsub("[^*]","",est50),'100bmp'=gsub("[^*]","",est100),'200bmp' = gsub("[^*]","",est200))
    
    # Lo unico que falta es juntar los dos dataframe: <dataframe.wil.organizado> y <data.bmp.organizado> para tener un solo dataframe con la significancia de 
    # ambos tests
    dataframe.final <- cbind(dataframe.wil.organizado,data.bmp.organizado)
    dataframe.final <- dataframe.final %>% 
      mutate('50' = paste((rep(c('', paste0('[1,',1:15,']')),3)),`50`,`50bmp`),'100'=paste((rep(c('', paste0('[1,',1:15,']')),3)),`100`,`100bmp`),
             '200'=paste((rep(c('', paste0('[1,',1:15,']')),3)),`200`,`200bmp`)) %>% 
      dplyr::select(Estimacion,`50`,`100`,`200`)
    
    # Exportar a latex
    kable.final <- kable(dataframe.final,format='latex', booktabs=T, caption = paste0('Significancia para eventos ', nombre.columna,'. Nota: para ', serie, 
                                                                                          '. Estudio sobre la ', tipo.estudio, ' utilizando ',regresor.mercado, 
                                                                                          ' como retorno de mercado.'))
    writeLines(kable.final, paste0(getwd(),'/Resultados_regresion/Tablas_Latex/',nombre.columna,'_', serie, '_',tipo.estudio, '_',regresor.mercado,'.tex')) 
  }
}


# Tablas para la varianza -------------------------------------------------

if(tipo.estudio == 'varianza'){
  # Con <setdiff(unique(unlist(lapply(lista.bootstrap, colnames))), c('Ventana'))> encontramos todos los nombres de columnas de las tablas, eliminando 'Ventana'
  # Lo anterior debido a que se va a crear una tabla para cada tipo de desastre, o para cada pais, o para cada pais-tipo de desastre
  # Por otro lado, se necesita que hayan datos para cada elemento en <lista.bootstrap>, por lo que es necesario hallar la union de los nombres 
  # de columnas de todos los dataframes en <lista.bootstrap>
  columnas.union <- setdiff(unique(unlist(lapply(lista.bootstrap, colnames))), c('Ventana'))
  for (nombre.columna in columnas.union){
    lista.interes <- lista.bootstrap
    # <lista.interes> es una lista de dataframes, donde las columnas son:
    # <Ventana>, que se refiere a la ventana de eventi
    # <Todos> que se refiere a la totalidad de los desastres
    # El resto de columnas es la division de los desastres, ya sea por tipo de desastre, pais, o por una combinacion
    # entre pais y tipo de desastre.
    # Es posible que no existan ciertas columnas para la combinacion entre pais y tipo de desastres
    # Por ejmplo: el primer elemento de <lista.interes> hace referencia a la estimacion a 500 dias de estimacion y 50 de traslape,
    # y puede que no hayan eventos geofisicos en Brazil con estas ventanas. En este caso, para un correcto funcionamiento del codigo
    # se tienen que completar los dataframes, porque si no, mas adelante <purrr::map_dfc> no va a funcionar
    # ¿Como se van a completar los dataframes? Veamos que cada uno de ellos debe tener length(columnas.union) + 1 columnas,
    # donde +1 hace referencia a la columna 'Ventana', por lo que para aquellos que tengan menos columnas, se va a buscar cual es 
    # la(s) columna(s) faltantes y escribir '' (espacios en blanco) en ellas
    no.columnas.dataframes <- unlist(lapply(lista.interes, ncol))
    for(p in seq_along(no.columnas.dataframes)) {
      no.columnas <- no.columnas.dataframes[p]
      # Encontramos los dataframes que les falta al menos una columna
      if(no.columnas != (length(columnas.union) + 1)) {
        nombres.columnas   <- colnames(lista.interes[[p]])
        # Encontramos cuales son las columnas que le faltan
        columnas.faltantes <- setdiff(columnas.union, nombres.columnas)
        lista.interes[[p]][columnas.faltantes] <- ''
      }
    }
    dataframe.var     <- purrr::map_dfc(lista.interes, ~.x[,nombre.columna])
    dataframe.var500  <- dataframe.var[,grep('Estimacion_500',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 200 dias
    dataframe.var750  <- dataframe.var[,grep('Estimacion_750',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 300 dias
    dataframe.var1000 <- dataframe.var[,grep('Estimacion_1000',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 500 dias
    # Retirar nombres de columnas para hacer rbind 
    colnames(dataframe.var500) <- NA
    colnames(dataframe.var750) <- NA
    colnames(dataframe.var1000) <- NA
    # Juntarlos en un gran dataframe
    dataframe.var.organizado <- rbind(dataframe.var500,dataframe.var750, dataframe.var1000)
    # Nombres de columnas
    colnames(dataframe.var.organizado) <- c('50','100','200')
    # Añadir columna de dias de estimacion
    dataframe.var.organizado$`Estimacion` <- c(rep('',7),500,rep('',14),750,rep('',14),1000,rep('',7)) # se elige asi ya que <dataframe.var200> y <dataframe.var300> son NA
    # Reordenar las columnas
    dataframe.var.final <- dataframe.var.organizado %>% 
      mutate('50'= paste(rep(paste0('[0,',0:14,']'),3),`50`),
             '100'= paste(rep(paste0('[0,',0:14,']'),3),`100`),
             '200'= paste(rep(paste0('[0,',0:14,']'),3),`200`)) %>% 
      dplyr::select(Estimacion,`50`,`100`,`200`)
    
    # Exportar a latex
    kable.final <- kable(dataframe.var.final,format='latex', booktabs=T, caption = paste0('Significancia para eventos ', nombre.columna,'. Nota: para ', serie, 
                                                                           '. Estudio sobre la ', tipo.estudio, ' utilizando ',regresor.mercado, 
                                                                           ' como retorno de mercado.'))
    writeLines(kable.final, paste0(getwd(),'/Resultados_regresion/Tablas_Latex/',nombre.columna,'_', serie, '_',tipo.estudio, '_',regresor.mercado,'.tex'))
  }
}
