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


# Creacion objeto tabla.media ---------------------------------------------
# Se crea un tipo de objeto S4 para guardar la tabla de la media, y aparte el numero de eventos para cada pais
setClass('Tabla.media', slots = list(dataframe = 'data.frame', no.eventos = 'numeric'))
# Crear clase de objetos
setClass("ESmean",slots=list(retornos = "xts",error_estandar = "numeric",res_estandar_estimacion="xts",
                             res_no_estandar_estimacion="xts",variance_forecast="xts",
                             evento='data.frame',fit='list'))

# Prueba de filtro  -------------------------------------------------------
directorio.saved        <- paste0(getwd(),'/Resultados_regresion/')
directorio.guardar      <- paste0(directorio.saved,'Tablas/')
tipo.serie              <- 'Indices'   #<<<--- Puede ser 'CDS' o 'Indices'  
tipo.estudio            <- 'media' #<<<--- Puede ser de 'media' o 'varianza'
regresor.mercado        <- 'PM'    #<<<--- Retornos de mercado 'PM' es promedio movil y 'benchmark' es el retorno MSCI Emerging Markets
tipos.desastre.eliminar <- c('Biological','Climatological') #<<<--- NULL si no se desea eliminar ningun tipo de desastre 
paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
# de todos los paises de los que se tiene informacion
eventos.fecha.exac      <- T  #<<<--- booleano para indicar si se quieren usar solamente los eventos que tengan una fecha exacta
# <T> en caso de querer solo los que tienen fecha exacta.<F>si se quieren usar tambien aquellos eventos de
# los que se asumio el dia
columna.agrupar          <- 'Ambas'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
# 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
# 'Ambas' implica que se va a analizar por ambas columbas, por ejemplo: brazil - hidrologico, brazil - geofisico, ...
max_abnormal_returns     <- 15   #<<<--- No. dias maximos despues del evento para calcular retorno anormal
length_car_window        <- 15   #<<<--- Ventana para calcular el CAR (por ejemplo 5 significa [0,+5], donde 0 es el dia del evento)
length_event_window      <- length_car_window + 1 # Longitud ventana de evento es 1 mas <length_car_window>

ventanas.estimacion      <- c('200','300','500')   #<<<--- Puede ser 200, 300 o 500   (Importante que sea string)
ventanas.traslape        <- c('50','100','200')   #<<<--- Puede ser 50, 100 o 200   (Importante que sea string)

for(ventana.estimacion in ventanas.estimacion){
  for(ventana.traslape in ventanas.traslape){
    length_estimation_window <- as.numeric(ventana.estimacion)
    
    # Cargar los resultados de la regresion -----------------------------------
    load(paste0(directorio.saved,tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.estudio,'_',regresor.mercado,'.RData'))
    # Despues de load tenemos una lista de resultados de regresion <all_events_list>. 
    # Hay que tener en cuenta que es probable que hayan objetos NA, para aquellos casos donde el GARCH no convergio
    # Primero es necesario eliminar esos datos NA
    suppressWarnings(all.events.list <- purrr::discard(all_events_list,is.na))
    
    # Filtracion de los resultados -------------------------------------------
    # Tipo de desastre
    round(table(unlist(purrr::map(all.events.list, ~.x@evento$Disaster.Subgroup)))/length(all.events.list),3)
    # Conservar solamente los eventos geofisicos, meteorologicos e hidrologicos 
    if(!is.null(tipos.desastre.eliminar)) all.events.list <- all.events.list[!(purrr::map(all.events.list, ~.x@evento$Disaster.Subgroup) %in% tipos.desastre.eliminar)]
    # Dejar solamente los eventos que tengan fecha exacta, para el caso en que eventos.fecha.exac == T
    if(eventos.fecha.exac) all.events.list <- purrr::keep(all.events.list, ~ .x@evento$na_start == 0)
    # Dejar solo los eventos que hayan sucedido en algun pais de <paises.resultados>
    all.events.list <- all.events.list[purrr::map(all.events.list,~.x@evento$Country) %in% paises.resultados]
    # Ver cuantos desastres hay por cada valor de <columna.agrupar>. Por ejemplo: 1 biological, 3 climatological
    # 37 geophysical, 110 hydrological, 31 meteorological. De este modo se observa que hay muy pocos eventos biologicos
    # y climatologicos para poder realizar cualquier analisis
    table(unlist(purrr::map(all.events.list,~.x@evento$Disaster.Subgroup)))
    
    # Separar la lista <all.events.list> segun <columna.agrupar>
    if(columna.agrupar != 'Ambas') lista.separada <- split(all.events.list, sapply(all.events.list, function(x) x@evento[[columna.agrupar]]))
    
    # Generar listas distintas para cada valor de la <columna.agrupar>, en caso de querer utilizarlas mas adelante
    # for(i in seq_along(lista.separada)) assign(paste0("list.", names(lista.separada)[i]), lista.separada[[i]])
    # Cuando <columna.agrupar> == <'Ambas'> toca tener un trato especial
    if(columna.agrupar == 'Ambas'){
      # En primer lugar por cada desastre se necesita una combinacion del pais y el tipo de desastre
      # Para eso creamos una funcion, solo para mejor lectura
      crear.columna <- function(df) {
        df <- df %>%
          mutate(Desastre.Pais = paste0(Country, Disaster.Subgroup))
        return(df)
      }
      all.events.list <- purrr::map(all.events.list, function(s4_object) {
        s4_object@evento <- crear.columna(s4_object@evento)
        return(s4_object)
      })
      # Ahora si podemos separar <volatility_results> en listas dependiendo del desastre y el pais donde sucedio
      lista.separada <- split(all.events.list, sapply(all.events.list, function(x) x@evento[['Desastre.Pais']]))
      # Por otro lado, como la especificacion de Bialkowski (2008) ecuacion 5 esta hecha para mas de un evento, solamente se van
      # a guardar los elementos de <v.lista.separada> que contengan mas de un desastre
      lista.separada <- purrr::keep(lista.separada, ~(length(.x) > 1))
    }
    
    # Se observo que generalmente, el dia del evento el retorno es positivo, pero en adelante es negativo. Falta explicar porque podria
    # ser que el retorno en el dia del evento sea positivo,mientras que para el resto de la ventana de evento es negativo.
    # Por lo anterior, el CAR va a ser menor si se elimina el dia del evento de la ventana de evento y solo se miran los dias posteriores
    inicio.ventana.evento <- 1 #<<<--- indica en que dia comenzara la ventana de evento <0> si se desea que inicie el dia del desastre, 
    # 1 si se desea el dia siguiente, 2 si se desea 2 dias despues ...
    
    # Graficas CAR ------------------------------------------------------------
    for(i in seq_along(lista.separada)){
      element <- lista.separada[[i]]
      name    <- names(lista.separada)[i]  
      grafico_car(element,length_estimation_window,length_event_window,inicio.ventana.evento)
      title(name,line=0.75)
    }
    grafico_car(all.events.list,length_estimation_window,length_event_window,inicio.ventana.evento)
    
    # Wilcoxon --------------------------------------------------------------
    # Usamos la funcion <wilcoxon.jp.test> para realizar la prueba de Wilcoxon asociada a los <CAR> 
    # Dataframe con muchas ventanas
    matrix.wilcoxon <- matrix(nrow=(max_abnormal_returns- inicio.ventana.evento+1),ncol=(length(lista.separada)+1))
    for(i in seq_along(lista.separada)){
      abnormal <- cumsum(rowMeans(purrr::map_dfc(lista.separada[[i]], 
                                                 ~ (.x@retornos$Abnormal)[(length_estimation_window+1+inicio.ventana.evento):(max_abnormal_returns+1+length_estimation_window)])))
      for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))) 
        matrix.wilcoxon[j,i] <- paste(round(abnormal[j],2),
                                      wilcoxon.jp.test(lista.separada[[i]],length_estimation_window,j,inicio.ventana.evento)$Significancia)
    }
    
    k <- length(lista.separada)+1
    for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))){ 
      abnormal <- cumsum(rowMeans(purrr::map_dfc(all.events.list, 
                                                 ~ (.x@retornos$Abnormal)[(length_estimation_window+1+inicio.ventana.evento):(max_abnormal_returns+1+length_estimation_window)])))
      matrix.wilcoxon[j,k] <- paste(round(abnormal[j],2),
                                    wilcoxon.jp.test(all.events.list,length_estimation_window,j,inicio.ventana.evento)$Significancia)
    }
    
    colnames(matrix.wilcoxon) <- c(names(lista.separada),'Todos')
    Ventana                   <- 1:(max_abnormal_returns+1- inicio.ventana.evento)
    matrix.wilcoxon           <- cbind(Ventana,matrix.wilcoxon)
    dataframe.wilcoxon <- data.frame(matrix.wilcoxon)
    
    # BMP Savickas para media con GARCH ---------------------------------------
    # Dataframe con muchas ventanas
    matrix.bmp <- matrix(nrow=(max_abnormal_returns-inicio.ventana.evento+1),ncol=(length(lista.separada)+1))
    for(i in seq_along(lista.separada)){
      for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))) 
        matrix.bmp[j,i] <- paste(round(mean(colSums(data.frame(purrr::map(lista.separada[[i]],~coredata(.x@retornos$Abnormal[(length_estimation_window+1+inicio.ventana.evento):(length_estimation_window+j+inicio.ventana.evento)]))))),2),
                                 bmp_savickas(lista.separada[[i]],length_estimation_window,j,inicio.ventana.evento)$Significancia)
    }
    
    k <- length(lista.separada)+1
    for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))) 
      matrix.bmp[j,k] <- paste(round(mean(colSums(data.frame(purrr::map(all.events.list,~coredata(.x@retornos$Abnormal[(length_estimation_window+1+inicio.ventana.evento):(length_estimation_window+j+inicio.ventana.evento)]))))),2),
                               bmp_savickas(all.events.list,length_estimation_window,j,inicio.ventana.evento)$Significancia)
    colnames(matrix.bmp) <- c(names(lista.separada),'Todos')
    Ventana              <- 1:(max_abnormal_returns+1- inicio.ventana.evento)
    matrix.bmp           <- cbind(Ventana,matrix.bmp)
    dataframe.bmp <- data.frame(matrix.bmp)
    
    # Adicion de numero de eventos --------------------------------------------
    # Tanto <dataframe.wilcoxon> como <dataframe.bmp> cuentan con los mismos paises y el mismo numero de desastres por cada pais
    # Se genera un vector con el numero de desastres por cada pais y el total de desastres
    numero.total.desastres        <- length(all.events.list)
    names(numero.total.desastres) <- 'Todos'
    numero.desastres              <- c(unlist(lapply(lista.separada,length)), numero.total.desastres)
    # Se crean objetos de tipo <Tabla.media>, donde se guarda un dataframe y el numero de desastres
    tabla.bmp      <- new('Tabla.media', dataframe = dataframe.bmp, no.eventos = numero.desastres)
    tabla.wilcoxon <- new('Tabla.media', dataframe = dataframe.wilcoxon, no.eventos = numero.desastres)
    
    # Guardar las tablas de significancia. No es necesario agregar el tipo de test ya que podemos guardar ambas tablas
    if(columna.agrupar=='Disaster.Subgroup') agrupacion <- 'tipodesastre'
    if(columna.agrupar=='Country') agrupacion <- 'pais'
    if(columna.agrupar == 'Ambas') agrupacion <- 'paistipodesastre'
    save(tabla.bmp,
         tabla.wilcoxon,file=paste0(directorio.guardar,'Tablas_',tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.estudio,'_',regresor.mercado,'_',agrupacion,'.RData'))
    
  }
}







