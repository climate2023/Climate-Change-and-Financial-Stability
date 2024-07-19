##########################################################
# Event_Study_Sin_Dummies se rerfiere al analisis hecho por Tommaso et al (2023), aumentado con un 
# GARCH. Modelos de contagio solamente para 500 E - 50 T
# Autores: Juan Pablo Bermudez.
##########################################################

rm(list = ls())
# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))

# Lectura de datos --------------------------------------------------------
# Se genera un vector con el nombre de los paises de los cuales se tiene datos de indice bursatil
bool_paper <- T #<<<--- Parametro que indica si se carga la base de datos que utilizaremos o los retornos de Pagnottoni (2022). 
# <T> si se desea la base de datos para el paper. <F> si los retornos de Pagnottoni
bool_cds   <- T #<<<--- Parametro que indice si se hara el analisis sobre los CDS (<TRUE>) y <F> si se realizara sobre los stocks
promedio.movil <- T #<<<-- parametro (booleano) para que el usuario decida cual sera el retorno de mercado, <T> si es el promedio movil de 
# de los retornos de los indices, <F> si es otra variable

# Para el análisis de Pagnottoni (2022) se utilizaba una base con la muestra reducida a <dia.inicial>, lo 
# cual no se hace para Tommaso (2023), ya que necesitamos la mayor cantidad posible de datos antes de 
# <dia.inicial>. 
# En <base_Tommaso> estan las series que van a servir como variables dependientes, un indice de mercado,
# y las variables exogenas del modelo.
tipo.serie <- ifelse(bool_cds,'cds','indices')
market     <- ifelse(promedio.movil, 'PM','benchmark')
load(paste0(getwd(),'/Bases/Procesado/Base_Tommaso_',tipo.serie,'_rm_',market,'.RData'))

# Por construccion, <base_Tommaso> tiene dos columnas para Estados Unidos para gdp y fdi: <gdp_USA1>/<gdp_USA2> y <fdi_USA1>/<fdi_USA2>
# Dejar solamente una columna para gdp y una para fdi que se llamen <gdp_USA> y <fdi_USA>
base_Tommaso <- base_Tommaso[,!colnames(base_Tommaso) %in% c('gdp_USA2','fdi_USA2')]
colnames(base_Tommaso) <- gsub("USA1$", "USA", colnames(base_Tommaso))

# Para un correcto funcionamiento del codigo, toca colocar el nombre de indices y paises, que son los mismos del codigo
# <02a_Replicacion_Climate_Change.R>
if(!bool_paper){
  indexes   <- c("S.PASX200","BEL20","Bovespa","S.PTSXComposite","S.PCLXIPSA","OMXCopenhagen20","OMXHelsinki25","CAC40",
                 "DAX","HangSeng","Nifty50","JakartaStockExchange","S.PBMVIPC","AEX","OSEBenchmark","WIG20","MOEXRussia",
                 "SouthAfricaTop40","KOSPI","IBEX35","OMXStockholm30","SMI","SETIndex","BIST100","FTSE100","NASDAQComposite",
                 "Nasdaq100") #<<<--- Lista de los indices analizados
  
  countries <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
                 "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Norway","Poland","Russia",
                 "SouthAfrica","SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", 
                 "UnitedKingdom","USA1","USA2") #<<<--- Lista de los paises de cada indice, con el proposito de leer los excel con los datos
}else{
  countries   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                   'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice
  if(bool_cds){
    indexes  <- c('CDSBrazil','CDSChile','CDSChina','CDSColombia','CDSIndonesia','CDSKorea',
                  'CDSMalaysia','CDSMexico','CDSPeru','CDSSouthAfrica','CDSTurkey') #<<<--- Lista de los indices analizados.
  }else{
    indexes         <- c('BIST100','Bovespa','ChinaA50','JSX','KOSPI','S.PBMVIPC','S.PCLXIPSA','SouthAfricaTop40',
                         'IGBVL','KLCI','COLCAP') # Nombre indices para el paper. JSX es el de Jakarta
  }
}

# Base de eventos ---------------------------------------------------------
# Lectura de la base de eventos
load(paste0(getwd(),'/Bases/EMDAT_PAPER.RData'))

# Agregar rezagos a base de datos -----------------------------------------

# Se usa la funcion <create.lags>, que toma una base de datos junto a unas variables de interes. Retorna la base de datos inicial junto con 
# rezagos de las variables de interes. Si se desea un numero de rezagos en especifico para todas las variables de interes, asignar el numero 
# a <number_lags>. Si se desea un numero de rezagos para cada variable de interes, asignar una lista a <number_lags> con los numeros de 
# rezagos. Nota: Si se coloca la lista tiene que tener el mismo numero de datos que numero de variables de interes.
# Si se desea que se elijan los rezagos siguiendo el criterio de informacion de Akaike, dejar <number_lags> como NULL
number_lags <- NULL
base_lagged <- create.lags(base = base_Tommaso,interest.vars = indexes,no.lags = number_lags,AR.m = 20)
# Parametros event study --------------------------------------------------------------

estimation_start <- c(500) #<<<--- No. de dias antes del evento para comenzar la estimacion.

estimation_end           <- 1    #<<<--- No. dias antes del evento para finalizar la estimacion
max_abnormal_returns     <- 15   #<<<--- No. dias maximos despues del evento para calcular retorno anormal
days_to_be_evaluated     <- 5    #<<<--- No. dias despues del evento a ser evaluados
length_car_window        <- 15   #<<<--- Ventana para calcular el CAR (por ejemplo 5 significa [0,+5], donde 0 es el dia del evento)
length_estimation_window <- estimation_start - estimation_end + 1 # Tamaño de la ventana de estimacion
length_event_window      <- length_car_window + 1 # Longitud ventana de evento es 1 mas <length_car_window>
# <length_car_window> no puede ser mayor a <max_abnormal_returns>, ya que implica una ventana de evento mayor al numero de retornos
# anormales estimados.
# Asegurar que la ventana de evento no sea mayor que los retornos anormales estimados. 
if(length_car_window > max_abnormal_returns) length_car_window <- max_abnormal_returns

# Se eliminan los eventos que no cuentan con la ventana minima de estimacion ni con la ventana minima de evento usando la funcion <drop.events>
date_col_name <- "Start.Date" #<<<--- Parametro que indica el nombre de la columna clase <Date>, la cual contiene la fecha de eventos
geo_col_name  <- "Country"    #<<<--- Parametro que indica el nombre de la columna que contiene los paises, o puede ser cualquier otra variable 
#       que se quiera estudiar, como regiones, ciudades, etc
eventos_filtrado <- drop.events(data.events = emdat_base,base = base_lagged,estimation.start = estimation_start,max.ar=max_abnormal_returns, 
                                date_col_name, geo_col_name)

# Filtrar la base de datos para solamente dejar los eventos mas significativos, y tambien asegurar que dentro de la 
# ventana de estimacion no hayan otros eventos.
umbral.evento <- c(50)#<<<--- Numero de dias minimo entre cada evento. Lo anterior para que no se traslapen los eventos

columna.filtrar <- 'Total.Affected' #<<<--- Columna para filtrar la base de eventos 'Total.Affected' o 'Damages'
eventos.final <- reducir.eventos(umbral = umbral.evento,base=base_lagged,eventos = eventos_filtrado,
                                 col.fecha='Start.Date',col.grupo = 'Country',col.filtro = columna.filtrar) # Se siguen filtrando eventos mas importantes por pais

# -------------------------- Regresion estimation window ---------------------------------------------

# <estimation.event.study> realiza la estimacion por OLS para cada evento en <data.events>. Retorna una lista para cada evento que incluye:
#     Dataframe      : retornos observados, estimados y anormales para la ventana de estimacion y ventana de evento. 
#     Standard_error : error estandar de los errores de la estimacion por OLS
# El objeto de salida de esta funcion sera la base para las pruebas de Wilcoxon y bootstrap

# Otras variables exogenas de una base de datos que se quieren incluir. 
var_exo <- c("gdp_","fdi_")

load.eventslist <- 0    #<<<<-- 1 si se cargan los datos, 0 si se corre la funcion para estimar 
if(!load.eventslist){
  all_events_list <- estimation.event.study.contagio(bool.paper = bool_paper, bool.cds=bool_cds,base = base_lagged,data.events = eventos.final,market_returns = "market.returns",
                                            max.ar = 15,es.start = estimation_start,es.end = estimation_end,add.exo = TRUE,vars.exo = var_exo,GARCH = "sGARCH",
                                            overlap.events = eventos_filtrado, overlap.max = umbral.evento, all.indexes = indexes)
  if(bool_cds){serie <- 'CDS'}else{serie <- 'Indices'}
  if(promedio.movil){regresor.mercado <- 'PM'}else{regresor.mercado <- 'benchmark'}
  save(all_events_list, 
       file=paste0(getwd(),'/Resultados_regresion_contagio/',serie,'_tra',umbral.evento,'_est',estimation_start,'_media_',regresor.mercado,'.RData'))
}else{
  if(bool_cds){serie <- 'CDS'}else{serie <- 'Indices'}
  if(promedio.movil){regresor.mercado <- 'PM'}else{regresor.mercado <- 'benchmark'}
  load(paste0(getwd(),'/Resultados_regresion_contagio/',serie,'_tra',umbral.evento,'_est',estimation_start,'_media_',regresor.mercado,'.RData'))
} 

# Volatility event study --------------------------------------------------

# El siguiente programa sigue la metodologia del paper The impact of natural disasters on stock returns and volatilities
# of local firms (Bourdeau-Brien)
estimation_vol_start <- c(500)

vol_ev_window        <- 15  #<<<--- Tamaño de la ventana de evento

# Filtrar los eventos para que solo queden aquellos que cumplan con una ventana minima de estimacion y una ventana minima de 
# evento
eventos.filtrado.volatilidad <- drop.events(data.events = emdat_base,base = base_lagged,estimation.start = estimation_vol_start,max.ar=vol_ev_window, 
                                            date_col_name, geo_col_name)

umbral.evento.vol <- c(50) #<<<--- Numero de dias minimo entre cada evento. Lo anterior para que no se traslapen los eventos

# Filtrar la base de datos para solamente dejar los eventos mas significativos, y tambien asegurar que dentro de la 
# ventana de estimacion no hayan otros eventos.
columna.filtrar.vol <- 'Total.Affected' #<<<--- Columna para filtrar la base de eventos 'Total.Affected' o 'Damages'
eventos.volatilidad <- reducir.eventos(umbral.evento.vol,base_lagged,eventos.filtrado.volatilidad,
                                       col.fecha='Start.Date',col.grupo = 'Country',col.filtro = columna.filtrar.vol)

load.volatility <- 0           #<<<<-- 1 si se cargan los resultados de volatilidad, 0 si es necesario correr el codigo
if(!load.volatility){
  volatility_results <- volatility_event_study_contagio(base.evento = eventos.volatilidad,date.col.name = "Start.Date",geo.col.name = "Country",
                                               base.vol = base_Tommaso,interest.vars = indexes,num_lags = NULL,es.start=estimation_vol_start,
                                               len.ev.window = vol_ev_window,var.exo="market.returns",var.exo.pais = c("gdp","fdi"),
                                               bool.cds = bool_cds,bool.paper = bool_paper,garch = 'sGARCH',overlap.events = eventos.filtrado.volatilidad,
                                               overlap.max = umbral.evento.vol)
  if(bool_cds){serie <- 'CDS'}else{serie <- 'Indices'}
  if(promedio.movil){regresor.mercado <- 'PM'}else{regresor.mercado <- 'benchmark'}
  save(volatility_results, 
       file=paste0(getwd(),'/Resultados_regresion_contagio/',serie,'_tra',umbral.evento.vol,'_est',estimation_vol_start,'_varianza_',regresor.mercado,'.RData'))
}else load(paste0(getwd(),'/Resultados_regresion_contagio/',serie,'_tra',umbral.evento.vol,'_est',estimation_vol_start,'_varianza_',regresor.mercado,'.RData'))
