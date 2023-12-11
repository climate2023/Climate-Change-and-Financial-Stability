# El siguiente codigo busca encontrar cuales combinaciones de  pais-tipodesastre no tienen desastres en el analisis de la media, 
# y cuales tienen solamente uno. Se hara principalmente para las ventanas de estimacion y traslape de interes, es decir 250 E - 50 T y 500 E - 50 T

rm(list = ls())
# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))
  
countries   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                   'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice

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
if(tipo.serie == 'CDS')     cola <- 1
if(tipo.serie == 'Indices') cola <- -1
tipo.estudio            <- 'media' #<<<--- Puede ser de 'media' o 'varianza'
regresor.mercado        <- 'benchmark'    #<<<--- Retornos de mercado 'PM' es promedio movil y 'benchmark' es el retorno MSCI Emerging Markets
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

ventanas.estimacion      <- c('250','500')   #<<<--- Puede ser 250, 375 o 500   (Importante que sea string)
ventanas.traslape        <- c('50')   #<<<--- Puede ser 50, 100 o 150   (Importante que sea string)
table.caar              <- 0 #<<<--- booleano para indicar si las tablas se construiran mostrando el CAAR o el estadistico. 0 para estadistico, 1  para CAAR

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
    tipos.desastres <- unique(unlist(purrr::map(all.events.list,~.x@evento$Disaster.Subgroup)))
    
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
      
      # Buscamos la combinacion de paises-tiposdesastres que generan solamente un desastre
      lista.un.evento <- purrr::keep(lista.separada, ~(length(.x) == 1))
      # Por otro lado, como la especificacion de Bialkowski (2008) ecuacion 5 esta hecha para mas de un evento, solamente se van
      # a guardar los elementos de <v.lista.separada> que contengan mas de un desastre
      lista.separada  <- purrr::keep(lista.separada, ~(length(.x) > 1))
      
      # Ya tenemos las combinaciones de pais y tipo desastre con un evento, falta buscar aquellas que no tienen ningun evento en absoluto
      posibles.combinaciones <- apply(expand.grid(countries, tipos.desastres),1, function(x) paste0(x[1],x[2]))
      # Concatenamos los valores de los que si tenemos datos
      nombres.listas <- c(names(lista.un.evento),names(lista.separada))
      # Combinaciones sin evento
      nombres.sin.eventos <- setdiff(posibles.combinaciones, nombres.listas)
    }
    print(paste0('Estimacion ', ventana.estimacion,'. Traslape ', ventana.traslape,' no hay desastres para: ',paste0(sort(nombres.sin.eventos), collapse=' - ')))
    print(paste0('Estimacion ', ventana.estimacion,'. Traslape ', ventana.traslape,' hay solamente un desastre para: ',paste0(sort(names(lista.un.evento)),collapse= ' - ')))
  }
}