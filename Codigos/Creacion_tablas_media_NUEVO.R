##########################################################
# Codigo para generar las tablas de los tests de la media
# Arreglado para tener en cuenta BEI
# Autores: Juan Pablo Bermudez. 
##########################################################

rm(list =ls())
# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))

countries   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                 'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice

# Prueba de filtro  -------------------------------------------------------

tipo.serie              <- 'BEI'   #<<<--- Puede ser 'CDS' o 'Indices' , o 'BEI' 
regresor.mercado        <- 'PM'    #<<<--- Retornos de mercado 'PM' es promedio movil y 'benchmark' es el retorno MSCI Emerging Markets
# Actualizar informacion si <tipo.serie> == BEI 
if(tipo.serie == 'BEI'){
  regresor.mercado <- NULL
  countries        <- c('Colombia')
  tipo.bei         <- 'originales'  #<<<--- parametro que indica cuales BEI se analizan, los originales o sin prima
  plazo.bei        <- 1             #<<<--- que plazo de BEI se analiza, 1 2 3 4 o 5
}
tipos.desastre.eliminar <- c('Biological','Climatological') #<<<--- NULL si no se desea eliminar ningun tipo de desastre 
paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
# de todos los paises de los que se tiene informacion
eventos.fecha.exac      <- T  #<<<--- booleano para indicar si se quieren usar solamente los eventos que tengan una fecha exacta
# <T> en caso de querer solo los que tienen fecha exacta.<F>si se quieren usar tambien aquellos eventos de
# los que se asumio el dia
columna.agrupar          <- 'Disaster.Subgroup'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
# 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
# 'Ambas' implica que se va a analizar por ambas columbas, por ejemplo: brazil - hidrologico, brazil - geofisico, ...
max_abnormal_returns     <- 15   #<<<--- No. dias maximos despues del evento para calcular retorno anormal
length_car_window        <- 15   #<<<--- Ventana para calcular el CAR (por ejemplo 5 significa [0,+5], donde 0 es el dia del evento)
length_event_window      <- length_car_window + 1 # Longitud ventana de evento es 1 mas <length_car_window>

ventanas.estimacion      <- c('250','375','500')   #<<<--- Puede ser 250, 375 o 500   (Importante que sea string)
ventanas.traslape        <- c('50','100','150')   #<<<--- Puede ser 50, 100 o 150   (Importante que sea string)
table.caar              <- 0 #<<<--- booleano para indicar si las tablas se construiran mostrando el CAAR o el estadistico. 0 para estadistico, 1  para CAAR

# <ifelse> porque con indices queremos ver la cola izquierda, mientras que con CDS y BEI queremos ver la cola derecha
cola <- ifelse(tipo.serie == 'Indices', -1, 1)
# Establecemos un flag para cuando el tipo de serie sea BEI
flag.bei <- ifelse(tipo.serie == 'BEI',1,0)

for(ventana.estimacion in ventanas.estimacion){
  for(ventana.traslape in ventanas.traslape){
    length_estimation_window <- as.numeric(ventana.estimacion)
    
    # Cargar los resultados de la regresion -----------------------------------
    if(!flag.bei){load(paste0(directorio.saved,tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_media_',regresor.mercado,'.RData'))
    } else {load(paste0(directorio.saved,tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.bei,'_',plazo.bei,'_year.RData'))}
    
    # Despues de load tenemos una lista de resultados de regresion <all_events_list>. 
    # Sin embargo, con BEI tenemos tres listas: all_events_list,all_events_list_desanclaje y all_events_list_expectativas
    # Entonces, es necesario crear una lista de estas tres, para poder iterar a traves de ellas para el resto del codigo
    # Pero toca recalcar que tambien toca convertir a lista al objeto que sale en caso que <tipo.series> no sea BEI
    if(!flag.bei){lista.resultados.regresion <- list(all_events_list)
    } else {
      lista.resultados.regresion <- list(all_events_list, all_events_list_desanclaje, all_events_list_expectativas)
      names(lista.resultados.regresion) <- c('Sin_regresoras','Desanclaje','Desanclaje_PIB')}
    
    # Ahora si realizamos la iteracion
    for(ind in seq_along(lista.resultados.regresion)) {
      lista.iteracion <- lista.resultados.regresion[[ind]]
      # Hay que tener en cuenta que es probable que hayan objetos NA, para aquellos casos donde el GARCH no convergio
      # Primero es necesario eliminar esos datos NA
      suppressWarnings(all.events.list <- purrr::discard(lista.iteracion,is.na))
      
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
      
      # Se observo que generalmente, el dia del evento el retorno es positivo, pero en adelante es negativo. Falta explicar porque podria
      # ser que el retorno en el dia del evento sea positivo,mientras que para el resto de la ventana de evento es negativo.
      # Por lo anterior, el CAR va a ser menor si se elimina el dia del evento de la ventana de evento y solo se miran los dias posteriores
      inicio.ventana.evento <- 1 #<<<--- indica en que dia comenzara la ventana de evento <0> si se desea que inicie el dia del desastre, 
      # 1 si se desea el dia siguiente, 2 si se desea 2 dias despues ...
      
      # Graficas CAR ------------------------------------------------------------
      # for(i in seq_along(lista.separada)){
      #   element <- lista.separada[[i]]
      #   name    <- names(lista.separada)[i]  
      #   grafico_car(element,length_estimation_window,length_event_window,inicio.ventana.evento)
      #   title(name,line=0.75)
      # }
      # grafico_car(all.events.list,length_estimation_window,length_event_window,inicio.ventana.evento)
      
      # Wilcoxon --------------------------------------------------------------
      # Usamos la funcion <wilcoxon.jp.test> para realizar la prueba de Wilcoxon asociada a los <CAR> 
      # Dataframe con muchas ventanas
      matrix.wilcoxon <- matrix(nrow=(max_abnormal_returns- inicio.ventana.evento+1),ncol=(length(lista.separada)+1))
      # Tambien vamos a crear una matriz mas completa, agregando el pvalue y el error estandar
      matrix.wilcoxon.completa <- matrix.wilcoxon
      for(i in seq_along(lista.separada)){
        abnormal <- cumsum(rowMeans(purrr::map_dfc(lista.separada[[i]], 
                                                   ~ (.x@retornos$Abnormal)[(length_estimation_window+1+inicio.ventana.evento):(max_abnormal_returns+1+length_estimation_window)])))
        for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))){
          # El siguiente codigo genera las tablas de Wilcoxon, presentando el CAAR y el pvalue asociado. if(0) para presentar 
          if(table.caar){
            prueba <- wilcoxon.jp.test(data.list = lista.separada[[i]],es.window.length = length_estimation_window,
                                       ev.window.length = j,ev.window.begin = inicio.ventana.evento,tail = cola)
            matrix.wilcoxon[j,i]          <- paste(round(abnormal[j],2),prueba$Significancia)
            matrix.wilcoxon.completa[j,i] <- paste0(prueba[1,], collapse = '/ ') # Cada celda tendra el estadistico, significancia, pvalue y error estandar
          }
          # El siguiente codigo genera las tablas de Wilcoxon, presentando el estadistico de Wilcoxon y el pvalue asociado
          if(!table.caar){
            prueba <- wilcoxon.jp.test(data.list = lista.separada[[i]],es.window.length = length_estimation_window,
                                       ev.window.length = j,ev.window.begin = inicio.ventana.evento,tail = cola)
            matrix.wilcoxon[j,i] <- paste(prueba$Estadistico, prueba$Significancia) 
            matrix.wilcoxon.completa[j,i] <- paste0(prueba[1,], collapse = '/ ')
          }
        }
      }
      
      k <- length(lista.separada)+1
      for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))){ 
        abnormal <- cumsum(rowMeans(purrr::map_dfc(all.events.list, 
                                                   ~ (.x@retornos$Abnormal)[(length_estimation_window+1+inicio.ventana.evento):(max_abnormal_returns+1+length_estimation_window)])))
        # El siguiente codigo genera las tablas de Wilcoxon, presentando el CAAR y el pvalue asociado. if(0) para presentar 
        if(table.caar){
          prueba <- wilcoxon.jp.test(all.events.list,length_estimation_window,j,inicio.ventana.evento,tail = cola)
          matrix.wilcoxon[j,k]          <- paste(round(abnormal[j],2), prueba$Significancia)
          matrix.wilcoxon.completa[j,k] <- paste0(prueba[1,], collapse = '/ ')
        }
        # El siguiente codigo genera las tablas de Wilcoxon, presentando el estadistico de Wilcoxon y el pvalue asociado
        if(!table.caar){
          prueba <- wilcoxon.jp.test(all.events.list,length_estimation_window,j,inicio.ventana.evento,tail = cola)
          matrix.wilcoxon[j,k]          <- paste(prueba$Estadistico, prueba$Significancia)
          matrix.wilcoxon.completa[j,k] <- paste0(prueba[1,], collapse = '/ ')
        }
      }
      
      colnames(matrix.wilcoxon) <- colnames(matrix.wilcoxon.completa) <- c(names(lista.separada),'Todos')
      Ventana                   <- 1:(max_abnormal_returns+1- inicio.ventana.evento)
      matrix.wilcoxon           <- cbind(Ventana,matrix.wilcoxon)
      matrix.wilcoxon.completa  <- cbind(Ventana, matrix.wilcoxon.completa)
      dataframe.wilcoxon          <- data.frame(matrix.wilcoxon)
      dataframe.wilcoxon.completo <- data.frame(matrix.wilcoxon.completa)
      # dataframe.wilcoxon.completa guarda para cada ventana y tipo de desastre el estadistico wilcoxon,la significancia, el pvalue
      # y el error estandar
      
      # BMP Savickas para media con GARCH ---------------------------------------
      # Dataframe con muchas ventanas
      matrix.bmp <- matrix(nrow=(max_abnormal_returns-inicio.ventana.evento+1),ncol=(length(lista.separada)+1))
      # Tambien hacemos una matriz completa para el BMP
      matrix.bmp.completa <- matrix.bmp
      for(i in seq_along(lista.separada)){
        for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))){
          # Se realiza el mismo procedimiento que con Wilcoxon
          if(table.caar) {
            prueba          <- bmp_savickas(lista.separada[[i]],length_estimation_window,j,inicio.ventana.evento,tail = cola) 
            matrix.bmp[j,i] <- paste(round(mean(colSums(data.frame(purrr::map(lista.separada[[i]],~coredata(.x@retornos$Abnormal[(length_estimation_window+1+inicio.ventana.evento):(length_estimation_window+j+inicio.ventana.evento)]))))),2),
                                     prueba$Significancia)
            matrix.bmp.completa[j,i] <- paste0(prueba[1,],collapse = '/ ') # Guardar el estadistico, la significancia y el pvalue
          }
          if(!table.caar) {
            prueba.bmp <- bmp_savickas(data.list = lista.separada[[i]],es.window.length = length_estimation_window,
                                       ev.window.length = j,ev.window.begin = inicio.ventana.evento,tail = cola)
            matrix.bmp[j,i] <- paste(round(prueba.bmp$Estadistico,2), prueba.bmp$Significancia)
            matrix.bmp.completa[j,i] <- paste0(prueba.bmp[1,], collapse='/ ') # Guardar el estadistico, la significancia y el pvalue
          }
        } 
      }
      
      k <- length(lista.separada)+1
      for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))){
        if(table.caar) {
          prueba <- bmp_savickas(all.events.list,length_estimation_window,j,inicio.ventana.evento, tail=cola)
          matrix.bmp[j,k] <- paste(round(mean(colSums(data.frame(purrr::map(all.events.list,~coredata(.x@retornos$Abnormal[(length_estimation_window+1+inicio.ventana.evento):(length_estimation_window+j+inicio.ventana.evento)]))))),2),
                                   prueba$Significancia)
          matrix.bmp.completa[j,k] <- paste0(prueba[1,], collapse = '/ ')
        }
        if(!table.caar) {
          prueba.bmp <- bmp_savickas(all.events.list,length_estimation_window,j,inicio.ventana.evento, tail=cola)
          matrix.bmp[j,k] <- paste(round(prueba.bmp$Estadistico,2), prueba.bmp$Significancia)
          matrix.bmp.completa[j,k] <- paste0(prueba.bmp[1,], collapse = '/ ')
        }
      } 
      
      colnames(matrix.bmp) <- colnames(matrix.bmp.completa) <- c(names(lista.separada),'Todos')
      Ventana              <- 1:(max_abnormal_returns+1- inicio.ventana.evento)
      matrix.bmp           <- cbind(Ventana,matrix.bmp)
      matrix.bmp.completa  <- cbind(Ventana, matrix.bmp.completa) 
      dataframe.bmp          <- data.frame(matrix.bmp)
      dataframe.bmp.completo <- data.frame(matrix.bmp.completa)
      
      # Adicion de numero de eventos --------------------------------------------
      # Tanto <dataframe.wilcoxon> como <dataframe.bmp> cuentan con los mismos paises y el mismo numero de desastres por cada pais
      # Se genera un vector con el numero de desastres por cada pais y el total de desastres
      numero.total.desastres        <- length(all.events.list)
      names(numero.total.desastres) <- 'Todos'
      numero.desastres              <- c(unlist(lapply(lista.separada,length)), numero.total.desastres)
      # Se crean objetos de tipo <Tabla.media>, donde se guarda un dataframe y el numero de desastres
      tabla.bmp      <- new('Tabla.media', dataframe = dataframe.bmp, no.eventos = numero.desastres)
      tabla.wilcoxon <- new('Tabla.media', dataframe = dataframe.wilcoxon, no.eventos = numero.desastres)
      
      # Tambien se guardan los dataframes completos
      tabla.bmp.completo <- new('Tabla.media', dataframe = dataframe.bmp.completo, no.eventos = numero.desastres)
      tabla.wilcoxon.completo <- new('Tabla.media', dataframe = dataframe.wilcoxon.completo, no.eventos = numero.desastres)
      
      # Guardar las tablas de significancia. No es necesario agregar el tipo de test ya que podemos guardar ambas tablas
      if(columna.agrupar=='Disaster.Subgroup') agrupacion <- 'tipodesastre'
      if(columna.agrupar=='Country') agrupacion <- 'pais'
      if(columna.agrupar == 'Ambas') agrupacion <- 'paistipodesastre'
      if(!flag.bei) {save(tabla.bmp, tabla.wilcoxon,tabla.bmp.completo, tabla.wilcoxon.completo,
           file=paste0(directorio.guardar,'Tablas_',tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_media_',regresor.mercado,'_',agrupacion,'.RData'))
      } else{
        save(tabla.bmp, tabla.wilcoxon, tabla.bmp.completo, tabla.wilcoxon.completo,
             file = paste0(directorio.guardar,'BEI/Tablas_',tipo.serie,'_tra',ventana.traslape,'_est', ventana.estimacion, '_',tipo.bei,'_', plazo.bei,'_', 
                          str_to_lower(names(lista.resultados.regresion)[ind]), '_', agrupacion, '.RData'))
      }
    }
  }
}