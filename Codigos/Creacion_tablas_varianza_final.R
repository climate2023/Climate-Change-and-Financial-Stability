##########################################################
# Creacion de tablas de varianza
# Autores: Juan Pablo Bermudez.
##########################################################

rm(list = ls())
# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))
  
countries <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                   'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice

# Prueba de filtro  -------------------------------------------------------
tipo.serie              <- 'Indices'   #<<<--- Puede ser 'CDS' o 'Indices'  
regresor.mercado        <- 'PM'    #<<<--- Retornos de mercado 'PM' es promedio movil y 'MSCI' es el retorno MSCI Emerging Markets
paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
# de todos los paises de los que se tiene informacion
columna.agrupar         <- 'Country'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
# 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
# 'Ambas' implica que se va a analizar por ambas columbas, por ejemplo: brazil - hidrologico, brazil - geofisico, ...
vol_ev_window           <- 15  #<<<--- Tamano de la ventana de evento

ventanas.estimacion <- c(500, 750, 1000)
ventanas.traslape   <- c('50', '100', '150')   #<<<--- Puede ser 50, 100 o 150   (Importante que sea string)
for(ventana.estimacion in ventanas.estimacion){
  for(ventana.traslape in ventanas.traslape){
    # Volatility event study --------------------------------------------------
    # load de los objetos de la varianza
    load(paste0(directorio.saved,tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_varianza_',regresor.mercado,'.RData'))
    # Eliminar objetos NA
    volatility_results <- suppressWarnings(purrr::discard(volatility_results,is.na))
    
    # Filtracion de los resultados -------------------------------------------
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
    
    eventos.separados        <- c(unlist(lapply(v.lista.separada, length)),sum(unlist(lapply(v.lista.separada, length)))) 
    names(eventos.separados) <- c(names(unlist(lapply(v.lista.separada, length))),'Todos')
    
    # Tabla CAV/significancia ------------------------------------------------
    
    # Dataframe con muchas ventanas
    matrix.volatilidad <- matrix(nrow=(vol_ev_window),ncol=(length(v.lista.separada)+1))
    # Creamos una matriz con las mismas dimensiones de <matrix.volatilidad>, pero que en cada celda va a contener toda la informacion, ya
    # que <matrix.volatilidad> solo tendra el CAV y su significancia, mientras que <matrix.volatilidad.completa> tendra adicionalmente el pvalue
    # y el error estandar
    matrix.volatilidad.completa <- matrix.volatilidad
    iteraciones.bool  <- 10000
    for(i in seq_along(v.lista.separada)){
      for(j in (1:(vol_ev_window))){
        prueba <- bootstrap.volatility2(volatility.list = v.lista.separada[[i]],es.window.length = as.numeric(ventana.estimacion)
                                        ,ev.window.length = j,bootstrap_vol_iterations = iteraciones.bool)
        matrix.volatilidad[j,i] <- paste(prueba$CAV,prueba$Significancia)
        matrix.volatilidad.completa[j,i] <- paste(prueba[1,], collapse = '/ ')
      }
    }
    
    k <- length(v.lista.separada)+1
    for(j in (1:(vol_ev_window))){ 
      prueba.cav <- bootstrap.volatility2(volatility_results,as.numeric(ventana.estimacion),j,bootstrap_vol_iterations = iteraciones.bool)
      matrix.volatilidad[j,k]          <- paste(prueba.cav$CAV,prueba.cav$Significancia)
      matrix.volatilidad.completa[j,k] <- paste(prueba.cav[1,], collapse = '/ ')
    }
    colnames(matrix.volatilidad) <- c(names(v.lista.separada),'Todos')
    colnames(matrix.volatilidad.completa) <- c(names(v.lista.separada), 'Todos')
    Ventana                      <- 1:(vol_ev_window)
    matrix.volatilidad           <- cbind(Ventana,matrix.volatilidad)
    matrix.volatilidad.completa  <- cbind(Ventana, matrix.volatilidad.completa)
    
    # Creacion objeto para guardar
    tabla.volatilidad <- new('Tabla.varianza', dataframe = data.frame(matrix.volatilidad),no.eventos = eventos.separados)
    tabla.volatilidad.completa <- new('Tabla.varianza', dataframe = data.frame(matrix.volatilidad.completa), no.eventos = eventos.separados)    
    
    # Guardar las tablas de significancia. No es necesario agregar el tipo de test ya que podemos guardar ambas tablas
    if(columna.agrupar=='Disaster.Subgroup') agrupacion <- 'tipodesastre'
    if(columna.agrupar=='Country') agrupacion <- 'pais'
    if(columna.agrupar=='Ambas')   agrupacion <- 'paistipodesastre'
    save(tabla.volatilidad, tabla.volatilidad.completa,
         file=paste0(directorio.guardar,'Tablas_',tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_varianza_',regresor.mercado,'_',agrupacion,'.RData'))
  }
}