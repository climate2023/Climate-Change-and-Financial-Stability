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
tipo.serie              <- 'CDS'   #<<<--- Puede ser 'CDS' o 'Indices'  
regresor.mercado        <- 'PM'    #<<<--- Retornos de mercado 'PM' es promedio movil y 'benchmark' es el retorno MSCI Emerging Markets
paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
# de todos los paises de los que se tiene informacion
columna.agrupar         <- 'Continent'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
# 'Country' la separa por pais donde sucedio el desastre vs pais de la serie y 'Continent' por continente donde sucedio el desastre vs continente de la serie
vol_ev_window           <- 15  #<<<--- Tamano de la ventana de evento

ventana.estimacion <- c(500)
ventana.traslape   <- c('50')   #<<<--- Puede ser 50, 100 o 150   (Importante que sea string)

# Volatility event study --------------------------------------------------
# load de los objetos de la varianza
load(paste0(substr(directorio.saved,start=1,stop = nchar(directorio.saved)-1),'_contagio/',tipo.serie,
            '_tra',ventana.traslape,'_est',ventana.estimacion,'_varianza_',regresor.mercado,'.RData'))
# Eliminar objetos NA
volatility_results <- suppressWarnings(purrr::discard(volatility_results,is.na))

# Filtracion de los resultados -------------------------------------------
# Dejar solo los eventos que hayan sucedido en algun pais de <paises.resultados>
volatility_results <- volatility_results[purrr::map(volatility_results,~.x@info.evento$Country) %in% paises.resultados]
# Ver cuantos desastres hay por cada valor de <columna.agrupar>. Por ejemplo: 1 biological, 3 climatological
# 37 geophysical, 110 hydrological, 31 meteorological. De este modo se observa que hay muy pocos eventos biologicos
# y climatologicos para poder realizar cualquier analisis
table(unlist(purrr::map(volatility_results,~.x@info.evento$Disaster.Subgroup)))

# Para cada elemento en <volatility_results> lo mas optimo es obtener primero el pais donde sucedio el desastre
# junto al pais de la serie
nombres_separados <- strsplit(names(volatility_results),'_')
if(tipo.serie == 'Indices'){
  paises_series   <- lapply(nombres_separados, function(x) reverse.matching(x[1],F,T))
}else{
  paises_series   <- lapply(nombres_separados, function(x) as.character(na.omit(str_match(x[1], countries))))
}
paises_desastres  <- lapply(nombres_separados, function(x) as.character(na.omit(str_match(x[2], countries))))

# Separar la lista dependiendo de una columna en especifico introducida por el usuario 
# Generar listas distintas para cada valor de la <columna.agrupar>, en caso de querer utilizarlas mas adelante
if(columna.agrupar == 'Country'){
  paises_series     <- paste0('serie', paises_series)
  paises_desastres  <- paste0('desastre',paises_desastres)
  paises_joint      <- paste0(paises_series,'_', paises_desastres)
  # En primer lugar por cada desastre se necesita una combinacion del pais donde sucedio y el pais de la serie
  for(i in seq_along(paises_joint)) volatility_results[[i]]@info.evento$agrupar <- paises_joint[i]
  # Ahora si podemos separar <volatility_results> en listas dependiendo del pais del desastre y pais de la serie
  v.lista.separada <- split(volatility_results, sapply(volatility_results, function(x) x@info.evento[['agrupar']]))
  # Por otro lado, como la especificacion de Bialkowski (2008) ecuacion 5 esta hecha para mas de un evento, solamente se van
  # a guardar los elementos de <v.lista.separada> que contengan mas de un desastre
  v.lista.separada <- purrr::keep(v.lista.separada, ~(length(.x) > 1))
}

if(columna.agrupar == 'Continent'){
  # Encontrar los continentes para los paises (tener en cuenta que sudafrica queda excluido)
  continentes_series    <- est_continent(paises_series)
  continentes_desastres <- est_continent(paises_desastres)
  # Eliminar objetos donde el desastre o la serie fuese sudafrica
  volatility_results_filtro <- volatility_results[!(is.na(continentes_desastres) | is.na(continentes_series))]
  
  # Realizar el procedimiento de nuevo
  nombres_separados2     <- strsplit(names(volatility_results_filtro),'_')
  if(tipo.serie == 'Indices'){
    paises_series2       <- lapply(nombres_separados2, function(x) reverse.matching(x[1],F,T))
  }else{
    paises_series2       <- lapply(nombres_separados2, function(x) as.character(na.omit(str_match(x[1], countries))))
  }
  paises_desastres2      <- lapply(nombres_separados2, function(x) as.character(na.omit(str_match(x[2], countries))))
  continentes_series2    <- paste0('serie',est_continent(paises_series2))
  continentes_desastres2 <- paste0('desastre',est_continent(paises_desastres2))
  
  continentes_joint      <- paste0(continentes_series2, '_', continentes_desastres2)
  
  for(i in seq_along(continentes_joint)) volatility_results_filtro[[i]]@info.evento$agrupar <- continentes_joint[i]
  # Ahora si podemos separar <volatility_results> en listas dependiendo del pais del desastre y pais de la serie
  v.lista.separada <- split(volatility_results_filtro, sapply(volatility_results_filtro, function(x) x@info.evento[['agrupar']]))
  # Por otro lado, como la especificacion de Bialkowski (2008) ecuacion 5 esta hecha para mas de un evento, solamente se van
  # a guardar los elementos de <v.lista.separada> que contengan mas de un desastre
  v.lista.separada <- purrr::keep(v.lista.separada, ~(length(.x) > 1))
}

eventos.separados        <- c(unlist(lapply(v.lista.separada, length)),sum(unlist(lapply(v.lista.separada, length)))) 
names(eventos.separados) <- c(names(unlist(lapply(v.lista.separada, length))),'Todos')

# Tabla CAV/significancia ------------------------------------------------

# Dataframe con muchas ventanas
matrix.volatilidad <- matrix(nrow=(1),ncol=(length(v.lista.separada)+1))
# Creamos una matriz con las mismas dimensiones de <matrix.volatilidad>, pero que en cada celda va a contener toda la informacion, ya
# que <matrix.volatilidad> solo tendra el CAV y su significancia, mientras que <matrix.volatilidad.completa> tendra adicionalmente el pvalue
# y el error estandar
matrix.volatilidad.completa <- matrix.volatilidad
iteraciones.bool  <- 10000
for(i in seq_along(v.lista.separada)){
  prueba <- bootstrap.volatility2(volatility.list = v.lista.separada[[i]],es.window.length = as.numeric(ventana.estimacion)
                                  ,ev.window.length = vol_ev_window,bootstrap_vol_iterations = iteraciones.bool)
  matrix.volatilidad[1,i] <- paste(prueba$CAV,prueba$Significancia)
  matrix.volatilidad.completa[1,i] <- paste(prueba[1,], collapse = '/ ')
  print(i)
}

k <- length(v.lista.separada)+1
prueba.cav <- bootstrap.volatility2(volatility_results,as.numeric(ventana.estimacion),vol_ev_window,bootstrap_vol_iterations = iteraciones.bool)
matrix.volatilidad[1,k]          <- paste(prueba.cav$CAV,prueba.cav$Significancia)
matrix.volatilidad.completa[1,k] <- paste(prueba.cav[1,], collapse = '/ ')

colnames(matrix.volatilidad) <- c(names(v.lista.separada),'Todos')
colnames(matrix.volatilidad.completa) <- c(names(v.lista.separada), 'Todos')
Ventana                      <- vol_ev_window
matrix.volatilidad           <- cbind(Ventana,matrix.volatilidad)
matrix.volatilidad.completa  <- cbind(Ventana, matrix.volatilidad.completa)

# Creacion objeto para guardar
tabla.volatilidad <- new('Tabla.varianza', dataframe = data.frame(matrix.volatilidad),no.eventos = eventos.separados)
tabla.volatilidad.completa <- new('Tabla.varianza', dataframe = data.frame(matrix.volatilidad.completa), no.eventos = eventos.separados)    

# Guardar las tablas de significancia. No es necesario agregar el tipo de test ya que podemos guardar ambas tablas
if(columna.agrupar=='Country') agrupacion <- 'pais'
if(columna.agrupar=='Continent')   agrupacion <- 'continente'
save(tabla.volatilidad, tabla.volatilidad.completa,
       file=paste0(getwd(),'/Resultados_regresion_contagio/Tablas/Tablas_',tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_varianza_',regresor.mercado,'_',agrupacion,'.RData'))

