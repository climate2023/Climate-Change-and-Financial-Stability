##########################################################
# Convertir las diversas tablas en solo una para exportar a latex
# Autores: Juan Pablo Bermudez.
##########################################################

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

# Los siguientes argumentos van a filtrar los resultados y tablas
serie             <- 'Indices'      #<<<--- puede ser 'Indices' o 'CDS'
tipo.estudio      <- 'media'     #<<<--- puede ser 'media' o 'varianza'
regresor.mercado  <- 'benchmark'    #<<<--- puede ser 'PM' o 'benchmark', para CDS todavia no hay benchmark
umbrales.evento   <- c(50,100,150)  #<<<--- puede ser 50 100 o 150
if(tipo.estudio=='media') es.windows <- c(250,375,500) #<<<--- Para media puede ser 250, 375 o 500.
if(tipo.estudio=='varianza') es.windows <- c(500,750,1000) #<<<--- Para media puede ser 500, 750 o 1000.
columnas.tabla    <- 'paistipodesastre' #<<<--- Las tablas de la media estan guardadas tanto por tipo de desastre como por pais
# <columnas.tabla> toma el valor de 'tipodesastre' o 'pais'. Tambien estan guardadas por 'ambas, en cuyo caso <columnas.tabla> toma el valor
# de 'paistipodesastre'
table.caar        <- 0 #<<<--- booleano para indicar si las tablas se construiran mostrando el CAAR o el estadistico. 0 para estadistico, 1  para CAAR
# Debe corresponder al mismo <table.caar> de "Creacion_tablas_media.R"

# Organizacion tablas -----------------------------------------------------

lista.wilcoxon  <- list()
numero.eventos.wilcoxon <- list()
lista.bmp       <- list()
lista.bootstrap <- list()
numero.eventos.bootstrap <- list()
indice.lista    <- 0
for(i in seq_along(umbrales.evento)){
  umbral.del.evento <- umbrales.evento[i]
  for(j in seq_along(es.windows)){
    indice.lista <- indice.lista +1
    estimation.window <- es.windows[j]
    if(tipo.estudio == 'media') load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Tablas_',serie,'_tra',umbral.del.evento,'_est',
                       estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
    # Cargamos las tablas con el nuevo bootstrap
    if(tipo.estudio == 'varianza') load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Tablas_',serie,'_tra',umbral.del.evento,'_est',
                     estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
    if(tipo.estudio=='media'){
      lista.wilcoxon[[indice.lista]]                  <- tabla.wilcoxon@dataframe
      names(lista.wilcoxon)[[indice.lista]]           <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
      numero.eventos.wilcoxon[[indice.lista]]         <- tabla.wilcoxon@no.eventos
      names(numero.eventos.wilcoxon)[[indice.lista]]  <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
      
      lista.bmp[[indice.lista]]        <- tabla.bmp@dataframe
      names(lista.bmp)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
    }
    if(tipo.estudio=='varianza'){
      lista.bootstrap[[indice.lista]]                 <- tabla.volatilidad@dataframe
      names(lista.bootstrap)[[indice.lista]]          <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
      numero.eventos.bootstrap[[indice.lista]]        <- tabla.volatilidad@no.eventos
      names(numero.eventos.bootstrap)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
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
    dataframe.wil200  <- dataframe.wil[,grep(paste0('Estimacion_', es.windows[1]),colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 250 dias
    dataframe.wil300  <- dataframe.wil[,grep(paste0('Estimacion_', es.windows[2]),colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 375 dias
    dataframe.wil500  <- dataframe.wil[,grep(paste0('Estimacion_', es.windows[3]),colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 500 dias
    
    # Eventos para cada dataframe
    eventos.wil200 <- paste('Eventos: ', numero.eventos[grep(paste0('Estimacion_', es.windows[1]), names(numero.eventos))])
    eventos.wil300 <- paste('Eventos: ', numero.eventos[grep(paste0('Estimacion_', es.windows[2]), names(numero.eventos))])
    eventos.wil500 <- paste('Eventos: ', numero.eventos[grep(paste0('Estimacion_', es.windows[3]), names(numero.eventos))])
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
    colnames(dataframe.wil.organizado) <- c(paste0('est',umbrales.evento[1]),paste0('est',umbrales.evento[2]),paste0('est',umbrales.evento[3]))
    # Añadir columna de dias de estimacion
    dataframe.wil.organizado$`Estimacion` <- c(rep('',8),250,rep('',15),375,rep('',15),500,rep('',7))
    # Mutar las columnas <50>, <100> y <200> para agregar un '/', para poder colocar la significancia de BMP en la misma tabla
    dataframe.wil.organizado <- dataframe.wil.organizado %>% 
      mutate('50' = paste(est50,'/'), '100' = paste(est100,'/'),'150' = paste(est150,'/'))
    # Seleccionar solamente las columnas de interes
    dataframe.wil.organizado <- dataframe.wil.organizado %>% dplyr::select(Estimacion,`50`,`100`,`150`)
    
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
    data.bmp200      <- data.bmp[,grep(paste0('Estimacion_', es.windows[1]),colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 200 dias
    data.bmp300      <- data.bmp[,grep(paste0('Estimacion_', es.windows[2]),colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 300 dias
    data.bmp500      <- data.bmp[,grep(paste0('Estimacion_', es.windows[3]),colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 500 dias
    
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
    colnames(data.bmp.organizado) <- c('est50','est100','est150')
    # Mutar para solamente tener los * de significancia
    data.bmp.organizado <- data.bmp.organizado %>% 
      mutate('50bmp' = gsub("[^*]","",est50),'100bmp'=gsub("[^*]","",est100),'150bmp' = gsub("[^*]","",est150))
    
    # Lo unico que falta es juntar los dos dataframe: <dataframe.wil.organizado> y <data.bmp.organizado> para tener un solo dataframe con la significancia de 
    # ambos tests
    dataframe.final <- cbind(dataframe.wil.organizado,data.bmp.organizado)
    if(table.caar) {dataframe.final <- dataframe.final %>% 
      mutate('50' = paste((rep(c('', paste0('[1,',1:15,']')),3)),`50`,`50bmp`),'100'=paste((rep(c('', paste0('[1,',1:15,']')),3)),`100`,`100bmp`),
             '150'=paste((rep(c('', paste0('[1,',1:15,']')),3)),`150`,`150bmp`)) %>% 
      dplyr::select(Estimacion,`50`,`100`,`150`)
    }else{
      # Primero se debe eliminar el numero de eventos en las columnas <est50> <est100> y <est150>, para poder unir correctamente la wilcoxon y BMP
      dataframe.final <- dataframe.final %>%
        mutate(est50 = str_replace_all(est50, "Eventos\\s*:\\s*\\d+", " ")) %>% 
        mutate(est100 = str_replace_all(est100,  "Eventos\\s*:\\s*\\d+", " ")) %>% 
        mutate(est150 = str_replace_all(est150,  "Eventos\\s*:\\s*\\d+", " "))
      
      # Por ultimo creamos el dataframe para exportar
      dataframe.final <- dataframe.final %>% 
        mutate('50' = paste((rep(c('', paste0('[1,',1:15,']')),3)),`50`,`est50`),'100'=paste((rep(c('', paste0('[1,',1:15,']')),3)),`100`,`est100`),
               '150'=paste((rep(c('', paste0('[1,',1:15,']')),3)),`150`,`est150`)) %>% 
        dplyr::select(Estimacion,`50`,`100`,`150`)
    }
    
    # Exportar a latex
    kable.final <- kable(dataframe.final,format='latex', booktabs=T, caption = paste0('Significancia para eventos ', nombre.columna,'. Nota: para ', serie, 
                                                                                          '. Estudio sobre la ', tipo.estudio, ' utilizando ',regresor.mercado, 
                                                                                          ' como retorno de mercado.'))
    writeLines(kable.final, paste0(getwd(),'/Resultados_regresion/Tablas_Latex/Media/',nombre.columna,'_', serie, '_',tipo.estudio, '_',regresor.mercado,'.tex')) 
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
    numero.eventos.v  <- purrr::map(numero.eventos.bootstrap, ~.x[nombre.columna])
    numero.eventos.v  <- unlist(lapply(numero.eventos.v, function(x) as.character(ifelse(is.na(x), '', x))))
    dataframe.var500  <- dataframe.var[,grep('Estimacion_500',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 200 dias
    dataframe.var750  <- dataframe.var[,grep('Estimacion_750',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 300 dias
    dataframe.var1000 <- dataframe.var[,grep('Estimacion_1000',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 500 dias
    # Eventos para cada dataframe
    eventos.var500  <- paste('Eventos: ', numero.eventos.v[grep(paste0('Estimacion_', es.windows[1]), names(numero.eventos.v))])
    eventos.var750  <- paste('Eventos: ', numero.eventos.v[grep(paste0('Estimacion_', es.windows[2]), names(numero.eventos.v))])
    eventos.var1000 <- paste('Eventos: ', numero.eventos.v[grep(paste0('Estimacion_', es.windows[3]), names(numero.eventos.v))])
    
    # Retirar nombres de columnas para hacer rbind 
    colnames(dataframe.var500) <- NA
    colnames(dataframe.var750) <- NA
    colnames(dataframe.var1000) <- NA
    
    # Agregar el numero de eventos a cada dataframe
    dataframe.var500  <- rbind(eventos.var500, dataframe.var500)
    dataframe.var750  <- rbind(eventos.var750, dataframe.var750)
    dataframe.var1000 <- rbind(eventos.var1000, dataframe.var1000)
    
    # Juntarlos en un gran dataframe
    dataframe.var.organizado <- rbind(dataframe.var500,dataframe.var750, dataframe.var1000)
    # Nombres de columnas
    colnames(dataframe.var.organizado) <- c('50','100','150')
    # Añadir columna de dias de estimacion
    dataframe.var.organizado$`Estimacion` <- c(rep('',8),500,rep('',15),750,rep('',15),1000,rep('',7)) # se elige asi ya que <dataframe.var200> y <dataframe.var300> son NA
    # Reordenar las columnas
    dataframe.var.final <- dataframe.var.organizado %>% 
      mutate('50'= paste(rep(c('',paste0('[0,',0:14,']')),3),`50`),
             '100'= paste(rep(c('',paste0('[0,',0:14,']')),3),`100`),
             '150'= paste(rep(c('',paste0('[0,',0:14,']')),3),`150`)) %>% 
      dplyr::select(Estimacion,`50`,`100`,`150`)
    
    # Exportar a latex
    kable.final <- kable(dataframe.var.final,format='latex', booktabs=T, caption = paste0('Significancia para eventos ', nombre.columna,'. Nota: para ', serie, 
                                                                           '. Estudio sobre la ', tipo.estudio, ' utilizando ',regresor.mercado, 
                                                                           ' como retorno de mercado.'))
    writeLines(kable.final, paste0(getwd(),'/Resultados_regresion/Tablas_Latex/Varianza/',nombre.columna,'_', serie, '_',tipo.estudio, '_',regresor.mercado,'.tex'))
  }
}
