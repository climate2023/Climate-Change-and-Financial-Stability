##########################################################
# Graficas Series BEI.
# Autores: Juan Pablo Bermudez.
##########################################################

# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))

# Carga de datos ----------------------------------------------------------
# Como es un excel toca asegurarnos de leer todas las hojas
columna.fecha <- NULL
sheet.names    <- excel_sheets(paste0(Dir, 'BEI_LFM.xlsx'))
bei.dataframes <- lapply(sheet.names, function(x){
  df.temp <- read.xlsx(paste0(Dir, 'BEI_LFM.xlsx'), sheet = x, detectDates = T)
  # Verificar en la base de datos cual es la columna que indica la fecha. Si ninguna tiene formato fecha
  # toca que se indique explicitamente. Si hay mas de dos con clase fecha lo mismo
  if(sum(sapply(df.temp, function(col) inherits(col, 'Date'))) != 1) 
    stop('Por favor indicar cual es la variable de fechas')
  # El siguiente codigo solo corre si no se especifica claramente la columna de fecha
  if(is.null(columna.fecha)){
    columna.fecha <- colnames(df.temp)[sapply(df.temp, function(col) inherits(col, 'Date'))]
  }
  otras.columnas        <- setdiff(colnames(df.temp),columna.fecha)
  df.temp.xts           <- xts(df.temp[,otras.columnas], order.by = df.temp[,columna.fecha])
  colnames(df.temp.xts) <- paste0(colnames(df.temp.xts),'_',x) 
  
  return(df.temp.xts)
}) 

bei.dataframe <- do.call(merge, bei.dataframes)
# En <bei.dataframe> esta un objeto xts con todas las series que se encuentran en todas las hojas del excel
# Toca tener cuidado con elementos NA por las fechas.
sum(is.na(bei.dataframe))
# Como las dos hojas tenian las mismas fechas entonces no hay problemas con NA

# Creacion retornos y primeras diferencias --------------------------------
# Para poder analizar el BEI vamos a crear un dataframe de retornos y otro de primeras diferencias para poder
# graficarlos y tomar decisiones
bei.retornos <- apply(bei.dataframe, 2, function(x) 100*diff(log(x)))
# Hay datos que son negativos entonces sale NA al tomar el logaritmo

# Primeras diferencias BEI
bei.pd       <- apply(bei.dataframe, 2, function(x) na.omit(diff(x)))

# Graficas ----------------------------------------------------------------
# Niveles
par(mfrow = c(ncol(bei.dataframe)/4,2))
for(i in 1:ncol(bei.dataframe)){
  plot.zoo(bei.dataframe[,i],type='l',col='blue', ylab='BEI',xlab='t',main= paste0('Serie en Niveles. ',colnames(bei.dataframe)[i]))
}

par(mfrow = c(ncol(bei.pd)/4,2))
for(i in 1:ncol(bei.pd)){
  plot(bei.pd[,i],type='l',col='red', ylab='Primera Diferencia',xlab='t',main= paste0('P.Diferencia ',colnames(bei.pd)[i]))
}
