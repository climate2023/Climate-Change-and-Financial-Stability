##########################################################
# Replicacion paper Climate Change and Financial Stability de Pagnottoni et al 2022, con codigo
# para el nuevo SUR. Ahora los desastres afectan solamente al pais donde sucedió y no a los demas
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
bool_cds   <- T  #<<<--- Parametro que indice si se hara el analisis sobre los CDS (<TRUE>) y <F> si se realizara sobre los stocks
promedio.movil <- T #<<<-- parametro (booleano) para que el usuario decida cual sera el retorno de mercado, <T> si es el promedio movil de 
# de los retornos de los indices, <F> si es otra variable

if(!bool_paper){
  indexes   <- c("S.PASX200","BEL20","Bovespa","S.PTSXComposite","S.PCLXIPSA","OMXCopenhagen20","OMXHelsinki25","CAC40",
                 "DAX","HangSeng","Nifty50","JakartaStockExchange","S.PBMVIPC","AEX","OSEBenchmark","WIG20","MOEXRussia",
                 "SouthAfricaTop40","KOSPI","IBEX35","OMXStockholm30","SMI","SETIndex","BIST100","FTSE100","NASDAQComposite",
                 "Nasdaq100") #<<<--- Lista de los indices analizados
  
  countries <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
                 "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Norway","Poland","Russia",
                 "SouthAfrica","SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", 
                 "UnitedKingdom","USA1","USA2") #<<<--- Lista de los paises de cada indice, con el proposito de leer los excel con los datos
  
  no.rezagos.de.desastres <- 4  #<<<--- Numero de rezagos de los desastres <w> (i.e. t0, t1, ..., tw)
  
  # Establecemos el directorio de los datos
  Dir  = paste0(getwd(),'/Bases/') #Directorio de datos, se supone que el subdirectorio <Bases> existe
  
  # Genera una lista de los codigos bursatiles en formato xts.
  # La longitud de la lista es igual al numero de archivos con nombre stocks_<country> que existe en <Dir>.
  
  # Para cada pais que se encuentra en <countries>, vamos a tener una base de datos del indice bursatil que le corresponde. Cada una tiene 7 columnas:
  # la primera, <Date>, corresponde a los dias en los cuales tenemos datos para el indice; la segunda, <Price>, corresponde a los precios de cierre 
  # para cada dia; la tercera, <Open>, corresponde a los precios de apertura para cada dia; la cuarta, <High>, corresponde al mayor precio registrado
  # en el dia; la quinta, <Low>, corresponde al menor precio registrado en el dia; la sexta, <Vol.>, corresponde al volumen de acciones que se tranzaron;
  # la septima, <Change%>, corresponde al cambio porcentual en el precio de cierre. 
  # Dado que de las bases de datos nos interesan sobre todo <Date> y <Price>, al final generaremos una base que contenga todos los precios de cierre de
  # todos los paises que esta en <countries>. Cabe recalcar que en el vector <indexes> tenemos los nombres de los indices bursatiles en el orden 
  # correspondiente a los paises en <countries>.
  xts_list     <- list() 
  for (country in countries) {
    # Genera el nombre del archivo csv, siguiendo el Directorio especificado, añadiendole /Stocks_ country.csv
    csv_file <- paste0(Dir,"Stocks_", country, ".csv")
    # Genera un archivo csv para el país <country>
    csv      <- read.csv(csv_file, header = TRUE, sep = ";", quote = "\"", col.names = c("Date","Price", "Open", 
                                                                                         "High","Low","Vol.","Change%"))
    colnames <- names(csv)
    
    # Loop que corre por todas las columnas del archivo <csv> menos la primera (ya que es un dia), retirandole las
    # comas que podrían generar problemas para reconocerlo como formato número
    for (colname in colnames[2:length(colnames)]) {
      csv[, colname] <- as.numeric(gsub(",","",csv[, colname]))
    } # Muestra warning() ya que hay una columna que contiene caracteres "M" 
    
    csv$Date <- as.Date(csv$Date, "%m/%d/%Y")
    
    # Generar la lista de los xts, solamente de la columna <Price>, teniendo en cuenta los índices incluidos en <Date>
    xts_list[[country]] <- xts(csv$Price, csv$Date)
  }
  
  # Generar una base de datos que junte todos los indices bursatiles en formato xts.
  # Las matriz <base_test> tiene las siguientes dimensiones:
  #     columnas: numero de <indexes> analizados
  #     filas   : total de dias en el que hay al menos un dato para cualquier indice
  base_test <- do.call(merge,xts_list)
  # Cambiar nombres de las columnas por los nombres de los <indexes>
  colnames(base_test) <- indexes
  xts.mercado <- NA # Para base de Pagnottoni no hay retorno de mercado "exogeno", ya que es el promedio movil
}else{
  date_column <- "Date"  #<<<--- Parametro que le indica al usuario el nombre de la columna de las fechas
  countries   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                   'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice
  
  no.rezagos.de.desastres <- 15  #<<<--- Numero de rezagos de los desastres <w> (i.e. t0, t1, ..., tw)
  
  # Establecemos el directorio de los datos
  Dir  = paste0(getwd(),'/Bases/') #Directorio de datos, se supone que el subdirectorio <Bases> existe
  
  # Leer la base de datos
  if(bool_cds){
    indexes  <- c('CDS_Brazil','CDS_Chile','CDS_China','CDS_Colombia','CDS_Indonesia','CDS_Korea',
                  'CDS_Malaysia','CDS_Mexico','CDS_Peru','CDS_SouthAfrica','CDS_Turkey') #<<<--- Lista de los indices analizados. 
    # Corresponde a los nombres en <base_cds>
    base_cds <- readxl::read_excel(path = paste0(Dir,"CDS_Data_VIX_EMBI.xlsx"))
    # Seleccionar las columnas de interes (<indexes>)
    cds <- base_cds %>% dplyr::select(all_of(indexes))
    
    # Generar una base de datos que junte todos los CDS en formato xts.
    # Las matriz <base_test> tiene las siguientes dimensiones:
    #     columnas: numero de <indexes> analizados
    #     filas   : total de dias en el que hay al menos un dato para cualquier indice
    base_test <- as.xts(cds,order.by = as.Date(base_cds[[date_column]]))
    # Por ultimo, es necesario asegurar que tanto <indexes> como los nombres de las columnas de <base_test>
    # no contengan caracteres especiales ni espacios, para lo cual se usa la funcion <gsub>
    indexes <- gsub("[^[:alnum:]]", "", indexes)
    colnames(base_test) <- gsub("[^[:alnum:]]", "", colnames(base_test))
    xts.mercado <- NA # Para los cDS no se tiene un retorno de mercado exogeno, ya que es el promedio movil
  }else{
    indexes         <- c('BIST100','Bovespa','ChinaA50','JSX','KOSPI','S.PBMVIPC','S.PCLXIPSA','SouthAfricaTop40',
                         'IGBVL','KLCI','COLCAP') # Nombre indices para el paper. JSX es el de Jakarta
    columna.mercado <- c('MSCI')
    base.stocks     <- readxl::read_excel(path = paste0(Dir, 'Stocks_Paper_Completos.xlsx')) 
    # <Stocks_Paper.xlsx> tiene la base desde el 5 de junio del 2006, <Stocks_Paper_Completos> desde el 7 de octubre del 2004
    # Volver objeto xts
    base_test <- as.xts(base.stocks[, !colnames(base.stocks) %in% c(date_column,columna.mercado)],order.by = as.Date(base.stocks[[date_column]]))  
    # Cambiar nombres de las columnas por los nombres de los <indexes>
    colnames(base_test) <- indexes
    xts.mercado <- as.xts(base.stocks[,colnames(base.stocks)%in%columna.mercado],order.by=as.Date(base.stocks[[date_column]]))
  }
}

# Generar un vector de fechas en los que solo se tiene valores para n < <min.dias.stock> mercados.
min.dias.stock <- (ceiling((length(indexes)/2)))#<<<--- minimo numero de <indexes> en donde debe haber datos
navalues = c()
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))>=length(indexes)- min.dias.stock){
    navalues <- c(navalues, index(row))}
} 

# Eliminar aquellos dias que hacen parte del vector <navalues>, dejando solo los dias en los que se tiene datos para al
# menos <min.dias.stock> mercados. Se utilizó la función weekdays() para comprobar que ningún dia fuese sábado o domingo.
for(day in navalues) base_test <- subset(base_test, subset = index(base_test) != day, drop = TRUE)
if((any(weekdays(index(base_test))=='Sunday' | weekdays(index(base_test))=='Saturday')) == TRUE)  warning("En la base de datos hay sabados o domingos")

# Interpolacion lineal de los datos faltantes. Las dimensiones de <base> son:
#       columnas: las mismas que <base_test>
#       filas   : las de <base_test> menos la longitud de <navalues>, que son las fechas eliminadas
base <- na.approx(base_test[,1])
for(i in 2:length(indexes)) base <- merge(base,na.approx(base_test[,i]))
# Asegurarnos que quede con los nombres de columnas correctos
colnames(base) <- colnames(base_test)

# Eliminar las filas en las que hay valores NA. Las dimensiones de <base_precios> son:
#       columnas: las mismas que <base_test>
#       filas   : dependiendo de los valores en <base>, <base_precios> puede tener o el mismo numero de filas que <base> o una fila menos, 
#                 o dos filas menos.
base_precios <- base[complete.cases(base),]

# Recordar que tenemos la serie benchmark que es el MSCI Emerging markets, pero tiene un indice distinto de <base_precios>
# por lo que tenemos que reducir su indice
xts.mercado <- xts.mercado[index(base_precios)]
# Tambien es necesario realizar interpolacion a la base <xts.mercado>
xts.mercado <- na.approx(xts.mercado)
if(!bool_cds){
  # Genera la base de retornos. Se coloca [2:nrow(base_precios)] porque de no hacerlo toda la primera fila serian valores
  # NA, por lo que se perdio un dato. El operador diff se realizo para toda la <base_precios>,pero el <[2:nrow(base_precios)]>
  # lo que hace es solamente quitar la primera fila de NA.
  # La dimension de <base_retornos> es la siguiente:
  #       columnas: las mismas que <base_test> y las demás bases anteriores
  #       filas   : una fila menos que <base_precios>
  base_retornos    <- 100*diff(log(base_precios))[2:nrow(base_precios),]
  mercado.retornos <- na.omit(100*diff(log(xts.mercado)))
}else{
  base_retornos    <- na.omit(diff(base_precios))
}

if(promedio.movil){
  # Otra variable importante es la media de los promedios moviles, por lo cual se genera el promedio movil de cada
  # retorno de orden 22, ya que hay aproximadamente 22 dias para cada mes, usando la funcion <moving_average>.
  orden <- 22 #<<<---  Orden del promedio movil del indice global de largo plazo de los indices accionarios 
  # mov_average_base <- moving_average(base_retornos,orden)
  # La siguiente funcion <apply> genera una base de datos (<mov_average_base>). Las dimensiones de la matriz son:
  #       columnas: las mismas que las demas bases anteriores
  #       filas   : aquellas de <base_retornos> menos <orden>-1
  mov_average_base <- apply(base_retornos, MARGIN=2, FUN=rollmean, k=orden, align="right")
  
  # Media de los anteriores promedios. Genera vector con una longitud igual al numero de filas de <mov_average_base>
  market.returns           <- apply(mov_average_base, MARGIN=1, FUN=mean)
  market.returns           <- xts(market.returns, order.by=index(base_retornos)[-c(1:(orden-1))]) #-> XTS
  colnames(market.returns) <- c("market.returns") # Nombre de la variable
}else{
  market.returns <- mercado.retornos
  colnames(market.returns) <- c("market.returns")
}

# Hay que tener en cuenta que la muestra que se utiliza en el paper no es la misma que la que se tiene en las bases,
# por lo que se reduce la base para los datos del febrero 08 2001 a diciembre 30 2019.
# La funcion <muestra_paper> va a seleccionar desde un cierto dia, el cual elegimos 08 febrero 2001 ("2001-02-08") siguiendo el paper.
# Para el analisis con CDS no tenemos fecha especifica, asi que se colocaria NULL
if(!bool_paper){
  dia.inicial <- "2001-02-08"  #<<<--- Dia inicial de la muestra
}else{
  dia.inicial <- NULL
}

if(!is.null(dia.inicial)){
  # Se obliga que la muestra comience en <dia.inicial>
  # La matriz <Retornos> conserva el mismo numero de columnas.
  Retornos             = base_retornos[paste0(dia.inicial,"/"),]
  market.returns = market.returns[paste0(dia.inicial,"/")]
  mercado.retornos     = mercado_retornos[paste0(dia.inicial,"/")]
  
  # Tabla 1 Pagnottoni: Estadistica descriptiva -----------------------------
  
  ## Generar (skewness, kurtosis, mean, max, min, sd) de los retornos de los <indexes> acc. 
  skewness <- moments::skewness(Retornos)
  kurtosis <- moments::kurtosis(Retornos)
  mean     <- apply(Retornos, MARGIN=2, FUN=mean)
  max      <- apply(Retornos, MARGIN=2, FUN=max)
  min      <- apply(Retornos, MARGIN=2, FUN=min)
  sd       <- apply(Retornos, MARGIN=2, FUN=sd)
  
  # La matriz <Stats> tiene por numero de columnas a aquellas medidas de estadística descriptiva, y las filas son igual al numero de <indexes>
  Stats = cbind(min,max,mean,sd,skewness,kurtosis )
  print(Stats, digits=3)
}else{
  # Tabla 1 Pagnottoni: Estadistica descriptiva -----------------------------
  digitos.redondear <- 3 #<<<--- a cuantos digitos se desea redondear las estadisticas descriptivas
  ## Generar (skewness, kurtosis, mean, max, min, sd) de los retornos de los <indexes> acc. 
  skewness <- round(moments::skewness(merge(base_retornos,market.returns),na.rm = T),digitos.redondear)
  kurtosis <- round(moments::kurtosis(merge(base_retornos,market.returns),na.rm = T),digitos.redondear)
  mean     <- apply(merge(base_retornos,market.returns), MARGIN=2, function(x) round(mean(x,na.rm=T),digitos.redondear))
  max      <- apply(merge(base_retornos,market.returns), MARGIN=2, function(x) round(max(x,na.rm=T),digitos.redondear))
  min      <- apply(merge(base_retornos,market.returns), MARGIN=2, function(x) round(min(x,na.rm=T),digitos.redondear))
  sd       <- apply(merge(base_retornos,market.returns), MARGIN=2, function(x) round(sd(x,na.rm=T),digitos.redondear))
  
  # La matriz <Stats> tiene por numero de columnas a aquellas medidas de estadística descriptiva, y las filas son igual al numero de <indexes>
  Stats = cbind(min,max,mean,sd,skewness,kurtosis)
  print(Stats)
}

# Desagregacion temporal --------------------------------------------------

# Las variables a desagregar diariamente son el crecimiento del GDP trimestral y el crecimiento del FDI anual.

## Datos para GDP trimestral ===
if(1){
  # La base de datos que se lee a continuacion tiene las siguientes columnas:
  #        <Time>      : indice trimestral del 2001 al 2019
  #        <countries> : El nombre de las demas columnas es el mismo que los paises que estan en <countries>
  # Por otro lado, a cada fila le corresponde una observacion trimestral del producto interno bruto.
  # Por tanto, si nos encontramos en la fila 2001Q1 y la columna <Australia>, seria el PIB de <Australia> en el primer trimestre del 
  # 2001.
  
  # Leer la base de datos, establecer el formato fecha y generar la base de datos en xts y la lista a ser desagregada
  # De este modo se genera una lista <quarterly_series>, de longitud igual al numero de <indexes>.
  # Cada elemento de <quarterly_series> es un vector numerico con la misma longitud de los datos en los archivos excel
  if(!bool_paper){
    gdp_countries <- readxl::read_xlsx(paste0(Dir,"GDP_countries_corregida.xlsx"), sheet="GDP") #<<<--- Base de datos con GDPs
  }else{
    gdp_countries <- readxl::read_xlsx(paste0(Dir,"GDP_countries_cds.xlsx")) #<<<--- Base de datos con GDPs
    if(!bool_cds){
      # Por el momento la idea es  reducir tanto <base_precios> como <base_retornos> y <mercado.retornos> para que terminen en el tercer trimestre del 2022, 
      # ya que hasta ese punto hay datos de GDP. 
      trimestre.final    <- as.Date(as.yearqtr(tail(gdp_countries$Time,1)),frac=1) # Final del tercer trimestre del 2022
      # La razon de transformar <base_precios> es porque la desagregacion temporal se hace en base al indice que tenga <base_precios>.
      # La razon de transformar <base_retornos> es porque es la base que se va a utilizar para el SUR.
      base_retornos <- base_retornos[index(base_retornos)<=trimestre.final]
      base_precios  <- base_precios[index(base_precios) <=trimestre.final]
      # Tambien se reduce <market.returns> porque se agrega a la base final
      market.returns <- market.returns[index(market.returns) <= trimestre.final]
      # Tambien se reduce <mercado.retornos> porque se agrega a la base final
      mercado.retornos <- mercado.retornos[index(mercado.retornos) <= trimestre.final]
    }
  }
  gdp_countries[,-1] <- apply(gdp_countries[,-1],2,as.double) # Asegurar que esten en formato numerico
  dates.low.freq     <- as.Date(as.yearqtr(gdp_countries$Time),frac=1) #date format, se supone que existe una col llamada <Time>
  quarterly_series   <- as.list(gdp_countries[,-1]) #Se quita la columna de fechas y se genera una lista de sus columnas
  
  ## -- Matriz de agregacion GDP trimestral <qtr_agr> ---
  # El metodo de chow-lin requiere una matriz de agregacion. Sin embargo, en la version fast se tiene en cuenta las 
  # diferencias de dias que puede haber en cada mes. Por tanto, es necesario crear una matriz de agregacion que 
  # tenga lo anterior en cuenta. 
  
  nrows <- length(quarterly_series[[1]])   ## Hay 76 trimestres en la base de datos, numero de datos trimestrales
  ncols <- nrow(base_precios)   ## No. de dias, con los retornos se pierde un dato, y al tomarle diferencias se pierde un dato, por lo que
  ## al final las matrices <Retornos> y la de desagregacion tendran el mismo numero de filas
  
  # La matriz de agregacion <qtr_agr> tiene entonces dimensiones <nrows>x<ncols>
  qtr_agr <- matrix(0, nrow = nrows, ncol = ncols) #matriz de agregacion 
  dates.high.freq   <- as.character(index(base_precios)) #Fechas de freq alta
  cat('\nHigh frequency range:',range(dates.high.freq))
  cat('\nLow frequency range:'); print(range(dates.low.freq))
  
  ## WARNING si los rangos son distintos, primero pasar <dates.high.freq> al ultimo dia de su trimestre
  quarter.high.freq <- ceiling_date(as.Date(dates.high.freq), unit = "quarter") - 1
  cat('\nHigh frequency range in trimesters:');print(range(quarter.high.freq))
  if ((range(quarter.high.freq)[1] == range(dates.low.freq)[1] & range(quarter.high.freq)[2] == range(dates.low.freq)[2]) == FALSE)
    warning("Los rangos de baja y alta frecuencia son distintos")
  
  ## Extrae los meses de alta freq. en formato yyyy-mm sin repeticiones.
  meses <- unique(substr(dates.high.freq,1,7))
  
  #Realizamos la matriz de agregacion usando la funcion days.
  for(i in 0:(nrows-1))
    qtr_agr <- days(x=i, m=qtr_agr, months=meses, dates=dates.high.freq)
  
  ### Definicion de parametros usados en Chow Lin. 
  # <alpha>:            Coef. de persitencia del error del modelo  
  # <matriz_var_cov_0>: La matriz de var-cov del error del modelo
  
  vec_cte <- c(rep(1, ncols)) ## vector constante usado como variable indicadora, dado que no tenemos otra
  alpha_fast = 0.99999        #<<---- Parametro de la version fast de chow-Lin
  
  #generar la matriz de var-cov de acuerdo al paper analizado (falta multiplicarla por sigma^2/(1-alpha^2) )
  matriz_var_cov_0 <- matrix(1, nrow=ncols, ncol=ncols)
  for(i in 1:ncols){
    for(j in 1:ncols)
      if(j != i) matriz_var_cov_0[i, j] = alpha_fast^(abs(j-i))
  } ##Crear la matriz de var-cov
  
  # Se ejecuta la funcion de <chow_lin>, que da como resultado una base de datos de las series desagregadas.
  # La base <gdp_growth_base> tiene las siguientes dimensiones: columnas - el numero de <indexes>, filas - el mismo de <base_retornos> 
  daily_series    = chow_lin(time_Series_list=quarterly_series, c=qtr_agr, w=vec_cte, var_covar=matriz_var_cov_0,base_indice = base_precios)
  gdp_growth_base = apply(daily_series, MARGIN=2, function(x) diff(log(x))) #diff(log(series))
  gdp_growth_base = as.xts(gdp_growth_base, order.by=index(base_precios[-1,]))
  
  #Colocamos los nombres de las series
  names                     <- (countries)
  colnames(gdp_growth_base) <- names[1:ncol(gdp_growth_base)]
  #Para diferenciar los nombres de los retornos le agregamos un prefijo gdp
  colnames(gdp_growth_base) <- paste("gdp",colnames(gdp_growth_base),sep="_")
}

### DESAGREGACION TEMPORAL FDI - Datos ===
if(1){
  # La base de datos que se lee a continuacion tiene las siguientes columnas:
  #       <Year>      : Año del cual tenemos datos
  #       <countries> : El nombre de las demas columnas es el mismo que los paises que estan en <countries>
  # A cada fila le corresponde una observacion anual  del indice de desarrollo financiero.
  # Por tanto, si nos encontramos en la fila 2001 y la columna <Australia>, seria el FDI de <Australia> en el 2001.
  
  #Ahora para hacer la desagregacion temporal del FDI necesitaremos los mismos cinco argumentos: una lista de las 
  # series a desagregar, un vector constante, una matriz de agregación y una matriz de varianzas covarianzas-
  # el alpha puede seguir siendo el mismo, ya que para el metodo fast debe ser 0.99999.
  # De este modo se genera una lista <fdi_series>, de longitud igual al numero de <indexes>.
  # Cada elemento de <fdi_series> es un vector numerico con la misma longitud de los datos en los archivos excel
  if(!bool_paper){
    fdi_countries <- readxl::read_xlsx(paste0(Dir,"FDI_anual.xlsx"), sheet="FDI") #<<--- Base datos de los FDI
  }else{
    fdi_countries <- readxl::read_xlsx(paste0(Dir,"fdi_cds.xlsx")) #<<<--- Base de datos con GDPs
    if(0){
      # En este caso, para los stocks, solamente se tienen datos a partir del 2006, por lo que se retiran los datos del 2004 y 2005
      # de la base <fdi_countries>
      years.remove <- c('2004','2005')
      fdi_countries <- fdi_countries %>% 
        dplyr::filter(!(Year%in% years.remove))
    } # ya no es necesario remover el 2004 y 2005
  }
  fdi_countries    <- fdi_countries %>% dplyr::mutate_all(as.numeric) # Asegurar que sean datos numericos
  date.col         <- "Year"  #<<<---parametro que indica titulo de la columna de las fechas
  fdi_countries_ts <- ts(fdi_countries[,-which(names(fdi_countries)==date.col)],
                         start=min(fdi_countries[,date.col]),frequency=1)   #<<<--- <frequency> es parametro de frecuencia de los datos  
  fdi_series       <- as.list(fdi_countries_ts)
  
  ##Matriz de agregacion anual FDI 
  fdi_rows <- nrow(fdi_countries_ts)
  ##El numero de columnas es igual al de <qtr_agr> porque se quiere desagregar en esa cantidad de dias (<ncols>)
  fdi_agregacion_matriz  <- matrix(0, nrow = fdi_rows, ncol = ncols)
  
  #Se genera la matriz de agregacion, colocando uno a los dias que pertenezcan al anho correspondiente.
  #Por ejemplo en la primera fila tendran uno aquellos dias que pertenezcan al 2001 (primer anho)
  i1 = 0
  for(i in as.numeric(unique(substr(dates.high.freq,1,4)))){
    i1 = i1 + 1
    for(date in dates.high.freq){
      if(substr(date,1,4)==i){
        pos <- which(dates.high.freq == date)
        fdi_agregacion_matriz[i1, pos] <- 1
      }
    }
  }
  
  # <vec_cte> corresponde a la serie indicadora, al igual que la matriz de var-cov permanecen iguales a las usadas en los GDPs
  # La base <fdi_growth_base> tiene las siguientes dimensiones: columnas - el numero de <indexes>, filas - el mismo de <base_retornos> 
  fdi_daily_series = chow_lin(fdi_series, fdi_agregacion_matriz, vec_cte, matriz_var_cov_0,base_indice = base_precios)
  # Necesitamos controlar posibles datos que den <= 0 (Revisar)
  fdi_daily_series2 = apply(fdi_daily_series, MARGIN = 2, function(col) {
    min.value   <- min(col[col>0])
    col[col<=0] <- min.value
    return(col)
  })
  fdi_daily_series2 <- xts(fdi_daily_series2, order.by = index(fdi_daily_series))
  
  fdi_growth_base  = apply(fdi_daily_series2, MARGIN=2, function(x) diff(log(x))) #diff(log(series))
  fdi_growth_base  = as.xts(fdi_growth_base, order.by=index(base_precios[-1,]))
  
  # Colocamos los mismos nombres que la base de retornos pero le agregamos un prefijo fdi
  colnames(fdi_growth_base) <- names[1:ncol(fdi_growth_base)]
  colnames(fdi_growth_base) <- paste("fdi",colnames(fdi_growth_base),sep="_")
}

if(!is.null(dia.inicial)){
  # Por otro lado, tambien es necesario reducir la muestra a las bases para que concuerden con la muestra de paper
  # Podemos usar la funcion que estaba anteriormente especificada
  # Ambas matrices (<Crecimiento_PIB> y <Crecimiento_FDI>) conservan el mismo numero de columnas, pero sus filas se ven reducidas a las mismas de <Retornos> 
  Crecimiento_PIB <- gdp_growth_base[paste0(dia.inicial,"/"),]
  Crecimiento_FDI <- fdi_growth_base[paste0(dia.inicial,"/"),]
}

# Revisar autocorrelacion serial ------------------------------------------

#El siguiente codigo es para revisar la autocorrelacion serial de la serie de retornos de cada indice, con 
#50, 100 y n/4 rezagos. Al 5% para todos los indices se viola la hipótesis nula para al menos un rezago
if(0){
  lags.test = round(nrow(base_retornos)/4)
  correlacionados <- c()
  no_correlacionados <- c()
  
  #Generamos un loop for, que crea dos vectores, en el primero incluye aquellos paises con un p-valor menor al 5%
  #para un test Ljung-Box con lags.test rezagos; es decir, incluye a los paises que se puede rechazar la no autocorrelacion
  #Mientras que en el segundo vector incluye a los paises que no tienen evidencia para rechazar la no autocorrelacion
  
  for (i in 1:ncol(base_retornos)) {
    result <- Box.test(base_retornos[, i], lag = lags.test, type = "Ljung-Box")
    if(result$p.value < 0.05){
      correlacionados <- c(correlacionados,colnames(base_retornos[,i]))
    }else{
      no_correlacionados <- c(no_correlacionados,colnames(base_retornos[,i]))
    }
  }
}

# Agregar rezagos a las ecuaciones ----------------------------------------

# Como se encontró correlacion serial para casi todas las series usando el test de Ljung - Box con 20 rezagos y n/4
# rezagos. Modelamos cada retorno siguiendo un modelo AR(p), siendo p = 0 a 20, y elegimos el modelo segun el 
# criterio de Akaike

# if(0) porque se corrige la eficiencia del codigo en la linea 474
if(0){
  ## Loop para obtener las matrices de rezagos para cada <indice>
  if(!is.null(dia.inicial)){
    for(indice in indexes){
      var_name <- paste0("lags_",indice)
      Lags     <- lag_function(base_retornos,indice,AR.m=20, MA.m=0, d=0, bool=TRUE, metodo="CSS",dia.inicial)
      assign(var_name,Lags)
    }
  }else{
    for(indice in indexes){
      var_name <- paste0("lags_",indice)
      Lags     <- lag_function(base_retornos,indice,AR.m=20, MA.m=0, d=0, bool=TRUE, metodo="CSS",dia.inicial)
      Lags     <- Lags[complete.cases(Lags)]
      assign(var_name,Lags)
      # Falta reducir las bases <base_retornos>, <market.returns>,<gdp_growth_base>, <fdi_growth_base>, <mercado.retornos> 
      # y todas las de rezagos para que tengan la misma cantidad de datos
      # Guardar el indice mas reducido entre todas las matrices de rezagos, comparando con el indice de <market.returns>,
      # que es la serie con el indice mas reducido hasta el momento
      indice.mas.reducido <- min(length(index(market.returns)),length(index(base_retornos)),
                                 length(index(fdi_growth_base)),length(index(gdp_growth_base)))
      indice_mas_reducido <- index(tail(base_retornos,indice.mas.reducido))
      if(length(index(Lags))<length(index(market.returns))) indice_mas_reducido <- index(Lags)
    }
  }
}

## Loop para obtener las matrices de rezagos para cada <indice>
if(!is.null(dia.inicial)){
  for(indice in indexes){
    var_name <- paste0("lags_",indice)
    Lags     <- lag_function(base_retornos,indice,AR.m=20, MA.m=0, d=0, bool=TRUE, metodo="CSS",dia.inicial)
    assign(var_name,Lags)
  }
}else{
  # Se reemplaza el ciclo for con un lapply
  lags.database <- lapply(indexes, function(x){
    lags.temp <- lag_function(base_retornos, x, AR.m=20, MA.m=0, d=0, bool=TRUE, metodo="CSS",dia.inicial)
    lags.temp <- lags.temp[complete.cases(lags.temp)]
    return(lags.temp)
  })
  names(lags.database) <- indexes
  
  # Ahora tenemos que generar el indice comun entre todas las bases de datos. Para eso, primero tenemos que comprobar 
  # que todas tengan la misma fecha en el ultimo dato, ya que nunca se manipulo el fin de la serie
  last.index <- as.Date(unlist(lapply(lags.database, function(x) as.character(tail(index(x),1)))))
  if(all(last.index == last.index[1]) == F) stop('Hay mas datos en una base de rezagos.')
  # Si no se detiene el codigo, se pasa a revisar con las demas bases que se tienen
  flag <- identical(unique(last.index),tail(index(base_retornos),1), tail(index(market.returns),1),
                    tail(index(fdi_growth_base),1),tail(index(gdp_growth_base),1))
  if(flag == F) stop('Alguna base tiene mas datos al final de la serie')
  
  # Ya una vez comprobado que la ultima fecha es la misma para todas las bases, se reducen las bases segun aquella
  # que tenga menos datos
  indice.mas.reducido <- min(unlist(lapply(lags.database, function(x) length(index(x)))), length(index(market.returns)),
                             length(index(base_retornos)),length(index(fdi_growth_base)),length(index(gdp_growth_base)))
  # Como todas las bases tienen el mismo dato al final, se puede seleccionar el indice de <base_retornos> para los ultimos
  # <indice.mas.reducido> datos
  fechas.mas.reducidas <- tail(index(base_retornos), indice.mas.reducido)
}

# Por ultimo, si <dia.inicial> es NULL se reducen las bases de acuerdo con <fechas.mas.reducidas>
if(is.null(dia.inicial)){
  Retornos        <- base_retornos[fechas.mas.reducidas,]
  market.returns  <- market.returns[fechas.mas.reducidas,]
  Crecimiento_PIB <- gdp_growth_base[fechas.mas.reducidas,]
  Crecimiento_FDI <- fdi_growth_base[fechas.mas.reducidas,]
  lags.database   <- lapply(lags.database, function(x) x[fechas.mas.reducidas,])
}

# Dummies por pais --------------------------------------------------------

# Corremos la función <dummies_por_pais> sobre el archivo que contiene las fechas de las dummies
# El archivo excel al cual se hace referencia enseguida tiene 5 hojas: una por cada tipo de desastre (Biological, Climatological, Geophysical, Hydrological y 
# meteorological). En cada una de las hojas tenemos cuatro columnas:
#                  <t0>       : corresponde al dia en el que sucedio un desastre
#                  <na.start> : dummy que toma el valor de 1 si se supuso que el dia del evento fue el primer dia del mes y 0 en otro caso
#                  <end>      : corresponde al ultimo dia del desastre
#                  <na.end>   : dummy que toma el valor de 1 si se supuso que el ultimo dia del evento fue el ultimo dia del mes y 0 en otro caso
#                  <Total.Affected> : total afectados por el desastre
#                  <Country>        : pais donde sucedio el desastre 
if(!bool_paper){
  dummies <- dummies_por_pais(excel_file=paste0(Dir,"emdata_dummies_arregladas.xlsx"), 
                            Retornos, no.rezagos=no.rezagos.de.desastres) 
}else{
  dummies <- dummies_por_pais(excel_file=paste0(Dir,"emdata_dummies_cds.xlsx"), 
                            Retornos, no.rezagos=no.rezagos.de.desastres) 
}

# Por construccion de la funcion <dummies>, el nombre de cada columna de <dummies> incluye el tipo de desastre,
# seguido de underscore, luego el nombre del pais donde sucedio el desastre, seguido de underscore, y por ultimo, el dia
# relativo a la ventana de evento, donde t0 es el dia del desastre (o D para la dummy de toda la ventana de evento )
# Por tanto, podemos obtener los tipos de desastres usando el nombre de columnas de <dummies>
Tipos.Desastres <- unique(unlist(purrr::map(strsplit(colnames(dummies),'_'), ~.x[1])))

# Viendo la base de datos de <dummies> vemos que las variables D dependen del tipo de desastre, mientras que en la regresion
# a estimar esta agregado, es decir solamente depende del pais, y ya no del tipo de desastre. Para poder lograrlo
# tenemos que crear una variable dummy por cada pais, que tome el valor de 1 en todos los dias que cualquiera de las 3 variables D
# (geophysical, hydrological y meteorological) sea 1, y 0 en otro caso
# Primero obtenemos todas las variables que terminen en _D y creamos un dataframe solo para ellas
colnames.D <- colnames(dummies)[grep('_D$', colnames(dummies))]
dummies.D  <- dummies[,colnames.D]

# Generamos el xts del resto de variables
colnames.not.D <- setdiff(colnames(dummies),colnames.D)
dummies.not.D  <- dummies[,colnames.not.D] 

# Calculamos la variable D para cada pais, sin importar el tipo de desastre
dummies.D.country <- lapply(countries, function(x){
  dummies.temp             <- dummies.D[,grep(x, colnames(dummies.D))]
  dummies.temp.D           <- (rowSums(dummies.temp)!=0) +0
  dummies.temp.D           <- xts(dummies.temp.D, order.by = index(dummies.temp))
  colnames(dummies.temp.D) <- paste0(x,'_D')
  return(dummies.temp.D)
})
dummies.D.country <- do.call(merge, dummies.D.country)

# Realizar la interaccion entre la variable dummy D y la variable Rmt
dummies.interaction <- as.xts(apply(dummies.D.country, MARGIN = 2, function(x) x * as.numeric(market.returns)),
                              order.by = index(dummies.D.country))
colnames(dummies.interaction) <- paste0(unlist(purrr::map(str_split(colnames(dummies.interaction),'_'), ~.x[1])),'_Interaction')

# Se juntan las dos bases de dummies
dummies.final <- merge(dummies.not.D, dummies.interaction)

# Base de datos con todas las variables usadas en la estimacion -----------

# El siguiente codigo junta las bases de datos principales en una sola. De este modo tenemos que <base_datos> tiene el mismo numero
# de filas que <Retornos>, y sus columnas son todas las series de indices bursatiles que se encuentran en <Retornos>, <market.returns>,
# las variables dummies, las serie de crecimiento de producto interno bruto y del indice de desarrollo financiero para todos los paises de <countries>
base_datos <- merge(Retornos, market.returns, dummies.final, do.call(cbind, lags.database),
                    Crecimiento_PIB, Crecimiento_FDI)

# Estimacion del modelo SUR -----------------------------------------------

### Realizar for loop a lo largo de todos los paises para obtener las ecuaciones a estimar. 
# Tambien a lo largo de los 5 tipos de desastres

# if(0) dado que se utilizo el comando <save> para guardar los modelos. Los elementos guardados seran <models_disasters_list> y 
# <resid_disasters_list>, que incluyen por un lado los modelos estimados y por el otro los residuales.
# ----COLOCAR <if(1)> SI SE DESEA ESTIMAR EL MODELO ----#
load.SUR  = 0           #<<<<-- 1 si se carga el SUR inicial, 0 si se corre y salva el SUR inicial
if(bool_cds){tipo.serie <- 'cds'}else{tipo.serie <- 'indices'}
if(promedio.movil){market <- 'PM' }else{market <- 'benchmark'}
if(!load.SUR){
  # Creamos una lista para guardar el sistema de ecuaciones
  eqsystem <- list()
  # Tenemos una ecuacion para cada serie en <indexes>
  for(k in seq_along(indexes)){
    # Establecer la serie
    serie.k <- indexes[k]
    # Establecer el pais relacionado con la serie para obtener las variables exogenas
    pais  <- reverse.matching(serie.k, bool.cds = bool_cds, bool.paper = bool_paper)
    # Guardar nombres de columnas que corresponden a <serie.k>
    variables.por.serie  <- colnames(base_datos)[grep(serie.k, colnames(base_datos))]
    # Guardar nombres de columnas que corresponden a <pais>
    variables.por.pais   <- colnames(base_datos)[grep(pais, colnames(base_datos))]
    # Guardar todas las variables en un solo vector
    variables.serie.pais <- unique(c(variables.por.serie,variables.por.pais))
    # Excluir a <serie.k> ya que es la variable endongena
    variables.exogenas   <- variables.serie.pais[variables.serie.pais != serie.k]
    # Por ultimo, falta agregar el retorno de mercado
    variables.exogenas   <- c('market.returns', variables.exogenas)
    # Generar la formula para <serie.k>
    eqsystem[[serie.k]]  <- as.formula(paste0(serie.k, ' ~ ' ,paste(variables.exogenas, collapse = ' + ')))
  }
  estimated.sur <- systemfit(eqsystem, data = base_datos, method = 'SUR')
  save(estimated.sur, file = paste0(getwd(), '/Resultados_SUR/Nuevo_SUR/Resultado_SUR_',tipo.serie,'_',market,'.RData'))
}

# Guardar base de datos necesaria para el analisis de Tommaso (2023) --------
base_Tommaso <- merge(base_retornos,market.returns,gdp_growth_base,fdi_growth_base)
save(base_Tommaso, file = paste0(getwd(),'/Bases/Procesado/Base_Tommaso_',tipo.serie,'_rm_',market,'.RData'))