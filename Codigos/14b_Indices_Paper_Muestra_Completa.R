# El siguiente codigo es para transformar los indices para el paper, dando un dataframe
# con datos de 11 indices desde el 5 de junio del 2006 al 8 de agosto del 2023
# Para correr este codigo es mejor haber corrido antes <Replicacion_climate_change.R> desde la primera
# linea hasta la linea 58.

# Carga de las bases ------------------------------------------------------

# Directorio donde se encuentran los csv de los indices para el paper
directory <- paste0(getwd(),'/Bases/Indices_Muestra_Completa/Completos')
# Vector con todos los nombres de los archivos que sean csv
archivos.csv <- list.files(directory, pattern='\\.csv$',full.names = T)
# Leer todos los archivos y guardarlos en una lista de dataframes usando la funcion <purrr::map>
lista.archivos <- purrr::map(archivos.csv,read_csv)
# Extraer el nombre de los archivos y nombrar los elementos de la lista
names(lista.archivos) <- tools::file_path_sans_ext(basename(archivos.csv))

# La serie de Colombia y de Malasia estan en otro directorio
directorio.colombia       <- paste0(getwd(),'/Bases/Indices_Muestra_Completa/Colombia')
archivos.colombia         <- list.files(directorio.colombia, pattern='\\.csv$',full.names = T)
lista.archivos.col        <- purrr::map(archivos.colombia,read_csv)
names(lista.archivos.col) <- tools::file_path_sans_ext(basename(archivos.colombia))

directorio.malasia        <- paste0(getwd(),'/Bases/Indices_Muestra_Completa/Malaysia')
archivos.malasia          <- list.files(directorio.malasia, pattern='\\.csv$',full.names = T)  
lista.archivos.mal        <- purrr::map(archivos.malasia,read_csv)
names(lista.archivos.mal) <- tools::file_path_sans_ext(basename(archivos.malasia))

# Manejo de base de Peru --------------------------------------------------

# Todas las bases de <lista.archivos> tienen el mismo formato a excepcion de <lista.archivos$IGBVL>, la cual fue extraida de una 
# fuente distinta, por lo que es necesario dejarla en el mismo formato que las demas

# Eliminar datos <NA> y renombrar las columnas para seguir el mismo formato de los demas dataframes
IGBVL <- lista.archivos$IGBVL %>% 
  mutate('Price' = as.numeric(PD38026MD)) %>% 
  dplyr::select('Date'=...1,Price) %>% 
  dplyr::filter(!is.na(Price)) 

# El formato fecha es distinto, ya que es <ddmonthyy> por ejemplo <02Ene06>. Es necesario
# cambiar los meses que estan en abreviacion espanhol a abreviacion ingles
month.translation <- c("Ene" = "Jan", "Abr" = "Apr", "Ago" = "Aug",'Set'= 'Sep', "Dic" = "Dec")
for (month_abbrev in names(month.translation)) {
  IGBVL$Date <- gsub(month_abbrev, month.translation[month_abbrev], IGBVL$Date)
}

# Arreglar el formato de las fechas
IGBVL$Date <- sort(as.Date(IGBVL$Date,format='%d%b%y'),decreasing = F)

# Manejo MSCI -------------------------------------------------------------

# La diferencia que tiene MSCI es que no cuenta con una columna <Price>, sino, Open, Close, High y Low. Sin embargo, 
# al contrastar la informacion del MSCI EM con la pagina investing.com (que es de donde salieron las demas), se observa que
# <Close> corresponde a <Price>, por lo cual simplemente se cambia el nombre de <Close> a <Price> para esta base
MSCI <- lista.archivos$MSCI %>% 
  rename('Price'=Close,'Fecha'=Date) %>% 
  mutate('Date'=as.Date(Fecha, format = "%m/%d/%Y")) %>% 
  dplyr::select(Date,Price)

# Manejo de las demas bases  ----------------------------------------------
# Para poder arreglar el formato de las demas, es necesario quitar el <IGBVL> de la lista
lista.archivos.arreglados       <- lista.archivos
lista.archivos.arreglados$IGBVL <- NULL
lista.archivos.arreglados$MSCI  <- NULL
# Todos los indices  que quedan en <lista.archivos> tienen el mismo formato, pero toca arreglar la fecha
# y usar solamente la columna <Price>
lista.archivos.arreglados <- lapply(lista.archivos.arreglados, function(x){
  x <- x %>%
    dplyr::mutate('Date' = as.Date(Date,format='%m/%d/%Y')) %>%
    dplyr::select(Date,Price)
  return(x)
})

lista.archivos.arreglados[['IGBVL']] <- IGBVL
lista.archivos.arreglados[['MSCI']]  <- MSCI

# Manejo de la base de Colombia -------------------------------------------

colcap <- lista.archivos.col$COLCAP %>% 
  dplyr::select('Date'=`Fecha (dd/mm/aaaa)`,'Price'=`Valor COLCAP`,'Variacion'=`Variación porcentual (%)`) %>% 
  mutate('Date' = as.Date(Date,format='%d/%m/%Y'))

# Arreglar las fechas del igbc
igbc <- lista.archivos.col$IGBC %>% 
  dplyr::select('Date'=Fecha,'Price'=Valor) %>% 
  separate(Date, into = c('Day','Month','Year'), sep = '/', convert = TRUE) %>% 
  # Algunos años de <Year> estan sin 2000, por ejemplo 9 hace referencia a 2009, 10 a 2010...
  mutate(Year = ifelse(Year < 2000, Year+2000, Year)) %>% 
  # Ya podemos juntar <Day> <Month> y <Year> en formato fecha
  unite(Date, Year, Month, Day, sep = '-') %>% 
  mutate(Date = as.Date(Date))

if(0){
  indice.igbc <- igbc$Date[igbc$Date >= colcap$Date[1]]
  indice.colcap <- colcap$Date[colcap$Date <= igbc$Date[nrow(igbc)]]
  # El <indice.colcap> y el <indice.igbc> son practicamente identicos, menos por un dato, el 20 de abril del 
  # 2012, para el cual no hay dato para el colcap.
  
  colcap.xts <- xts(colcap[colnames(colcap)!='Date'],order.by=colcap$Date)
  igbc.xts   <- xts(igbc[colnames(igbc)!='Date'] ,order.by = igbc$Date)
  
  # Para hacer una regresion de las variaciones, necesitamos los datos de las fechas cuando hay datos para ambos
  datos.igbc   <- igbc.xts[index(igbc.xts) %in% indice.colcap]
  datos.colcap <- colcap.xts[index(colcap.xts) %in% indice.colcap]
  
  y <- as.numeric(datos.colcap$Variacion)
  x <- as.numeric(datos.igbc$Variacion)
  
  modelo <- lm(y~x)
  
  # Ahora con los coeficientes se puede estimar la tasa de crecimiento del colcap, usando los datos del igbc
  crecimiento.colcap <- as.numeric(cbind(1,igbc.xts$Variacion[index(igbc.xts) < index(colcap.xts)[1]]) %*% coef(modelo))
  crecimiento.colcap.xts <- xts(crecimiento.colcap,order.by = index(igbc.xts)[index(igbc.xts) < index(colcap.xts)[1]])
  
  crecimiento.colcap.xts <- cbind(NA,crecimiento.colcap.xts)
  colnames(crecimiento.colcap.xts) <- colnames(colcap.xts)
  
  colcap.completo <- rbind(crecimiento.colcap.xts,colcap.xts)
}

# Encontrar la primera fecha que hay de colcap que traslape con igbc
igbc.xts   <- xts(igbc$Price,order.by=igbc$Date)
colcap.xts <- xts(colcap$Price,order.by=colcap$Date)

# Calcular las tasas de crecimiento del IGBC para las fechas anteriores al dia de traslape
igbc.pre.colcap       <- igbc.xts[index(igbc.xts)<=index(colcap.xts)[1]]
igbc.tasa.crecimiento <- xts((diff(igbc.pre.colcap) / lag(igbc.pre.colcap, default = 1) * 100), order.by = index(igbc.pre.colcap))

# Calcular los niveles de COLCAP despues de tener los datos de variacion de IGBC

colcap.completo <- xts(c(rep(NA,(nrow(igbc.tasa.crecimiento)-1)),coredata(colcap.xts)),order.by = c(index(igbc.tasa.crecimiento),index(colcap.xts)[2:length(index(colcap.xts))]))
for(i in sum(is.na(colcap.completo)):1) colcap.completo[i] <- colcap.completo[i+1]/(1+(igbc.tasa.crecimiento[1+i]/100))
colnames(colcap.completo) <- c('Price')
if(0){
  # Por ultimo, es necesario llenar los datos faltantes en el indice COLCAP
  for(i in sum(is.na(colcap.completo$Price)):1)
    colcap.completo$Price[i] <- colcap.completo$Price[i+1]/(1+(colcap.completo$Variacion[i+1]/100))
  # <colcap.completo> ya es formato xts por lo que no se agrega a <lista.archivos.arreglados>
}
# Manejo base de Malasia --------------------------------------------------

# Ahora se realiza a través de la API de yahoo finance, usando libreria <quantmod> 
if(0){
  # La base de datos de Malasia solo pudo ser descargada por intervalos de 1 anho calendario, por lo cual se 
  # tienen muchas bases en <lista.archivos.mal>. Lo mas importante es juntar todas estas bases
  KLCI <- do.call(rbind, lista.archivos.mal)
  KLCI$Date<-as.Date(KLCI$Date,format='%m/%d/%Y')
  
  # Por la manera en que se descargaron los datos, se tienen dias duplicados, por lo que solamente se 
  # utilizan las filas unicas por 'Date'
  KLCI <- KLCI %>% 
    distinct(Date, .keep_all = TRUE) %>% 
    dplyr::select(Date,'Price'=Open)
}
getSymbols('^KLSE',src='yahoo',from='2004-10-07',to='2022-08-10') # Carga KLSE, que es el FTSE Bursa Malaysia KLCI
# Nos interesa el precio de cierre
KLCI           <- data.frame(cbind(index(KLSE),coredata(KLSE$KLSE.Close)))
colnames(KLCI) <- c('Date','Price')
KLCI$Date <- as.Date(KLCI$Date)
lista.archivos.arreglados[['KLCI']] <- KLCI


# Conversion a xts --------------------------------------------------------

lista.archivos.xts <- lapply(lista.archivos.arreglados, function(x) xts(x$Price, order.by = x$Date))
# Ahora si podemos agregar el indice colcap
if(0) lista.archivos.xts[['COLCAP']] <- colcap.completo$Price #No se corre porque se cambio el modo de completar el colcap
if(1) lista.archivos.xts[['COLCAP']] <- colcap.completo

# Juntar todas las series de tiempo en una base de datos
dataframe.xts <- do.call(merge,lista.archivos.xts)
colnames(dataframe.xts) <- names(lista.archivos.xts) # Para conservar los mismos nombres en la lista

# Recortar la base desde el dia minimo en que se tienen datos
# Para eso se puede hacer con codigo, el cual busca el dia minimo donde todas las series ya podrian
# tener datos
dia.min <- as.Date(max(unlist(lapply(lista.archivos.xts, function(x) head(index(x),1)))))
# Recortar la base hasta el dia maximo en que todas tienen datos
dia.max <- as.Date(min(unlist(lapply(lista.archivos.xts, function(x) tail(index(x),1)))))

dataframe.xts <- dataframe.xts[between(index(dataframe.xts),dia.min,dia.max)]

# Revisar festivos, es necesario hacerlo de manera separada, ya que el calendario de festivos es distinto para los
# paises. 

# Cargar los calendarios de business days, faltan los de Colombia, Peru, Chile y Malasia
load_quantlib_calendars(c('Brazil','China','Indonesia','SouthKorea','Mexico','SouthAfrica','Turkey'),
                        from = index(dataframe.xts)[1],to = tail(index(dataframe.xts),1))
dataframe.xts[!is.bizday(index(dataframe.xts),'QuantLib/Brazil'),'Bovespa']       <- NA
dataframe.xts[!is.bizday(index(dataframe.xts),'QuantLib/China'),'FTSE_China_A50'] <- NA
dataframe.xts[!is.bizday(index(dataframe.xts),'QuantLib/Indonesia'),'Jakarta_Stock_Exchange_Composite_Index'] <- NA
dataframe.xts[!is.bizday(index(dataframe.xts),'QuantLib/SouthKorea'),'KOSPI'] <- NA
dataframe.xts[!is.bizday(index(dataframe.xts),'QuantLib/Mexico'),'S&P_BMV_IPC'] <- NA
dataframe.xts[!is.bizday(index(dataframe.xts),'QuantLib/SouthAfrica'),'South_Africa_Top_40'] <- NA
dataframe.xts[!is.bizday(index(dataframe.xts),'QuantLib/Turkey'),'BIST_100'] <- NA

# Falta comprobar Peru, Malasia, Chile y Colombia, usando el siguiente calendario https://www.public-holidays.us/
fechas.peru <- c('2005-03-24','2005-03-25','2006-04-13','2006-04-14','2007-04-05','2007-04-06','2008-03-20','2008-03-21',
                 '2009-04-09','2009-04-10','2010-04-01','2010-04-02','2011-04-21','2011-04-22','2012-04-05','2012-04-06',
                 '2013-03-28','2013-03-29','2014-04-17','2014-04-18','2015-04-02','2015-04-03','2016-03-24','2016-03-25',
                 '2017-04-13','2017-04-14','2018-03-29','2018-03-30','2019-04-18','2019-04-19','2020-04-09','2020-04-10',
                 '2021-04-01','2021-04-02','2022-04-14','2022-04-15',c(paste0(as.character(2004:2022),'-01-01')),
                 (paste0(as.character(2004:2022),'-05-01')),(paste0(as.character(2004:2022),'-06-29')),
                 (paste0(as.character(2004:2022),'-07-28')),(paste0(as.character(2004:2022),'-07-29')),
                 (paste0(as.character(2004:2022),'-08-30')),(paste0(as.character(2004:2022),'-10-08')),
                 (paste0(as.character(2004:2022),'-11-01')),(paste0(as.character(2004:2022),'-12-08')),
                 (paste0(as.character(2004:2022),'-12-25')))
dataframe.xts[fechas.peru,'IGBVL'] <- NA

fechas.colombia <- c((paste0(as.character(2004:2022),'-01-01')),(paste0(as.character(2004:2022),'-05-01')),
                     (paste0(as.character(2004:2022),'-07-20')),(paste0(as.character(2004:2022),'-08-07')),
                     (paste0(as.character(2004:2022),'-12-08')),(paste0(as.character(2004:2022),'-12-25')),
                     paste0('2004-',c('01-12','03-22','04-08','04-09','04-11','05-24','06-14','06-21','07-05','08-16','10-18','11-01','11-15')),
                     paste0('2005-',c('01-10','03-21','03-24','03-25','03-27','05-09','05-30','06-06','07-04','08-15','10-17','11-07','11-14')),
                     paste0('2006-',c('01-09','03-20','04-13','04-14','04-16','05-29','06-19','06-26','07-03','08-21','10-16','11-06','11-13')),
                     paste0('2007-',c('01-08','03-19','04-05','04-06','04-08','05-21','06-11','06-18','07-02','08-20','10-15','11-05','11-12')),
                     paste0('2008-',c('01-07','03-20','03-21','03-23','03-24','05-05','05-26','06-02','06-30','08-18','10-13','11-03','11-17')),
                     paste0('2009-',c('01-12','03-23','04-09','04-10','04-12','05-25','06-15','06-22','06-29','08-17','10-12','11-02','11-16')),
                     paste0('2010-',c('01-11','03-22','04-01','04-02','04-04','05-17','06-07','06-14','07-05','08-16','10-18','11-01','11-15')),
                     paste0('2011-',c('01-10','03-21','04-21','04-22','04-24','06-06','06-27','07-04','08-15','10-17','11-07','11-14')),
                     paste0('2012-',c('01-09','03-19','04-05','04-06','04-08','05-21','06-11','06-18','07-02','08-20','10-15','11-05','11-12')),
                     paste0('2013-',c('01-07','03-25','03-28','03-29','03-31','05-13','06-03','06-10','07-01','08-19','10-14','11-04','11-11')),
                     paste0('2014-',c('01-06','03-24','04-17','04-18','04-20','06-02','06-23','06-30','08-18','10-13','11-03','11-17')),
                     paste0('2015-',c('01-12','03-23','04-02','04-03','04-05','05-18','06-08','06-15','06-29','08-17','10-12','11-02','11-16')),
                     paste0('2016-',c('01-11','03-21','03-24','03-25','03-27','05-09','05-30','06-06','07-04','08-15','10-17','11-07','11-14')),
                     paste0('2017-',c('01-09','03-20','04-13','04-14','04-16','05-29','06-19','06-26','07-03','08-21','10-16','11-06','11-13')),
                     paste0('2018-',c('01-08','03-19','03-29','03-30','04-01','05-14','06-04','06-11','07-02','08-20','10-15','11-05','11-12')),
                     paste0('2019-',c('01-07','03-25','04-18','04-19','04-21','06-03','06-24','07-01','08-19','10-14','11-04','11-11')),
                     paste0('2020-',c('01-06','03-23','04-09','04-10','04-12','05-25','06-15','06-22','06-29','08-17','10-12','11-02','11-16')),
                     paste0('2021-',c('01-11','03-22','04-01','04-02','04-04','05-17','06-07','06-14','07-05','08-16','10-18','11-01','11-15')),
                     paste0('2022-',c('01-10','03-21','04-14','04-15','04-17','05-30','06-20','06-27','07-04','08-15','10-17','11-07','11-14')))
dataframe.xts[fechas.colombia,'COLCAP'] <- NA

# Revisando dia por dia, Chile y Turquia tienen NA para todos los dias festivos en la muestra

# Guardar la base de datos ------------------------------------------------
# Guardar en el directorio de bases Dir, la base de datos ya transformada
Dir  = paste0(getwd(),'/Bases/')
# Para poder guardarlo es necesario transformarlo en objecto <data.frame>
dataframe.paper           <- cbind(index(dataframe.xts),as.data.frame(coredata(dataframe.xts)))
colnames(dataframe.paper) <- c('Date',colnames(dataframe.xts))
write_xlsx(dataframe.paper, path= paste0(Dir,'Stocks_Paper_Completos.xlsx'))
