# El siguiente codigo es para transformar los indices para el paper, dando un dataframe
# con datos de 11 indices desde el 5 de junio del 2006 al 8 de agosto del 2023
# Para correr este codigo es mejor haber corrido antes <Replicacion_climate_change.R> desde la primera
# linea hasta la linea 58.

# Carga de las bases ------------------------------------------------------

# Directorio donde se encuentran los csv de los indices para el paper
directory <- paste0(getwd(),'/Bases/Indices_Paper')
# Vector con todos los nombres de los archivos que sean csv
archivos.csv <- list.files(directory, pattern='\\.csv$',full.names = T)
# Leer todos los archivos y guardarlos en una lista de dataframes usando la funcion <purrr::map>
lista.archivos <- purrr::map(archivos.csv,read_csv)
# Extraer el nombre de los archivos y nombrar los elementos de la lista
names(lista.archivos) <- tools::file_path_sans_ext(basename(archivos.csv))

# La serie de Colombia y de Malasia estan en otro directorio
directorio.colombia       <- paste0(getwd(),'/Bases/Indices_Colombia')
archivos.colombia         <- list.files(directorio.colombia, pattern='\\.csv$',full.names = T)
lista.archivos.col        <- purrr::map(archivos.colombia,read_csv)
names(lista.archivos.col) <- tools::file_path_sans_ext(basename(archivos.colombia))

directorio.malasia        <- paste0(getwd(),'/Bases/Indices_Malaysia')
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
IGBVL$Date <- sort(as.Date(IGBVL$Date,format='%d%b%y'),decreasing = T)


# Manejo MSCI -------------------------------------------------------------

# La diferencia que tiene MSCI es que no cuenta con una columna <Price>, sino, Open, Close, High y Low. Sin embargo, 
# al contrastar la informacion del MSCI EM con la pagina investing.com (que es de donde salieron las demas), se observa que
# <Close> corresponde a <Price>, por lo cual simplemente se cambia el nombre de <Close> a <Price> para esta base
MSCI <- lista.archivos$MSCI %>% 
  rename('Price'=Close,'Fecha'=Date) %>% 
  mutate('Date'=as.Date(Fecha, format = "%m/%d/%y")) %>% 
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

igbc <- lista.archivos.col$IGBC %>% 
  dplyr::select('Date'=`Fecha (dd/mm/aaaa)`,'Price'=`Valor IGBC`,'Variacion'=`Variación porcentual`) %>% 
  mutate('Date' = as.Date(Date,format='%d/%m/%Y')) %>% 
  dplyr::filter(!is.na(Date)) %>% 
  mutate('Variacion' =as.numeric(sub('%','',Variacion)))
# Hay un error en la base de datos del banco, por lo que se corrige en la siguiente linea
igbc$Variacion[1605] <- ((igbc$Price[1605]-igbc$Price[1604])/igbc$Price[1604])*100

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

# La base de datos de Malasia solo pudo ser descargada por intervalos de 1 anho calendario, por lo cual se 
# tienen muchas bases en <lista.archivos.mal>. Lo mas importante es juntar todas estas bases
KLCI <- do.call(rbind, lista.archivos.mal)
KLCI$Date<-as.Date(KLCI$Date,format='%m/%d/%Y')

# Por la manera en que se descargaron los datos, se tienen dias duplicados, por lo que solamente se 
# utilizan las filas unicas por 'Date'
KLCI <- KLCI %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  dplyr::select(Date,'Price'=Open)

lista.archivos.arreglados[['KLCI']] <- KLCI

# Conversion a xts --------------------------------------------------------

lista.archivos.xts <- lapply(lista.archivos.arreglados, function(x) xts(x$Price, order.by = x$Date))
# Ahora si podemos agregar el indice colcap
if(0) lista.archivos.xts[['COLCAP']] <- colcap.completo$Price #No se corre porque se cambio el modo de completar el colcap
if(1) lista.archivos.xts[['COLCAP']] <- colcap.completo

# Juntar todas las series de tiempo en una base de datos
dataframe.xts <- do.call(merge,lista.archivos.xts)
colnames(dataframe.xts) <- names(lista.archivos.xts) # Para conservar los mismos nombres en la lista

# Recortar la base desde '2006-06-05', ya que no hay datos para Colombia y no es posible interpolar
# Para eso se puede hacer con codigo, el cual busca el dia minimo donde todas las series ya podrian
# tener datos
dia.min <- as.Date(max(unlist(lapply(lista.archivos.xts, function(x) head(index(x),1)))))

dataframe.xts <- dataframe.xts[index(dataframe.xts) >= dia.min]

# Guardar la base de datos ------------------------------------------------
# Guardar en el directorio de bases Dir, la base de datos ya transformada
Dir  = paste0(getwd(),'/Bases/')
# Para poder guardarlo es necesario transformarlo en objecto <data.frame>
dataframe.paper           <- cbind(index(dataframe.xts),as.data.frame(coredata(dataframe.xts)))
colnames(dataframe.paper) <- c('Date',colnames(dataframe.xts))
write_xlsx(dataframe.paper, path= paste0(Dir,'Stocks_Paper.xlsx'))