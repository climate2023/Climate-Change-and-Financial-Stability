# Para el análisis de Pagnottoni (2022) se utilizaba una base con la muestra reducida a <dia.inicial>, lo 
# cual no se hace para Tommaso (2023), ya que necesitamos la mayor cantidad posible de datos antes de 
# <dia.inicial>. 
# En <base_Tommaso> estan las series que van a servir como variables dependientes, un indice de mercado,
# y las variables exogenas del modelo.
base_Tommaso <- merge(base_retornos,market.returns,gdp_growth_base,fdi_growth_base)
# Por construccion, <base_Tommaso> tiene dos columnas para Estados Unidos para gdp y fdi: <gdp_USA1>/<gdp_USA2> y <fdi_USA1>/<fdi_USA2>
# Dejar solamente una columna para gdp y una para fdi que se llamen <gdp_USA> y <fdi_USA>
base_Tommaso <- base_Tommaso[,!colnames(base_Tommaso) %in% c('gdp_USA2','fdi_USA2')]
colnames(base_Tommaso) <- gsub("USA1$", "USA", colnames(base_Tommaso))
# Base de eventos ---------------------------------------------------------

# Si <unico_pais> es NULL, se obtiene una base de datos con los eventos de todos los paises de <countries>
# En caso de querer analizar solamente un pais, se escribe en <unico_pais>
unico_pais <- NULL

# Lectura de y filtros de la base de eventos <emdat_completa>. 
if(1){
  # Lectura de la base de datos <EMDAT>,  en excel, se dejaron los desastres entre el 8-feb-2001 y 31-dic-2019 (fechas usadas en el paper).
  # Para los eventos que corresponden a CDS se dejaron del primer trimestre de 2004 al tercero del 2022
  if(!bool_paper){
    emdat_completa <- openxlsx::read.xlsx(paste0(Dir,"EMDAT_Mapamundi.xlsx"),sheet = "Mapamundi") #<<<--- base de datos 
  }else{
    emdat_completa <- openxlsx::read.xlsx(paste0(Dir,'EMDAT_CDS_ORIGINAL.xlsx'),sheet='emdat data') #<<<--- base de datos 
  }
  # Correccion del nombre de algunos paises en <emdat_base>
  emdat_completa <- emdat_completa %>% 
    mutate(Country = case_when(
      Country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "UnitedKingdom",
      Country == "United States of America (the)" ~ "USA",
      Country == "Hong Kong" ~ "HongKong",
      Country == "Netherlands (the)" ~ "Netherlands",
      Country == "Russian Federation (the)" ~ "Russia",
      Country == "South Africa" ~ "SouthAfrica",
      Country == "Korea (the Republic of)" ~ "SouthKorea",
      TRUE ~ Country
    ))
  # Se seleccionan las columnas de interes
  if(0){
    #  <Disaster Subgroup>: uno de los cinco subgrupos de desastres: meteorologico, geofisico, hidrologico, climatologico, extraterrestrial
    #  <Disaster Type>: tipo de desastre
    #  <Disaster Subtype>: subtipo del desastre
    #  <Country>: pais donde sucedio el desastre
    #  <Start Year>: año en que inicio el desastre
    #  <Start Month>: mes en que inicio el desastre
    #  <Start Day>: dia en que inicio el desastre
    #  <End Year>: año en que termino el desastre
    #  <End Month>: mes en que termino el desastre
    #  <End Day>: dia en que termino el desastre
    #  <Total Deaths>: total de muertes 
    #  <No Injured>: numero de heridos
    #  <No Affected>: numero de afectados
    #  <No Homeless>: numero de personas cuya casa fue destruida
    #  <Total Affected>: total de afectados
    #  <Damages>: total de daños totales en miles de dolares ajustados al 2021
  }
  emdat_base <- emdat_completa %>% 
    dplyr::select('Disaster.Subgroup','Disaster.Type','Disaster.Subtype','Country','Start.Year','Start.Month','Start.Day','End.Year','End.Month',
                  'End.Day','Total.Deaths','No.Injured','No.Affected','No.Homeless','Total.Affected',
                  Damages = "Total.Damages,.Adjusted.('000.US$)")
  
  # Generacion de <Start.Date> para cada evento  ---------------------------------------------
  
  # Algunas de las fechas tienen NA en el dia, por lo cual se asume que es el primer dia del mes.
  # En estos casos, la variable dummy <na_start> es igual a 1, o.w. es 0.
  if(sum(is.na(emdat_base$Start.Day))!=0){
    warning("Hay dias faltantes en la base de datos! Se va a asumir que el dia de inicio del desastre es el primero del mes")
    # Creacion de la variable <na_start>
    emdat_base <- emdat_base %>%
      mutate(na_start = ifelse(is.na(Start.Day),1,0))
    # Cambiar valores <NA> por el primer dia del mes
    emdat_base <- emdat_base %>% 
      mutate(Start.Day=replace_na(Start.Day,1)) # <replace_na> se utiliza para reemplazar los valores <NA> por <1>
  }
  # Generacion de la fecha completa del inicio de evento, <Start.Date>, 
  # a partir de <Start.Year>, <Start.Month> y <Start.Day>
  emdat_base <- emdat_base %>% 
    unite(Start.Date, c(Start.Year, Start.Month, Start.Day), sep = "-",remove=FALSE) %>% 
    mutate(Start.Date = as.Date(Start.Date))
  
  # Ahora, con el fin de generar una columna con la duracion del desastre, debemos manipular las variables <End.Month> y 
  # <End.Day>, ya que contienen datos faltantes
  # Se va a asumir que a los que les falta el mes de finalizacion sera diciembre, es decir, 12
  emdat_base <- emdat_base %>% 
    mutate(End.Month=replace_na(End.Month,12))
  # Realizamos el mismo procedimieto que cuando habian dias de inicio faltantes
  # Creacion de la variable <na_end>
  emdat_base <- emdat_base %>%
    mutate(na_end = ifelse(is.na(End.Day),1,0))
  # Cambiar valores <NA> por el ultimo dia del mes
  emdat_base <- emdat_base %>% 
    mutate(End.Day = ifelse(is.na(End.Day), 
                            make_date(End.Year, End.Month, 1) %>% ceiling_date(unit = "month") - lubridate::days(1),
                            End.Day))
  emdat_base$End.Day <- ifelse(emdat_base$End.Day > 31, day(as.Date(emdat_base$End.Day)), emdat_base$End.Day)
  # <replace_na> se utiliza para reemplazar los valores <NA> por elultimo dia del mes
  # Ahora que se asumieron los dias finales y los meses finales, se procede a crear la fecha de fin del desastre
  emdat_base <- emdat_base %>% 
    unite(End.Date, c(End.Year, End.Month, End.Day), sep = "-",remove=FALSE) %>% 
    mutate(End.Date = as.Date(End.Date))
  
  # Ahora se crea una columna de duracion del evento, que sera la diferencia entre <End.Date> y <Start.Date>
  emdat_base <- emdat_base %>% 
    mutate(Duracion = as.numeric(End.Date - Start.Date))
  quantile(emdat_base$Duracion, probs = c(seq(from=0.1, to= 0.9, by=0.1),0.95,0.99))
  # Es posible que la duracion en realidad sea menor, ya que se esta asumiendo el ultimo dia del mes para aquellos de los 
  # que no se posee informacion
    
  # Vector con los nombres de paises usados para la generacion de la base de eventos
  if(is.null(unico_pais)){
    paises.usados <- countries 
  }else{
    paises.usados <- unico_pais
  }
  
  # Filtrar la base solo por los paises de <paises.usados>
  emdat_base <- emdat_base %>% 
    dplyr::filter(Country %in% paises.usados)
}

# El codigo anterior genera una base de datos con variables:
#    <Country>    : pais donde sucede el evento
#    <Start.Date> : fecha de inicio del evento

# Descriptivo base de desastres--------------------------------------------
emdat_base %>% group_by(Disaster.Subgroup) %>% 
  summarise('Media Duracion' = mean(Duracion))

# Filtrar la base de eventos para buscar eventos mas significativos -------
emdat_base <- emdat_base %>% 
  dplyr::filter(Total.Deaths >= 1000 | No.Injured >= 1000 | Total.Affected >= 50000 | Damages >= 1000000) %>% 
  dplyr::filter(Disaster.Subgroup %in% c('Geophysical','Hydrological','Meteorological'))
# Lo anterior siguiendo a Gassebner, Keck, Teh

# Agregar rezagos a base de datos -----------------------------------------

# Se usa la funcion <create.lags>, que toma una base de datos junto a unas variables de interes. Retorna la base de datos inicial junto con 
# rezagos de las variables de interes. Si se desea un numero de rezagos en especifico para todas las variables de interes, asignar el numero 
# a <number_lags>. Si se desea un numero de rezagos para cada variable de interes, asignar una lista a <number_lags> con los numeros de 
# rezagos. Nota: Si se coloca la lista tiene que tener el mismo numero de datos que numero de variables de interes.
# Si se desea que se elijan los rezagos siguiendo el criterio de informacion de Akaike, dejar <number_lags> como NULL
number_lags <- NULL
base_lagged <- create.lags(base = base_Tommaso,interest.vars = indexes,no.lags = number_lags,AR.m = 20)
# Parametros event study --------------------------------------------------------------

estimation_windows <- c(200,300,500) #<<<--- No. de dias antes del evento para comenzar la estimacion
for(estimation_start in estimation_windows){
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
  umbral.evento   <- 50 #<<<--- Numero de dias minimo entre cada evento. Lo anterior para que no se traslapen los eventos
  columna.filtrar <- 'Total.Affected' #<<<--- Columna para filtrar la base de eventos 'Total.Affected' o 'Damages'
  eventos.final <- reducir.eventos(umbral = umbral.evento,base=base_lagged,eventos = eventos_filtrado,
                                   col.fecha='Start.Date',col.grupo = 'Country',col.filtro = columna.filtrar)
  
  # -------------------------- Regresion estimation window ---------------------------------------------
  
  # <estimation.event.study> realiza la estimacion por OLS para cada evento en <data.events>. Retorna una lista para cada evento que incluye:
  #     Dataframe      : retornos observados, estimados y anormales para la ventana de estimacion y ventana de evento. 
  #     Standard_error : error estandar de los errores de la estimacion por OLS
  # El objeto de salida de esta funcion sera la base para las pruebas de Wilcoxon y bootstrap
  
  # Otras variables exogenas de una base de datos que se quieren incluir. 
  var_exo <- c("gdp_","fdi_")
  
  load.eventslist <- 0     #<<<<-- 1 si se cargan los datos, 0 si se corre la funcion para estimar 
  if(!load.eventslist){
    all_events_list <- estimation.event.study(bool.paper = bool_paper, bool.cds=bool_cds,base = base_lagged,data.events = eventos.final,market.returns = "market.returns",
                                              max.ar = 15,es.start = estimation_start,es.end = estimation_end,add.exo = TRUE,vars.exo = var_exo,GARCH = "sGARCH",
                                              overlap.events = eventos_filtrado, no.overlap = 4)
    if(bool_cds){serie <- 'CDS'}else{serie <- 'Indices'}
    if(promedio.movil){regresor.mercado <- 'PM'}else{regresor.mercado <- 'benchmark'}
    save(all_events_list, 
         file=paste0(getwd(),'/Resultados_regresion/',serie,'_tra',umbral.evento,'_est',estimation_start,'_media_',regresor.mercado,'.RData'))
  }else{
    load(paste0(getwd(),'/Resultados_regresion/',serie,'_tra',umbral.evento,'_est',estimation_start,'_media_',regresor.mercado,'.RData'))
  } 
}

# Hay elementos en <all_events_list> que son <NA> dado que la estimacion no convergio, por lo que es necesario
# eliminarlos
suppressWarnings(all.events.list <- purrr::discard(all_events_list,is.na))

# Ver cuantos eventos tienen fecha exacta
fecha.exacta        <- round(table(unlist(purrr::map(all.events.list, ~.x@evento$na_start)))/length(all.events.list)*100,2)
names(fecha.exacta) <- c('Tiene fecha exacta','No tiene fecha exacta'); fecha.exacta
# Dejar solamente los eventos que tengan fecha exacta
all.events.list.true <- purrr::keep(all.events.list, ~ .x@evento$na_start == 0)

columna.agrupar <- 'Country' #<<<--- Columna del evento con la cual se quiere separar la lista <all.events.list.true>
                                       # 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
# Separar la lista dependiendo de una columna en especifico introducida por el usuario 
lista.separada <- split(all.events.list.true, sapply(all.events.list.true, function(x) x@evento[[columna.agrupar]]))

# Generar listas distintas para cada valor de la <columna.agrupar>, en caso de querer utilizarlas mas adelante
for(i in seq_along(lista.separada)) assign(paste0("list.", names(lista.separada)[i]), lista.separada[[i]])

# Se observo que generalmente, el dia del evento el retorno es positivo, pero en adelante es negativo. Falta explicar porque podria
# ser que el retorno en el dia del evento sea positivo,mientras que para el resto de la ventana de evento es negativo.
# Por lo anterior, el CAR va a ser menor si se elimina el dia del evento de la ventana de evento y solo se miran los dias posteriores
inicio.ventana.evento <- 1 #<<<--- indica en que dia comenzara la ventana de evento <0> si se desea que inicie el dia del desastre, 
                           # 1 si se desea el dia siguiente, 2 si se desea 2 dias despues ...

# Graficas CAR ------------------------------------------------------------
for(i in seq_along(lista.separada)){
  element <- lista.separada[[i]]
  name    <- names(lista.separada)[i]  
  grafico_car(element,length_estimation_window,length_event_window,inicio.ventana.evento)
  title(name,line=0.75)
}
grafico_car(all.events.list.true,length_estimation_window,length_event_window,inicio.ventana.evento)

# Wilcoxon --------------------------------------------------------------

# Usamos la funcion <wilcoxon.jp.test> para realizar la prueba de Wilcoxon asociada a los <CAR> 
# de los eventos. Obtenemos el estadistico junto a su significancia
# <Significancia> = "*" indica que el estadistico es significativo al 10%, "**" al 5% y "***" al 1%.
# <Significancia> = " "  indica que el estadistico no es significativo a ningun nivel convencional
# Aparte, se usa la funcion <stats::wilcox.test> para obtener el p-valor
# La prueba que realiza wilcoxon.jp.test es a dos colas
wilcoxon.resultado <- wilcoxon.jp.test(data.list = all.events.list.true,es.window.length = length_estimation_window,
                                       ev.window.length = length_event_window,ev.window.begin = 0);wilcoxon.resultado
# Dataframe con muchas ventanas
matrix.wilcoxon <- matrix(nrow=(max_abnormal_returns- inicio.ventana.evento+1),ncol=(length(lista.separada)+1))
for(i in seq_along(lista.separada)){
  abnormal <- cumsum(rowMeans(purrr::map_dfc(lista.separada[[i]], 
                                 ~ (.x@retornos$Abnormal)[(length_estimation_window+1+inicio.ventana.evento):(max_abnormal_returns+1+length_estimation_window)])))
  for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))) 
    matrix.wilcoxon[j,i] <- paste(round(abnormal[j],2),
                                  wilcoxon.jp.test(lista.separada[[i]],length_estimation_window,j,inicio.ventana.evento)$Significancia)
}

k <- length(lista.separada)+1
for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))){ 
  abnormal <- cumsum(rowMeans(purrr::map_dfc(all.events.list.true, 
                                             ~ (.x@retornos$Abnormal)[(length_estimation_window+1+inicio.ventana.evento):(max_abnormal_returns+1+length_estimation_window)])))
  matrix.wilcoxon[j,k] <- paste(round(abnormal[j],2),
                               wilcoxon.jp.test(all.events.list.true,length_estimation_window,j,inicio.ventana.evento)$Significancia)
}

colnames(matrix.wilcoxon) <- c(names(lista.separada),'Todos')
Ventana                   <- 1:(max_abnormal_returns+1- inicio.ventana.evento)
matrix.wilcoxon           <- cbind(Ventana,matrix.wilcoxon)
  
dataframe.wilcoxon <- data.frame(matrix.wilcoxon)

# Exportarla a latex con <format = 'latex'>
table.wilcoxon <- kable(dataframe.wilcoxon, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"));table.wilcoxon

# Bootstrap CAAR  (sin GARCH) ----------------------------------------------------------

# length(unlist(purrr::map(all.events.list, ~.x@error_estandar))) se utiliza para ver la longitud de los elementos
# <@error.estandar>, ya que si todos son 0 significa que la estimacion se realizo con GARCH y el siguiente
# procedimiento no tiene sentido
if(length(unlist(purrr::map(all.events.list, ~.x@error_estandar)))>0){
  # Para hacer el procedimiento por Bootstrap se sigue el procedimiento usado por Corrado & Truong (2008)
  boot_n_simul <- 1000 #<<<--- parametro que indica el numero de repeticiones para bootstrapping
  
  bootstrap.resultado <- bootstrap_CT(data.list = all.events.list.true,market.returns=market.returns,
                                      es.window.length = length_estimation_window, ev.window.length = length_event_window,
                                      no.simul = boot_n_simul);bootstrap.resultado
}
# Corrado and Zivney rank test --------------------------------------------

corrado.resultado <- corrado_zivney(data.list = all.events.list.true,es.window.length = length_estimation_window,
                                    ev.window.length = length_event_window); corrado.resultado


# BMP Savickas para media con GARCH ---------------------------------------
# <if(0)> porque la funcion <bmp_savickas> ahora se corre para cada ventana 
if(0){
bmp.savickas.resultado <- bmp_savickas(data.list = all.events.list.true, es.window.length = length_estimation_window,
                                       ev.window.length = length_event_window,ev.window.begin = inicio.ventana.evento); bmp.savickas.resultado
}

# Dataframe con muchas ventanas
matrix.bmp <- matrix(nrow=(max_abnormal_returns-inicio.ventana.evento+1),ncol=(length(lista.separada)+1))
for(i in seq_along(lista.separada)){
  for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))) 
    matrix.bmp[j,i] <- paste(round(mean(colSums(data.frame(purrr::map(lista.separada[[i]],~coredata(.x@retornos$Abnormal[(length_estimation_window+1+inicio.ventana.evento):(length_estimation_window+j+inicio.ventana.evento)]))))),2),
                                  bmp_savickas(lista.separada[[i]],length_estimation_window,j,inicio.ventana.evento)$Significancia)
}

k <- length(lista.separada)+1
for(j in (1:(max_abnormal_returns+1-inicio.ventana.evento))) 
  matrix.bmp[j,k] <- paste(round(mean(colSums(data.frame(purrr::map(all.events.list.true,~coredata(.x@retornos$Abnormal[(length_estimation_window+1+inicio.ventana.evento):(length_estimation_window+j+inicio.ventana.evento)]))))),2),
                                bmp_savickas(all.events.list.true,length_estimation_window,j,inicio.ventana.evento)$Significancia)
colnames(matrix.bmp) <- c(names(lista.separada),'Todos')
Ventana              <- 1:(max_abnormal_returns+1- inicio.ventana.evento)
matrix.bmp           <- cbind(Ventana,matrix.bmp)

dataframe.bmp <- data.frame(matrix.bmp)

# Exportarla a latex
table.bmp <- kable(dataframe.bmp, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"));table.bmp

# Tambien se puede hacer la tabla para BMP con bootstrap usando la funcion <bmp.bootstrap>

# J statistic para media con GARCH ----------------------------------------
# <if(0)> porque todavia no se ha incluido en la funcion un argumento para cuando <inicio.ventana.evento> sea diferente de 0.
# Pero no es el estadistico que vamos a usar, por lo que no es problematico
if(0){
j.statistic.resultado  <- j_statistic(data.list = all.events.list.true, es.window.length = length_estimation_window,
                                      ev.window.length = length_event_window); j.statistic.resultado
}
# Volatility event study --------------------------------------------------

# El siguiente programa sigue la metodologia del paper The impact of natural disasters on stock returns and volatilities
# of local firms (Bourdeau-Brien)
estimation_windows <- c(500,750,1000)
for(estimation_vol_start in estimation_windows){
  # estimation_vol_start <- 750 #<<<-- ventana para la estimacion de la volatilidad previa al evento. 
  vol_ev_window        <- 15  #<<<--- Tamaño de la ventana de evento
  
  # Filtrar los eventos para que solo queden aquellos que cumplan con una ventana minima de estimacion y una ventana minima de 
  # evento
  eventos.filtrado.volatilidad <- drop.events(data.events = emdat_base,base = base_lagged,estimation.start = estimation_vol_start,max.ar=vol_ev_window, 
                                  date_col_name, geo_col_name)
  
  # Filtrar la base de datos para solamente dejar los eventos mas significativos, y tambien asegurar que dentro de la 
  # ventana de estimacion no hayan otros eventos.
  umbral.evento.vol   <- 100 #<<<--- Numero de dias minimo entre cada evento. Lo anterior para que no se traslapen los eventos
  columna.filtrar.vol <- 'Total.Affected' #<<<--- Columna para filtrar la base de eventos 'Total.Affected' o 'Damages'
  eventos.volatilidad <- reducir.eventos(umbral.evento.vol,base_lagged,eventos.filtrado.volatilidad,
                                   col.fecha='Start.Date',col.grupo = 'Country',col.filtro = columna.filtrar.vol)
  
  load.volatility <- 1           #<<<<-- 1 si se cargan los resultados de volatilidad, 0 si es necesario correr el codigo
  if(!load.volatility){
      volatility_results <- volatility_event_study(base.evento = eventos.volatilidad,date.col.name = "Start.Date",geo.col.name = "Country",
                                        base.vol = base_Tommaso,interest.vars = indexes,num_lags = NULL,es.start=estimation_vol_start,
                                        len.ev.window = vol_ev_window,var.exo="market.returns",var.exo.pais = c("gdp","fdi"),
                                        bool.cds = bool_cds,bool.paper = bool_paper,garch = 'sGARCH')
      if(bool_cds){serie <- 'CDS'}else{serie <- 'Indices'}
      if(promedio.movil){regresor.mercado <- 'PM'}else{regresor.mercado <- 'benchmark'}
      save(volatility_results, 
           file=paste0(getwd(),'/Resultados_regresion/',serie,'_tra',umbral.evento.vol,'_est',estimation_vol_start,'_varianza_',regresor.mercado,'.RData'))
  }else load(paste0(getwd(),'/Resultados_regresion/',serie,'_tra',umbral.evento.vol,'_est',estimation_vol_start,'_varianza_',regresor.mercado,'.RData'))
}
# Eliminar objetos NA
volatility_results <- purrr::discard(volatility_results,is.na)

v.columna.agrupar <- 'Disaster.Subgroup' #<<<--- Columna del evento con la cual se quiere separar la lista <volatility_results>
# 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
# Separar la lista dependiendo de una columna en especifico introducida por el usuario 
v.lista.separada <- split(volatility_results, sapply(volatility_results, function(x) x@info.evento[[v.columna.agrupar]]))

# Generar listas distintas para cada valor de la <columna.agrupar>, en caso de querer utilizarlas mas adelante
for (i in seq_along(v.lista.separada)) assign(paste0("v.list.", names(v.lista.separada)[i]), v.lista.separada[[i]])

# Graficas CAV ------------------------------------------------------------
for(i in seq_along(v.lista.separada)){
  element <- v.lista.separada[[i]]
  name    <- names(v.lista.separada)[i]  
  grafico_cav(element,estimation_vol_start,vol_ev_window)
  title(name,line=0.75)
}

# Tabla CAV/significancia ------------------------------------------------

# Dataframe con muchas ventanas
matrix.volatilidad <- matrix(nrow=(vol_ev_window),ncol=(length(v.lista.separada)+1))
iteraciones.bootstrap <- 1
for(i in seq_along(v.lista.separada)){
  for(j in (1:(vol_ev_window))){
    prueba <- bootstrap.volatility(v.lista.separada[[i]],estimation_vol_start,j,iteraciones.bootstrap)
    matrix.volatilidad[j,i] <- paste(prueba$CAV,prueba$Significancia)
  }
}

k <- length(v.lista.separada)+1
for(j in (1:(vol_ev_window))){ 
  prueba.cav <- bootstrap.volatility(volatility_results,estimation_vol_start,j,iteraciones.bootstrap)
  matrix.volatilidad[j,k] <- paste(prueba.cav$CAV,prueba.cav$Significancia)
}
colnames(matrix.volatilidad) <- c(names(v.lista.separada),'Todos')
Ventana                      <- 1:(vol_ev_window)
matrix.volatilidad           <- cbind(Ventana,matrix.volatilidad)

dataframe.volatilidad        <- data.frame(matrix.volatilidad)
# save(dataframe.volatilidad, file = 'Tabla_volatilidad_Paises.RData') # Para guardar el dataframe de volatilidad por paises
# save(dataframe.volatilidad, file = 'Tabla_volatilidad_Tipodesastre.RData') # Para guardar el dataframe de volatilidad por tipo de desastre
#load('Tabla_volatilidad_Paises.RData')
#load('Tabla_volatilidad_Tipodesastre.RData')

# Exportarla a latex
table.volatilidad <- kable(dataframe.volatilidad, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"));table.volatilidad