# CODIGO PARA CARGAR LA BASE DE DATOS DE EVENTOS

if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")

rm(list = ls())
if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')

cat("\014")

# Librerias ---------------------------------------------------------------
library(tidyverse)

# Cargar funciones --------------------------------------------------------
source(paste0(getwd(),'/Codigos/Functions_Climate_Change.r')) # Source de las funciones

# Lectura de datos --------------------------------------------------------
bool_paper <- T #<<<--- Parametro que indica si se carga la base de datos que utilizaremos o los retornos de Pagnottoni (2022). 
# <T> si se desea la base de datos para el paper. <F> si los retornos de Pagnottoni
Dir  = paste0(getwd(),'/Bases/') #Directorio de datos, se supone que el subdirectorio <Bases> existe
countries   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                 'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice


# Lectura de y filtros de la base de eventos <emdat_completa>. 
if(1){
  # Si <unico_pais> es NULL, se obtiene una base de datos con los eventos de todos los paises de <countries>
  # En caso de querer analizar solamente un pais, se escribe en <unico_pais>
  unico_pais <- NULL
  
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
  
  # Revisar proporcion de datos faltantes en agregado y por tipo de desastre
  prop.dias.faltantes.agregado <- round((table(emdat_base$na_start) %>% prop.table)*100,2)['1']
  # Separar la base de datos dependiendo el valor en <Disaster.Subgroup>
  bases.separadas        <- emdat_base %>% group_split(Disaster.Subgroup)
  # Anhadir los nombres de los valores en <Disaster.Subgroup>
  names(bases.separadas) <- unlist(lapply(bases.separadas, function(x) unique(x$Disaster.Subgroup)))
  # Obtener la proporcion de dias faltantes y no faltantes por cada base de datos
  proporcion.na.start <- lapply(bases.separadas, function(x) table(x$na_start) %>% prop.table())
  # Obtener la proporcion de dias faltantes para cada base de datos
  prop.dias.faltantes <- round(as.numeric(unlist(purrr::map(proporcion.na.start, ~.x['1'])))*100,2)
  names(prop.dias.faltantes) <- names(bases.separadas)
  
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
  
  # Crear una variable del fin del desastre
  emdat_base <- emdat_base %>% 
    unite(End.Date, c(End.Year, End.Month, End.Day), sep = "-",remove=FALSE) %>% 
    mutate(End.Date = as.Date(End.Date))
  
  # Ahora se crea una columna de duracion del evento, que sera la diferencia entre <End.Date> y <Start.Date>
  emdat_base <- emdat_base %>% 
    mutate(Duracion = as.numeric(End.Date - Start.Date+1))
  
  # Creacion de la variable <na_end>
  emdat_base <- emdat_base %>%
    mutate(na_end = ifelse(is.na(End.Day),1,0))
  
  # Creacion de duracion media por tipo de desastre
  duracion.media <- emdat_base %>% group_by(Disaster.Subgroup) %>% 
    summarise('Media Duracion' = round(mean(Duracion, na.rm=T)))
  
  # Cambiar valores <NA> por la duracion media del tipo de desastre
  emdat_base <- emdat_base %>% 
    dplyr::mutate(Duracion = ifelse(is.na(Duracion),unlist(duracion.media[match(Disaster.Subgroup,duracion.media$Disaster.Subgroup), 'Media Duracion']),
                                    Duracion))
  
  # Por ultimo, con la duracion ya se puede completar la columna <End.Date>
  emdat_base <- emdat_base %>% 
    dplyr::mutate(End.Date = ifelse(is.na(End.Date), Start.Date + Duracion , 
                                    End.Date))
  
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

# Filtrar la base de eventos para buscar eventos mas significativos -------
emdat_base <- emdat_base %>% 
  dplyr::filter(Total.Deaths >= 1000 | No.Injured >= 1000 | Total.Affected >= 10000 | Damages >= 1000000) %>% 
  dplyr::filter(Disaster.Subgroup %in% c('Geophysical','Hydrological','Meteorological')) %>% 
  dplyr::filter(na_start == 0)

# Guardar en .RData -------------------------------------------------------
save(emdat_base, file= paste0(getwd(),'/Bases/EMDAT_PAPER.RData'))

