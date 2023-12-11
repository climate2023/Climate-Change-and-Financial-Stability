##########################################################
# Codigo para generar las dummies para el SUR
# Autores: Juan Pablo Bermudez.
##########################################################

rm(list = ls())
# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))


#Primero creo la base de datos original, y la transformo en un objeto tbl ya que es más facil de manejar usando la libreria dplyr
emdat     <- openxlsx::read.xlsx(paste0(Dir,"BASE_EMDAT.xlsx"),sheet = "Table1") #<<<---cargar base de la web (aunque tiene algunos cambios como los dias y paises estudiados) 
emdat_tbl <- tibble::as_tibble(emdat) # las columnas que nos interesan son <Country>, pais del desastre, <Continent>, continente del desastre, <Disaster.subgroup>, tipo de desastre
                           # <Start.Year>, anho en que inicio el desastre, <Start.Month>, mes en que inicio el desastre, <Start.Day>, dia en que inicio el desastre
                           # <End.Year>, anho en que temrino el desastre, <End.Month>, mes en que termino el desastre, <End.Day> dia en que termino el desastre

#Selecciono solamente las variables que me interesan para poder manejar mejor la base
emdat_interest_variables <- emdat_tbl %>% 
  dplyr::select(Disaster.Subgroup,Country,Continent,Start.Year,Start.Month,Start.Day,End.Year,End.Month,End.Day)

#Filtro la base,eliminando las filas que contengan NA en el mes de inicio, ya que no se puede suponer el mes en que empezo el desastre.
#Siendo la base final que vamos a manejar la llamaré emdat_final.
emdat_final <- emdat_interest_variables %>% 
  dplyr::filter(!is.na(Start.Month))

#Tambien hay fechas donde no se especifica el dia en la base original, por lo que se asumira que el dia de inicio fue el primer dia 
#del mes correspondiente. A su vez, se asumira que el dia del fin del evento fue el ultimo de cada mes si no hay dato en la base 
#original. Es un supuesto fuerte, por lo que es necesario saber a cuales observaciones se les hizo el supuesto.
#Creo dos dummies. La primera toma 1 si se hizo el supuesto en el dia de inicio y 0 en otro caso. La segunda toma 1 si se hizo el 
#supuesto en el dia final y 0 en otro caso.

emdat_final <- emdat_final %>%
  mutate(na_start = ifelse(is.na(Start.Day),1,0))

emdat_final <- emdat_final %>% 
  mutate(na_end = ifelse(is.na(End.Day),1,0))

## Y ahora ya teniendo las dummies, procedo a llenar los datos NA como se propuso anteriormente, creando nuevas columnas, por si 
## es necesario comparar las columnas originales

emdat_final <- emdat_final %>% 
  mutate(Start.Day2 = ifelse(is.na(Start.Day),1,Start.Day) )

 # El siguiente codigo crea una nueva columna End.Day2 que llena los NA de End.Day, a excepcion de aquellos donde End.Month es NA
 # ya que no los elimine (no me parecio sensato). Llena los NA seleccionando el último día del mes, usando la función celing_date.
emdat_final <- emdat_final %>% 
  mutate(End.Day2 = ifelse(is.na(End.Day)&!is.na(End.Month),
                           day(ceiling_date(ymd(paste(End.Year, End.Month, "01", sep = "-")), "month")-1),
                           End.Day))
  
# Creo dos columnas, cada una con formato fecha, indicando el dia de inicio y el dia final del desastre
emdat_final <- emdat_final %>% 
  mutate(Start.Date = as.Date(paste(Start.Year,Start.Month,Start.Day2,sep="-")))

emdat_final <- emdat_final %>% 
  mutate(End.Date = as.Date(paste(End.Year,End.Month,End.Day2,sep="-")))

emdat_selected <- emdat_final %>% 
  dplyr::select(Country, Continent, na_start, Start.Date) %>% 
  distinct()

## Los nombres de los paises generan problemas en la estimacion al contener espacios y parentesis, por lo cual es necesario eliminarlos

## Revisar como hacerlo mas general
emdat_selected$Country <- gsub(" ","_",emdat_selected$Country) 
emdat_selected$Country <- str_remove_all(emdat_selected$Country, "[(),']")
emdat_selected$Country <- gsub("[\u2018\u2019']", "", emdat_selected$Country) # Revisar por que este si elimina todos los '
emdat_selected$Country <- iconv(emdat_selected$Country, to = "ASCII//TRANSLIT") #Cambia caracteres acentuados a caracteres sin acento.


countries_base <- unique(emdat_selected$Country)
countries_base <- sort(countries_base)

wb <- xlsx::createWorkbook()

for(country in countries_base){
  country_data <- emdat_selected %>% 
    dplyr::filter(Country == country)
  colnames(country_data) <- c("Country","na_start","t0")
  sheet <- xlsx::createSheet(wb, sheetName = country)
  addDataFrame(country_data,sheet,startRow = 1,startColumn = 1)
}

saveWorkbook(wb, paste0(Dir,"emdata_dummies_countries.xlsx"))



