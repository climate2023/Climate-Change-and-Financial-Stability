if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")

if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')

cat("\014")
#
# Cargar librerias --------------------------------------------------------
require(pacman)
p_load(tidyverse, xts, timeDate, zoo, tempdisagg, tsbox, quantmod, timeSeries, forecast, nlme, seasonal, 
       openxlsx, urca, fable, lmtest, moments, stargazer, Hmisc, scales, vars, smoots, dynlm, systemfit,
       ks, knitr, gridExtra, stringr, maps, mapproj, ggthemes, tmap, sf, ggsci, classInt, gnFit, rugarch,
       kableExtra, janitor, xtable, RColorBrewer, tools, writexl, readxl, readxl, bizdays, RQuantLib, gplots,
       datawizard, ggbreak, ggforce)

# Directorios -------------------------------------------------------------
Dir         = paste0(getwd(),'/Bases/') #Directorio de datos, se supone que el subdirectorio <Bases> existe
cd.graficos = paste0(getwd(),'/Graficos_Paper/') # Directorio para las imagenes
# Directorios para las tablas latex
directorio.saved        <- paste0(getwd(),'/Resultados_regresion/')
directorio.guardar      <- paste0(directorio.saved,'Tablas/')

# Cargar funciones --------------------------------------------------------
source(paste0(getwd(),'/Codigos/00_Functions_Climate_Change.R')) # Source de las funciones

# Creacion de clases ------------------------------------------------------

# Generar la clase ESVolatility, para poder manejar los resultados de la estimacion para la varianza
setClass("ESVolatility",slots=list(coefficients = "numeric",goodness_of_fit = "numeric",res_estandar_estimacion="xts",
                                   res_no_estandar_estimacion="xts",variance_forecast="xts",residuales_evento="xts",
                                   info.evento = 'data.frame'))
# Crear clase de objetos
setClass("ESmean",slots=list(retornos = "xts",error_estandar = "numeric",res_estandar_estimacion="xts",
                             res_no_estandar_estimacion="xts",variance_forecast="xts",
                             evento='data.frame',fit='list'))

# Creacion objeto tabla.media ---------------------------------------------
# Se crea un tipo de objeto S4 para guardar la tabla de la media, y aparte el numero de eventos para cada pais
setClass('Tabla.media', slots = list(dataframe = 'data.frame', no.eventos = 'numeric'))


# Se crea un tipo de objeto S4 para guardar la tabla de la media, y aparte el numero de eventos para cada pais
setClass('Tabla.varianza', slots = list(dataframe = 'data.frame', no.eventos = 'numeric'))
