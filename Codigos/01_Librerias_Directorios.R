if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")

rm(list = ls())
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
       datawizard)

# Directorios -------------------------------------------------------------
Dir         = paste0(getwd(),'/Bases/') #Directorio de datos, se supone que el subdirectorio <Bases> existe
cd.graficos = paste0(getwd(),'/Graficos_Paper/') # Directorio para las imagenes

# Cargar funciones --------------------------------------------------------
source(paste0(getwd(),'/Codigos/00_Functions_Climate_Change.R')) # Source de las funciones