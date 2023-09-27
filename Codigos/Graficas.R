############# FALTA ARREGLAR EL CODIGO PARA QUE GRAFIQUE CDS / INDICES MSCI /INDICES PM
if(1){
  if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")
  
  rm(list = ls())
  if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
  if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')
  
  cat("\014")
  #
  # Cargar librerias --------------------------------------------------------
  
  library(tidyverse)
  library(xts)
  library(timeDate)
  library(zoo)
  library(tempdisagg)
  library(tsbox)
  library(quantmod)
  library(timeSeries)
  library(forecast)
  library(nlme)
  library(seasonal)   
  library(openxlsx)
  library(urca)
  library(fable)
  library(lmtest)
  library(moments)
  library(stargazer)
  library(Hmisc)
  library(scales)
  library(vars)
  library(smoots)
  library(dynlm)
  library(systemfit)
  library(ks)
  library(knitr)
  library(gridExtra)
  library(stringr)
  library(maps)
  library(mapproj)
  library(ggthemes)
  library(tmap)
  library(sf)
  library(ggsci)
  library(classInt)
  library(gnFit)
  library(rugarch)
  library(knitr)
  library(kableExtra)
  library(janitor) # Para manejo de tablas descriptivas
  library(xtable)  # Para exportar tablas a latex 
  library(RColorBrewer)
  library(tools)
  library(writexl)  # Para crear excel
  library(readxl)
  library(bizdays);library('RQuantLib') # Revisar si las series contienen dias festivos
  
  # Cargar funciones --------------------------------------------------------
  
  source(paste0(getwd(),'/Codigos/Functions_Climate_Change.r')) # Source de las funciones
}

## Parametros
no.rezagos.de.desastres <- 15     #<<<--- Numero de rezagos de los desastres <w> (i.e. t0, t1, ..., tw)
tipo.serie              <- 'cds'  #<<<--- Puede ser 'cds' o 'indices
market                  <- 'PM'   #<<<--- Puede ser 'PM' o 'benchmark', pero si tenemos CDS solamente puede ser PM

if(tipo.serie == 'cds') indexes     <- c('CDS_Brazil','CDS_Chile','CDS_China','CDS_Colombia','CDS_Indonesia','CDS_Korea',
                                      'CDS_Malaysia','CDS_Mexico','CDS_Peru','CDS_SouthAfrica','CDS_Turkey') #<<<--- Lista de los indices analizados. 
if(tipo.serie == 'indices') indexes <- c('BIST100','Bovespa','ChinaA50','JSX','KOSPI','S.PBMVIPC','S.PCLXIPSA','SouthAfricaTop40',
                                                 'IGBVL','KLCI','COLCAP') # Nombre indices para el paper. JSX es el de Jakarta
# Cargar resultados
load(paste0('Resultados_SUR/Resultados_Desastres_',tipo.serie,'_',market,'.RData'))

### CODIGO PARA GRAFICAS

# De acuerdo con la notacion de los modelos estimados, los coeficientes el dia del evento terminan en <t0>, 
# el dia siguiente en <t1>, dos dias despues <t2>, y asi hasta llegar a <t4>. 
steps <- paste('t',(0:no.rezagos.de.desastres),sep='')  # vector con los días adelante del evento, hace referencia a como termina el nombre de las dummies


## El siguiente ciclo genera la densidad Kernel de los coeficientes para cada tipo de desastre 
## y para todos los <t0>, <t1> ...
for(step in steps){
  for(model_name in names(coefficients_disasters_list)){
    dens_name <- paste("dens",model_name,step,sep="_")
    assign(dens_name,dens(coefficients_disasters_list[[model_name]][,"Estimate"],step))
  }
}

# Por otro lado, necesitamos hacer la grafica de los CAR, que es la suma de los retornos anormales.
# Con el loop for estamos haciendo el mismo proceso para cada uno de los 5 modelos estimados.
# Al final tendremos un vector para cada modelo que incluye los coeficientes relacionados para las 5 
# dummies temporales para todos los paises. Lo anterior para posteriormente ser sumadas por cada pais para 
# generar el retorno anormal acumulado t_0+t_1+t_2+t_3+t_4

for(model_name in names(coefficients_disasters_list)){
  #Vamos a generar una lista para cada modelo
  var_name <- paste0("coef_vec_",model_name)
  coef_vec <- c()
  #reunimos los coeficientes en <coefs>
  coefs <- coefficients_disasters_list[[model_name]][,"Estimate"]
  for(step in steps){
    #seleccionamos solamente los coeficientes que acaben con <step> y lo añadimos a <coef_vec>
    interest_indices <- grep(paste0(step, "$"),names(coefs)) # eL $ es para dejar explicito que tiene que terminar en <step>
    interest_coefficients <- coefs[interest_indices]
    coef_vec <- c(coef_vec, interest_coefficients)
  }
  # al final asignamos <coef_vec> al nombre especifico por modelo.
  assign(var_name, coef_vec)
}

# Generamos la densidad de los retornos anormales acumulados para cada tipo de desastre
densidad_CAR_bio <- densidad_CAR(coef_vec_fitsur_Biological,indexes)
densidad_CAR_cli <- densidad_CAR(coef_vec_fitsur_Climatological,indexes)
densidad_CAR_geo <- densidad_CAR(coef_vec_fitsur_Geophysical,indexes)
densidad_CAR_hyd <- densidad_CAR(coef_vec_fitsur_Hydrological,indexes)
densidad_CAR_met <- densidad_CAR(coef_vec_fitsur_Meteorological,indexes)

### =============================== Graficas de retornos anormales ==================================

#Ya con las densidades de los retornos acumulados y de las dummies t_0, t_1, ..., t_4 podemos graficarlas

labels <- c("Biological","Climatological","Geophysical","Hydrological","Meteorological")  #<<<--- leyendas del grafico
colors <- c("blue", "tomato", "orange", "darkorchid4", "green")   #<<<--- colores que usara la grafica

# Para los CAR el vector seria
main_car   <- "Kernel denisty of CAR"  #<<<--- titulo para la grafica
vector_car <- c("densidad_CAR_bio","densidad_CAR_cli","densidad_CAR_geo","densidad_CAR_hyd", 
                "densidad_CAR_met") #<<<---vector de elementos a graficar
grafico_densidad(vector_car,main_car,labels,colors)


#Para los AR_t_0 seria
main_t_0   <- "Kernel density of AR t_0"  #<<<--- titulo para la grafica
vector_t_0 <- c("dens_fitsur_Biological_t0","dens_fitsur_Climatological_t0","dens_fitsur_Geophysical_t0",
                "dens_fitsur_Hydrological_t0","dens_fitsur_Meteorological_t0") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_0,main_t_0,labels,colors)


#Para los AR_t_1 seria
main_t_1   <- "Kernel density of AR t_1"  #<<<--- titulo para la grafica
vector_t_1 <- c("dens_fitsur_Biological_t1","dens_fitsur_Climatological_t1","dens_fitsur_Geophysical_t1",
                "dens_fitsur_Hydrological_t1","dens_fitsur_Meteorological_t1") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_1,main_t_1,labels,colors)


#Para los AR_t_2 seria
main_t_2   <- "Kernel density of AR t_2"  #<<<--- titulo para la grafica
vector_t_2 <- c("dens_fitsur_Biological_t2","dens_fitsur_Climatological_t2","dens_fitsur_Geophysical_t2",
                "dens_fitsur_Hydrological_t2","dens_fitsur_Meteorological_t2") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_2,main_t_2,labels,colors)

#Para los AR_t_3 seria
main_t_3   <- "Kernel density of AR t_3"  #<<<--- titulo para la grafica
vector_t_3 <- c("dens_fitsur_Biological_t3","dens_fitsur_Climatological_t3","dens_fitsur_Geophysical_t3",
                "dens_fitsur_Hydrological_t3","dens_fitsur_Meteorological_t3") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_3,main_t_3,labels,colors)

#Para los AR_t_4 seria
main_t_4   <- "Kernel density of AR t_4"  #<<<--- titulo para la grafica
vector_t_4 <- c("dens_fitsur_Biological_t4","dens_fitsur_Climatological_t4","dens_fitsur_Geophysical_t4",
                "dens_fitsur_Hydrological_t4","dens_fitsur_Meteorological_t4") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_4,main_t_4,labels,colors)

# Grafica de tiempo retornos anormales acumulados  -----------------------------------

coefficients_list <- coefficients_disasters_list  #<<<--- parametro que indica para que lista de coeficientes se quiere
                                                  #       realizar el grafico, ya sea por tipo de desastre <coefficients_disasters_list>
                                                  #       o por pais donde sucedio el desastre <coefficients_countries_list>
# Si se desea graficar solamente los CAR para algun(os) tipos de desastres o para algun(os) paises en especifico, se deben escribir en el 
# parametro <var.interes>. Tienen que tener la misma ortografia que <Tipos.desastres>, en caso de haber escogido <coefficients_disasters_list>
# o <paises> en caso de haber escogido <coefficients_countries_list>.
# Por ejemplo, si solo se quiere ver el CAR asociado a Colombia, entonces var.interes <- "Colombia". 
# Si se desea ver el CAR para todos los paises o todos los tipos de desastres, var.interes <- Tipos.desastres o var.interes <- paises
var.interes <- Tipos.Desastres

# Filtrar <coefficients_disasters_list> para que solo incluyan coeficientes para los tipos de desastre o paises incluidos en <var.interes>
coefficients_list <- coefficients_list[grep(paste(var.interes,collapse = "|"),names(coefficients_list))]

for(model in coefficients_list){
  # Los coeficientes estan en la columna <Estimate>
  coefficients <- model[,"Estimate"]
  # Seleccionar solo los que pertenecen a una dummy
  interest.index <- str_ends(names(coefficients),paste(steps,collapse="|"))
  coefficients   <- coefficients[interest.index]
  # <car_pagnottoni> genera la grafica de los CAR dependiendo de dia relativo al evento
  car_pagnottoni(coefficients,indices = indexes,interest.vars = var.interes,average = TRUE)
}

## ============================== Graficos A.3 de densidad de los retornos ========================================
densidad_retornos <- apply(Retornos, MARGIN = 2, FUN = density)
# <if(!bool_paper)> porque solamente funciona con el procedimiento de Pagnottoni
if(!bool_paper){
  colores <- c("blue","tomato","orange","darkorchid4","green","cyan","firebrick") #<<<--- colores para grafica de retornos
  
  # Para utilizar la función <grafico_retornos> necesitamos un objeto a graficar, mas un vector con los paises de cada continente, mas un
  # titulo para el grafico y un vector de leyendas, el cual sera el nombre del indice de cada país. Ademas necesitamos los colores.
  
  # Para America seria:
  main_America      <- "Densidad retornos America" #<<<--- titulo para la grafica
  indexes_America <- c("Bovespa","S.PCLXIPSA","NASDAQComposite","Nasdaq100","S.PTSXComposite","S.PBMVIPC")  #<<<--- vector de indices que pertencen al continente
  labels_America    <- c("Bovespa","S&P CLX IPSA","NASDAQ Composite","Nasdaq 100","S&P TSX Composite",
                         "S&P BMV IPC") #<<<--- leyendas, indices de cada pais del continente
  grafico_retornos(densidad_retornos,indexes_America,main_America,labels_America,colores)
  
  # Para Europa del Este:
  main_Europa_Este      <- "Densidad retornos Europa del Este" #<<<--- titulo para la grafica
  indexes_Europa_Este <- c("MOEXRussia","OMXCopenhagen20","BIST100","OSEBenchmark","WIG20","OMXHelsinki25","OMXStockholm30") #<<<--- vector de indices que pertencen al continente
  labels_Europa_Este    <- c("Moex Russia","OMX Copenhagen 20","BIST 100","OSE Benchmark","WIG20",
                             "OMX Helsinki 25","OMX Stockholm 30") #<<<--- leyendas, indices de cada pais del continente
  grafico_retornos(densidad_retornos,indexes_Europa_Este,main_Europa_Este,labels_Europa_Este,colores)
  
  # Para Europa del Oeste
  main_Europa_Oeste      <- "Densidad retornos Europa del Oeste"  #<<<--- titulo para la grafica
  indexes_Europa_Oeste <- c("FTSE100","SMI","DAX","IBEX35","AEX","BEL20","CAC40") #<<<--- vector de indices que pertencen al continente
  labels_Europa_Oeste    <- c("FTSE 100","SMI","DAX","IBEX 35","AEX","BEL 20","CAC 40") #<<<--- leyendas, indices de cada pais del continente
  grafico_retornos(densidad_retornos,indexes_Europa_Oeste,main_Europa_Oeste,labels_Europa_Oeste,colores)
  
  #Para Asia
  main_Asia      <- "Densidad retornos Asia" #<<<--- titulo para la grafica
  indexes_Asia <- c("SETIndex","KOSPI","Nifty50","JakartaStockExchange","HangSeng") #<<<--- vector de paises que pertencen al continente
  labels_Asia    <- c("SET Index","KOSPI","Nifty 50","Jakarta Stock Exchange","Hang Seng") #<<<--- leyendas, indices de cada pais del continente
  grafico_retornos(densidad_retornos,indexes_Asia,main_Asia,labels_Asia,colores)
  
  #Para Africa y Oceania
  main_Africa_Oceania      <- "Densidad retornos Africa y Oceania" #<<<--- titulo para la grafica
  indexes_Africa_Oceania <- c("SouthAfricaTop40","S.PASX200")  #<<<--- vector de paises que pertencen al continente
  labels_Africa_Oceania    <- c("South Africa Top 40","S&P ASX 200") #<<<--- leyendas, indices de cada pais del continente
  grafico_retornos(densidad_retornos,indexes_Africa_Oceania,main_Africa_Oceania,labels_Africa_Oceania,colores)
}else{
  # CDS solamente tenemos de tres continentes
  colors_palette <- c("#FF0000", "#00FF00", "#0000FF", "#FFA500", "#800080", "#FFFF00", "#00FFFF", "#FFC0CB", "#008000", "#FF00FF", "#800000")
  # Primero se puede graficar todos los retornos
  grafico_retornos(densidad_retornos,names(densidad_retornos),'Densidad retornos',indexes,colors_palette)
  
  # Ahora para Africa
  cds_africa    <- c('CDSSouthAfrica')
  labels_africa <- c('CDS South Africa')
  grafico_retornos(densidad_retornos,cds_africa,'Densidad Retornos Africa',labels_africa,colors_palette)
  
  # America
  cds_america    <- c('CDSBrazil','CDSChile','CDSColombia','CDSMexico','CDSPeru')
  labels_america <- c('CDS Brazil','CDS Chile','CDS Colombia','CDS Mexico','CDS Peru')
  grafico_retornos(densidad_retornos,cds_america,'Densidad Retornos America',labels_america, colors_palette)
  
  # Asia
  cds_asia    <- c('CDSChina','CDSIndonesia','CDSKorea','CDSMalaysia','CDSTurkey')
  labels_asia <- c('CDS China','CDS Indonesia','CDS Korea','CDS Malaysia','CDS Turkey')
  grafico_retornos(densidad_retornos,cds_asia,'Densidad Retornos Asia',labels_asia, colors_palette)
}
### ============================ Grafico 3 Pagnottoni. AR estimates ============================================================

# Para el grafico, Pagnottoni tiene un orden especifico, por lo cual toca especificarlo
if(!bool_paper){ 
  pagn_orden <- c("SETIndex", "MOEXRussia", "KOSPI", "Nifty50", "JakartaStockExchange", "Bovespa", 
                "S.PCLXIPSA", "HangSeng", "NASDAQComposite", "Nasdaq100", "S.PTSXComposite", "S.PBMVIPC", 
                "SouthAfricaTop40", "OMXCopenhagen20", "BIST100", "OSEBenchmark", "WIG20", "OMXHelsinki25", 
                "FTSE100", "S.PASX200", "OMXStockholm30", "SMI", "DAX", 
                "IBEX35", "AEX", "BEL20", "CAC40") #<<<--- indices en el orden que aparece en la grafica #3
  labels_grafico <- c("SET Index", "MOEX Russia", "KOSPI", "Nifty 50", "Jakarta Stock Exchange", "Bovespa", 
                    "S&P CLX IPSA", "Hang Seng", "NASDAQ Composite", "Nasdaq 100", "S&P TSX Composite", "S&P BMV IPC", 
                    "South Africa Top 40", "OMX Copenhagen 20", "BIST 100", "OSE Benchmark", "WIG20", "OMX Helsinki 25", 
                    "FTSE 100", "S&P ASX 200", "OMX Stockholm 30", "SMI", "DAX", 
                    "IBEX 35", "AEX", "BEL 20", "CAC 40") #<<<--- indices en el orden que aparece en la grafica #3 (leyenda)
}else{
  pagn_orden     <- indexes
  labels_grafico <- indexes
}

group         <- rep(labels_grafico,each=(no.rezagos.de.desastres+1)) ## Variable que va a agrupar en grupos de a 5 los datos (porque cada 5 es un indice distinto)
colores.ar = c("#1964C4", "#C9675A", "#D5B259","darkorchid4","#709E3D")
grafico.solo.car <- T #<<<-- booleano, donde <TRUE> indica que solamente se quiere graficar el CAR estimado por pais y tipo de evento, y no todos los AR

if(!grafico.solo.car){
  ## Para biological
  ar_data_Bio       <- coef_vec_fitsur_Biological[order_coef(names(coef_vec_fitsur_Biological),pagn_orden)] ##ordenar
  ar_data_frame_Bio <- data.frame(values = ar_data_Bio,  group=group, subgroup =steps)
  ar_data_frame_Bio$group <- factor(ar_data_frame_Bio$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
  plot_Bio <- grafico_estimates(ar_data_frame_Bio,"Abnormal return","Biological",colors = colores.ar)
  
  ## Para climatological
  ar_data_Cli       <- coef_vec_fitsur_Climatological[order_coef(names(coef_vec_fitsur_Climatological),pagn_orden)] ##ordenar
  ar_data_frame_Cli <- data.frame(values = ar_data_Cli, group=group, subgroup =steps)
  ar_data_frame_Cli$group <- factor(ar_data_frame_Cli$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
  plot_Cli <- grafico_estimates(ar_data_frame_Cli,"Abnormal return","Climatological",colors = colores.ar)
  
  ## Para geophysical
  ar_data_Geo       <- coef_vec_fitsur_Geophysical[order_coef(names(coef_vec_fitsur_Geophysical),pagn_orden)] ##ordenar
  ar_data_frame_Geo <- data.frame(values = ar_data_Geo, group=group, subgroup =steps)
  ar_data_frame_Geo$group <- factor(ar_data_frame_Geo$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
  plot_Geo <- grafico_estimates(ar_data_frame_Geo,"Abnormal return","Geophysical",colors = colores.ar)
   
  ## Para hydrological
  
  ar_data_Hyd       <- coef_vec_fitsur_Hydrological[order_coef(names(coef_vec_fitsur_Hydrological),pagn_orden)] ##ordenar
  ar_data_frame_Hyd <- data.frame(values = ar_data_Hyd, group=group, subgroup =steps)
  ar_data_frame_Hyd$group <- factor(ar_data_frame_Hyd$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
  plot_Hyd <- grafico_estimates(ar_data_frame_Hyd,"Abnormal return","Hydrological",colors = colores.ar)
  
  ## Para meteorological
  ar_data_Met       <- coef_vec_fitsur_Meteorological[order_coef(names(coef_vec_fitsur_Meteorological),pagn_orden)] ##ordenar
  ar_data_frame_Met <- data.frame(values = ar_data_Met,  group=group, subgroup =steps)
  ar_data_frame_Met$group <- factor(ar_data_frame_Met$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
  plot_Met <- ggplot(ar_data_frame_Met, aes(x=group,y=values,fill=subgroup))+
                                           geom_bar(stat="identity", position="dodge", width=0.7) +
                                           scale_fill_manual(values=c("#1964C4", "#C9675A","#D5B259","#7C63CF","#709E3D")) +
                                           labs(x="index",y="Abnormal return",title="Meteorological") +
                                           theme(plot.title = element_text(hjust = 0.5),
                                           axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
  
  #graficas juntas
  complete_plot <- grid.arrange(plot_Bio,plot_Cli,plot_Geo,plot_Hyd,plot_Met,nrow=5,ncol=1,heights = c(1,1,1,1,1.7))
  #ggsave("abnormal_returns.pdf",plot=complete_plot,device="pdf", width = 8.27, height = 11.69) # Ya esta guardado en el github por lo que no es necesario volverlo a cargar
}

if(grafico.solo.car){
  # Intento de arreglar el codigo de graficos CAR para mayor eficiencia
  # Escoger solamente los valores estimados
  estimados <- (purrr::map(coefficients_disasters_list, ~ .x[,'Estimate']))
  # Para un vector de strings, la funcion <grep> busca si contienen un patron determinado
  # Sin embargo, para poder escoger mas de un patron, se necesita vectorizar la funcion, de este modo podra usarse con <steps>
  v_grep <- Vectorize(grep, vectorize.args = "pattern")
  # Para los coeficientes <estimados>, buscar si dentro de su nombre se encuentra alguno de los strings contenidos en <steps>
  results <- lapply(estimados, function(x) v_grep(paste0(steps, "$"),names(x)))
  
  lista.dataframes <- list()
  for(name in names(estimados)){
    # guardar solamente los coeficientes que contengan <name>, al multiplicar los coeficientes <estimados>, con las posiciones que incluyen
    # el nombre
    coeficientes.de.interes <- estimados[[name]][results[[name]]]
    # usar la funcion <order_coef> para ordenar los coeficientes en orden alfabetico y numerico
    ar_data <- coeficientes.de.interes[order_coef(names(coeficientes.de.interes),pagn_orden)]
    # establecerlo en data.frame
    ar_data_frame       <- data.frame(values = ar_data,  group=group, subgroup =steps)
    ar_data_frame$group <- factor(ar_data_frame$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
    lista.dataframes[[name]] <- ar_data_frame
  }
  
  grafico_estimates_car2(lista.dataframes,'Abnormal Returns',Tipos.Desastres,'red')
}
### ===========================  Grafico A.4 Pagnottoni, t-tests =======================================================

#Primero necesitamos el valor de los estadísticos t

for(model_name in names(coefficients_disasters_list)){
  #Vamos a generar una lista para cada modelo
  tests <- coefficients_disasters_list[[model_name]][,"t value"]
  var_name <- paste0("t_test_",model_name)
  t_test <- c()
  for(step in steps){
    #reunimos los coeficientes en <coefs>
    
    #seleccionamos solamente los coeficientes que acaben con <step> y lo añadimos a <t_test>
    interest_indices <- grep(step,names(tests))
    interest_tests <- tests[interest_indices]
    t_test <- c(t_test, interest_tests)
  }
  # al final asignamos <t_test> al nombre especifico por modelo.
  assign(var_name, t_test)
}

#Para biological

t_data_Bio       <- t_test_fitsur_Biological[order_coef(names(t_test_fitsur_Biological),pagn_orden)] ##ordenar
t_data_frame_Bio <- data.frame(values = t_data_Bio,  group=group, subgroup =steps)
t_data_frame_Bio$group <- factor(t_data_frame_Bio$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_t_Bio <- grafico_estimates(t_data_frame_Bio, "t_test", "Biological",colors = colores.ar)


#Para climatological

t_data_Cli       <- t_test_fitsur_Climatological[order_coef(names(t_test_fitsur_Climatological),pagn_orden)] ##ordenar
t_data_frame_Cli <- data.frame(values = t_data_Cli, group=group,subgroup =steps)
t_data_frame_Cli$group <- factor(t_data_frame_Cli$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_t_Cli <- grafico_estimates(t_data_frame_Cli, "t_test", "Climatological",colors = colores.ar)

## Para geophysical

t_data_Geo       <- t_test_fitsur_Geophysical[order_coef(names(t_test_fitsur_Geophysical),pagn_orden)] ##ordenar
t_data_frame_Geo <- data.frame(values = t_data_Geo, group=group, subgroup =steps)
t_data_frame_Geo$group <- factor(t_data_frame_Geo$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>

plot_t_Geo <- grafico_estimates(t_data_frame_Geo, "t_test", "Geophysical",colors = colores.ar)


## Para hydrological

t_data_Hyd       <- t_test_fitsur_Hydrological[order_coef(names(t_test_fitsur_Hydrological),pagn_orden)] ##ordenar
t_data_frame_Hyd <- data.frame(values = t_data_Hyd, group=group, subgroup =steps)
t_data_frame_Hyd$group <- factor(t_data_frame_Hyd$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>

plot_t_Hyd <- grafico_estimates(t_data_frame_Hyd, "t_test", "Hydrological",colors = colores.ar)


## Para meteorological

t_data_Met       <- t_test_fitsur_Meteorological[order_coef(names(t_test_fitsur_Meteorological),pagn_orden)] ##ordenar
t_data_frame_Met <- data.frame(values = t_data_Met, group=group, subgroup =steps)
t_data_frame_Met$group <- factor(t_data_frame_Met$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>

plot_t_Met <- ggplot(t_data_frame_Met, aes(x=group,y=values,fill=subgroup))+
  geom_bar(stat="identity", position="dodge", width=0.7) +
  scale_fill_manual(values=c("#1964C4", "#C9675A", "#D5B259","#7C63CF","#709E3D")) +
  labs(x="index",y="t-test",title="Meteorological") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

#graficas juntas

complete_t_plot <- grid.arrange(plot_t_Bio,plot_t_Cli,plot_t_Hyd,plot_t_Geo,plot_t_Met,nrow=5,ncol=1,heights = c(1,1,1,1,1.7))
#ggsave("t_tests.pdf",plot=complete_t_plot,device="pdf", width = 8.27, height = 11.69) # Ya esta guardado en el github por lo que no es necesario volverlo a cargar

# Manejo de los datos para graficar ---------------------------------------

### Para generar las graficas 4 y A.8 de Pagnottoni es necesario saber a que continente pertenece cada pais.

# Lectura de la base de datos EMDAT. En excel, 1) se filtraron los 104 paises usados en el paper 
#  y 2) se dejaron los desastres entre el 8-feb-2001 y 31-dic-2019 (fechas usadas en el paper).
# Cada fila de la base de datos hace referencia a un desastre natural, y sus columnas dan informacion acerca del desastre. Las columnas que nos interesan
# son <Country>, que indica el pais donde sucedio el desastre y <Continent> continente donde sucedio el desastre, 
if(!bool_paper){
  emdat <- openxlsx::read.xlsx(paste0(Dir,"BASE_EMDAT.xlsx"),sheet = "Table1") #<<<---cargar base <emdat>
}else{
  emdat <- read_excel(paste0(Dir,"EMDAT_CDS_ORIGINAL.xlsx"),sheet = "emdat data") #<<<---cargar base <emdat>
  # Filtrar la base para que solo contenga las fechas deseadas, para CDS mirar octubre 2004 a agosto 2002, que son las fechas con que contamos con datos
  # de CDS
  emdat <- emdat %>%  dplyr::filter(between(`Start Date`,as.Date('2004-10-01'),as.Date('2022-08-31')))
}

# Se transforma en un objeto <tbl>, usando la libreria <dplyr>
emdat_tbl <- tibble::as_tibble(emdat) 

# Para poder generar el vector por cada continente que incluya los paises de dicho continente selecciono de la base de datos solamente las columnas 
# <Country> y <Continent>
emdat_country_continent <- emdat_tbl %>%
  dplyr::select(Country,Continent) %>% 
  dplyr::mutate(Country = gsub(" ","",Country)) # Quitar espacios en todos los nombres de paises

# En el dataframe se repiten tanto el continente como el pais, por lo que solamente se toman los valores unicos
emdat_continents <- emdat_country_continent %>%  
  distinct()

# En el vector <continents> se tienen  los valores unicos para el continente: Asia, Europe, Americas, Oceania, Africa
continents <- sort(unique(emdat_continents$Continent))

# Generar un vector por cada continente que incluya los <paises> que lo conforman
for (continent in continents){
  paises_continent <- c()
  varname          <- paste0("countries_",continent)
  emdat_countries  <- emdat_continents %>% 
    dplyr::filter(Continent == continent)
  paises_continent <- unique(emdat_countries$Country)
  # Por otro lado, en la estimacion SUR los paises no pueden tener caracteres especiales. 
  paises_continent <- gsub(" ","_",paises_continent) # Cambiar los espacios de <paises_continent> por '_'
  paises_continent <- str_remove_all(paises_continent, "[(),'^]") # quitar los caracteres especificados de <paises_continent>
  paises_continent <- gsub("[\u2018\u2019']", "", paises_continent) # Asegurar que todos los caracteres <'> hayan sido removidos de <paises_continent>
  paises_continent <- iconv(paises_continent, to = "ASCII//TRANSLIT") #Cambiar caracteres acentuados a caracteres sin acento. 
  paises_continent <- str_remove_all(paises_continent, "[(),'^]") # quitar los caracteres especificados de <paises_continent>
  assign(varname, paises_continent)
}

# El siguiente codigo se utilizaba para revisar los modelos por continentes, pero fue mejorado por el codigo que sigue, ya que
# usa el objeto <coefficients_countries_list>, y no <models_countries_list>.
if(0){
  max.length <- max(sapply(fitted_models2,FUN = nchar)) ## Problema, systemfit solo genero maximo 39 caracteres
  for (continent in continents){
    varname          <- paste0("fitted_models2_",continent)
    countries_models <- paste0("fitcoun_",get(paste0("countries_",continent)))
    countries_models <- substr(countries_models,1,max.length)  ## Como systemfit creo objetos de maximo max.length, toca cortar los nombres en esta linea 
    # para que tengan el mismo numero de caracteres
    assign(varname, mget(countries_models)) 
  }
}

# maxima longitud de los nombres de la lista <coefficients_countries_list> [<fitcount_pais>]
max.length <- max(sapply(names(coefficients_countries_list),FUN = nchar))## Problema, systemfit solo genero maximo 39 caracteres

# El siguiente codigo genera una lista para cada continente: <fitted_coefficients_Africa> .. <fitted_coefficients_Oceania> 
# La longitud de cada lista anterior es el no. de paises del <continente> correspondiente. Su elto <i> corresponde al SUR 
# que incluye dummies del i-esimo <pais> del <continente> analizado
#   Y es una matriz de orden Nx4:
#      N: No.coeff.stock1 + No.coeff.stock2 + ... + No.coeff.stockFINAL (Ecuaciones que hace parte de cada SUR)
#      4: c(Estimate, Std. Error, t value, Pr(>|t|) ) 
# Por ultimo, se genera una lista <coefficients_continents_list>, que guarda las listas de cada continente.
coefficients_continents_list <- list()
for (continent in continents){
  varname                <- paste0("fitted_coefficients_",continent)
  countries_coefficients <- paste0("fitcoun_",get(paste0("countries_",continent)))
  countries_coefficients <- substr(countries_coefficients,1,max.length)  
  assign(varname, subset(coefficients_countries_list, names(coefficients_countries_list) %in% countries_coefficients)) 
  coefficients_continents_list[[varname]] <- get(varname)
}

# prueba fallida de mejora del codigo anterior
if(0){
  Coeff.por.pais = list(); ii = 0
  for (continent in continents){
    ii = ii +10
    Coeff.por.pais[[ii]]        <- paste0("fitcoun_",get(paste0("countries_",continent)))
    names(Coeff.por.pais[[ii]]) <- paste0("fitted_coefficients_",continent)
    #countries_coefficients      <- substr(countries_coefficients,1,max.length) 
    assign(varname, subset(coefficients_countries_list, names(coefficients_countries_list) %in% countries_coefficients)) 
  }
}

## ---- Graficas 1, A.1, A.6 y A.7 de Pagnottoni(2022) --- ##

# Lectura de la base de datos <EMDAT>,  en excel, se dejaron los desastres entre el 8-feb-2001 y 31-dic-2019 (fechas usadas en el paper).
if(!bool_paper){
  emdat_completa <- openxlsx::read.xlsx(paste0(Dir,"EMDAT_Mapamundi.xlsx"),sheet = "Mapamundi") #<<<--- base de datos
  # Se transforma en <tibble> para poder manejar con las funciones de <dplyr> 
  emdat_tbl_completa <- tibble::as_tibble(emdat_completa) 
  # las columnas que nos interesan son <Country>, pais del desastre, <Continent>, continente del desastre, <Disaster.subgroup>, tipo de desastre
  # <Start.Year>, anho en que inicio el desastre, <Start.Month>, mes en que inicio el desastre, <Start.Day>, dia en que inicio el desastre
  # <End.Year>, anho en que termino el desastre, <End.Month>, mes en que termino el desastre, <End.Day> dia en que termino el desastre
  
  # Se  seleccionan  las variables que interesan y se renombra <Country> como region, para adjuntarla con otra base 
  # de coordenadas para graficar.
  emdat_interest_variables <- emdat_tbl_completa %>% 
    dplyr::select(Disaster.Subgroup, region=Country, Continent, Start.Year, Start.Month, Start.Day, End.Year, End.Month, End.Day)
}else{
  emdat_completa <- emdat
  # Se transforma en <tibble> para poder manejar con las funciones de <dplyr> 
  emdat_tbl_completa <- tibble::as_tibble(emdat_completa) 
  # las columnas que nos interesan son <Country>, pais del desastre, <Continent>, continente del desastre, <Disaster.subgroup>, tipo de desastre
  # <Start.Year>, anho en que inicio el desastre, <Start.Month>, mes en que inicio el desastre, <Start.Day>, dia en que inicio el desastre
  # <End.Year>, anho en que termino el desastre, <End.Month>, mes en que termino el desastre, <End.Day> dia en que termino el desastre
  
  # Se  seleccionan  las variables que interesan y se renombra <Country> como region, para adjuntarla con otra base 
  # de coordenadas para graficar.
  emdat_interest_variables <- emdat_tbl_completa %>% 
    dplyr::select(Disaster.Subgroup = `Disaster Subgroup`,Disaster.Type = `Disaster Type`, region=Country, Continent, Start.Year= `Start Year`, 
                  Start.Month = `Start Month`, Start.Day= `Start Day`, End.Year= `End Year`, End.Month= `End Month`, End.Year=`End Year`)
  # Cambiar el nombre de <Korea> a <South Korea>
  emdat_interest_variables[which(emdat_interest_variables$region == 'Korea'),]$region <- 'South Korea'
}

#Se eliminan las filas (desastres) que contengan <NA> en el mes de inicio, ya que no se puede suponer el mes en que empezo el desastre.
emdat_final <- emdat_interest_variables %>% 
  dplyr::filter(!is.na(Start.Month))

# Cargo los datos del mapamundi del paquete <ggplot2>
world <- map_data("world")

# Los datos en <world> tienen un inconveniente: Hong Kong y Macao aparecen como regiones de China, no como "pais" propio.
# Es por tanto que tenemos que volverlos una region en la base <world>
hong_kong = which(world$subregion=="Hong Kong") # <x$subregion> equivale a una region de un pais de <x> y <x$region> a un pais de <x>
world[hong_kong,"region"] <- "Hong Kong"
macao = which(world$subregion == "Macao")
world[macao, "region"] <- "Macao"

# En la base de desastres consideraron a Antigua y Barbuda como un solo pais, mientras que en <world> los separan,
# por lo cual tenemos que unificar esas dos regiones en la base <world>.
antigua = which(world$region=="Antigua")
barbuda = which(world$region=="Barbuda")
world[c(antigua,barbuda),"region"] <- "Antigua and Barbuda"

# Lo mismo con Trinidad y Tobago
trinidad = which(world$region=="Trinidad")
tobago   = which(world$region=="Tobago")
world[c(trinidad,tobago),"region"] <- "Trinidad and Tobago"

# Con las islas virgenes tenemos dos nombres distintos en <emdata_final> mientras que en <world> solo uno, por lo 
# que se coloca el mismo nombre para ambos en <emdata_final>
virgin_british = which(emdat_final$region=="Virgin Island (British)" )
virgin_us      = which(emdat_final$region=="Virgin Island (U.S.)"  )
emdat_final[c(virgin_british,virgin_us),"region"] <- "Virgin Islands"

# En la base de datos <world> no existe Tokelau, territorio dependiente de Nueva Zelanda, por lo cual
# se le cambiara el nombre a nueva zelanda en la base <emdat_final>
tokelau = which(emdat_final$region == "Tokelau")
emdat_final[tokelau,"region"] <- "New Zealand"

# Tuvalu no existe en <world>, por lo que se quita.
tuvalu = which(emdat_final$region == "Tuvalu")
if(length(tuvalu)>0) emdat_final <- emdat_final %>% slice(-tuvalu)

## Por ultimo, En la base <emdat_final> hay datos para Montenegro, Serbia y aparte Serbia Montenegro, 
#  mientras que para <world> solamente hay datos para Serbia y Montenegro.
#  Por lo tanto, se agregan las regiones Serbia y Montenegro como Serbia Montenegro
serbia     = which(emdat_final$region=="Serbia" )
montenegro = which(emdat_final$region=="Montenegro"  )
emdat_final[c(serbia,montenegro),"region"] <- "Serbia Montenegro"

serbia2     = which(world$region == "Serbia")
montenegro2 = which(world$region == "Montenegro")
world[c(serbia2,montenegro2),"region"] <- "Serbia Montenegro"

## Se verifica si hay diferencias en los paises que estan en la columna region para ambas bases
diff <- sort(setdiff(emdat_final$region, world$region))

## Se cambian los nombres de <emdat_final> para que coincidan con los de <world>.
diff.world <- c("Bahamas","Bolivia", "Cape Verde", "Canary Islands","Cayman Islands", "Comoros", 
                "Democratic Republic of the Congo","Republic of Congo", "Cook Islands", "Ivory Coast", "Czech Republic",
                "Dominican Republic","Swaziland", "Gambia", "Iran", "North Korea" ,"South Korea", "Laos", "North Macedonia",
                "Marshall Islands", "Micronesia", "Moldova","Netherlands", "Niger" , "Northern Mariana Islands" ,"Palestine",
                "Philippines","Reunion","Russia" ,"Saint Barthelemy","Saint Helena", "Saint Kitts","Saint Martin" ,"Saint Vincent",
                "Sint Maarten","Sudan","Syria", "Taiwan","Tanzania", "Turks and Caicos Islands","United Arab Emirates",
                "UK","USA","Venezuela","Vietnam")

for (i in 1:length(diff)){
  indexes2 <- which(emdat_final$region == diff[i])
  emdat_final[indexes2,"region"] <- diff.world[i]
}

# En el primer grafico (Fig.1) los desastres se agrupan por pais para contar (<tally()>) cuantos hubo (<emdat_country$n>)
emdat_country <- emdat_final %>% 
  dplyr::group_by(region) %>% 
  tally()

emdat_country$decil <- ntile(emdat_country$n,n=10)

# Se juntan las dos bases, <world> y <emdat_country> por pais, i.e. <region>
merged_data <- inner_join(world, emdat_country) #<inner_join> es intereseccion y <outer_join> es union
merged_data$decil <- factor(merged_data$decil) ## Dejar claro que es variable discreta, no continua

# El siguiente codigo era para generar los deciles usando la funcion <cut>, pero fue reemplazado con <ntile> (la que se usa arriba)
if(0){
  #Generacion de deciles para el mapa
  deciles     <- quantile(emdat_country$n, probs=seq(0,1, by=0.1))
  # La funcion <cut> clasifica cada elemento de <merged_data$n> segun el decil al cual corresponda
  merged_data$decile <- cut(merged_data$n, breaks=deciles, labels=FALSE,include.lowest=TRUE) # A cada <region> le asigan un numero (decil) entre 1 y 10
  merged_data$decile <- factor(merged_data$decile) ## Dejar claro que es variable discreta, no continua
}

# Para realizar el resto de mapamundis primero es necesario ordenar la columna <Disaster.subgroup> alfabeticamente
emdat_final_sorted <- emdat_final %>% arrange(Disaster.Subgroup)

# El siguiente codigo generara una lista de objetos tipo dataframe. <group_split> divide el dataframe <emdat_final_sorted> en dataframes
# dependiendo del valor de la columna <Disaster.subgroup>, por lo que la longitud de la lista sera igual a los elementos unicos en 
# <Disaster.subgroup>. Cada dataframe de la lista contiene todos los desastres que pertenezcan a un valor de <Disaster.Subgroup>.
subgroup_splits <- emdat_final_sorted %>% 
  group_split(Disaster.Subgroup)
# Por otro lado, como anteriormente organizamos <emdat_final> alfabeticamente, los nombres de la lista <subgroup_splits> corresponde
# a <Tipos.Desastres>, que tambien esta organizado alfabeticamente.
names(subgroup_splits) <- Tipos.Desastres

# Por otro lado, por cada elemento de <subgroup_splits>, vamos a generar un dataframe que de cuenta del numero de desastres ocurridos por
# <region>. <results> es una lista que guarda dichos dataframes. Por tanto, cada elemento de <results> contiene un dataframe que indica 
# cuantos desastres de cierto tipo de desastre ocurrieron en cada pais.
results <- lapply(subgroup_splits, function(df) {
  df %>%
    group_by(region) %>% 
    tally()
})

# El siguiente codigo era para generar los deciles usando la funcion <quantile> y <cut>, pero fue reemplazada por la funcion <ntile>, ya que no genera
# problemas cuando los deciles tienen valores repetidos
if(0){
  # Para cada elemento de la lista <results> se generan los deciles. La funcion <tally> utilizada anteriormente genera la columna <n>.
  results_deciles <- lapply(results, function(df) {
    quantile(df$n, probs = seq(0, 1, by = 0.1))
  })
  
  # Agregar una columna <decile> a cada elemento de la lista <results>, ya que son dataframes.
  results_final<- lapply(1:length(results), function(i) {
    mutate(results[[i]], decile = cut(n, breaks=results_deciles[[i]], labels=FALSE,include.lowest=TRUE)) %>% 
      mutate(decile = factor(decile))
  })
}

results_final <- lapply(1:length(results), function(i) {
  mutate(results[[i]], 
         decile = ntile(n, 10)) %>% 
    mutate(decile = factor(decile))
})

# Colocarle nombres a la lista <results_final>
names(results_final) <- names(results)

# Cada elemento de la lista <results> se junta con los datos de <world> para poder graficarla
merged_data_disasters <- lapply(results_final, function(x) {
  inner_join(world, x ,by = "region")
})


#####=========================================== FIGURA 4 Y A.8 Pagnottoni  ==============================================###

niv.significancia <- 0.05 #<<<--- nivel de significancia para los estimados de retornos anormales
pattern_step      <- paste(steps, collapse = "|") # patron que indica los pasos. # collapse = "|" indica que puede ser cualquier valor de <steps>
pattern_indexes   <- paste(indexes, collapse = "|") #patron que indica los indices
pattern_countries <- paste(paises, collapse = "|")    #patron que indica los paises del desastre


if(!bool_paper) {europe_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Europe, 
                               significance.level=niv.significancia, pattern.step=pattern_step, 
                               pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                               labels=labels_grafico, color="orange", title.graph="Europe"); europe_plot2}
#ggsave(paste0(paste0("Graficos_CAR/Europe_",as.character(niv.significancia*100)),".png"),plot=europe_plot2,device="png")

#====
america_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Americas, 
                                significance.level=niv.significancia, pattern.step=pattern_step, 
                                pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                labels=labels_grafico, color="blue", title.graph="America");america_plot2
#ggsave(paste0(paste0("Graficos_CAR/America_",as.character(niv.significancia*100)),".png"),plot=america_plot2,device="png")

#====
asia_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Asia, 
                             significance.level=niv.significancia, pattern.step=pattern_step, 
                             pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                             labels=labels_grafico, color="tomato", title.graph="Asia");asia_plot2
#ggsave(paste0(paste0("Graficos_CAR/Asia_",as.character(niv.significancia*100)),".png"),plot=asia_plot2,device="png")

#====
africa_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Africa, 
                               significance.level=niv.significancia, pattern.step=pattern_step, 
                               pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                               labels=labels_grafico, color="magenta4", title.graph="Africa");africa_plot2
#ggsave(paste0(paste0("Graficos_CAR/Africa_",as.character(niv.significancia*100)),".png"),plot=africa_plot2,device="png")

#====
if(!bool_paper){ oceania_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Oceania, 
                                significance.level=niv.significancia, pattern.step=pattern_step, 
                                pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                labels=labels_grafico, color="olivedrab3", title.graph="Oceania");oceania_plot2}
#ggsave(paste0(paste0("Graficos_CAR/Oceania_",as.character(niv.significancia*100)),".png"),plot=oceania_plot2,device="png")

### Grafica con promedios en vez de CAR ======

if(!bool_paper){ europe_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Europe, 
                                       significance.level=niv.significancia, pattern.step=pattern_step, 
                                       pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                       labels=labels_grafico, color="orange", title.graph="Europe");europe_plot_aver}

#====
america_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Americas, 
                                        significance.level=niv.significancia, pattern.step=pattern_step, 
                                        pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                        labels=labels_grafico, color="blue", title.graph="America");america_plot_aver

#====
asia_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Asia, 
                                     significance.level=niv.significancia, pattern.step=pattern_step, 
                                     pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                     labels=labels_grafico, color="tomato", title.graph="Asia");asia_plot_aver

#====
africa_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Africa, 
                                       significance.level=niv.significancia, pattern.step=pattern_step, 
                                       pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                       labels=labels_grafico, color="magenta4", title.graph="Africa");africa_plot_aver

#====
if(!bool_paper){ oceania_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Oceania, 
                                        significance.level=niv.significancia, pattern.step=pattern_step, 
                                        pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                        labels=labels_grafico, color="olivedrab3", title.graph="Oceania"); oceania_plot_aver}


#### Graficas 1, A.1, A.6 y A.7 (mapamundi) ============================

# Tabla explicativa de los desastres en la muestra

summary.table <- emdat_interest_variables %>% 
  group_by(region,Disaster.Subgroup,Disaster.Type) %>% 
  dplyr::summarize(Count = n()) %>% 
  pivot_wider(names_from = Disaster.Type,values_from=Count,values_fill = 0) %>% 
  adorn_totals('row') %>%  # Obtener fila de totales
  adorn_totals('col') # Obtener columna de totales 
  
summary.table.latex <- xtable(summary.table,include.rownames=F); summary.table.latex # Obtener la tabla en formato LateX

# Tabla con los subtipos de desastres
dataframe.eventos <- eventos.final #<<<--- dataframe de eventos a los que se quiere sacar la tabla descriptiva.
                      # <emdat_interest_variables> es la base con todos los eventos y <eventos.final> es la base despues 
                      # de reducirla
# Si la base de datos tiene de nombre de columna 'Country' para los paises, cambiarla a 'region' para poder utilizar correctamente la tabla
if('Country' %in% colnames(dataframe.eventos)) colnames(dataframe.eventos)[colnames(dataframe.eventos)=='Country'] <- 'region'
   
summary.table2 <- dataframe.eventos %>%
  unite(Disaster.Category, Disaster.Subgroup, Disaster.Type, sep = " - ") %>%
  group_by(region, Disaster.Category) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Disaster.Category, values_from = Count, values_fill = 0) %>% 
  adorn_totals('row') %>%  # Obtener fila de totales
  adorn_totals('col') %>% # Obtener columna de totales  
  dplyr::select(sort(names(.)))

summary.table2.latex <- xtable(summary.table2,include.rownames=F)

# Grafica mapamundi natural breaks 

grupos <- 5       #<<<--- en cuantos grupos se quiere dividir el mapa
estilo <- 'jenks' #<<<--- cual es el estilo de division de los grupos, 'jenks' indica natural breaks, 'quantile' por cuantiles.
                  # ver <classIntervals> para mas estilos

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(size = 18, hjust = 0.5),  # Change the '16' to the desired font size for the title
  legend.title = element_text(size = 16),  # Change the '14' to the desired font size for the legend title
  legend.text = element_text(size = 14)  # Change the '12' to the desired font size for the legend labels
)

# Paleta de colores
palette <- brewer.pal(grupos, "YlOrRd")
# Breaks para natural breaks
division <- classIntervals(emdat_country$n, n = (grupos-1), style = estilo)
merged_data$breaks <- findInterval(merged_data$n,division$brks,rightmost.closed = F)
# Volver factor para graficar
labels.breaks = character(length = grupos)
for(i in seq_along(division$brks)){
  if(i==1) labels.breaks[i] <- paste0('[0',',',division$brks[2],')')
  labels.breaks[i] <- paste0('[',division$brks[(i-1)],',',division$brks[i],')')
}
merged_data$breaks <- factor(merged_data$breaks, labels = labels.breaks)

natural_breaks <- ggplot() +
  geom_polygon(data = merged_data, aes(x = long, y = lat, group = group, fill = breaks), color = "gray")  +
  scale_fill_manual(values = palette, name = 'No. Desastres') +
  coord_fixed(1.2) +
  ggtitle("Mapamundi de Desastres Naturales")+
  plain + 
  geom_path(data = world, aes(x = long, y = lat, group = group), 
            color = "black", linewidth = 0.5);natural_breaks

# Para diversos grupos tipos de desastre
# Paleta de colores
palette2 <- brewer.pal(grupos, "RdPu")

plots <- lapply(names(merged_data_disasters), function(name) {
  if(name == 'Biological') return(NULL) # Skip the 'Biological' category
  df <- merged_data_disasters[[name]]
  agrupado <- df %>% dplyr::select(region,n) %>%  group_by(region,n) %>%  distinct()
  # Breaks para natural breaks
  division <- classIntervals(agrupado$n, n = (grupos-1), style = estilo)
  df$breaks <- findInterval(df$n,division$brks,rightmost.closed = F)
  # Volver factor para graficar
  labels.breaks = character(length = grupos)
  for(i in seq_along(division$brks)){
    if(i==1) labels.breaks[i] <- paste0('[0',',',division$brks[2],')')
    labels.breaks[i] <- paste0('[',division$brks[(i-1)],',',division$brks[i],')')
  }
  df$breaks <- factor(df$breaks, labels = labels.breaks)
  ggplot() +
    geom_polygon(data = df, aes(x = long, y = lat, group = group, fill = breaks), color = "gray")  +
    scale_fill_manual(values = palette2, name = 'No. Desastres') +
    coord_fixed(1.2) +
    ggtitle(paste("Mapamundi de Desastres Naturales de tipo",name))+
    plain + 
    geom_path(data = world, aes(x = long, y = lat, group = group), 
              color = "black", linewidth = 0.5)
})
plots <- Filter(Negate(is.null), plots) # Eliminar el objeto <NULL>

# Combinar los mapamundis por tipo de desastre en un solo grafico
gridExtra::grid.arrange(grobs = plots, ncol = 2)

# Graficas para Pagnottoni, con <if(!bool_paper)> porque solo se corren para Pagnottoni.
if(!bool_paper){
  # Grafica mapamundi
  
  my_colors <- c("#24203B", "#0028C1", "#C4E5F2", "#4891A8", "#63CB92",
                 "#F7D73B", "#D0706C", "#8D5355", "#DB48A3", "#BC92F2")
  
  disasters <- ggplot(data = merged_data, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = decil)) +
    scale_fill_manual(values = my_colors) +
    ggtitle("Distribution of natural disasters by country") +
    plain
  
  disasters + 
    geom_path(data = world, aes(x = long, y = lat, group = group), 
              color = "black", linewidth = 0.5)
  
  
  plots <- lapply(names(merged_data_disasters), function(name) {
    df <- merged_data_disasters[[name]]
    ggplot(data = df, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = decile)) +
      scale_fill_manual(values = my_colors) +
      ggtitle(name) +
      plain+
      geom_path(data = world, aes(x = long, y = lat, group = group), 
                color = "black", linewidth = 0.5)
  })
  
  # Combinar los mapamundis por tipo de desastre en un solo grafico
  gridExtra::grid.arrange(grobs = plots, ncol = 2)
}  
