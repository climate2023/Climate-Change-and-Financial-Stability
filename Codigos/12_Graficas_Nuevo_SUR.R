
##########################################################
# Codigo de las graficas de resultados del nuevo sUR
# Autores: Juan Pablo Bermudez. 
##########################################################

rm(list=ls())
# Cargar librerias y directorios ------------------------------------------
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))

Tipos.Desastres  <- c("Geophysical","Hydrological","Meteorological")  #<<<--- Tipos de desastres considerados
paises   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
              'SouthAfrica','Turkey') 

# Parametros --------------------------------------------------------------
bool_paper <- T # booleano que toma el valor de T si se quiere revisar el paper que vamos a escribir, F para Pagnottoni
no.rezagos.de.desastres <- 15     #<<<--- Numero de rezagos de los desastres <w> (i.e. t0, t1, ..., tw)
tipo.serie              <- 'cds'  #<<<--- Puede ser 'cds' o 'indices
market                  <- 'PM'   #<<<--- Puede ser 'PM' o 'benchmark', pero si tenemos CDS solamente puede ser PM
if(market == 'benchmark') retorno.mercado <- 'MSCI'
if(market == 'PM')        retorno.mercado <- 'Promedio Movil'
if(tipo.serie == 'cds'){
  indexes <- c('CDSBrazil','CDSChile','CDSChina','CDSColombia','CDSIndonesia','CDSKorea',
               'CDSMalaysia','CDSMexico','CDSPeru','CDSSouthAfrica','CDSTurkey') #<<<--- Lista de los indices analizados.
  bdwidth <- 1 #<<<--- bandwidth para los graficos de densidad. Con una banda de 1 se suavizan los kernels para las series de CDS
}
if(tipo.serie == 'indices'){
  indexes <- c('BIST100','Bovespa','ChinaA50','JSX','KOSPI','S.PBMVIPC','S.PCLXIPSA','SouthAfricaTop40',
               'IGBVL','KLCI','COLCAP') # Nombre indices para el paper. JSX es el de Jakarta
  bdwidth <- 0.3 #<<<--- bandwidth para los graficos de densidad. Con una banda de 0.3 se suavizan los kernels para las series de indices
}


# Creacion de una funcion vectorizada de <grep>
vgrep <- Vectorize(grep, vectorize.args = 'pattern')
# Cargar resultados -------------------------------------------------------
load(paste0(getwd(), '/Resultados_SUR/Nuevo_SUR/Resultado_SUR_',tipo.serie,'_',market,'.RData'))

# Graficas ----------------------------------------------------------------
# De acuerdo con la notacion de los modelos estimados, los coeficientes el dia del evento terminan en <t0>, 
# el dia siguiente en <t1>, dos dias despues <t2>, y asi hasta llegar a <t4>. 
steps <- paste('t',(0:no.rezagos.de.desastres),sep='')  # vector con los días adelante del evento, hace referencia a como termina el nombre de las dummies

## El siguiente ciclo genera la densidad Kernel de los coeficientes para cada tipo de desastre 
## y para todos los <t0>, <t1> ...
# <if(0)> porque se encontro una manera mas eficiente
if(0){
  for(step in steps){
    for(model_name in names(coefficients_disasters_list)){
      dens_name <- paste("dens",model_name,step,sep="_")
      assign(dens_name,dens(coefficients_disasters_list[[model_name]][,"Estimate"],step))
    }
  }
}

# No existen resultados del SUR desagregados por pais y tipo de desastre al tiempo
if(nivel.desagregacion != 'pais.tipodesastre'){
  densidades.AR <- list()
  for(step in steps){
    lista.temp <- lapply(coefficients_disasters_list, function(x) dens(x[,'Estimate'],step, bdwidth))
    names(lista.temp) <- paste(names(lista.temp),step,sep='_')
    densidades.AR <- c(densidades.AR, lista.temp)
  }
  
  #  Por otro lado, si se desean graficas de CAR hasta t0, t1, t2 ..., se realiza un procedimiento similar
  densidades.CAR <- list()
  for(step in steps){
    accumulated.steps <- paste0('t',0:as.numeric(gsub('t','',step)),'$')
    lista.temp        <- lapply(coefficients_disasters_list, function(x) dens(x[,'Estimate'],accumulated.steps,bdwidth))
    names(lista.temp) <- paste(names(lista.temp),step,sep='_')
    densidades.CAR    <- c(densidades.CAR, lista.temp)
  }
}

# Por otro lado, necesitamos hacer la grafica de los CAR, que es la suma de los retornos anormales.
# Con el loop for estamos haciendo el mismo proceso para cada uno de los 5 modelos estimados.
# Al final tendremos un vector para cada modelo que incluye los coeficientes relacionados para las 5 
# dummies temporales para todos los paises. Lo anterior para posteriormente ser sumadas por cada pais para 
# generar el retorno anormal acumulado t_0+t_1+t_2+t_3+t_4

# if<0> porque mas adelante se realiza una manera mas eficiente de obtener los coeficientes
if(0){
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
}

# <if(0)> porque ya se realiza la densidad del CAR en el codigo de la linea 116
if(0){
  coefficients.dummies.list <- lapply(coefficients_disasters_list, function(x){
    # Se reunen los coeficientes en <coefs>
    coefs.temp <- x[,"Estimate"]
    # Se seleccionan solamente los coeficientes que acaben con <step> para retornarlos a <coefficients.dummies.list>
    return(coefs.temp[as.numeric(vgrep(paste0(steps, "$"),names(coefs.temp)))])
  })
  
  # Generamos la densidad de los retornos anormales acumulados para cada tipo de desastre
  # if<0> porque mas adelante se realiza una manera mas eficiente de obtener las densidades
  if(0){
    densidad_CAR_bio <- densidad_CAR(coef_vec_fitsur_Biological,indexes)
    densidad_CAR_cli <- densidad_CAR(coef_vec_fitsur_Climatological,indexes)
    densidad_CAR_geo <- densidad_CAR(coef_vec_fitsur_Geophysical,indexes)
    densidad_CAR_hyd <- densidad_CAR(coef_vec_fitsur_Hydrological,indexes)
    densidad_CAR_met <- densidad_CAR(coef_vec_fitsur_Meteorological,indexes)
  }
  densidades.CAR <- lapply(coefficients.dummies.list, densidad_CAR, indexes)
}

### =============================== Graficas de retornos anormales ==================================

# <if(0)> cuando no se quiera correr el codigo de las graficas de retornos anormales. <if(1)> cuando si se desee
if(0){
  # Establecer un directorio para los graficos de densidad de retornos anormales
  cd.retornos.anormales <- paste0(cd.graficos,'Dens_AR/')
  #Ya con las densidades de los retornos acumulados y de las dummies t_0, t_1, ..., t_4 podemos graficarlas
  
  # if(0) porque ya se grafica el CAR para cada step en el codigo que sigue
  if(0){
    # Para los CAR el vector seria
    for(step in steps){
      main_car <- paste0('Kernel Density of CAR ',step) #<<<--- titulo grafica
      # En primer lugar se obtienen  los objetos de <densidades.AR> que se desea graficar
      dens.CAR.keep <- densidades.CAR[map_lgl(names(densidades.CAR), ~endsWith(.x, step))]
      names.plot   <- sub("^fitsur_", "", names(dens.CAR.keep))
      names.plot   <- sub("^fitcoun_", "", names(dens.CAR.keep))
      names.plot   <- sub(paste0("_",step,"$"), "", names.plot)
      grafico_densidad(dens.CAR.keep, main= main_car, labels = sub(paste0("^fitsur_|_t0$"), "", names.plot))
      #savePlot(filename=paste0(cd.retornos.anormales,'Densidad_',tipo.serie,'_',market,'_',nivel.desagregacion,'_',step),type='png')
    }
  }
  
  # Ahora graficar los AR para cada <step> acumulados
  for(step in steps){
    title <- paste0('Kernel Density of CAR ',step, '. ',str_to_title(tipo.serie),' ', retorno.mercado)
    # En primer lugar se obtienen  los objetos de <densidades.AR> que se desea graficar
    dens.AR.keep <- densidades.AR[map_lgl(names(densidades.AR), ~endsWith(.x, step))]
    names.plot   <- sub("^fitsur_", "", names(dens.AR.keep))
    names.plot   <- sub("^fitcoun_", "", names(dens.AR.keep))
    names.plot   <- sub(paste0("_",step,"$"), "", names.plot)
    grafico_densidad(vector = dens.AR.keep, main= title, 
                     labels = sub(paste0("^fitsur_|_t0$"), "", names.plot))
    savePlot(filename=paste0(cd.retornos.anormales,'Densidad_',tipo.serie,'_',market,'_',nivel.desagregacion,'_',step),type='png')
  }
  
  # <if(0)> porque arriba ya se programo de una manera mas eficiente
  if(0){
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
  }
}

### ============================ Grafico 3 Pagnottoni. AR estimates ============================================================
# Directorio para imagenes de CAR
cd.ar <- paste0(cd.graficos,'ARestimates/')

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
  # Para los coeficientes <estimados>, buscar si dentro de su nombre se encuentra alguno de los strings contenidos en <steps>
  results <- lapply(estimados, function(x) vgrep(paste0(steps, "$"),names(x)))
  
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
  
  if(nivel.desagregacion == 'pais')         suppressWarnings(argumentos <- grafico_estimates_car2(lista.dataframes,'Abnormal Returns',paises,'red'))
  if(nivel.desagregacion == 'tipodesastre') suppressWarnings(argumentos <- grafico_estimates_car2(lista.dataframes,'Abnormal Returns',Tipos.Desastres,'red'))
  
  for(l in seq_along(argumentos)){
    png(filename=paste0(cd.ar,tipo.serie,'_',market,'_',nivel.desagregacion,'_',l,'_AR','.png'))
    do.call(grid.arrange, argumentos[[l]])
    dev.off()
  }
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
