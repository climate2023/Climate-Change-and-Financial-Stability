##########################################################
# Tabla A1. Desastres separados por pais
# Autores: Juan Pablo Bermudez.
##########################################################

rm(list = ls())
# Cargar librerias y directorios ------------------------------------------
# Dentro de <01_Librerias_Directorios.R> se encuentra el source a las funciones
source(paste0(getwd(),'/Codigos/01_Librerias_Directorios.R'))

# Lectura de datos --------------------------------------------------------
# Lectura base BEI
load(paste0(Dir,'Procesado/Base_BEI.RData'))
# Lectura de la base de eventos
load(paste0(getwd(),'/Bases/EMDAT_PAPER.RData'))

desastres_pais <- emdat_base %>% 
  dplyr::select((c(Country,Disaster.Subgroup))) %>% 
  group_by(Country) %>% 
  summarise(Geophysical = sum(Disaster.Subgroup == 'Geophysical'),
            Hydrological = sum(Disaster.Subgroup == 'Hydrological'),
            Meteorological = sum(Disaster.Subgroup == 'Meteorological'), 
            Total=n())

total_row <- desastres_pais %>% 
  summarise(
    Country = "Total",
    Geophysical = sum(Geophysical),
    Hydrological = sum(Hydrological),
    Meteorological = sum(Meteorological),
    Total = sum(Total)
  )

desastres_pais <- bind_rows(desastres_pais, total_row)

### =============================== Graficas CAV==================================
# Directorio para imagenes de CAV
cd.cav <- paste0(cd.graficos,'CAV_BEI/')

# Prueba de filtro  -------------------------------------------------------
directorio.saved        <- paste0(getwd(),'/Resultados_regresion/')
# Parametros --------------------------------------------------------------
tipo.estudio            <- 'varianza' #<<<--- Puede ser de 'media' o 'varianza'
vol_ev_window           <- 15  #<<<--- Tamaño de la ventana de evento
nivel.desagregacion     <- 'tipodesastre'
tipo.serie              <- 'BEI'  
market                  <- 'PM'
ventana.estimacion <- c(500)
ventana.traslape   <- c('50')
tipo.bei           <- 'sinprima'
plazo.bei          <- 4

if(nivel.desagregacion == 'tipodesastre')      columna.agrupar <- 'Disaster.Subgroup'

# Volatility event study --------------------------------------------------
# load de los objetos de la varianza
load(paste0(getwd(),'/Resultados_regresion/BEI','_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.bei,'_',plazo.bei,'_year_Varianza','.RData'))
# Eliminar objetos NA ( Se realiza sobre los resultados con expectativas y desanclaje)
volatility_results_expectativas <- suppressWarnings(purrr::discard(volatility_results_expectativas,is.na))

# Eliminar tipos de desastres con con 0 y 1 evento
singular_disasters <- which((purrr::map(volatility_results_expectativas, ~.x@info.evento$Disaster.Subgroup) %>% 
                               as.character %>% 
                               table) < 2) %>% names
volatility_results_expectativas <- purrr::discard(volatility_results_expectativas, 
                                                  ~.x@info.evento$Disaster.Subgroup %in% singular_disasters)

# Separar la lista dependiendo de una columna en especifico introducida por el usuario 
if(columna.agrupar != 'Ambas') v.lista.separada <- split(volatility_results_expectativas, sapply(volatility_results_expectativas, function(x) x@info.evento[[columna.agrupar]]))

eventos.separados        <- c(unlist(lapply(v.lista.separada, length)),sum(unlist(lapply(v.lista.separada, length)))) 
names(eventos.separados) <- c(names(unlist(lapply(v.lista.separada, length))),'Todos')

# if(0) si no se quiere graficar CAV relativo al evento, if(1) si se desea
if(1){
  # Graficas CAV (separadas) ------------------------------------------------------------
  if(0){
    for(i in seq_along(v.lista.separada)){
      element <- v.lista.separada[[i]]
      name    <- names(v.lista.separada)[i] 
      png(filename=paste0(cd.cav,tipo.serie,'_',market,'_',name,'_CAV_Est_',ventana.estimacion,'_tra_',ventana.traslape,'.png'),
          width = 800, height = 800)
      grafico_cav(element,as.numeric(ventana.estimacion),vol_ev_window,serie.rm = paste('BEI'))
      title(paste0(name,'. ','Estimacion: ',ventana.estimacion,'. Traslape: ',ventana.traslape),line=0.75)
      dev.off()
    }
  }
  
  # Graficas CAV (agregadas) -------------------------------------------------------------
  if(columna.agrupar == 'Disaster.Subgroup'){
    png(filename=paste0(cd.cav,'Ag/',tipo.serie,'_',market,'_CAV_Est_',ventana.estimacion,'_tra_',ventana.traslape,'_plazo_',plazo.bei,'.png'),
        width = 1750, height = 800 )
    add.to.title <- paste0('Estimation window: ',ventana.estimacion,'. Overlap window: ',ventana.traslape,'. Disasters for Colombia')
    grafico_cav_agregado_bei(aggregated.events.list = volatility_results_expectativas, disagg.events.list = v.lista.separada, 
                         es.window.length = as.numeric(ventana.estimacion), ev.window.length = vol_ev_window, 
                         serie = (paste0(as.character(plazo.bei),' years ')), rm = 'BEI', extra.title = add.to.title
                         , cumulative = F)
    dev.off()
  }
}
    
