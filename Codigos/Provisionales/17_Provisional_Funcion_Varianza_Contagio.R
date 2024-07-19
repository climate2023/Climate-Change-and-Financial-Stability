volatility_event_study_contagio = function(base.evento, date.col.name, geo.col.name, base.vol, interest.vars, num_lags, AR.m = 20, MA.m = 0,d = 0,
                                  bool = TRUE,metodo = "CSS", es.start,len.ev.window,var.exo,var.exo.pais,bool.paper,bool.cds,garch,
                                  overlap.events = NULL,overlap.max){
  
  # Generamos un dataframe que incluya los ordenes de rezagos para cada variable de interes, stock (Pagnottoni) o CDS
  # Tambien se tendra un parametro que le permita al usuario decidir cuantos rezagos desea en especifico para todas las variables de interes.
  # Si se desa el mismo numero de rezagos para todas las variables de interes, asignar el numero a <num_lags>. Si se desea un numero de 
  # rezagos para cada variable de interes, asignar una lista a <num_lags> con los numeros de rezagos. 
  # Nota: Si se coloca la lista, tiene que tener el mismo numero de datos que numero de variables de interes.
  
  # Si se desea que se elijan los rezagos siguiendo el criterio de informacion de Akaike, dejar <num_lags> como NULL
  
  lags_database <- arma_lags_database(base=base.vol,interest.vars,num_lags,AR.m, MA.m,d,bool,metodo)
  
  # Primero se crea una columna de los indices del <Start.Date> de cada desastre respectivo a <base>
  indices2 <- c()
  for (i in 1:nrow(base.evento)) {
    indices2[i] <- match(base.evento[i,'Start.Date'],index(base.vol))
    if(is.na(indices2[i])){
      for(j in 1:nrow(base.vol)){
        indices2[i] <- match(base.evento[i,'Start.Date'] + j, index(base.vol))
        if(!is.na(indices2[i])) break
      }
    }
  }
  base.evento$indices2 <- indices2
  
  # Realizar lo mismo para <overlap.events> , si no es <NULL>
  if(!is.null(overlap.events)){
    indices2 <- c()
    for (i in 1:nrow(overlap.events)) {
      indices2[i] <- match(overlap.events[i,'Start.Date'],index(base.vol))
      if(is.na(indices2[i])){
        for(j in 1:nrow(base.vol)){
          indices2[i] <- match(overlap.events[i,'Start.Date'] + j, index(base.vol))
          if(!is.na(indices2[i])) break
        }
      }
    }
    overlap.events$indices2 <- indices2
  }
  
  # Siguiendo a di Tommaso et al es necesario eliminar aquellos eventos que esten cercanos (5 dias) a algunos de otro pais. (p 3)
  eventos_reducidos_contagio <- base.evento %>% 
    arrange(indices2) %>% 
    mutate(diferencia1 = c(0,diff(sort(base.evento$indices2)))) %>% 
    mutate(diferencia2 = c(diff(sort(base.evento$indices2)),0)) %>% 
    dplyr::filter(diferencia1 > 5, diferencia2 > 5)
  
  lista_volatilidad <- list()
  for(i in 1:nrow(eventos_reducidos_contagio)){
    evento <- eventos_reducidos_contagio[i,]
    pais_evento   <- evento[geo.col.name]
    # Se obtiene primero el indice del pais donde sucedio el desastre
    indices <- matching(as.character(pais_evento),bool.cds,bool.paper)[1] # El 1 solamente se coloca porque con la base de Pagnottoni, USA tiene dos stocks
    # Ya no es necesario escribirlo cuando se utilicen los CDS, cada pais solo tiene un CDS
    
    #### Indices seran todos menos al que pertenece el pais
    indices <- setdiff(interest.vars, indices)
    
    # Variable que indica el inicio del evento
    indice_del_evento <- eventos_reducidos_contagio[i, 'indices2']
    
    # <fin_estimacion> indica que la ventana de estimacion va hasta el dia anterior al dia de evento. Si se desea que termine mucho antes
    # faltaria parametrizarlo
    fin_estimacion      <- indice_del_evento - 1
    inicio_estimacion   <- indice_del_evento - es.start
    
    # Estimacion APARCH con modelo de media ARMA -------------------------------
    for(indice in indices){
      # Se obtienen los ordenes para el modelo ARMA(p,q) del indice
      p   <- lags_database[indice,"p"]
      q   <- lags_database[indice,"q"]
      
      # Variables exogenas que dependen del pais (distinto al del desastre)
      pais_indice    <- reverse.matching(indice, bool.cds, bool.paper)
      variables_pais <- paste(var.exo.pais,pais_indice,sep="_")
      
      # Loop necesario para asegurar que para cada evento siempre haya una estimacion. Cuando haya algun warning durante la estimacion,
      # el codigo va a volver a correr con 500 datos, pero rezagados un periodo, con el fin de buscar que siempre converja la estimacion
      # El loop va a correr hasta que <ugarchfit> corra sin ningun warning
      
      # <while_count> sera utilizado para contar cuantas veces se ha compleado una iteracion, con el fin de terminar el loop despues de una 
      # cantidad limite de iteraciones
      # <warning_count> sera el numero de veces que se encontro un warning, con el fin de reestimar el modelo con los datos
      # correctos pero con los coeficientes de los datos rezagados
      while_count   <- 0
      warning_count <- 0
      
      if(!is.null(overlap.events)) {
        # Ver si hay eventos en la base completa, es decir <overlap.events> por dentro de la ventana de estimacion del desastre
        subset_data <- subset(overlap.events, indices2 <= fin_estimacion)
        # Una anotacion importante es que es posible que el desastre de interes este dentro de <subset_data>, dependiendo del fin
        # de la ventana de estimacion, por lo cual se realiza <anti_join> solo para asegurar que no este dentro de <subset_data>
        subset_data <- suppressMessages(anti_join(subset_data, eventos_reducidos_contagio[i,]))
        # <subset_data> es un data.frame que contiene todos los desastres de <overlap.events> que se encuentran anterior a nuestro desastre
        # de interes. La idea entonces es generar una variable dummy donde 1 sea en los dias que hubo uno de estos eventos
        # con el fin de controlar el posible confounding effect. 
        # Tambien seria interesante agregar a la dummy ciertos dias despues de cada desastre en <subset_data>, para lo cual se utilizara
        # la media de duracion por el tipo de desastre
        overlap.dummy <- rep(0, fin_estimacion)
        
        # Se van a revisar solamente los eventos que esten en una determinada ventana de traslape, definida por <overlap.max>, por lo que se va a filtrar
        # <subset_data> para solo dejar los eventos pertenecientes a la ventana de traslape
        subset_data <- subset_data %>% dplyr::filter(indices2 %in% ((indice_del_evento - overlap.max):indice_del_evento))
        # En <overlap.dummy> se va a colocar 1 para cada desastre en <subset_data> tanto para el dia del evento, como para los dias en que duro el evento, siguiendo
        # la columna <$Duracion>, pero para no tener eventos con una ventana de evento extremadamente larga debido a su duracion, va a colocarse 1 hasta maximo <max.ar>
        # dias
        subset_data <- subset_data %>% mutate(Duracion2 = ifelse(Duracion>len.ev.window,len.ev.window,Duracion)) %>% 
          mutate(fin.evento = indices2+Duracion2-1)
        # Por ultimo, es necesario asegurar que <fin.evento> no tenga valores mayores a la longitud de <overlap.dummy>
        subset_data <- subset_data %>% mutate(fin.evento = ifelse(fin.evento>length(overlap.dummy),length(overlap.dummy),fin.evento))
        
        # Ahora bien, asignamos el valor de 1 al dia del evento junto con su correspondiente duracion
        if(nrow(subset_data)>0) for(k in 1:nrow(subset_data)){
          overlap.dummy[subset_data[k,'indices2']:subset_data[k,'fin.evento']] <- 1
        }
        # <overlap.dummy> es del tamaño de <fin_estimacion>, por lo que en la estimacion se restringira a <inicio_estimacion>- <fin_estimacion>
        
        
      }
      
      while (TRUE) {
        warning_dummy <- FALSE
        # <tryCatch> corre el codigo, pero si encuentra algun warning o error, realiza un codigo especifico.
        tryCatch({
          if(!is.null(overlap.events)) {
            # Asegurar que la dummy tenga valores de 1 y 0, porque si solamente tiene valores de 1 , no va a converger y 
            # sera necesario rezagarla
            if(mean(overlap.dummy[inicio_estimacion:fin_estimacion]) == 1){
              warning('La ventana de estimacion tiene traslape completo con otros eventos')
            }
            # El warning va a forzar a la funcion <tryCatch> a rezagar la dummy y correr la siguiente iteracion
            
            # Se formula una especificacion de garch teniendo en cuenta que la dummy <overlap.dummy>
            # tiene datos 0 y 1, ya que si fuese solamente 0, no habría necesidad de la dummy
            if(mean(overlap.dummy[inicio_estimacion:fin_estimacion])>0){
              spec <- ugarchspec(
                variance.model = list(model = garch, garchOrder = c(1, 1)),
                mean.model = list(
                  armaOrder = c(p, q),
                  # Para la primera iteracion del loop <While> se utilizan los datos de la ventana de estimacion
                  external.regressors = as.matrix(cbind(base.vol[(inicio_estimacion:fin_estimacion),c(var.exo,variables_pais)], 
                                                        overlap.dummy[inicio_estimacion:fin_estimacion]))
                ),
                distribution.model = "std"
              )
            }else{
              # Creamos una especificacion para el garch sin dummy (en el caso que <dummy.overlap> sea completamente 0)
              spec <- ugarchspec(
                variance.model = list(model = garch, garchOrder = c(1, 1)),
                mean.model = list(
                  armaOrder = c(p, q),
                  # Para la primera iteracion del loop <While> se utilizan los datos de la ventana de estimacion
                  external.regressors = as.matrix(cbind(base.vol[(inicio_estimacion:fin_estimacion),c(var.exo,variables_pais)]))
                ),
                distribution.model = "std"
              )
            }
          } else {
            # el caso cuando <overlap.dummy> es NULL
            spec <- ugarchspec(
              variance.model = list(model = garch, garchOrder = c(1, 1)),
              mean.model = list(
                armaOrder = c(p, 0),
                # Para la primera iteracion del loop <While> se utilizan los datos de la ventana de estimacion
                external.regressors = as.matrix(base.vol[(inicio_estimacion:fin_estimacion),c(var.exo,variables_pais)])
              ),
              distribution.model = "std"
            )
            
          }
          fit <- ugarchfit(spec, data = base.vol[(inicio_estimacion:fin_estimacion), indice], solver = "hybrid")
          if(is.na(persistence(fit)) | persistence(fit)>=1) warning('El GARCH no es estacionario') # Lo anterior porque con un evento la persistencia era 11, 
          # y el forecast de la volatilidad daba numeros muy grandes
          
          # En algunos casos, <fit> tendra datos rezagados, por lo que debemos tomar los coeficientes de <fit> (el que convergio)
          # y realizar la estimacion para los datos correctos
          if(warning_count > 0 ){
            # La especificacion del GARCH es la misma que <fit>, y por tanto se usa <getspec()>
            adjusted_spec <- getspec(fit)
            # <setfixed> permite fijar los parametros de <fit>
            setfixed(adjusted_spec) <- as.list(coef(fit))
            # En el paquete <rugarch> cuando se fijan todos los parametros, no estima un nuevo modelo. La funcion <ugarchfit> sugiere
            # utilizar <ugarchfilter>, que "filtra" los nuevos datos con base al modelo previo.
            # Se necesita fit para obtener los residuales estandarizados y no estandarizados para el pronostico de la varianza condicional
            fit <- ugarchfilter(adjusted_spec, data = base.vol[((inicio_estimacion+warning_count):(fin_estimacion+warning_count)), indice])
          }
        },
        warning = function(wrn) {
          
          # El siguiente codigo solo corre en caso de que haya habido un warning en el bloque superior
          # Es importante usar <<- en vez de <-, ya que <warning> es una funcion, <function(wrn)>, por lo que se necesita asignar 
          # <warning_dummy>, <warning_count>, <inicio_estimacion> y <fin_estimacion> por fuera de <function(wrn)>
          warning_dummy <<- TRUE
          warning_count <<- warning_count + 1
          
          # Rezagar los indices del <inicio_estimacion> y <fin_estimacion>, para tener datos distintos en la siguiente iteracion
          # del loop <While>
          inicio_estimacion <<- inicio_estimacion - 1
          fin_estimacion <<- fin_estimacion - 1
          cat('Fallo la convergencia, intentando con datos rezagados.','\n')
        }, 
        error = function(e) {
          
          # Este error-handling se coloco porque puede haber un error de convergencia:
          # Error in diag(fit$robust.cvar) : invalid 'nrow' value (too large or NA)
          
          # El siguiente codigo solo corre en caso de que haya habido un error en el bloque de <tryCatch>
          # Es importante usar <<- en vez de <-, ya que <error> es una funcion, <function(e)>, por lo que se necesita asignar 
          # <warning_dummy>, <warning_count>, <inicio_estimacion> y <fin_estimacion> por fuera de <function(e)>
          warning_dummy <<- TRUE
          warning_count <<- warning_count + 1
          
          # Rezagar los indices del <inicio_estimacion> y <fin_estimacion>, para tener datos distintos en la siguiente iteracion
          # del loop <While>
          inicio_estimacion <<- inicio_estimacion - 1
          fin_estimacion <<- fin_estimacion - 1
          cat('Fallo la convergencia, intentando con datos rezagados','\n')
        })
        
        while_count <- while_count + 1
        # Romper el loop si no hubo warning
        if(!warning_dummy) break
        
        
        if(while_count == 40){
          warning("El modelo no converge despues de rezagar los datos 40 dias") # Falta parametrizar el numero maximo, <40>, pero no
          # estoy muy seguro de si es completamente necesario
          # Asignar <NULL> a <fit>
          fit <- NULL
          break
        }
      }
      
      if(is.null(fit)){
        lista_volatilidad[[paste(pais_evento,i,sep="_")]] <- NA
        next
      }
      
      # Asegurar que <fit> tenga los datos correctos usando <fitted()>. El ultimo dia de <fitted(fit)> debe ser igual que
      # <index(base.vol[(indice_del_evento-1),])>, ya que ese fue el ultimo dia de estimacion.
      if(index(tail(fitted(fit),1)) != index(base.vol[(indice_del_evento-1),])){
        # <format> se necesita para que aparezca con el formato fecha
        cat(format((index(tail(fitted(fit), 1))), "%Y-%m-%d"), '\n')
        cat(format(index(base.vol[fin_estimacion,]),"%Y-%m-%d"),'\n')
        stop('Las fechas del modelo GARCH no corresponden a la ventana de estimacion')
      }else{
        cat(i,indice,'Las fechas del modelo GARCH corresponden con la ventana de estimacion','\n')
      }
      
      gof_p_values        <- gof(fit,c(20,30,40,50))[,"p-value(g-1)"]
      names(gof_p_values) <- c("20 bins","30 bins","40 bins","50 bins")
      if(any(as.logical(gof_p_values < 0.05))) warning("Los residuales estandarizados no siguen la distribucion seleccionada.")
      cat(i,"\n")
      
      # Crear la serie de los residuales para la ventana de evento
      # <fit> puede ser de clase <ugarchfit> o de clase <ugarchfilter> dependiendo si no hubo convergencia la primera vez que se
      # estimo el GARCH. Dependiendo de su clase, toca realizar un proceso diferente ya que <ugarchforecast> no puede ser aplicada 
      # a objetos tipo <ugarchfilter>
      # La base de datos de variables exogenas durante la ventana de evento en caso que <overlap.events> no sea nula, y en el caso en que
      # <overlap.dummy> efectivamente haya entrado como regresora es:
      if(!is.null(overlap.events) & ncol(spec@model$modeldata$mexdata) == 4) base_ev_window <- cbind(base.vol[(indice_del_evento:(indice_del_evento+len.ev.window-1)), c(var.exo, variables_pais)],0)
      # Para el caso en que <overlap.events> no sea nula, pero la dummy no haya entrado como regresora es
      if(!is.null(overlap.events) & ncol(spec@model$modeldata$mexdata) == 3) base_ev_window <- base.vol[(indice_del_evento:(indice_del_evento+len.ev.window-1)), c(var.exo, variables_pais)]
      # Lo anterior porque el cuarto regresor es siempre 0 en la ventana de evento. Si hubiese un 1 estaríamos diciendo que se va a pronosticar
      # el efecto de un desastre durante la ventana de evento
      # Por otro lado, si no hay <overlap.events> la base de exogenas durante la ventana de evento seria
      if(is.null(overlap.events)) base_ev_window <- base.vol[(indice_del_evento:(indice_del_evento+len.ev.window-1)), c(var.exo, variables_pais)]
      if(inherits(fit,"uGARCHfit")){
        forecast <- ugarchforecast(fit,n.ahead = len.ev.window, 
                                   external.forecasts = list(mregfor= as.matrix(base_ev_window)))
        residual_evento <- base.vol[(indice_del_evento:(indice_del_evento+len.ev.window-1)),indice] - forecast@forecast$seriesFor
        fcast_var       <- as.numeric((forecast@forecast$sigmaFor)^2)
      }else if(inherits(fit,"uGARCHfilter")){
        forecast        <- ugarchforecast(adjusted_spec,data = base.vol[((inicio_estimacion+warning_count):(fin_estimacion+warning_count)), indice],
                                          n.ahead = len.ev.window, 
                                          external.forecasts = list(mregfor= as.matrix(base_ev_window)))
        residual_evento <- base.vol[(indice_del_evento:(indice_del_evento+len.ev.window-1)),indice] - forecast@forecast$seriesFor
        fcast_var       <- as.numeric((forecast@forecast$sigmaFor)^2)
      }
      
      # Convertir en xts <fcast_var>
      fcast_var <- as.xts(fcast_var, order.by = index(residual_evento))
      # Guardar objetos importantes
      object <- new("ESVolatility",coefficients=coef(fit),goodness_of_fit=gof_p_values,res_estandar_estimacion=residuals(fit,standardize=TRUE),
                    res_no_estandar_estimacion=residuals(fit,standardize=FALSE),variance_forecast=fcast_var,
                    residuales_evento=residual_evento,info.evento= evento)
      
      # Agregar <object> a la lista <lista.volatilidad>, por lo que seria una lista de listas
      lista_volatilidad[[paste(indice, paste0('desastre', pais_evento),i,sep='_')]] <- object
    }
  }
  return(lista_volatilidad)
}