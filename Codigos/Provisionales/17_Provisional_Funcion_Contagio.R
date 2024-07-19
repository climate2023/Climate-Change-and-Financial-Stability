# Agregar parametro <all_indexes> para guardar vector de todos los indices y asi poder realizar el procedimiento para todos menos el del pais
# donde sucedio el evento

estimation.event.study.contagio <- function(bool.paper,bool.cds,base, data.events, market_returns, max.ar, es.start, es.end, add.exo =FALSE,
                                   vars.exo=NULL, GARCH=NULL, overlap.events = NULL, overlap.max, all_indexes = indexes){
  
  events.list <- list() # lista que contendra todas la informacion del modelo
  
  # Si GARCH = FALSE: Por cada evento se hace una regresion OLS con la muestra [-<es.start>,-<es.end>] dias antes del evento para estimar alfa, beta 
  # Si GARCH = TRUE:  Por cada evento se hace una regresion con ML para estimar la media y el GARCH con la muestra [-<es.start>,-<es.end>] dias 
  # antes del evento para estimar alfa, beta
  
  # Primero se crea una columna de los indices del <Start.Date> de cada desastre respectivo a <base>
  indices2 <- c()
  for (i in 1:nrow(data.events)) {
    indices2[i] <- match(data.events[i,'Start.Date'],index(base))
    if(is.na(indices2[i])){
      for(j in 1:nrow(base)){
        indices2[i] <- match(data.events[i,'Start.Date'] + j, index(base))
        if(!is.na(indices2[i])) break
      }
    }
  }
  data.events$indices2 <- indices2
  
  # Realizar lo mismo para <overlap.events> , si no es <NULL>
  if(!is.null(overlap.events)){
    indices2 <- c()
    for (i in 1:nrow(overlap.events)) {
      indices2[i] <- match(overlap.events[i,'Start.Date'],index(base))
      if(is.na(indices2[i])){
        for(j in 1:nrow(base)){
          indices2[i] <- match(overlap.events[i,'Start.Date'] + j, index(base))
          if(!is.na(indices2[i])) break
        }
      }
    }
    overlap.events$indices2 <- indices2
  }
  
  # Siguiendo a di Tommaso et al es necesario eliminar aquellos eventos que esten cercanos (5 dias) a algunos de otro pais. (p 3)
  eventos_reducidos_contagio <- data.events %>% 
    arrange(indices2) %>% 
    mutate(diferencia1 = c(0,diff(sort(data.events$indices2)))) %>% 
    mutate(diferencia2 = c(diff(sort(data.events$indices2)),0)) %>% 
    dplyr::filter(diferencia1 > 5, diferencia2 > 5)
  
  for(i in 1:nrow(data.events)){
    # Primero se encuentra a que dato le corresponde el dia del evento, y el dia final de la ventana de evento es el dia del evento
    # mas <max.ar>
    pais        <- as.character(data.events[i,'Country']) # Establece el pais donde sucedio el evento
    index_names <- matching(pais,bool.cds,bool.paper) # Nombre de la variable del <pais> con la que se calculan retornos anormales (ej: stock-index del pais)
    
    #### Indices seran todos menos al que pertenece el pais
    index_names <- setdiff(all_indexes, index_names)
    
    # Detener la funcion si no se tiene indice para el pais especificado
    if(is.null(index_names)) stop(paste0("No hay indice para el pais: ", pais))
    # Seleccionar indice y fecha del desastre
    event_start_index <- data.events[i,'indices2']
    event_start_date  <- data.events[i,'Start.Date']
    
    if(is.null(GARCH)){
      # Regresion por OLS del modelo de mercado
      # Loop para los casos en que haya mas de un indice por pais, se realiza regresion OLS para estimar alpha y beta
      # Nota: En general solo hay un indice por pais, pero en USA hay dos.
      for(name in index_names){
        # <window.event.dates> son las fechas que pertenecen a la ventana de evento
        window_event_dates <- index(base[,name][(event_start_index):(event_start_index+max.ar)])
        
        # Creacion de una base de datos exclusiva para el indice <name>, que luego sera utilizada para la estimacion
        # Asegurar que existe alguna columna de rezagos a traves de <length(grep(paste0(name,".l"),colnames(base)))> !=0, 
        # ya que si es igual a 0, entonces la funcion <create.lags> no genero ningun rezago para el indice <name>
        if(length(grep(paste0(name,".l"),colnames(base))) != 0){
          lags_name   <- colnames(base)[grep(paste0(name,".l"),colnames(base))]
          base_indice <- merge(base[,c(name,market_returns)],base[,lags_name])
        }else base_indice <- base[,c(name, market_returns)]
        
        ## Añadir <vars.exo> si <add.exo> ==<TRUE>
        ### pais index es el pais del indice . no del evento
        pais_index <- reverse.matching(name, bool.cds, bool.paper)
        if(add.exo == TRUE) base_indice <- merge(base_indice, base[,paste0(vars.exo,pais_index)])
        
        # Reducir el indice de la base para la estimacion.
        # La posicion del primer dia de la ventana de estimacion respecto al indice de <asset.returns> o <market_returns>
        # es (<event_start_index> - <es.start>) mientras que la posicion de ultimo dia de la ventana de estimacion es 
        # (<event_start_index> - <es.end>)
        base_estimacion <- base_indice[(event_start_index-es.start):(event_start_index-es.end),]
        # Realizar la estimacion usando <lm>
        model <- lm(as.formula(paste0(name,"~.")),data=base_estimacion) # <name> es la variable dependiente
        
        # Obtener los parametros estimados
        betas         <- coef(model)
        # Obtener el error estandar de los residuales
        standard_error <- summary(model)$sigma
        
        # Para obtener los datos "predicted" para la ventana de evento, se crea una base de variables exogenas 
        # cuyo indice sea <window_event_dates>, inlcuyendo los rezagos
        base_ev_window <- cbind(1,base_indice[,!colnames(base_indice) == name])[window_event_dates,]
        
        # Creacion series <observed>, <predicted> y <abnormal> solamente para la ventana de estimacion y la ventana de evento
        observed <- base_indice[,name][c(index(base_estimacion),window_event_dates)]
        # para <predicted> se usa model$fitted.values para los valores estimados durante la ventana de estimacion
        # y para la ventana de evento se usa <base_ev_window> %*% <betas>
        predicted <- rbind(xts(model$fitted.values,order.by = index(base_estimacion)),xts(base_ev_window %*% betas,order.by = window_event_dates))
        
        # Restar retornos estimados de los observados
        abnormal <- observed - predicted
        
        # Se juntan las tres series en un solo dataframe
        df             <- merge(observed,predicted,abnormal)
        # Cambio de nombre de columnas
        colnames(df)   <- c('Observed','Predicted','Abnormal')
        
        object <- new("ESmean",retornos=df,error_estandar=standard_error,res_estandar_estimacion=xts(NULL),
                      res_no_estandar_estimacion=xts(NULL),variance_forecast=xts(NULL))
        
        # Agregar <object> a la lista <events.list>, por lo que seria una lista de listas
        ### Guardar donde sucedio el desastre
        events.list[[paste(name,paste0('desastre',pais),i,sep="_")]] <- object
      }
    }else{
      # Regresion por ML del modelo de mercado + GARCH para la varianza
      # Loop para los casos en que haya mas de un indice por pais
      # Nota: En general solo hay un indice por pais, pero en USA hay dos.
      ### Para regresiones de contagio ya son mas. Debido a que son todos los idnices menos el del pais.
      for(name in index_names){
        # <window.event.dates> son las fechas que pertenecen a la ventana de evento
        window_event_dates <- index(base[,name][(event_start_index):(event_start_index+max.ar)])
        
        # Obtener el numero de rezagos para el modelo de la media, calculando el numero de columnas que en su nombre tengan
        # <(paste0(name,".l"))>
        p <- length(grep(paste0(name,".l"),colnames(base)))
        
        # Variables exogenas que dependen del pais
        ### pais_index es el pais del indice, no del desastre
        pais_index <- reverse.matching(name, bool.cds, bool.paper)
        variables_pais <- paste0(vars.exo,pais_index)
        
        # Loop necesario para asegurar que para cada evento siempre haya una estimacion. Cuando haya algun warning durante la estimacion,
        # el codigo va a volver a correr con el mismo numero de datos, pero rezagados un periodo, con el fin de buscar que siempre converja la estimacion
        # El loop va a correr hasta que <ugarchfit> corra sin ningun warning
        
        # <while_count> sera utilizado para contar cuantas veces se ha compleado una iteracion, con el fin de terminar el loop despues de una 
        # cantidad limite de iteraciones
        # <warning_count> sera el numero de veces que se encontro un warning, con el fin de reestimar el modelo con los datos
        # correctos pero con los coeficientes de los datos rezagados
        while_count   <- 0
        warning_count <- 0
        
        # <fin_estimacion> indica que la ventana de estimacion va hasta el dia anterior al dia de evento. Si se desea que termine mucho antes
        # faltaria parametrizarlo
        fin_estimacion      <- event_start_index - 1
        inicio_estimacion   <- event_start_index - es.start
        
        if(!is.null(overlap.events)) {
          # Ver si hay eventos en la base completa, es decir <overlap.events> por dentro de la ventana de estimacion del desastre
          subset_data <- subset(overlap.events, indices2 <= fin_estimacion)
          # Una anotacion importante es que es posible que el desastre de interes este dentro de <subset_data>, dependiendo del fin
          # de la ventana de estimacion, por lo cual se realiza <anti_join> solo para asegurar que no este dentro de <subset_data>
          subset_data <- suppressMessages(anti_join(subset_data, data.events[i,]))
          # <subset_data> es un data.frame que contiene todos los desastres de <overlap.events> que se encuentran anterior a nuestro desastre
          # de interes. La idea entonces es generar una variable dummy donde 1 sea en los dias que hubo uno de estos eventos
          # con el fin de controlar el posible confounding effect. 
          # Tambien seria interesante agregar a la dummy ciertos dias despues de cada desastre en <subset_data>, para lo cual se utilizara
          # la media de duracion por el tipo de desastre
          overlap.dummy <- rep(0, fin_estimacion)
          # Se van a revisar solamente los eventos que esten en una determinada ventana de traslape, definida por <overlap.max>, por lo que se va a filtrar
          # <subset_data> para solo dejar los eventos pertenecientes a la ventana de traslape
          subset_data <- subset_data %>% dplyr::filter(indices2 %in% ((event_start_index - overlap.max):event_start_index))
          # En <overlap.dummy> se va a colocar 1 para cada desastre en <subset_data> tanto para el dia del evento, como para los dias en que duro el evento, siguiendo
          # la columna <$Duracion>, pero para no tener eventos con una ventana de evento extremadamente larga debido a su duracion, va a colocarse 1 hasta maximo <max.ar>
          # dias
          subset_data <- subset_data %>% mutate(Duracion2 = ifelse(Duracion>max.ar,max.ar,Duracion)) %>% mutate(fin.evento = indices2+Duracion2-1)
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
            # Especificacion GARCH
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
                  variance.model = list(model = GARCH, garchOrder = c(1, 1)),
                  mean.model = list(
                    armaOrder = c(p, 0),
                    # Para la primera iteracion del loop <While> se utilizan los datos de la ventana de estimacion
                    external.regressors = as.matrix(cbind(base[(inicio_estimacion:fin_estimacion),c(variables_pais,market_returns)], 
                                                          overlap.dummy[inicio_estimacion:fin_estimacion]))
                  ),
                  distribution.model = "std"
                )
              }else{
                # Creamos una especificacion para el garch sin dummy (en el caso que <dummy.overlap> sea completamente 0)
                spec <- ugarchspec(
                  variance.model = list(model = GARCH, garchOrder = c(1, 1)),
                  mean.model = list(
                    armaOrder = c(p, 0),
                    # Para la primera iteracion del loop <While> se utilizan los datos de la ventana de estimacion
                    external.regressors = as.matrix(cbind(base[(inicio_estimacion:fin_estimacion),c(variables_pais,market_returns)]))
                  ),
                  distribution.model = "std"
                )
              }
            } else {
              # el caso cuando <overlap.dummy> es NULL
              spec <- ugarchspec(
                variance.model = list(model = GARCH, garchOrder = c(1, 1)),
                mean.model = list(
                  armaOrder = c(p, 0),
                  # Para la primera iteracion del loop <While> se utilizan los datos de la ventana de estimacion
                  external.regressors = as.matrix(base[(inicio_estimacion:fin_estimacion),c(variables_pais,market_returns)])
                ),
                distribution.model = "std"
              )
              
            }
            fit <- ugarchfit(spec, data = base[(inicio_estimacion:fin_estimacion), name], solver = "hybrid")
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
              fit <- ugarchfilter(adjusted_spec, data = base[((inicio_estimacion+warning_count):(fin_estimacion+warning_count)), name])
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
            cat('Fallo la convergencia, intentando con datos rezagados','\n')
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
          events.list[[paste(name,i,sep="_")]] <- NA
          break
        }
        
        # Asegurar que <fit> tenga los datos correctos usando <fitted()>. El ultimo dia de <fitted(fit)> debe ser igual que
        # <index(base[(indice_del_evento-1),])>, ya que ese fue el ultimo dia de estimacion.
        if(index(tail(fitted(fit),1)) != index(base[(event_start_index-1),])){
          # <format> se necesita para que aparezca con el formato fecha
          cat(format((index(tail(fitted(fit), 1))), "%Y-%m-%d"), '\n')
          cat(format(index(base[fin_estimacion,]),"%Y-%m-%d"),'\n')
          stop('Las fechas del modelo GARCH no corresponden a la ventana de estimacion')
        }else{
          cat(i,'Las fechas del modelo GARCH corresponden con la ventana de estimacion','\n')
        }
        
        # Ahora con el GARCH estimado necesitamos guardar la informacion en un objeto de clase "Esmean" (el cual creamos anteriormente)
        # Primero comenzamos con el dataframe de retornos, el cual es un objeto xts con los retornos observados, estimados y anormales
        # tanto para la ventana de estimacion como para la ventana de evento
        
        # La base de datos de variables exogenas durante la ventana de evento en caso que <overlap.events> no sea nula, y en el caso en que
        # <overlap.dummy> efectivamente haya entrado como regresora es:
        if(!is.null(overlap.events) & ncol(spec@model$modeldata$mexdata) == 4) base_ev_window <- cbind(base[(window_event_dates), c(variables_pais, market_returns)],0)
        # Para el caso en que <overlap.events> no sea nula, pero la dummy no haya entrado como regresora es
        if(!is.null(overlap.events) & ncol(spec@model$modeldata$mexdata) == 3) base_ev_window <- base[(window_event_dates), c(variables_pais, market_returns)]
        # Lo anterior porque el cuarto regresor es siempre 0 en la ventana de evento. Si hubiese un 1 estaríamos diciendo que se va a pronosticar
        # el efecto de un desastre durante la ventana de evento
        # Por otro lado, si no hay <overlap.events> la base de exogenas durante la ventana de evento seria
        if(is.null(overlap.events)) base_ev_window <- base[(window_event_dates), c(variables_pais, market_returns)]
        
        # Creacion series <observed>, <predicted> y <abnormal> solamente para la ventana de estimacion y la ventana de evento
        observed <- rbind(base[((inicio_estimacion+warning_count):(fin_estimacion+warning_count)),name],base[window_event_dates,name])
        
        # Para <predicted> se usa fitted(fit) para los valores estimados durante la ventana de estimacion y para la ventana de 
        # evento se usa el forecast de la media
        
        # <fit> puede ser de clase <ugarchfit> o de clase <ugarchfilter> dependiendo si no hubo convergencia la primera vez que se
        # estimo el GARCH. Dependiendo de su clase, toca realizar un proceso diferente ya que <ugarchforecast> no puede ser aplicada 
        # a objetos tipo <ugarchfilter>
        if(inherits(fit,"uGARCHfit")){
          forecast <- ugarchforecast(fit,n.ahead = (max.ar+1), 
                                     external.forecasts = list(mregfor= as.matrix(base_ev_window)))
        }else if(inherits(fit,"uGARCHfilter")){
          forecast <- ugarchforecast(adjusted_spec,data = base[((inicio_estimacion+warning_count):(fin_estimacion+warning_count)), name],
                                     n.ahead = (max.ar+1), 
                                     external.forecasts = list(mregfor= as.matrix(base_ev_window)))
        }
        predicted <- rbind(fitted(fit),xts(as.numeric(forecast@forecast$seriesFor),order.by = window_event_dates))
        
        # Restar retornos estimados de los observados
        abnormal <- observed - predicted
        
        # Se juntan las tres series en un solo dataframe
        df             <- merge(observed,predicted,abnormal)
        # Cambio de nombre de columnas
        colnames(df)   <- c('Observed','Predicted','Abnormal')
        
        if(inherits(fit,"uGARCHfit")){
          object <- new("ESmean",retornos=df,error_estandar=numeric(),res_estandar_estimacion=residuals(fit,standardize=TRUE),
                        res_no_estandar_estimacion=residuals(fit,standardize=FALSE),
                        variance_forecast=xts(forecast@forecast$sigmaFor^2,order.by = window_event_dates),
                        evento=data.events[i,],fit=fit@fit)
        }else if(inherits(fit,"uGARCHfilter")){
          object <- new("ESmean",retornos=df,error_estandar=numeric(),res_estandar_estimacion=residuals(fit,standardize=TRUE),
                        res_no_estandar_estimacion=residuals(fit,standardize=FALSE),
                        variance_forecast=xts(forecast@forecast$sigmaFor^2,order.by = window_event_dates),
                        evento=data.events[i,],fit=fit@filter)
        }
        
        
        # Agregar <object> a la lista <events.list>, por lo que seria una lista de listas
        ### Guardar donde sucedio el desastre
        events.list[[paste(name,paste0('desastre',pais),i,sep="_")]] <- object
      }
    }
  }
  return(events.list)
}