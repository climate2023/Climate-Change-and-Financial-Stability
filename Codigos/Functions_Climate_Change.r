#---------------------------------- 1. chow_lin ------------------------------------#
# Genera la desagregacion temporal de una base de datos siguiendo el método de Chow-lin.
# Tanto la matriz de varianzas covarianzas como el procedimiento para realizar la desagregacion temporal fueron
# extraidas del articulo de Hurtado y Melo (2015).
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- time_Series_list: lista con las series de tiempo a las cuales se les quiere realizar la desagregacion temporal
#-- c               : matriz de agregacion
#-- w               : vector constante de valores 1
#-- var_covar       : matriz de varianzas y covarianzas
#-- base_indice     : base de datos en xts que dara el indice para la base de datos desagregada
# ----Argumentos de salida  ----#
#-- return_base: base de datos con la desagregacion temporal de cada una de los elementos en la lista time_Series_list
#---------------------------------------------------------------------------------------#
chow_lin <- function(time_Series_list, c, w, var_covar,base_indice){
  return_list <- list()
  for(series in time_Series_list){
    CW        <- c%*%w
    #model_gls <- gls(series ~ CW - 1, correlation = corAR1(value = alpha, fixed = TRUE)) 
    #sigma_sq  <- (model_gls$sigma)^2
    #V         <- (sigma_sq/(1-alpha^2))*var_covar # no se requiere <sigma_sq>, ya que no se necesita, se cancela  
    CVC       <- solve(c%*%(var_covar%*%t(c)))
    beta      <- (solve(t(CW)%*%(CVC%*%CW))%*%t(CW))%*%(CVC%*%series)
    y         <- w%*%beta + ((var_covar%*%t(c))%*%CVC)%*%(series-CW%*%beta)
    y          <- xts(y,order.by= index(base_indice)) 
    #gdp_growth <- diff(log(y))[2:nrow(y),]
    #return_list[[length(return_list)+1]] <- gdp_growth
    return_list[[length(return_list)+1]] <- y
  }
  return_base <- do.call(merge,return_list)
  return(return_base)
} 
#---------------------------------------------------------------------------------------#


#---------------------------------- 2. days  ------------------------------------#
# Genera la matriz de agregación trimestral, anhadiendo uno a los dias que corresponden a cada trimestre
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- x           : un entero que corresponde al trimestre (menos uno) tenido en cuenta
#-- m           : matriz de agregación (inicializada con ceros)
#-- months      : vector caracter tipo "yyyy-mm" de los meses que hacen parte de los trimestres a desagregar 
#-- dates       : vector caracter que contiene todos los dias que están en cierta base de datos (high-freq)
#-- startstring : numero que indica el inicio de la string a comparar (default = 1) 
#-- endstring   : numero que indica el final de la string a comparar (default = 7, ya que de ese modo la fecha de cada día estara en formato "yyyy-mm")
# ----Argumentos de salida  ----#
#-- m: la matriz de agregación ya con el valor de 1 en los días que pertenezcan a cierto trimestre
#---------------------------------------------------------------------------------------#
days <- function(x, m, months, dates, startstring=1, endstring=7){
  first_month  <- months[3*x+1] #Primer mes del trimeste
  second_month <- months[3*x+2]
  third_month  <- months[3*x+3]
  # Si alguno de los anteriores es <NA>, establecerlo como <FALSE> para que no moleste con el if statement
  if(is.na(first_month)) first_month <- FALSE
  if(is.na(second_month)) second_month <- FALSE
  if(is.na(third_month)) third_month <- FALSE
  for(date in dates){
    if(substr(date,startstring,endstring)==first_month |substr(date,startstring,endstring)==second_month|substr(date,startstring,endstring)==third_month){
      pos <- which(dates == date)
      m[x+1,pos] <- 1
    }
  }
  return(m)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 3. create_dummies  ------------------------------------#
# Genera las dummies t_0, t_1, t_2, t_3, t_4 y D para cada tipo de desastre (o país)
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- excel_file: un archivo excel que contiene los dias correspondientes a las dummies
#-- base.de.retornos   : serie de tiempo, se usara su indice para generar las dummies
#-- no.rezagos : Numero de rezagos de la primera dummy (t_0) se interpretan como n dias despues del desastre
# ----Argumentos de salida  ----#
#-- xts_dummies: Array con las dummies de todos los tipos de desastres (o paises) de tres dominsiones, donde la primera es el tipo 
#                de desastre (o el pais), la segunda el indice de fechas y la tercera los pasos adelante del desastre (0,...,no.rezagos) 
#---------------------------------------------------------------------------------------#
create_dummies <- function(excel_file, base.de.retornos, no.rezagos, bool.overlap = F, overlap.window =NULL) {
  #Lee el nombre de las hojas del archivo, cada hoja corresponde a un tipo de desastre
  sheet_names      <- excel_sheets(excel_file)
  xts_dummies      <- array(NA,dim=c(length(sheet_names), nrow(base.de.retornos), no.rezagos+2), 
                            dimnames = list(sheet_names,as.character(index(base.de.retornos)),c(paste0('t',0:no.rezagos),'D')))# +2: Por rezago 0 y por D
  #Loop para todas las hojas
  for(sheet_name in sheet_names) {
    #lee la hoja especifica
    current_sheet <- openxlsx::read.xlsx(excel_file, sheet = sheet_name, detectDates = TRUE)
    current_sheet <-  current_sheet %>% 
      dplyr::filter(between(t0, index(base.de.retornos)[1],tail(index(base.de.retornos),1)))
    
    # Se realiza un procedimiento cuando <bool.overlap> == F, el cual es asignar 1 a la dummy t0 siempre que haya un
    # desastre.
    if(bool.overlap == F){
      #Selecciona la columna t0 del archivo excel
      dummies_t0    <- current_sheet$t0
      #Se inicializa en ceros 
      t_0 <- c(rep(0,nrow(base.de.retornos)))
      
      ##Para generar las dummies, se utiliza un loop que evalua si el dia del desastre esta dentro de indice de
      # base.de.retornos. Si el dia esta, establece 1 en la posicion de ese dia.
      # Si no se encuentra, evalua si el dia calendario siguiente está en la base de base.de.retornos, si se encuentra establece 1
      # en ese dia. Así sucesivamente hasta encontrar el dia transable mas cercano al dia del desastre. 
      for(i in 1:length(dummies_t0)){
        for(j in 0:nrow(base.de.retornos)){
          if((as.Date(dummies_t0[i])+j) %in% index(base.de.retornos)){
            index_f <- which(index(base.de.retornos) == (as.Date(dummies_t0[i])+j)) 
            t_0[index_f] <- 1
            break
          }
        }
      }
    }else{
      # Cuando <bool.overlap> == T, se asignara 1 a la dummy <t_0> solamente para los eventos mas significativos
      # de acuerdo con una ventana de traslape <overlap.window>. Se comienza seleccionando el evento mas significativo
      # en terminos de personas afectadas. Luego, en orden de numero de afectados se van seleccionando los eventos, asegurando
      # que no esten dentro de +-<overlap.window> de otro evento mas significativo.
      # Tambien se va a generar una dummy <t_overlap>, donde 1 se asigne a los eventos que no estan siendo considerados en <t_0>
      #Selecciona la columna t0 del archivo excel
      current_sheet     <- current_sheet %>% arrange(desc(Total.Affected)) # ordenar base por orden de personas afectadas
      # Generar el indice que corresponde con <index(base.de.retornos)> para cada desastre, si el dia no se encuentra
      # entonces se busca el dia transable mas cercano al desastre
      indices.des <- c()
      for(p in 1:nrow(current_sheet)){
        for(j in 0:nrow(base.de.retornos)){
          if((as.Date(current_sheet[p,'t0'])+j) %in% index(base.de.retornos)){
            indices.des[p] <- which(index(base.de.retornos) == (as.Date(current_sheet[p,'t0'])+j)) 
            break
          }
        }
      }
      current_sheet$indices <- indices.des
      
      considered.events <- c(1) # en <considered.events> va a agregarse los indices de los eventos que no se traslapan.
      ventanas.traslape <- (current_sheet[considered.events,'indices']-bool.overlap):(current_sheet[considered.events,'indices']+bool.overlap)
      if(nrow(current_sheet > 1)) for(m in 2:nrow(current_sheet)){
        evento.iteracion <- current_sheet[m,]
        # <inicio.ventanas.traslape>indica para cada evento considerado el indice inicial donde no se va a considerar mas eventos
        # Por ejemplo, si se considero un evento en el indice 4000 y la ventana de traslape es 50, en <inicio.ventanas.traslape> habra
        # un 3950.
        if(!(evento.iteracion$indices %in% ventanas.traslape)){
          considered.events <- c(considered.events, m)
          ventanas.traslape <- c(ventanas.traslape, (current_sheet[m, 'indices']-bool.overlap):(current_sheet[m, 'indices']+bool.overlap))
        }
      }
      not.considered.events <- setdiff(1:nrow(current_sheet),considered.events)
      
      dummies.interest.t0  <- current_sheet[considered.events,c('t0','indices')]
      dummies.overlap.t0   <- current_sheet[not.considered.events,c('t0','indices')]
      #Se inicializa en ceros 
      t_0 <- c(rep(0,nrow(base.de.retornos)))
      t_0[dummies.interest.t0[,'indices']] <- 1
      
      # Se genera otra dummy para los eventos de traslape
      t_0.overlap <- rep(0, nrow(base.de.retornos))
      t_0.overlap[dummies.overlap.t0[,'indices']] <- 1
      }
    
    
    #Por otro lado, para formar las dummies t_1, t_2, t_3 y t_4 se usan rezagos de t_0, ya que en esta funcion
    #se asume que el n-paso adelante del evento es igual al n-ésimo día hábil después de t_0.
    #t_1 <- dplyr::lag(t_0,1)
    #t_2 <- dplyr::lag(t_0,2)
    #t_3 <- dplyr::lag(t_0,3)
    #t_4 <- dplyr::lag(t_0,4)
    
    #Se genera una matriz de la forma cbind(t_0, t_1, t_2, t_3, ...), los valores de <NA> se reemplazan por <0>  
    dummies_lags = matrix(0, length(t_0) , no.rezagos+1, dimnames=list(as.character(index(base.de.retornos)),paste0('t_',0:no.rezagos)) )
    for(Lags in 0:no.rezagos)
      dummies_lags[(1+Lags):length(t_0),Lags+1] = dplyr::lag(t_0, Lags)[(1+Lags):length(t_0)]
    
    #Se genera un dataframe de las dummies
    #dummies_df       <- cbind(t_0, t_1, t_2, t_3, t_4)
    #Se reemplaza los valores NA al inicio del dataframe por cero, ya que no se tienen en cuenta eventos anteriores
    #dummies_df_c     <- ifelse(is.na(dummies_df),0,dummies_df)
    
    #se genera la dummy D, la cual es 0 si en el dia i tanto t_0, t_1, t_2, t_3 y t_4 son 0, y 1 en otro caso
    #D <- c()
    #for(i in 1:nrow(dummies_df_c)){
    #  if(sum(dummies_df_c[i,]) == 0)        D <- c(D, 0)
    #  else if(sum(dummies_df_c[i,]) != 0)   D <- c(D, 1) 
    #}
    D = (rowSums(dummies_lags)!=0) + 0
    
    #Generamos el objeto xts
    dummies_complete <- cbind(dummies_lags, D)  
    dummies_xts      <- xts(dummies_complete,order.by=index(base.de.retornos))
    
    #Crear objetos distintos para cada hoja
    #xts_name        <- paste0(sheet_name, "_dummies_xts")
    #xts_dummies_list[[xts_name]] <- dummies_xts
    xts_dummies[sheet_name,,] <- dummies_xts
  }
  return(xts_dummies)
}
#---------------------------------------------------------------------------------------#


#--------------------------- 4. arma_seleccion_df --------------------------------------------#
#---- Toma una serie de tiempo, un rezago p y q maximos del modelo ARMA(p,q)
# y crea un dataframe de todos los posibles modelos con el criterio de Akaike y bayesiano.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- object: la serie de tiempo a al cual se le quiere encontrar el modelo
#-- AR.m  : rezago máximo de la parte autorregresiva
#-- Ma.m  : rezago máximo de la parte de promedio movil
#-- d     : orden de diferenciación
#-- bool  : booleano que indica si realizar la estimación arima con constante
#-- metodo: método por el cual se hará la estimación ARIMA (existe CS, ML y CSS-ML)
# ----Argumentos de salida  ----#
#-- df : dataframe donde cada fila representa los modelos ARIMA estimados. Las variables 
#--      del dataframe son: orden del polinomino AR (p), orden de integracion (d),
#--      orden del polinomio MA (q), criterio de Akaike (AIC) y Bayesiano (BIC)
#---------------------------------------------------------------------------------------#
arma_seleccion_df = function(object, AR.m, MA.m, d, bool, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(object, order = c(p, d, q), include.mean = bool, 
                    method = metodo)
      T.model = length(resid(fitp))
      Aic = T.model*log(sum(resid(fitp)^2))+ 2*(p+q+1)   ## De acuerdo con Enders 2014.
      Bic = T.model*log(sum(resid(fitp)^2)) + T.model*(p+q+1)  
      df[index,] = c(p, d, q, Aic, Bic)
      index = index + 1
    }
  }  
  return(df)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 5. lag_function  ------------------------------------#
# Encuentra los rezagos optimos para cada pais utilizando el criterio de Akaike, utilizando la función arma_seleccion_df.
# Además, la función tambien va a generar los rezagos y agregarlos a un dataframe. 
# Como tiene una función adentro, lag_function hereda los parámetros de arma_seleccion_df.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- base_niveles: base a la cual se le sacaran los rezagos
#-- ind         : caracteres indicando un indice bursatil
#-- AR.m        : rezago máximo de la parte autorregresiva
#-- MA.m        : rezago máximo de la parte de promedio movil
#-- d           : orden de diferenciación
#-- bool        : booleano que indica si realizar la estimación arima con constante (el default es TRUE)
#-- metodo      : metodo por el cual se hará la estimación ARIMA (existe CS, ML y CSS-ML) (el default es CSS)
#-- dia_inicial : sirve para seleccionar el dia desde el cual debe empezar la base de rezagos 
#                 (el default es <NULL>, en cuyo caso no se reduce la muestra)
# ----Argumentos de salida  ----#
#-- lags_reduced: objeto xts con los p-rezagos para cada país. La columna n de este objeto es el n-rezago de la serie original
#---------------------------------------------------------------------------------------#
lag_function <- function(base_niveles,ind,AR.m,MA.m,d,bool=TRUE,metodo="CSS",dia_inicial = NULL)
{
  
  #Utilizamos la funcion arma_seleccion_df para obtener el rezago para incluir en la ecuacion segun el 
  #criterio de Akaike. Como queremos ver AR(p), MA.m = 0, y como todos los retornos son estacionarios, 
  #entonces d =0.
  mod <- arma_seleccion_df(object=base_niveles[,ind], AR.m, MA.m, d, bool, metodo)
  p   <- mod[which.min(mod$AIC),'p']
  
  #generamos una base de datos que genere columnas de rezagos, el numero de columnas sera el mismo que el orden 
  #obtenido en el procedimiento anterior
  if(p>0) lags_df <- timeSeries::lag((base_niveles[,ind]), c(1:p) )
  
  if(!is.null(dia_inicial)){
    #Lo colocamos desde <dia_inicial> para cuadrar con el indice de la base de datos principal
    lags_df <- lags_df[paste0(dia_inicial,"/"),]
  }
  
  #Nombres del dataframe de rezagos
  colnames(lags_df) <- paste0(ind,'.l',1:p) 
  
  return(lags_df)
}
#---------------------------------------------------------------------------------------#

#--- Funcion que generaba el sistema de ecuaciones para estimar, fue mejorada por model_equation.LF ---#
if(0){
  #---------------------------------- 11. model_equation  ------------------------------------#
  # Genera una ecuación para el país country y teniendo en cuenta las variables 
  # exógenas exo. Lo anterior dado que vamos a estimar por el metodo SUR, el cual necesitara las 27 
  # ecuaciones siguiendo el paper de Pagnottoni.
  #---------------------------------------------------------------------------------------#
  # ----Argumentos de entrada ----#
  #-- database : base de datos donde se encuentran gran parte de las variables
  #-- country  : caracteres indicando un pais
  #-- exo      : conjunto de variables exogenas, especificamente las dummies. En el primer ejemplo son dummies 
  #            por tipo de desastre
  # ----Argumentos de salida  ----#
  #-- eq: ecuación de variable dependiente ~ regresoras.
  #---------------------------------------------------------------------------------------#
  model_equation <- function(database,country,exo){  
    
    #Busca el dataframe con los rezagos
    lags_name <- paste0("lags_reduced_", country)
    if(exists(lags_name)==TRUE) lags_df <- get(lags_name) ## Primero toca ver si lags_name existe, ya que si existe algun país que no
    #  tenga matriz de rezagos, lags_name no existe. En nuestro caso todos los países tienen rezagos.
    
    #Busca las variables para el gdp y el fdi
    gdp_variable <- paste("gdp",country,sep="_")
    fdi_variable <- paste("fdi",country,sep="_")
    
    #Genera la ecuacion n.4 por el país country
    eq  <- database[,country]  ~ database[,"Mean_Returns_Moving_Averages"] +exo + database[,gdp_variable] +
      database[,fdi_variable] + lags_df  ## Se asume el nombre "Mean_Returns_Moving_Averages, que fue nombrada en la linea 99 del otro codigo
    return(eq)
  }
  #---------------------------------------------------------------------------------------#
}

#-------------------------------- 6. model_equation.LF----------------------------------------#
# Genera una ecuación para el país country y teniendo en cuenta las variables 
# Lo anterior dado que vamos a estimar por el metodo SUR, el cual necesitara las 27 ecuaciones siguiendo el paper de Pagnottoni.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- database     : base de datos donde se encuentran gran parte de las variables
#-- country      : caracteres indicando un pais
#-- ind          : caracteres indicandon un indice
#-- var.exo      : conjunto de variables exogenas que no dependen del pais 
#-- var.exo.pais : conjunto de variables exogenas que dependen del país
#-- Lags         : string que indica el nombre por el cual empiezan las matrices de rezagos en el codigo principal
# ----Argumentos de salida  ----#
#-- eq: ecuación de variable dependiente ~ regresoras para el país country. 
#---------------------------------------------------------------------------------------#
model_equation.LF <- function(database, country, ind, var.exo, var.exo.pais, Lags){  
  
  #Busca el dataframe con los rezagos
  #lags_name <- paste0("lags_reduced_", country)
  #if(exists(lags_name)==TRUE) lags_df <- get(lags_name) ## Primero toca ver si lags_name existe, ya que si existe algun país que no
  #  tenga matriz de rezagos, lags_name no existe. En nuestro caso todos los países
  #  tienen rezagos.
  
  #Busca las variables para el gdp y el fdi
  var.exo.pais.total = c()
  for (i in 1:length(var.exo.pais))
    var.exo.pais.total = c(var.exo.pais.total, paste(var.exo.pais[i], country, sep="_"))
  
  lag.matrix           =  get(paste0(Lags,'_',ind))
  
  #Genera la ecuacion n.4 por el indice ind
  eq  <- database[,ind]  ~ database[,c(var.exo, var.exo.pais.total)] + lag.matrix  
  return(eq)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 7. dens  ------------------------------------#
# Genera la densidad kernel para los coeficientes de las dummies dependiendo de <step> y del modelo <fit>
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- fit : modelo estimado al que se le puede sacar coeficientes
#-- step: pasos adelante despues del evento
#-- bandwidth : bandwidth para la densidad
# ----Argumentos de salida  ----#
#-- densidad: densidad kernel de los coeficientes del modelo estimado
#---------------------------------------------------------------------------------------#
# Creacion de una funcion vectorizada de <grep>
vgrep <- Vectorize(grep, vectorize.args = 'pattern')
dens <- function(fit, step,bandwidth){
  interest_indices    <- (vgrep(step,names(fit)))
  coeficientes        <- fit[interest_indices]
  coeficientes.matrix <- matrix(coeficientes, nrow(interest_indices),ncol(interest_indices))
  # interest_coefficients <- fit[interest_indices]
  # Cada fila de <coeficientes.matrix> corresponde a una serie, mientras que cada columna corresponde a un dia
  # t0,t1,t2,...
  # Si hay mas de una columna, entonces se desea primero sumar por filas para obtener el CAR agregado hasta ese dia
  if(ncol(coeficientes.matrix) == 1) interest_coefficients <- coeficientes.matrix
  if(ncol(coeficientes.matrix) > 1)  interest_coefficients <- rowSums(coeficientes.matrix)
  densidad <- density(interest_coefficients, bw = bandwidth)
  return(densidad)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 8. densidad_CAR ------------------------------------#
# Genera la densidad de los retornos acumulados. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- x: toma un vector de coeficentes
#-- countries: vector de paises
# ----Argumentos de salida  ----#
#-- densidad_c: densidad de los retornos acumulados 
#---------------------------------------------------------------------------------------#

densidad_CAR <- function(x,indices){
  CAR <- c()
  for(ind in indices){
    #Mira dentro el vector de coeficientes cuales inician con ind, es decir el pais y luego los suma
    start_with <- paste0("^",ind)
    sum_of_coefficients <- sum(as.numeric(x[grep(start_with, names(x))]))
    #Agrega la suma (el CAR) en un vector, para luego hallar la densidad
    CAR <- c(CAR,sum_of_coefficients)
  }
  densidad_C <- density(CAR, bw = 0.3)
  return(densidad_C)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 9. grafico_densidad  ------------------------------------#
# Genera la densidad de los retornos acumulados. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- vector   : un vector que indica los objetos a graficar
#-- labels   : leyenda del grafico
#-- colors   : colores de las lineas del gráfico
#-- main     : título del grafico
#-- width    : ancho de las lineas (default = 2)
#-- position : posicion de la leyenda (default = "topright")
# ----Argumentos de salida  ----#
#-- NA. No retorna argumentos, más bien un gráfico que incluye las 5 densidades (biologico, climatológico
#-- hidrologico, geologico, meteorologico).
#---------------------------------------------------------------------------------------#
grafico_densidad <- function(vector,main,labels,width=2, position = "topright"){
  # if(0) porque se encontro una manera mas eficiente de desarrollarlo
  if(0){
    maximo_y <- c()
    minimo_x <- c()
    maximo_x <- c()
    for(i in vector[1:length(vector)]){
      maximo_y <- c(maximo_y, max(get(i)$y))
      minimo_x <- c(minimo_x, min(get(i)$x))
      maximo_x <- c(maximo_x, max(get(i)$x))
    }
    limite_y          <-  max(maximo_y)
    limite_min_x      <-  min(minimo_x)
    limite_max_x      <-  max(maximo_x)
  }
  
  # SE encuentra los limites de los ejes x y y para el grafico
  limite_y     <- max(unlist(purrr::map(vector, ~max(.x$y))))
  limite_min_x <- min(unlist(purrr::map(vector, ~min(.x$x))))
  limite_max_x <- max(unlist(purrr::map(vector, ~max(.x$x))))
  
  # Se determinan los colores para graficar
  if(length(vector) <= 9){
    colors <- brewer.pal(n=length(vector), name='Set1')
  }else{
    colors <- c(brewer.pal(n=7, name='Set1'), brewer.pal(n=(length(vector) - 7), name='Dark2'))
  }
  x11()
  plot(vector[[1]], main = main, col = colors[1],lwd=width,ylim=c(0,limite_y),xlim=c(limite_min_x,limite_max_x))
  for(i in 2:length(vector)){
    lines(vector[[i]],col=colors[i],lwd=width)
  }
  legend(position,legend = labels,col = colors, lwd = width)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 10. grafico_retornos  ------------------------------------#
# Genera la densidad de los retornos acumulados. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- list     : lista que contiene las densidades a graficar
#-- vector   : un vector especifico que incluye el nombre del gráfico más los objetos a graficar
#-- main     : titulo del grafico
#-- legends  : leyenda del grafico
#-- colors   : colores de las lineas del gráfico
#-- width    : tamaño de las lineas (default = 3)
#-- position : posicion de la leyenda (default = "topleft")
# ----Argumentos de salida  ----#
#-- NA. No retorna argumentos, más bien un gráfico que incluye las densidades de los retornos de los indices relacionados 
#-- a los paises incluidos en vector.
#---------------------------------------------------------------------------------------#
grafico_retornos <- function(list,vector,main,legends,colors, width=3, position="topleft"){
  maximo_y <- c()
  minimo_x <- c()
  maximo_x <- c()
  
  for(country in vector[1:length(vector)]){
    
    maximo_y <- c(maximo_y, max(list[[country]]$y))
    minimo_x <- c(minimo_x, min(list[[country]]$x))
    maximo_x <- c(maximo_x, max(list[[country]]$x))
  }
  
  limite_y          <-  max(maximo_y)
  limite_min_x      <-  min(minimo_x)
  limite_max_x      <-  max(maximo_x)
  
  x11()
  plot(list[[vector[1]]], main = main, col = colors[1],lwd=width,ylim=c(0,limite_y),xlim=c(limite_min_x,limite_max_x))
  for(i in 2:length(vector)){
    lines(list[[vector[i]]],col=colors[i], lwd=width)
  }
  legend(position,legend = legends,col = colors, lwd=width)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 11. grafico_estimates y grafico_estimates_car  ------------------------------------#
# La siguiente función es para evitar la repeticion en los graficos AR_estimate y sus t-tests. 
# La diferencia entre <grafico_estimates> y <grafico_estimates_car> es el hecho que <grafico_estimates> grafica todos los retornos
# anormales promedio estimados, mientras que <grafico_estimates_car> los agrega en CAAR (retorno anormal acumulado promedio)
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- object : objeto a graficar (el dataframe incluye group, values, subgroup)
#-- yaxis  : titulo del eje y
#-- title  : titulo del grafico
#-- colors : colores
# ----Argumentos de salida  ----#
#-- NA. No genera argumentos, pero si un grafico de los retornos anormales o estadísticos t.
#---------------------------------------------------------------------------------------#
grafico_estimates <- function(object,yaxis,title,colors){
  ggplot(object, aes(x=group,y=values,fill=subgroup))+
    geom_bar(stat="identity", position="dodge", width=0.7) +
    scale_fill_manual(values= colors) +
    labs(y=yaxis,title=title) +
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_blank(),
          axis.title.x = element_blank())
}

grafico_estimates_car2 <- function(object.list,yaxis,title,colors){
  car.maximo <- 0
  car.minimo <- 0
  result_list <- lapply(object.list, function(x){
    new_df <- x %>% 
      group_by(group) %>% 
      summarise(car = sum(values))
    if (max(new_df$car) > car.maximo) car.maximo <<- max(new_df$car)
    if (min(new_df$car) < car.minimo) car.minimo <<- min(new_df$car)
  })
  
  plots <- list()
  for(i in 1:length(object.list)){
    object <- object.list[[i]]
    titulo <- title[i]
    object_car <- object %>% 
      group_by(group) %>% 
      summarise(sum_ar = sum(values))
    
    plots[[titulo]] = ggplot(object_car, aes(x = group, y = sum_ar)) +
      geom_bar(stat = "identity", fill = colors, position = position_dodge(width = 0.8), width = 0.5) +
      labs(y = yaxis, title = titulo) +
      coord_cartesian(ylim = c(car.minimo, car.maximo))+
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white"), # Set the background color to white
        panel.border = element_rect(color = "black", fill = NA) # Add black border around the graph
      )+
      geom_hline(yintercept = 0, color = "black", linetype = "dashed") # anhadir linea solida en eje x
  }

  # Para una mejor creacion de graficos, se van a separar en <n> elementos de 6 objetos maximo
  elementos.por.lista <- 6
  num.sublistas       <- round(length(plots)/elementos.por.lista,0)
  # Separar la lista de plots
  plots.separado <- split(plots, rep(1:num.sublistas, each = elementos.por.lista))
  
  # colocar datos en eje x para los ultimos plots de cada sublista
  for(m in seq_along(plots.separado)){
    plots.separado[[m]][[length(plots.separado[[m]])]] <- plots.separado[[m]][[length(plots.separado[[m]])]] +
      labs(x='Index')+
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.title.x = element_text(hjust=0.5))
  }
  
  # Crear una lista de argumentos para poder graficar
  plots_args <- list()
  for(l in seq_along(plots.separado)){
    plots_args[[l]] <- c(plots.separado[[l]], list(nrow = length(plots.separado[[l]]), ncol = 1, heights = c(rep(1, length(plots.separado[[l]]) - 1), 1.7)))
  }

  return(plots_args)
}
#---------------------------------------------------------------------------------------#


#----------------------------------- 12. order_coef --------------------------------------------------#
# Ordena un vector de nombres de coeficientes basado en otro vector
# ------------------------------------------------------------------------------------------------
# ----Argumentos de entrada ----#
#-- vec_desordenado : vector que se esta buscando ordenar
#-- vec_orden       : vector que dara el orden
# ----Argumentos de salida  ----#
#-- vec_ordenado    : vector numerico con el orden 
#---------------------------------------------------------------------------------------#
order_coef <- function(vec_desordenado, vec_orden){
  indexes <- c()
  for (i in 1:length(vec_orden)){
    indexes <- c(indexes, grep(vec_orden[i],vec_desordenado))
  }
  vec_ordenado = vec_desordenado[indexes]
  return(vec_ordenado)
}
#---------------------------------------------------------------------------------------#


#----------------------------------- 13. car_countries2 --------------------------------------------------#
# Genera una grafica para cada indice de los retornos anormales acumulados (CAR) promedios dependiendo del continente y un
# nivel de significancia.
# ------------------------------------------------------------------------------------------------
# ----Argumentos de entrada ----#
# continent_coefficients    : Una lista de coeficientes, junto a su error estandar, t_value y p_value. 
# significance.level        : El nivel de significancia que deben tener los coeficientes estimados para graficarlos
# pattern.step              : Al estimar los modelos por continente, los coeficientes tienen un nombre característico, pattern.step
#                             se utiliza para extraer la parte que corresponde a en que dia despues del evento se refiere el coeficiente (t0,t1,t2...)
# pattern.indexes           : string que es la parte del nombre del coeficiente que se refiere al pais del indice bursatil
# pattern.countries         : string que es la parte del nombre del coeficiente que se refiere al pais donde ocurre el desastre
# order.graph               : vector de nombres de paises que indicara el orden en que se graficara
# labels                    : leyendas del eje de los indices bursatiles
# color                     : color para la grafica
# title.graph               : titulo para la grafica
# ----Argumentos de salida  ----#
#-- plot_continent   : objeto tipo ggplot para poder graficar los retornos anormales acumulados (CAR) promedio para cada continente
#                      dependiendo de un nivel de significancia
#---------------------------------------------------------------------------------------#
car_countries2 <- function(continent_coefficients, significance.level, pattern.step, pattern.indexes, pattern.countries, order.graph, labels, color, title.graph){
  
  # Generamos un dataframe que va a guardar los parametros estimados, el error estandar, el valor del t test y el p_value.
  dataframe_modelo <- data.frame(Estimate=double(), SD_error=double(), t_value=double(), p_value=double()) 
  
  # Filtrar por aquellos que acaben en t0, t1, t2, t3 o t4.
  for (element in continent_coefficients){
    ## Genera un dataframe con el estimado, error estandar, t_value, p_value, que salen de la estimacion element
    dataframe_coef <- as.data.frame(element) 
    ## Le cambiamos nombres al dataframe para mejor manejo, pero reflejan lo mismo
    colnames(dataframe_coef) <- c("Estimate","SD_error","t_value","p_value")
    ## Extraemos las filas que nos interesan, es decir aquellas de las dummies, que acaban en t0, t1, t2, t3 o t4, es decir steps
    dataframe_coef_filtrado  <- dataframe_coef %>% 
      dplyr::filter(str_ends(row.names(.),pattern = pattern.step)) %>% 
      # Filtramos tambien para aquellos coeficientes que tengan un nivel de significancia menor que niv.significancia
      dplyr::filter(p_value < significance.level)
    ## Dejamos todos los coeficientes que cumplen con las condiciones en un solo dataframe
    dataframe_modelo   <- rbind(dataframe_modelo, dataframe_coef_filtrado)
  }
  
  ## Ahora para poder promediar los retornos anormales es necesario poder extraer el pais del indice y el step, usando la funcion
  ## str_extract, que extrae el primer valor identico entre dos strings. Esto permite extraer el indice, que en cada fila aparece 
  ## de primer lugar
  string_start <- stringr::str_extract(row.names(dataframe_modelo), pattern.indexes) 
  # cualquier valor de countries
  string_end   <- stringr::str_extract(row.names(dataframe_modelo), pattern.step)  #En caso de querer revisar coeficientes por step
  
  ## Usamos la misma funcion para extraer el nombre del pais donde sucedio el desastre, que se encuentra de ultimas usando <str_extract_all>
  string_pais <- unlist(lapply(stringr::str_extract_all(row.names(dataframe_modelo),pattern.countries), function(x) x[(length(x))]))
  
  dataframe_modelo <- cbind(dataframe_modelo,string_start,string_pais,string_end)
  
  ## Ellos mencionan que realizaran un promedio del CAR, por lo cual realizaremos el CAR de cada pais que tenemos datos
  #  para cada indice y realizaremos un promedio
  
  promedio_car <- dataframe_modelo %>% 
    group_by(string_start, string_pais) %>% 
    summarise(CAR = sum(Estimate)) %>% # El nombre Estimate sale del nombre que le colocamos anteriormente
    group_by(string_start) %>% 
    summarise(mean_CAR = mean(CAR)) %>% 
    arrange(match(string_start,order.graph))
  
  ## Por ultimo, se agregan a las graficas los indices que no contienen valores, de modo que al graficar no saldra dato (es 0)
  #  pero sirve para comparar con la grafica del paper
  dataframe_pagnorden <- data.frame(string_start = order.graph)
  
  #Juntar las dos dataframes y rellenar con 0 los datos faltantes
  promedio_car_all <- left_join(dataframe_pagnorden, promedio_car, by = "string_start") # string_start sale de un nombre que le colocamos en la funcion
  promedio_car_all$mean_CAR <- ifelse(is.na(promedio_car_all$mean_CAR), 0, promedio_car_all$mean_CAR)
  promedio_car_all$string_start <- factor(promedio_car_all$string_start, levels = promedio_car_all$string_start)
  
  plot_continent <- ggplot(data = promedio_car_all, aes(y = mean_CAR, x = string_start, fill = mean_CAR < 0)) +
    geom_col() +
    scale_fill_manual(values = c(color, color)) +
    coord_flip()+ 
    scale_x_discrete(labels = labels)+
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill = "none") +
    labs(y = paste0("Average CAR | p-value < ",percent(significance.level)),x="Index",title = title.graph)
  return(plot_continent)
}
#---------------------------------------------------------------------------------------#


#----------------------------------- 14. average_countries2 --------------------------------------------------#
# Genera una grafica para cada indice de los retornos anormales promedio dependiendo del continente y un
# nivel de significancia. Es parecida a car_countries2, solamente que en vez de realizar el promedio de los retornos anormales
# acumulados, realiza un promedio de los promedios de los retornos anormales.
# ------------------------------------------------------------------------------------------------
# ----Argumentos de entrada ----#
# continent_coefficients    : Una lista de coeficientes, junto a su error estandar, t_value y p_value. 
# significance.level        : El nivel de significancia que deben tener los coeficientes estimados para graficarlos
# pattern.step              : Al estimar los modelos por continente, los coeficientes tienen un nombre característico, pattern.step
#                             se utiliza para extraer la parte que corresponde a en que dia despues del evento se refiere el coeficiente (t0,t1,t2...)
# pattern.indexes           : string que es la parte del nombre del coeficiente que se refiere al pais del indice bursatil
# pattern.countries         : string que es la parte del nombre del coeficiente que se refiere al pais donde ocurre el desastre
# order.graph               : vector de nombres de paises que indicara el orden en que se graficara
# labels                    : leyendas del eje de los indices bursatiles
# color                     : color para la grafica
# title.graph               : titulo para la grafica
# ----Argumentos de salida  ----#
#-- plot_continent   : objeto tipo ggplot para poder graficar los retornos anormales acumulados (CAR) promedio para cada continente
#                      dependiendo de un nivel de significancia
#---------------------------------------------------------------------------------------#
average_countries2 <- function(continent_coefficients, significance.level, pattern.step, pattern.indexes, pattern.countries, order.graph, labels, color, title.graph){
  
  # Generamos un dataframe que va a guardar los parametros estimados, el error estandar, el valor del t test y el p_value.
  dataframe_modelo <- data.frame(Estimate=double(), SD_error=double(), t_value=double(), p_value=double()) 
  
  # Filtrar por aquellos que acaben en t0, t1, t2, t3 o t4.
  for (element in continent_coefficients){
    ## Genera un dataframe con el estimado, error estandar, t_value, p_value, que salen de la estimacion element
    dataframe_coef <- as.data.frame(element) 
    ## Le cambiamos nombres al dataframe para mejor manejo, pero reflejan lo mismo
    colnames(dataframe_coef) <- c("Estimate","SD_error","t_value","p_value")
    ## Extraemos las filas que nos interesan, es decir aquellas de las dummies, que acaban en t0, t1, t2, t3 o t4, es decir steps
    dataframe_coef_filtrado  <- dataframe_coef %>% 
      dplyr::filter(str_ends(row.names(.),pattern = pattern.step)) %>% 
      # Filtramos tambien para aquellos coeficientes que tengan un nivel de significancia menor que niv.significancia
      dplyr::filter(p_value < significance.level)
    ## Dejamos todos los coeficientes que cumplen con las condiciones en un solo dataframe
    dataframe_modelo   <- rbind(dataframe_modelo, dataframe_coef_filtrado)
  }
  
  ## Ahora para poder promediar los retornos anormales es necesario poder extraer el pais del indice y el step, usando la funcion
  ## str_extract, que extrae el primer valor identico entre dos strings. Esto permite extraer el indice, que en cada fila aparece 
  ## de primer lugar
  string_start <- stringr::str_extract(row.names(dataframe_modelo), pattern.indexes) 
  # cualquier valor de countries
  string_end   <- stringr::str_extract(row.names(dataframe_modelo), pattern.step)  #En caso de querer revisar coeficientes por step
  
  ## Usamos la misma funcion para extraer el nombre del pais donde sucedio el desastre
  string_pais <- unlist(lapply(stringr::str_extract_all(row.names(dataframe_modelo),pattern.countries), function(x) x[(length(x))]))
  dataframe_modelo <- cbind(dataframe_modelo,string_start,string_pais,string_end)
  
  ## Ellos mencionan que realizaran un promedio del CAR, por lo cual realizaremos el CAR de cada pais que tenemos datos
  #  para cada indice y realizaremos un promedio
  
  promedio_car <- dataframe_modelo %>% 
    group_by(string_start, string_pais) %>% 
    summarise(AVERAGE = mean(Estimate)) %>% # El nombre Estimate sale del nombre que le colocamos anteriormente
    group_by(string_start) %>% 
    summarise(mean_AVERAGE = mean(AVERAGE)) %>% 
    arrange(match(string_start,order.graph))
  
  ## Por ultimo, se agregan a las graficas los indices que no contienen valores, de modo que al graficar no saldra dato (es 0)
  #  pero sirve para comparar con la grafica del paper
  
  dataframe_pagnorden <- data.frame(string_start = order.graph)
  #Juntar las dos dataframes y rellenar con 0 los datos faltantes
  promedio_car_all <- left_join(dataframe_pagnorden, promedio_car, by = "string_start") # string_start sale de un nombre que le colocamos en la funcion
  promedio_car_all$mean_AVERAGE <- ifelse(is.na(promedio_car_all$mean_AVERAGE), 0, promedio_car_all$mean_AVERAGE)
  
  promedio_car_all$string_start <- factor(promedio_car_all$string_start, levels = promedio_car_all$string_start)
  
  plot_continent <- ggplot(data = promedio_car_all, aes(y = mean_AVERAGE, x = string_start, fill = mean_AVERAGE < 0)) +
    geom_col() +
    scale_fill_manual(values = c(color, color)) +
    coord_flip()+
    scale_x_discrete(labels = labels)+
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill = "none") +
    labs(y = paste0("Average ARs | p-value < ",percent(significance.level)),x="Index",title = title.graph)
  return(plot_continent)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 15. matching  ------------------------------------#
# Hacer matching entre el pais y el nombre del indice. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- pais    : el nombre de un pais
#-- bool.cds: booleano donde T indica que se hara el matching con los nombres de las series de 
#             CDS y F que se haran con los nombres de las series de Pagnottoni
# ----Argumentos de salida  ----#
#-- index: el nombre del indice que le corresponde
#---------------------------------------------------------------------------------------#
matching <- function(pais,bool.cds,bool.paper){
  if(bool.paper & bool.cds){
    indexByCountry <- list(
      Brazil = 'CDSBrazil', Chile = 'CDSChile', China = 'CDSChina', Colombia = 'CDSColombia', Indonesia = 'CDSIndonesia',
      Korea = 'CDSKorea', Malaysia = 'CDSMalaysia', Mexico = 'CDSMexico', Peru = 'CDSPeru', SouthAfrica = 'CDSSouthAfrica',
      Turkey = 'CDSTurkey')
  }
  if(bool.paper &!bool.cds){
    indexByCountry <- list(
      Brazil = 'Bovespa', Chile = 'S.PCLXIPSA', China = 'ChinaA50', Colombia = 'COLCAP', Indonesia = 'JSX',
      Korea = 'KOSPI', Malaysia = 'KLCI', Mexico = 'S.PBMVIPC', Peru = 'IGBVL', SouthAfrica = 'SouthAfricaTop40',
      Turkey = 'BIST100')
  }
  if(!bool.paper){
    indexByCountry <- list(
      Australia = "S.PASX200", Belgium = "BEL20", Brazil = "Bovespa", Canada = "S.PTSXComposite", Chile = "S.PCLXIPSA",
      Denmark = "OMXCopenhagen20", Finland = "OMXHelsinki25", France = "CAC40", Germany = "DAX", HongKong = "HangSeng",
      India = "Nifty50", Indonesia = "JakartaStockExchange", Mexico = "S.PBMVIPC", Netherlands = "AEX",
      Norway = "OSEBenchmark", Poland = "WIG20", Russia = "MOEXRussia", SouthAfrica = "SouthAfricaTop40",
      SouthKorea = "KOSPI", Spain = "IBEX35", Sweden = "OMXStockholm30", Switzerland = "SMI", Thailand = "SETIndex",
      Turkey = "BIST100", UnitedKingdom = "FTSE100", USA = c("NASDAQComposite", "Nasdaq100"))
  }
  
  if(pais %in% names(indexByCountry)) {
    index <- indexByCountry[[pais]]
  }else{
    index <- NULL
  }
  return(index)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 16. drop.events  ------------------------------------#
# Filtrar una base de eventos para tener una ventana minima de estimacion y una ventana 
# minima de evento al momento de estimar por OLS. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.events        : dataframe de eventos, que debe incluir alguna columna en formato fecha para funcionar
#-- base               : base que sera utilizada para la estimacion en <estimation.event.study>
#-- estimation.start   : numero de dias previos al evento para el inicio de la ventana de estimacion
#-- max.ar             : numero de dias maximos despues del evento para calcular retornos anormales
#-- date.col.name      : nombre de la columna de fechas de eventos
#-- var.col.name      : nombre de la columna que tiene informacion del evento como su locacion
# ----Argumentos de salida  ----#
#-- data.droped.events : dataframe de eventos filtrados. En este df ya no estan los eventos que no cuentan con una 
#                        ventana minima de estimacion ni con una ventana minima de evento
#---------------------------------------------------------------------------------------#
drop.events <- function(data.events,base,estimation.start,max.ar,date.col.name, var.col.name){
  
  # Se renombran las columnas para un uso correcto de la funcion
  colnames(data.events)[colnames(data.events) == date.col.name] <- "Start.Date"
  colnames(data.events)[colnames(data.events) == var.col.name] <- "Country"
  # Se detiene la funcion en caso de que la columna <data.events$Start.Date> no sea de clase date
  if(!inherits(data.events$Start.Date,"Date")) {
    stop(paste0(paste0("La columna ",date.col.name)," no es formato fecha."))
  }
  
  # Fecha minima para que la estimacion pueda empezar desde <estimation.start> dias previos al evento
  Fecha_minima_estimacion <- index(base)[estimation.start+1]
  # Fecha minima para que se pueda realizar el calculo de retornos anormales para <max.ar>+1 dias
  Fecha_minima_evento     <- index(base)[length(index(base))-(max.ar)]
  # Filtracion <data.events>. Solamente contiene eventos entre <Fecha_minima_estimacion> y <Fecha_minima_evento>
  data.events <- data.events %>% 
    dplyr::filter(dplyr::between(Start.Date, Fecha_minima_estimacion, Fecha_minima_evento))
  return(data.events)
}
#---------------------------------------------------------------------------------------#

# <if(0)> porque fue mejoradas por <create.lags> y <estimation.event.study>, funciones que se 
# encuentran al finalizar el <if(0)>
if(0){  
  #------------------------------   19. estimation.event.study  --------------------------#
  # Realizar una estimacion por OLS siguiendo el modelo de mercado, obteniendo retornos anormales 
  # y error estandar de la estimacion.
  #---------------------------------------------------------------------------------------#
  # ----Argumentos de entrada ----#
  #-- data.events        : dataframe de eventos, que debe incluir alguna columna en formato fecha para funcionar
  #-- days.evaluated     : maximo numero de dias a evaluar en caso de que la fecha de un evento no este en el indice de las 
  #                        series a estimar
  #-- asset.returns      : base de datos con variables independientes en la estimacion de modelo de mercado (R_it)
  #--                      Supuesto: objeto ts 
  #-- market.returns     : serie que corresponde al indice de mercado (R_mt)
  #--                      Supuesto: objeto ts con mismo indice de <asset.returns>
  #-- max.ar             : numero de dias maximos despues del evento para calcular retornos anormales
  #-- es.start           : numero de dias previos al evento para comenzar la estimacion
  #-- es.end             : numero de dias previos al evento para terminar la estimacion
  #-- add.exo            : booleano donde <TRUE> indica que se van a agregar las variables <vars.exo> al modelo y <FALSE> 
  #                        si no se agrega ninguna variable exogena. Default es <FALSE>
  #-- lags_df            : string que indica el nombre por el cual empiezan las matrices de rezagos en el codigo principal
  #-- base               : base de datos donde se encuentran las variables exogenas
  #                        Supuesto: objeto ts con mismo indice de <asset.returns>
  #-- vars.exo           : nombres de las variables en <base> que se quieren usar como exogenas
  # ----Argumentos de salida  ----#
  #-- all.events.list    : lista que incluye para cada par evento-indice la siguiente informacion:
  #--   <Dataframe>      : base de datos con retornos observados, estimados y anormales para la ventana 
  #--                      de estimacion y para la ventana de evaluacion del evento
  #--   <Standard_Error> : error estandar de los residuales de la estimacion por OLS
  #---------------------------------------------------------------------------------------#
  
  estimation.event.study <- function(data.events, days.evaluated, asset.returns, market.returns, max.ar, es.start, es.end, add.exo =FALSE,
                                     lags_df,base,vars.exo){
    all_events_list      <- list() # lista que contendra todas las series de retornos + errores estandar
    # Loop: Por cada evento se hace una regresion OLS con la muestra [-<es.start>,-<es.end>] dias antes del evento para estimar alfa, beta 
    for(i in 1:nrow(data.events)){
      # Primero se encuentra a que dato le corresponde el dia del evento, y el dia final de la ventana de evento es el dia del evento
      # mas <max.ar>
      event_list  <- list() # lista donde se guarda por cada evento un dataframe de retornos observados, estimados y anormales;
      # junto a error estandar del error en la estimacion
      pais        <- as.character(data.events[i,'Country']) # Establece el pais donde sucedio el evento
      index_names <- matching(pais) # Nombre de la variable del <pais> con la que se calculan retornos anormales (ej: stock-index del pais)
      # Detener la funcion si no se tiene indice para el pais especificado
      if(is.null(index_names)) stop(paste0("No hay indice para el pais: ", pais))
      suppressWarnings({
        # Loop que genera la posicion de desastre respecto al indice de <asset.returns>. Si la fecha del evento no esta en el indice de 
        # <asset.returns>,se revisara hasta <days.evaluated> dias despues del desastre para ser considerado como el inicio del evento
        for(j in 0:days.evaluated){
          if((data.events[i,'Start.Date']+j) %in% index(asset.returns[,index_names])){ 
            # Generacion del dia del desastre (o j dias despues del desastre, si el dia del desastre no esta en el indice de 
            # <asset.returns>)
            event_start_date  <- data.events[i,'Start.Date']+j
            # Generacion de la posicion del dia de desastre en el indice de fechas de <asset.returns>
            # (o j dias despues del desastre, si el dia del desastre no esta en el indice de <asset.returns>)
            event_start_index <- which(index(asset.returns[,index_names])==event_start_date)
            break
          }
        }
      })
      
      # Regresion por OLS del modelo de mercado
      # Loop para los casos en que haya mas de un indice por pais, se realiza regresion OLS para estimar alpha y beta
      # Nota: En general solo hay un indice por pais, pero en USA hay dos.
      for(name in index_names){
        # <window.event.dates> son las fechas que pertenecen a la ventana de evento
        window_event_dates <- index(asset.returns[,name][(event_start_index):(event_start_index+max.ar)])
        
        # Creacion  de la base de datos de la ventana de estimacion en <asset.returns> para <name> 
        # y para el indice de mercado, <market.returns>, que es una var.exogena del modelo
        # <est.dependent.var> se refiere a la variable dependiente en la estimacion.
        # <est.independent.var> se refiere a la variable independiente en la estimacion
        # La posicion del primer dia de la ventana de estimacion respecto al indice de <asset.returns> o <market.returns>
        # es (<event_start_index> - <es.start>) mientras que la posicion de ultimo dia de la ventana de estimacion es 
        # (<event_start_index> - <es.end>)
        est.dependent.var   <- asset.returns[,name][(event_start_index-es.start):(event_start_index-es.end),]
        est.independent.var <- market.returns[(event_start_index-es.start):(event_start_index-es.end)]
        
        # Detener la funcion si los indices de fechas de <est.dependent.var> y de <est.independent.var> no son los mismos
        if(!identical(index(est.dependent.var),index(est.independent.var))) stop("Las series tienen indices de fechas diferentes")
        
        if(add.exo == TRUE){
          if(pais == "USA") pais <- "USA1" # Usando la base <base_datos>, las columnas de Estados Unidos se llaman <USA1>
          
          # Seleccionar las variables <vars.exo> de la base de datos <base>
          # Reducir variables exogenas al mismo indice que <est.dependent.var>
          exo           <- base[,paste0(vars.exo,pais)]
          exo_es_window <- exo[index(est.dependent.var)]
          
          ## Primero toca ver si <paste0(lags_df,name)> existe, 
          # ya que si existe algun país que no tenga matriz de rezagos, <lags_name> no existe. En nuestro caso todos los países tienen rezagos.
          if(exists(paste0(lags_df,name))==TRUE){
            # Seleccionar los rezagos solamente del indice <name>
            lags <- get(paste0(lags_df,name)) 
            # Reducir indice de <lags> al mismo de <est.dependent.var>
            lags_es_window <- lags[index(est.dependent.var)]
            # Generar matriz con todas las variables exogenas, contando con la ventana de evento, para obtener los retornos estimados
            matrix_model <- cbind(1, market.returns[c(index(est.dependent.var),window_event_dates)],
                                  lags[c(index(est.dependent.var),window_event_dates)],
                                  exo[c(index(est.dependent.var),window_event_dates)])
            # Usar <as.numeric> para evitar problemas con la estimacion <lm()>
            # Estimar el modelo por OLS
            model  <- lm(as.numeric(est.dependent.var) ~ as.numeric(est.independent.var)+
                           as.matrix(lags_es_window)+as.matrix(exo_es_window))
          }else{
            # Generar matriz con todas las variables exogenas, contando con la ventana de evento, para obtener los retornos estimados
            matrix_model <- cbind(1, market.returns[c(index(est.dependent.var),window_event_dates)],
                                  exo[c(index(est.dependent.var),window_event_dates)])
            # Usar <as.numeric> para evitar problemas con la estimacion <lm()>
            # Estimar el modelo por OLS
            model  <- lm(as.numeric(est.dependent.var) ~ as.numeric(est.independent.var)+
                           as.matrix(exo_es_window))
          }
        }else{
          matrix_model <- cbind(1, market.returns[c(index(est.dependent.var),window_event_dates)])
          model <- lm(as.numeric(est.dependent.var) ~ as.numeric(est.independent.var))
        }
        
        # Calcular los parametros 
        betas          <- coef(model)
        standard_error <- sd(residuals(model))
        
        # Creacion series <observed>, <predicted> y <abnormal> solamente para la ventana de estimacion y la ventana de evento
        # Se usa el indice de <est.dependent.var> para obtener las fechas pertenecientes a la ventana de estimacion.
        # Se selecciona de <asset.returns> solamente las observaciones que esten en la ventana de estimacion o la de evento
        observed           <- asset.returns[,name][c(index(est.dependent.var),window_event_dates)]
        # Se selecciona de <market.returns> solamente las observaciones que esten en la ventana de estimacion o la de evento
        predicted <- matrix_model %*% betas
        # Se restan los retornos estimados de los observados
        abnormal           <- observed - predicted
        
        # Se juntan las tres series en un solo dataframe
        df             <- merge(observed,predicted,abnormal)
        # Cambio de nombre de columnas
        colnames(df)   <- c('Observed','Predicted','Abnormal')
        # Agregar el dataframe a la lista <event_list>
        event_list[["Dataframe"]]       <- df 
        # Agregar el error estandar a la lista <event_list>
        event_list[["Standard_Error"]]  <- standard_error
        # Agregar la lista <event_list> a la lista <all_events_list>, por lo que seria una lista de listas
        all_events_list[[paste(name,i,sep="_")]] <- event_list
      }
    } 
    return(all_events_list)
  }
  
  #---------------------------------------------------------------------------------------#
}

#---------------------------------- 17. create.lags  ------------------------------------#
# Filtrar una base de eventos para tener una ventana minima de estimacion y una ventana 
# minima de evento al momento de estimar por OLS. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- base               : base de datos con variables a las que se desea obtener rezagos
#-- interest.vars      : variables de las que se desea sacar rezagos
#-- no.lags            : numero de rezagos a considerar. Si es un numero, se calcula ese numero de rezagos
#                        para todas las <interest.vars>. Si es una lista con numeros, a la i-esima variable de 
#                        <interst.vars> se le calcula el i-esimo elemento de no.lags.
#                        El default es <NULL>, en cuyo caso para cada variable se le calculan los rezagos optimos
#                        siguiendo el criterio de informacion de Akaike
#-- AR.m               : rezago máximo de la parte autorregresiva
#-- MA.m               : rezago máximo de la parte de promedio movil
#-- d                  : orden de diferenciación
#-- bool               : booleano que indica si realizar la estimación arima con constante (el default es TRUE)
#-- metodo             : metodo por el cual se hará la estimación ARIMA (existe CS, ML y CSS-ML) (el default es CSS)
# ----Argumentos de salida  ----#
#-- base_final         : base de datos con las mismas columnas de <base> junto a aquellos rezagos de las variables 
#                        <interest.vars>
#---------------------------------------------------------------------------------------#
create.lags <- function(base, interest.vars,no.lags=NULL, AR.m, MA.m=0,d=0,bool=TRUE,metodo="CSS"){
  
  all_events_list      <- list() # lista que contendra todas las series de retornos + errores estandar
  lags_df              <- NULL   # dataframe que va a guardar todos los rezagos
  
  # El objeto <base> debe tener clase <zoo> o <ts>, o la funcion tendra errores 
  if(!inherits(base,"zoo") & !inherits(base,"ts")) {
    stop("La base ingresada no es ni de clase zoo ni de clase ts")
  }
  
  # El objeto <no.lags> debe ser <numeric> o <integer> o <NULL> o la funcion tendra errores
  if(!inherits(no.lags,"numeric") & !inherits(no.lags,"integer") & !inherits(no.lags,"NULL")) {
    stop("El numero de rezagos ingresado no es numerico")
  }
  
  # La longitud de <no.lags> puede ser o 0, o 1 o <length(interest.vars)>, de otro modo surgiria un error
  if(length(no.lags)!=0 & length(no.lags)!=1 & length(no.lags)!=length(interest.vars)) 
    stop("no.lags tiene dimensiones incorrectas, se debe ingresar un solo numero o un vector del mismo tamaño que 
          las variables dependientes ingresadas")
  
  # Por cada elemento en <base[,interest.vars]> generar rezagos dependiendo del valor de <no.lags> 
  # Agregar los rezagos a una sola base
  # Si es <NULL> los numeros de rezagos se obtienen automaticamente
  
  for(i in 1:ncol(base[,interest.vars])){ 
    indice <- colnames(base[,interest.vars][,i])
    if(is.null(no.lags)){
      # Generar rezagos optimos
      lags_var <- lag_function(base,indice, AR.m,MA.m = 0,d = 0, bool = TRUE,metodo = "CSS")
      # Agregarlos al dataframe lags_df
      if(is.null(lags_df))lags_df <- lags_var else lags_df <- merge(lags_df, lags_var)
    }else if(length(no.lags) == 1){
      if(no.lags != 0){
        # Si <no.lags> es un numero, se crearan <no.lags> rezagos para todas las series
        lags_var <- timeSeries::lag((base[,indice]), c(1:no.lags))
        # Cambio de nombre de los rezagos
        colnames(lags_var) <- paste0(indice,'.l',1:no.lags)
        # Agregarlos al dataframe lags_df
        if(is.null(lags_df))lags_df <- lags_var else lags_df <- merge(lags_df, lags_var)
      }
    }else if(length(no.lags) == length(interest.vars)){
      # Si <no.lags> tiene la misma cantidad de numeros que elementos en <interest.vars> se toma el i-esimo 
      # numero para generar rezagos en el i-esimo indice, solamente si el i-esimo numero es distinto de 0
      if(no.lags[i]!=0){
        lags_var <- timeSeries::lag((base[,indice]), c(1:no.lags[i]))
        colnames(lags_var) <- paste0(indice,'.l',1:no.lags[i])
        if(is.null(lags_df))lags_df <- lags_var else lags_df <- merge(lags_df, lags_var)
      }
    }else if(length(no.lags) >1 & length(no.lags) != length(interest.vars)) 
      stop("El vector de rezagos no.lags tiene que tener el mismo numero de elementos que indices ingresados")
  }
  
  # Reducir la muestra de <base> y <lags_df> para que no contengan valores faltantes
  base.completa <- base[complete.cases(base),]
  lags_df       <- lags_df[complete.cases(lags_df),]
  
  # Asegurar que las bases tengan el mismo indice
  base.completa <- base.completa[index(lags_df),]
  lags_df       <- lags_df[index(base.completa),]
  
  base_final <- merge(base.completa,lags_df)
  return(base_final)
}
#---------------------------------------------------------------------------------------#

# if(0) porque se arreglo <overlap.dummy> en la funcion a partir del renglon 1400
if(0){
  #------------------------------   18. estimation.event.study  --------------------------#
  # Realizar una estimacion por OLS siguiendo el modelo de mercado, obteniendo retornos anormales 
  # y error estandar de la estimacion.
  #---------------------------------------------------------------------------------------#
  # ----Argumentos de entrada ----#
  #-- bool.cds: booleano donde T indica que se hara el matching con los nombres de las series de 
  #             CDS y F que se haran con los nombres de las series de Pagnottoni
  #-- base               : base de datos de clase zoo o ts donde deben estar las variables dependientes de las regresiones (Rit),
  #--                      el indice de mercado (Rmt) y las variables exogenas de la regresion. Tambien los rezagos de las 
  #                        variables dependientes (en caso de haber)
  #-- data.events        : dataframe de eventos, que debe incluir alguna columna en formato fecha para funcionar
  #-- days.evaluated     : maximo numero de dias a evaluar en caso de que la fecha de un evento no este en el indice de las 
  #                        series a estimar
  #-- market.returns     : nombre de la columna de <base> que corresponde al indice de mercado (Rmt)
  #-- max.ar             : numero de dias maximos despues del evento para calcular retornos anormales
  #-- es.start           : numero de dias previos al evento para comenzar la estimacion
  #-- es.end             : numero de dias previos al evento para terminar la estimacion
  #-- add.exo            : booleano donde <TRUE> indica que se van a agregar las variables <vars.exo> al modelo y <FALSE> 
  #                        si no se agrega ninguna variable exogena. Default es <FALSE>
  #-- vars.exo           : nombres de las variables en <base> que se quieren usar como exogenas
  #-- GARCH              : variable que indicara con cual modelo GARCH se estimara. 
  #        - NULL es el default, en cuyo caso se estimara la media con OLS, sin tener modelo para la varianza
  #        - sGARCH, es el modelo GARCH(1,1)
  #        - apARCH : asymmetric power-ARCH(1)
  #        - eGARCH : exponential-GARCH(1,1)
  # ----Argumentos de salida  ----#
  #-- events.list    : lista que incluye para cada par evento-indice un objeto de clase "ESmean" con la siguiente informacion:
  #--   <Retornos>       : base de datos con retornos observados, estimados y anormales para la ventana 
  #--                      de estimacion y para la ventana de evaluacion del evento
  #--   <error_estandar> : error estandar de los residuales de la estimacion por OLS
  #--   <res_estandar_estimacion>    : residuales estandarizados durante la estimacion, en caso de haber sido estimado con GARCH
  #--   <res_no_estandar_estimacion> : residuales no estandarizados durante la estimacion, en caso de haber sido estimado con GARCH
  #--   <variance_forecast>          : forecast de la varianza condicional durante los <max.ar> dias correspondientes a la ventana
  #                                    de evento
  #     <evento>                     : dataframe del evento asociado con la estimacion ARMA-GARCH
  #---------------------------------------------------------------------------------------#
  estimation.event.study <- function(bool.paper,bool.cds,base, data.events, market.returns, max.ar, es.start, es.end, add.exo =FALSE,
                                     vars.exo=NULL, GARCH=NULL, overlap.events = NULL, no.overlap = 0){
    
    events.list <- list() # lista que contendra todas la informacion del modelo
    
    # Crear una nueva clase de objetos para guardar informacion importante de la estimacion ARMA-GARCH
    setClass("ESmean",slots=list(retornos = "xts",error_estandar = "numeric",res_estandar_estimacion="xts",
                                 res_no_estandar_estimacion="xts",variance_forecast="xts",
                                 evento='data.frame',fit='list'))
    
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
    
    for(i in 1:nrow(data.events)){
      # Primero se encuentra a que dato le corresponde el dia del evento, y el dia final de la ventana de evento es el dia del evento
      # mas <max.ar>
      pais        <- as.character(data.events[i,'Country']) # Establece el pais donde sucedio el evento
      index_names <- matching(pais,bool.cds,bool.paper) # Nombre de la variable del <pais> con la que se calculan retornos anormales (ej: stock-index del pais)
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
            base_indice <- merge(base[,c(name,market.returns)],base[,lags_name])
          }else base_indice <- base[,c(name, market.returns)]
          
          ## Añadir <vars.exo> si <add.exo> ==<TRUE>
          if(add.exo == TRUE) base_indice <- merge(base_indice, base[,paste0(vars.exo,pais)])
          
          # Reducir el indice de la base para la estimacion.
          # La posicion del primer dia de la ventana de estimacion respecto al indice de <asset.returns> o <market.returns>
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
          events.list[[paste(name,i,sep="_")]] <- object
        }
      }else{
        # Regresion por ML del modelo de mercado + GARCH para la varianza
        # Loop para los casos en que haya mas de un indice por pais
        # Nota: En general solo hay un indice por pais, pero en USA hay dos.
        for(name in index_names){
          # <window.event.dates> son las fechas que pertenecen a la ventana de evento
          window_event_dates <- index(base[,name][(event_start_index):(event_start_index+max.ar)])
          
          # Obtener el numero de rezagos para el modelo de la media, calculando el numero de columnas que en su nombre tengan
          # <(paste0(name,".l"))>
          p <- length(grep(paste0(name,".l"),colnames(base)))
          
          # Variables exogenas que dependen del pais
          variables_pais <- paste0(vars.exo,pais)
          
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
            subset_data_list <- split(subset_data, subset_data$Disaster.Subgroup)
            # Recordemos que del analisis descriptivo, la media de duracion de los desastres geofisicos es 3, mientras que de los hidrologicos es 9
            # y de los meteorologicos es 7
            df.mediaduracion <- data.frame(c('Geophysical','Hydrological','Meteorological'), c(2,8,4))
            colnames(df.mediaduracion) <- c('Tipo.desastre','Media.duracion')
            for(df.m in seq_along(subset_data_list)) {
              for(num in subset_data_list[[df.m]]$indices2) overlap.dummy[num:(num+df.mediaduracion$Media.duracion[df.mediaduracion == names(subset_data_list)[df.m]])] <- 1 
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
                      external.regressors = as.matrix(cbind(base[(inicio_estimacion:fin_estimacion),c(variables_pais,market.returns)], 
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
                      external.regressors = as.matrix(cbind(base[(inicio_estimacion:fin_estimacion),c(variables_pais,market.returns)]))
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
                    external.regressors = as.matrix(base[(inicio_estimacion:fin_estimacion),c(variables_pais,market.returns)])
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
          if(!is.null(overlap.events) & ncol(spec@model$modeldata$mexdata) == 4) base_ev_window <- cbind(base[(window_event_dates), c(variables_pais, market.returns)],0)
          # Para el caso en que <overlap.events> no sea nula, pero la dummy no haya entrado como regresora es
          if(!is.null(overlap.events) & ncol(spec@model$modeldata$mexdata) == 3) base_ev_window <- base[(window_event_dates), c(variables_pais, market.returns)]
          # Lo anterior porque el cuarto regresor es siempre 0 en la ventana de evento. Si hubiese un 1 estaríamos diciendo que se va a pronosticar
          # el efecto de un desastre durante la ventana de evento
          # Por otro lado, si no hay <overlap.events> la base de exogenas durante la ventana de evento seria
          if(is.null(overlap.events)) base_ev_window <- base[(window_event_dates), c(variables_pais, market.returns)]
          
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
          events.list[[paste(name,i,sep="_")]] <- object
        }
      }
    }
    return(events.list)
  }
  #---------------------------------------------------------------------------------------#
}

#------------------------------   18. estimation.event.study  --------------------------#
# Realizar una estimacion por OLS siguiendo el modelo de mercado, obteniendo retornos anormales 
# y error estandar de la estimacion.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- bool.cds: booleano donde T indica que se hara el matching con los nombres de las series de 
#             CDS y F que se haran con los nombres de las series de Pagnottoni
#-- base               : base de datos de clase zoo o ts donde deben estar las variables dependientes de las regresiones (Rit),
#--                      el indice de mercado (Rmt) y las variables exogenas de la regresion. Tambien los rezagos de las 
#                        variables dependientes (en caso de haber)
#-- data.events        : dataframe de eventos, que debe incluir alguna columna en formato fecha para funcionar
#-- days.evaluated     : maximo numero de dias a evaluar en caso de que la fecha de un evento no este en el indice de las 
#                        series a estimar
#-- market_returns     : nombre de la columna de <base> que corresponde al indice de mercado (Rmt)
#-- max.ar             : numero de dias maximos despues del evento para calcular retornos anormales
#-- es.start           : numero de dias previos al evento para comenzar la estimacion
#-- es.end             : numero de dias previos al evento para terminar la estimacion
#-- add.exo            : booleano donde <TRUE> indica que se van a agregar las variables <vars.exo> al modelo y <FALSE> 
#                        si no se agrega ninguna variable exogena. Default es <FALSE>
#-- vars.exo           : nombres de las variables en <base> que se quieren usar como exogenas
#-- GARCH              : variable que indicara con cual modelo GARCH se estimara. 
#        - NULL es el default, en cuyo caso se estimara la media con OLS, sin tener modelo para la varianza
#        - sGARCH, es el modelo GARCH(1,1)
#        - apARCH : asymmetric power-ARCH(1)
#        - eGARCH : exponential-GARCH(1,1)
# ----Argumentos de salida  ----#
#-- events.list    : lista que incluye para cada par evento-indice un objeto de clase "ESmean" con la siguiente informacion:
#--   <Retornos>       : base de datos con retornos observados, estimados y anormales para la ventana 
#--                      de estimacion y para la ventana de evaluacion del evento
#--   <error_estandar> : error estandar de los residuales de la estimacion por OLS
#--   <res_estandar_estimacion>    : residuales estandarizados durante la estimacion, en caso de haber sido estimado con GARCH
#--   <res_no_estandar_estimacion> : residuales no estandarizados durante la estimacion, en caso de haber sido estimado con GARCH
#--   <variance_forecast>          : forecast de la varianza condicional durante los <max.ar> dias correspondientes a la ventana
#                                    de evento
#     <evento>                     : dataframe del evento asociado con la estimacion ARMA-GARCH
#---------------------------------------------------------------------------------------#
estimation.event.study <- function(bool.paper,bool.cds,base, data.events, market_returns, max.ar, es.start, es.end, add.exo =FALSE,
                                   vars.exo=NULL, GARCH=NULL, overlap.events = NULL, overlap.max){
  
  events.list <- list() # lista que contendra todas la informacion del modelo
  
  # Crear una nueva clase de objetos para guardar informacion importante de la estimacion ARMA-GARCH
  setClass("ESmean",slots=list(retornos = "xts",error_estandar = "numeric",res_estandar_estimacion="xts",
                               res_no_estandar_estimacion="xts",variance_forecast="xts",
                               evento='data.frame',fit='list'))
  
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
  
  for(i in 1:nrow(data.events)){
    # Primero se encuentra a que dato le corresponde el dia del evento, y el dia final de la ventana de evento es el dia del evento
    # mas <max.ar>
    pais        <- as.character(data.events[i,'Country']) # Establece el pais donde sucedio el evento
    index_names <- matching(pais,bool.cds,bool.paper) # Nombre de la variable del <pais> con la que se calculan retornos anormales (ej: stock-index del pais)
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
        if(add.exo == TRUE) base_indice <- merge(base_indice, base[,paste0(vars.exo,pais)])
        
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
        events.list[[paste(name,i,sep="_")]] <- object
      }
    }else{
      # Regresion por ML del modelo de mercado + GARCH para la varianza
      # Loop para los casos en que haya mas de un indice por pais
      # Nota: En general solo hay un indice por pais, pero en USA hay dos.
      for(name in index_names){
        # <window.event.dates> son las fechas que pertenecen a la ventana de evento
        window_event_dates <- index(base[,name][(event_start_index):(event_start_index+max.ar)])
        
        # Obtener el numero de rezagos para el modelo de la media, calculando el numero de columnas que en su nombre tengan
        # <(paste0(name,".l"))>
        p <- length(grep(paste0(name,".l"),colnames(base)))
        
        # Variables exogenas que dependen del pais
        variables_pais <- paste0(vars.exo,pais)
        
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
        events.list[[paste(name,i,sep="_")]] <- object
      }
    }
  }
  return(events.list)
}
#---------------------------------------------------------------------------------------#

#------------------------------   19. wilcoxon.jp.test  ----- --------------------------#
# Realizar una prueba de rank-signed Wilcoxon teniendo en cuenta una lista generada por la funcion
# <estimation.event.study>
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.list        : lista generada por la funcion <estimation.event.study>, que incluye para 
#--                    cada par evento-indice un dataframe con retornos anormales
#-- es.window.length : tamaño ventana de estimacion
#-- ev.window.length : tamaño ventana de evento
#-- tail             : a que cola se quiere la hipotesis alternativa, donde -1 significa cola izquierda, 1 significa cola derecha y 2 significa a dos colas. 
#                      el default es a dos colas
# ----Argumentos de salida  ----#
#-- result           : dataframe con el estadistico de Wilcoxon y su significancia (* para 10%, ** para 5% y *** para 1%)
#---------------------------------------------------------------------------------------#
wilcoxon.jp.test <- function(data.list,es.window.length,ev.window.length,ev.window.begin, tail = 2){
  # Para el calculo del CAR se toma la serie <Abnormal> a partir de la obs <es.window.length> + 1 hasta 
  # <es.window.length> + <ev.window.length>
  # Se crea el vector <all_car> para guardar los CAR
  # Nota: La longitud de este vector es igual al numero de eventos si solo hay un indice por pais.
  #       Si hay mas de un indice por pais, la longitud de <all_car> aumenta consecuentemente
  all_car <- c()
  for(element in data.list){
    car     <- sum(element@retornos$Abnormal[(es.window.length+1+ev.window.begin):(es.window.length+ev.window.length+ev.window.begin)])
    all_car <- c(all_car,car)
  }
  # <if(0)> porque la signficancia puede ser encontrada de una mejor manera con <wilcox.test> 
  if(0){
    # Se genera un dataframe para poder realizar el ordenamiento de los car
    df_car <- data.frame("car"=all_car,"magnitude"=abs(all_car),"sign"=sign(all_car))
    df_car <- df_car %>% 
      mutate(magnitude=ifelse(magnitude==0,NA,magnitude)) ## Se coloca NA si la magnitud es 0, ya que no se deben considerar
    # Funcion <rank> para ordenar las magnitudes de los car
    df_car <- df_car %>% 
      mutate(rank = rank(magnitude))
    
    # Suma de los rangos de car positivos: <positive_rank_sum>
    rank_sum <- df_car %>% 
      dplyr::group_by(sign) %>%
      dplyr::summarize(sum = sum(rank))
    positive_rank_sum <- rank_sum$sum[rank_sum$sign=="1"]
    
    # Calculo de la significancia del estadistico de Wilcoxon
    # La funcion para hallar los cuantiles de la distribucion del estadistico de Wilcoxon es <stats::qsignrank()>. 
    # A partir de 1000 observaciones, la funcion no se comporta adecuadamente, pero debido a que es muestra grande, la distribucion
    # converge a una normal con media N(N+1)/4 y varianza N(N+1)(2N+1)/24 por lo que usamos <stats::qnorm()>>
    significance <- ""
    ## Calculo para cada nivel de significancia si el valor de <positive_rank_sum> es lo suficientemente extremo para rechazar H_0
    #  La prueba se hace a dos colas, por lo que la primera condicion para cada prueba de significancia compara <positive_rank_sum> con el 
    #  valor critico de la cola derecha.
    #  La segunda condicion es la comparacion de <positive_rank_sum> con el valor critico de la cola izquierda.
    N <- length(data.list)
    
    # Uso de <qsignrank> o <qnorm> dependiendo del tamaño de la muestra
    if(N<=1000){
      # Si se evalua la significancia al 10%, con un test a dos colas, debemos buscar los percentiles 5 y 95, y comparar con estadistico
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.1/2,n=N)|
                     positive_rank_sum <= stats::qsignrank(0.1/2, n=N)] <- "*"
      # Para evaluar al 5%:
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.05/2, n=N)|
                     positive_rank_sum <= stats::qsignrank(0.05/2, n=N)] <- "**"
      # Al 1%:
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.01/2, n=N)|
                     positive_rank_sum <= stats::qsignrank(0.01/2, n=N)] <- "***"
      resultado <- data.frame("Wilcoxon_statistic" = positive_rank_sum,"Significancia" = significance)
    }else{
      mu = N*(N+1)/4
      sigma = sqrt(N*(N+1)*(2*N+1)/24)
      # Si se evalua la significancia al 10%, con un test a dos colas, debemos buscar los percentiles 5 y 95, y comparar con estadistico
      significance[positive_rank_sum >= stats::qnorm(1 - 0.1/2,mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.1/2, mean = mu,sd = sigma)] <- "*"
      # Para evaluar al 5%:
      significance[positive_rank_sum >= stats::qnorm(1 - 0.05/2, mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.05/2, mean = mu,sd = sigma)] <- "**"
      # Al 1%:
      significance[positive_rank_sum >= stats::qnorm(1 - 0.01/2, mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.01/2, mean = mu,sd = sigma)] <- "***"
      resultado <- data.frame("Wilcoxon_statistic" = positive_rank_sum,"Significancia" = round(significance,3))
    }
  }
  # Por ultimo, usando la funcion <wilcox.test> obtenemos el pvalor dependiendo de la cola, si tenemos que <tail> es 2, eso significa que la alternativa debe
  # ser con dos colas, es decir <alt> = 'two-sided'. Si <tail> = -1, entonces es para la cola izquierda de la hipotesis alterna (por ejemplo para indices de bolsa) entonces <alt> = 'less'
  # Por ultimo, si <tail> = 1, entonces es para la cola derecha, por lo que <alt> = 'greater0
  if(tail == 2)  alt <- 'two.sided'
  if(tail == -1) alt <- 'less'
  if(tail == 1)  alt <- 'greater'
  test      <- wilcox.test(all_car,alternative = alt)
  statistic <- as.numeric(test$statistic)
  p_value   <- test$p.value
  significancia <-''
  if(p_value<=0.10) significancia <- '*'
  if(p_value<=0.05) significancia <- '**'
  if(p_value<=0.01) significancia <- '***'
  resultado <- data.frame('Estadistico'=statistic,'Significancia'=significancia, "p_value" = round(p_value,3))
  return(resultado)
}
#---------------------------------------------------------------------------------------#


#------------------------------   20. bootstrap_CT  ----- --------------------------#
# Realizar una prueba usando bootstrap siguiendo el procedimiento de Corrado & Truong (2008)
# El estadistico usado es el de Patell
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.list        : lista generada por la funcion <estimation.event.study>, que incluye para 
#--                    cada par evento-indice un dataframe con retornos anormales
#-- market.returns   : serie que corresponde al indice de mercado (Rmt)
#-- es.window.length : tamaño ventana de estimacion
#-- ev.window.length : tamaño ventana de evento
#-- no.simul         : numero de simulaciones para el bootstrap
# ----Argumentos de salida  ----#
#-- result           : dataframe con el estadistico de Patell y su significancia (* para 10%, ** para 5% y *** para 1%)
#---------------------------------------------------------------------------------------#
bootstrap_CT <- function(data.list,market.returns,es.window.length,ev.window.length,no.simul){ 
  standardized_cars <- c()
  for(element in data.list){
    # Calculo de las fechas de ventana de estimacion y de evento para el evento relacionado con <element>
    # Como el objeto <data.list> viene de la funcion <estimation.event.study>, tiene el objeto $Dataframe$Abnormal
    # Aparte, en <element$Dataframe$Abnormal> tenemos observaciones solamente para la ventana de estimacion y la ventana de evento
    # por lo cual, los indices de la ventana de estimacion son (1:<es.window.length>) y los de ventana evento son
    # (<es.window.length>+1):(<es.window.length>+<ev.window.length>)
    estimation_dates <- index(element@retornos$Abnormal)[1:es.window.length]
    event_dates      <- index(element@retornos$Abnormal)[(es.window.length+1):(es.window.length+ev.window.length)]
    
    # Calculo promedio retornos anormales a lo largo de la ventana de evento
    averaged_car <- (1/ev.window.length)*sum(element@retornos$Abnormal[event_dates])
    # El error estandar para C&T(2008) incluye el promedio del retorno de mercado durante la ventana de estimacion y la ventana de evento
    # De la serie de retorno de mercado <market.returns> escogemos aquellas obs. que estan dentro de la ventana de estimacion y ventana de evento
    market_estimation <- market.returns[estimation_dates]
    market_event      <- market.returns[event_dates]
    # Error estandar siguiendo C&T (2008)
    prediction_error  <- (element@error_estandar/sqrt(ev.window.length))*(sqrt((1) + (ev.window.length/es.window.length) + 
                                                                                 (((ev.window.length*((mean(market_event)-mean(market_estimation))^2))/(sum((market_estimation-mean(market_estimation))^2))))))
    
    # Generar el car promedio estandarizado al dividir entre el error estandar estimado
    standardized_car  <- averaged_car/prediction_error
    standardized_cars <- c(standardized_cars, standardized_car)
  }
  
  # Estadistico de Patell que esta en C&T(2008), <Tp>
  N  <- length(data.list)
  Tp <- (sqrt(ev.window.length)/sqrt(N))*sum(standardized_cars)
  
  # Muestreo aleatorio con reemplazo sobre <standardized_cars> y calculo de estadistico
  
  boot_n <- no.simul  #<<<--- Numero de iteraciones para calcular el estadistico 
  boot_statistics <- c() # Vector para guardar los estadisticos calculados por bootstrap
  for(i in 1:boot_n){
    # Tomar muestra de <N> valores con reemplazo de los car estandarizados
    sample_standardized <- sample(standardized_cars,N, replace= TRUE)
    # <boot_Tp> hace referencia al estadistico de la muestra <sample_standardized>
    boot_Tp             <- (sqrt(ev.window.length)/sqrt(N))*sum(sample_standardized)
    boot_statistics     <- c(boot_statistics, boot_Tp)
  }
  
  # Percentil de <Tp> en la poblacion de estadisticos <boot_Tp>
  
  # Hallar los valores criticos de la distribucion usando <quantile> para significancia 10%
  significancia <- ""
  # Comprobar significancia al 10% con dos colas. La primera condicion es verificar si <Tp> es menor que el valor critico de
  # la distribucion hallada por bootstrap. La segunda condicion es <Tp> mayor que el valor critico de la distribucion bootstrap.
  significancia[Tp <= quantile(boot_statistics, 0.1/2) | Tp >= quantile(boot_statistics, 1- 0.1/2)] <- "*"
  # Significancia al 5%
  significancia[Tp <= quantile(boot_statistics, 0.05/2) | Tp >= quantile(boot_statistics, 1- 0.05/2)] <- "**"
  # Significancia al 1%
  significancia[Tp <= quantile(boot_statistics, 0.01/2) | Tp >= quantile(boot_statistics, 1- 0.01/2)] <- "**"
  # Dataframe que reune el estadistico junto a la significancia
  resultado_boot <- data.frame("Estadistico Patell" = Tp, "Significancia"= significancia)
  return(resultado_boot)
}
#---------------------------------------------------------------------------------------#


#------------------------------   21. Corrado_Zivney   ----- --------------------------#
# Realizar el test no parametrico de Corrado y Zivney (1992) siguiendo la formulacion de
# Pynnonen (2022) para CAAR
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.list        : lista generada por la funcion <estimation.event.study>, que incluye para 
#--                    cada par evento-indice un dataframe con retornos anormales
#-- es.window.length : tamaño ventana de estimacion
#-- ev.window.length : tamaño ventana de evento
# ----Argumentos de salida  ----#
#-- result           : dataframe con el estadistico de Corrado y su significancia (* para 10%, ** para 5% y *** para 1%)
#---------------------------------------------------------------------------------------#
corrado_zivney <- function(data.list,es.window.length,ev.window.length){
  # Establecer variables para guardar los rangos de retornos anormales
  full_rank  <- NULL
  event_rank <- NULL
  for(element in data.list){
    # Generar el ranking de los retornos anormales para toda los retornos anormales de la ventana de estimacion y evento
    # Acceder a los datos del objeto zoo con la funcion <coredata>. Ranking sin importar el signo
    element_full_rank  <- rank(zoo::coredata(element@retornos$Abnormal[1:(es.window.length+ev.window.length)]), 
                               na.last="keep", ties.method = "average")
    
    # Guardar el ranking exclusivamente para los retornos anormales de la ventana de evento 
    # (<es.window.length> + 1 a <es.window.length> + <ev.window.length>)
    element_event_rank <- element_full_rank[(es.window.length+1):(es.window.length+ev.window.length)] 
    
    # Generar matrices de rankings de los retornos anormales para todos los objetos en <data.list>
    if(is.null(full_rank)){
      full_rank <- element_full_rank
    }else{
      full_rank <- cbind(full_rank, element_full_rank)
    }
    if(is.null(event_rank)){
      event_rank <- element_event_rank
    }else{
      event_rank <- cbind(event_rank,element_event_rank)
    }
  }
  
  # Para cada columna de full_rank tenemos que restarle el valor esperado de K_{it}, siguiendo la ecuacion 44 de Pynnonen (2022), teniendo en cuenta que 
  # E[K_{it}] = (T_i'+1)/2 de acuerdo a Campbell y Wasley (1993), donde T_1' es el numero de retornos para la firma k asociada al evento en toda la muestra 
  full_rank_reduced <- apply(full_rank, 2, function(x) x - ((nrow(full_rank) + 1) / 2))
  
  # Generar k promedio, siguiendo la ecuacion 44 de Pynnonen (2022)
  neventos <- ncol(full_rank_reduced)
  average_rank <- apply(full_rank_reduced, 1, function(x) sum(x, na.rm = TRUE)/neventos)
  
  # Rangos promedio para la ventana de evento para usar la ecuacion 43 de Pynnonen (2022)
  average_event_rank <- average_rank[(es.window.length+1):(es.window.length+ev.window.length)]
  
  # El numerador del estadistico z_{cw} es entonces
  num_stat <- sum(average_event_rank)
  
  # Desviacion estandar de la ecuacion 45 de Pynnonen (2022). 
  sk <- sqrt((1/(es.window.length+ev.window.length))*sum(average_rank^2))
  
  # Denominador del estadistico z_{cw}
  den_stat <- sk*sqrt(ev.window.length)
  
  # El estadistico es
  stat <- num_stat/den_stat
  
  # Comparar con los valores criticos al 10%, 5% y 1% de normal estandar
  
  significancia <- ""
  significancia[stat <= qnorm(0.1/2) | stat >= qnorm(1- 0.1/2)] <- "*"
  # Significancia al 5%
  significancia[stat <= qnorm(0.05/2) | stat >= qnorm(1- 0.05/2)] <- "**"
  # Significancia al 1%
  significancia[stat <= qnorm(0.01/2) | stat >= qnorm(1- 0.01/2)] <- "**"
  
  result <- cbind(round(stat,4),significancia)
  colnames(result) <- c("Statistic","Significance")
  return(result)
}
#---------------------------------------------------------------------------------------#


#------------------------------   22. wilcoxon_Pagnottoni   ----- --------------------------#
# Realizar el test no parametrico de Wilcoxon para los CAR de cada SUR estimado
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- coefficients.list: lista que contiene los coeficientes estimados para los SUR
#-- name.variable    : nombre de una columna de <dataframe_wilcoxon>, que es el resultado de la funcion
#-- pattern.step     : vector de caracteres que indican los coeficientes relacionados con retornos anormales
#-- pattern.indexes  : vector caracter que indica el nombre de los indices de interes
#-- pattern.variable : vector caracter que indica el nivel de agregacion de los CAR, puede ser por tipo de 
#--                    desastre o por pais donde ocurrio el desastre
# ----Argumentos de salida  ----#
#-- dataframe_wilcoxon : dataframe que va a incluir el pais o tipo de desastre, su retorno anormal acumulado
#                        promedio (CAAR), su estadistico de Wilcoxon, su p_valor y su significancia
#---------------------------------------------------------------------------------------#
wilcoxon_Pagnottoni <- function(coefficients.list,name.variable,pattern.step,pattern.indexes,pattern.variable){
  # Dataframe que va a guardar el tipo de desastre/pais, el CAAR, el estadistico de wilcoxon, el p value y la significancia
  dataframe_wilcoxon <- data.frame() 
  
  # Filtrar por aquellos que acaben en cualquier valor de <pattern.step>.
  for (element in coefficients.list){
    ## Genera un dataframe con el estimado, error estandar, t_value, p_value, que salen de la estimacion element
    dataframe_coef <- as.data.frame(element) 
    ## Le cambiamos nombres al dataframe para mejor manejo, pero reflejan lo mismo
    colnames(dataframe_coef) <- c("Estimate","SD_error","t_value","p_value")
    ## Extraemos las filas que nos interesan, es decir aquellas de las dummies, que acaban en t0, t1, t2, t3 o t4, es decir <pattern.step>
    dataframe_coef_filtrado  <- dataframe_coef %>% 
      dplyr::filter(str_ends(row.names(.),pattern = paste(pattern.step,collapse="|")))
    
    ## Ahora para poder agregar los retornos anormales es necesario poder extraer el nombre del indice, usando la funcion
    ## <str_extract>, que extrae el primer valor identico entre dos strings. Esto permite extraer el indice, que en cada fila aparece 
    ## de primer lugar
    string_start <- stringr::str_extract(row.names(dataframe_coef_filtrado), pattern = paste(pattern.indexes,collapse="|")) 
    
    # Agrega la columna con el nombre de los indices al dataframe <dataframe_coef_filtrado>
    dataframe_coef_filtrado <- cbind(dataframe_coef_filtrado,string_start)
    
    # Agrega los retornos anormales por el indice al cual pertenecen
    CAR_df <- dataframe_coef_filtrado %>% 
      group_by(string_start) %>% 
      summarise(CAR = sum(Estimate)) # <Estimate> es el valor del retorno anormal estimado
    
    # Calcular el <CAAR>
    CAAR <- mean(CAR_df$CAR)
    
    # Usar la funcion <stringr::str_extract_all> para determinar cual es el tipo de desastre 
    matches       <- stringr::str_extract_all(row.names(dataframe_coef_filtrado),paste(paste0(']',pattern.variable),collapse = "|"))
    type_disaster <- unique(unlist(matches))
    # Detener la funcion si hay mas de un tipo de desastre o mas de un pais
    if(length(type_disaster)!=1) stop("La agregacion de retornos anormales solo se puede hacer para un pais/un tipo de desastre")
    
    # Se procede a realizar el test de Wilcoxon
    # Se genera un dataframe para poder realizar el ordenamiento de los car
    df_car <- data.frame("car"=CAR_df$CAR,"magnitude"=abs(CAR_df$CAR),"sign"=sign(CAR_df$CAR))
    df_car <- df_car %>% 
      mutate(magnitude=ifelse(magnitude==0,NA,magnitude)) ## Se coloca NA si la magnitud es 0, ya que no se deben considerar
    # Funcion <rank> para ordenar las magnitudes de los car
    df_car <- df_car %>% 
      mutate(rank = rank(magnitude))
    
    # Suma de los rangos de car positivos: <positive_rank_sum>
    rank_sum <- df_car %>% 
      dplyr::group_by(sign) %>%
      dplyr::summarize(sum = sum(rank))
    positive_rank_sum <- rank_sum$sum[rank_sum$sign=="1"]
    
    # Colocarle la sumatoria de rango 0 si no hubo ningun CAR positivo
    if(length(positive_rank_sum)==0) positive_rank_sum <- 0
    
    # Calculo de la significancia del estadistico de Wilcoxon
    # La funcion para hallar los cuantiles de la distribucion del estadistico de Wilcoxon es <stats::qsignrank()>. 
    # A partir de 1000 observaciones, la funcion no se comporta adecuadamente, pero debido a que es muestra grande, la distribucion
    # converge a una normal con media N(N+1)/4 y varianza N(N+1)(2N+1)/24 por lo que usamos <stats::qnorm()>>
    significance <- ""
    ## Calculo para cada nivel de significancia si el valor de <positive_rank_sum> es lo suficientemente extremo para rechazar H_0
    #  La prueba se hace a dos colas, por lo que la primera condicion para cada prueba de significancia compara <positive_rank_sum> con el 
    #  valor critico de la cola derecha.
    #  La segunda condicion es la comparacion de <positive_rank_sum> con el valor critico de la cola izquierda.
    N <- nrow(df_car)
    
    # Uso de <qsignrank> o <qnorm> dependiendo del tamaño de la muestra
    if(N<=1000){
      # Si se evalua la significancia al 10%, con un test a dos colas, debemos buscar los percentiles 5 y 95, y comparar con estadistico
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.1/2,n=N)|
                     positive_rank_sum <= stats::qsignrank(0.1/2, n=N)] <- "*"
      # Para evaluar al 5%:
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.05/2, n=N)|
                     positive_rank_sum <= stats::qsignrank(0.05/2, n=N)] <- "**"
      # Al 1%:
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.01/2, n=N)|
                     positive_rank_sum <= stats::qsignrank(0.01/2, n=N)] <- "***"
    }else{
      mu = N*(N+1)/4
      sigma = sqrt(N*(N+1)*(2*N+1)/24)
      # Si se evalua la significancia al 10%, con un test a dos colas, debemos buscar los percentiles 5 y 95, y comparar con estadistico
      significance[positive_rank_sum >= stats::qnorm(1 - 0.1/2,mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.1/2, mean = mu,sd = sigma)] <- "*"
      # Para evaluar al 5%:
      significance[positive_rank_sum >= stats::qnorm(1 - 0.05/2, mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.05/2, mean = mu,sd = sigma)] <- "**"
      # Al 1%:
      significance[positive_rank_sum >= stats::qnorm(1 - 0.01/2, mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.01/2, mean = mu,sd = sigma)] <- "***"
    }
    # Por ultimo, usando la funcion <wilcox.test> obtenemos el pvalor
    p_value <- wilcox.test(CAR_df$CAR)$p.value
    # Agregamos las variables a <dataframe_wilcoxon>
    dataframe_wilcoxon <- rbind(dataframe_wilcoxon,c(gsub("]","",type_disaster),round(CAAR,4),positive_rank_sum,round(p_value,4),significance))
  }
  colnames(dataframe_wilcoxon) <- c(name.variable, "CAAR","Wilcoxon_statistic","p_value","Significance")
  return(dataframe_wilcoxon)
}
#---------------------------------------------------------------------------------------#


#------------------------------   23. car_pagnottoni   ----- --------------------------#
# Genera una grafica de los retornos anormales acumulados dependiendo del dia relativo al evento. 
# Es posible generar una grafica con los cAR de cada evento de interes, o con el CAR promedio de los 
# eventos de interes.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#--   coeffs          : vector numerico con coeficientes
#--   indices         : vector con nombres de indices 
#--   interest.vars   : string que indica los tipos de desastres o paises que se encuentran en <coeffs>
#--   average         : booleano, <TRUE> indica que se quiere graficar solamente el promedio de los CAR
#--                     y <FALSE> indica que se quieren graficar todos los CAR resultantes del SUR
# ----Argumentos de salida  ----#
#-- 
#---------------------------------------------------------------------------------------#
car_pagnottoni = function(coeffs,indices,interest.vars,average,serie.rm,pattern){
  # El titulo del grafico se encuentra revisando los nombres de <coeffs> respecto a <interest.vars>. 
  # La siguiente linea busca cual es el tipo de desastre o pais que vamos a graficar
  # '_t' se anhade para asegurar que se busquen los coeficientes correctos, ya que para un coeficiente puede salir dos paises
  # distintps
  plot_title <- interest.vars[sapply(interest.vars, function(value) any(grepl(paste0(value,'_t'), names(coeffs))))]
  # La longitud de <plot_title> debe ser 1, o si no hay error. Si es mayor a 1 significa que hay alguna mezcla de 
  # coeficientes, ya que tendriamos para dos tipos de desastres o dos paises
  if(length(plot_title) !=1) stop("Hay un error con los coeficientes, por favor revisar que pertenezcan solamente a un tipo de
                                  desastre o a solamente un pais")
  car_matrix <- NULL
  for(indice in indices){
    # Segun el vector ingresado en <coeffs>, se buscan los coeficientes solamente para el indice
    # <indice> y se ordenan en orden cronologico, teniendo en cuenta que por construccion, t0 es el dia
    # del evento, t1 es el dia siguiente, t2 dos dias despues, ...
    index.coefs <- coeffs[startsWith(names(coeffs), indice)]
    # Generar matriz de CAR's, donde cada columna sera un indice y las filas seran los CAR para [t0,t0],[t0,t1],
    # [t0,t2],... El numero de filas de la matriz sera igual al numero de elementos en <index.coefs>
    if(is.null(car_matrix)) {
      # <cumsum> genera la suma acumulada del vector <index.coefs>
      car_matrix <- matrix(data=cumsum(index.coefs),nrow=length(index.coefs)) 
    }else{
      car_matrix <- cbind(car_matrix,as.numeric(cumsum(index.coefs)))
    }
  }
  
  # Cambiar el nombre de filas y columnas
  colnames(car_matrix) <- indices
  rownames(car_matrix) <- 0:(nrow(car_matrix)-1)
  if(average == FALSE){
    # Titulo
    plot_title <- paste0('CAR relativo al dia del evento. Para ',serie.rm, '. Eventos: ',plot_title)
    base::plot(x=rownames(car_matrix),y=car_matrix[,1],type="l",main=plot_title,xlab='Dia relativo al evento',ylab="Retorno Anormal Acumulado (CAR)",
               ylim=c(min(car_matrix),max(car_matrix)))
    for(i in 2:ncol(car_matrix)){
      lines(x=rownames(car_matrix), y = car_matrix[,i],type="l")
    }
  }else{
    # Titulo
    plot_title2 <- paste0('CAAR relativo al dia del evento. Para ',serie.rm, '. 
    Eventos: ',plot_title)
    base::plot(x=rownames(car_matrix), y = rowMeans(car_matrix),type="l",main=plot_title2,xlab='Dia relativo al evento',ylab="Retorno Anormal Acumulado Promedio (CAAR)")
  }
}
#---------------------------------------------------------------------------------------#


#------------------------------   24. arma_lags_database   -------------------------------#
# Por ahora la funcion solo trabaja con rezagos de la parte AR(p)
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#--   base          : base de datos que contiene las series a las que se quiere sacar el numero de rezagos
#--   interest.vars : vector con nombres de indices 
#--   no.lags       : numero de rezagos a considerar. Si es un numero, se calcula ese numero de rezagos
#                     para todas las <interest.vars>. Si es una lista con numeros, a la i-esima variable de 
#                     <interst.vars> se le calcula el i-esimo elemento de no.lags.
#                     El default es <NULL>, en cuyo caso para cada variable se le calculan los rezagos optimos
#                     siguiendo el criterio de informacion de Akaike
#--   AR.m          : rezago máximo de la parte autorregresiva
#--   MA.m          : rezago máximo de la parte de promedio movil
#--   d             : orden de diferenciación
#--   bool          : booleano que indica si realizar la estimación arima con constante
#--   metodo        : método por el cual se hará la estimación ARIMA (existe CS, ML y CSS-ML)
# ----Argumentos de salida  ----#
#--   base_num_rezagos : base de datos que contiene el numero de rezagos optimos para cada indice en <indices>
#---------------------------------------------------------------------------------------#
arma_lags_database <- function(base, interest.vars, no.lags, AR.m, MA.m, d, bool, metodo){
  
  base_num_rezagos <- data.frame(p=double(),q=double())   # dataframe que va a guardar todos los rezagos
  
  # El objeto <base> debe tener clase <zoo> o <ts>, o la funcion tendra errores 
  if(!inherits(base,"zoo") & !inherits(base,"ts")) {
    stop("La base ingresada no es ni de clase zoo ni de clase ts")
  }
  
  # El objeto <no.lags> debe ser <numeric> o <integer> o <NULL> o la funcion tendra errores
  if(!inherits(no.lags,"numeric") & !inherits(no.lags,"integer") & !inherits(no.lags,"NULL")) {
    stop("El numero de rezagos ingresado no es numerico")
  }
  
  # La longitud de <no.lags> puede ser o 0, o 1 o <length(interest.vars)>, de otro modo surgiria un error
  if(length(no.lags)!=0 & length(no.lags)!=1 & length(no.lags)!=length(interest.vars)) 
    stop("no.lags tiene dimensiones incorrectas, se debe ingresar un solo numero o un vector del mismo tamaño que 
          las variables dependientes ingresadas")
  
  # Por cada elemento en <base[,interest.vars]> hallar los rezagos optimos dependiendo del valor de <no.lags>
  # Si es <NULL> los numeros de rezagos se obtienen automaticamente con AIC
  
  for(i in 1:ncol(base[,interest.vars])){ 
    indice <- colnames(base[,interest.vars][,i])
    if(is.null(no.lags)){
      # Encontrar rezagos optimos
      lags_matrix <- arma_seleccion_df(base[,indice], AR.m =20,MA.m = 0,d = 0, bool = TRUE,metodo = "CSS")
      p           <- lags_matrix[which.min(lags_matrix$AIC),'p']
      q           <- lags_matrix[which.min(lags_matrix$AIC),'q']
      # Agregarlos al dataframe base_num_rezagos
      base_num_rezagos[indice,] <- c(p,q)
    }else if(length(no.lags) == 1){
      if(no.lags != 0){
        # Si <no.lags> es un numero, simplemente p sera igual a <no.lags>
        p <- no.lags
        q <- MA.m
        base_num_rezagos[indice,] <- c(p,q)
      }
    }else if(length(no.lags) == length(interest.vars)){
      # Si <no.lags> tiene la misma cantidad de numeros que elementos en <interest.vars> se toma el i-esimo 
      # numero como p-rezagos en el i-esimo indice, solamente si el i-esimo numero es distinto de 0
      if(no.lags[i]!=0){
        p <- no.lags[i]
        q <- MA.m
        base_num_rezagos[indice,] <- c(p,q)
      }
    }else if(length(no.lags) >1 & length(no.lags) != length(interest.vars)) 
      stop("El vector de rezagos no.lags tiene que tener el mismo numero de elementos que indices ingresados")
  }
  return(base_num_rezagos)
}
#---------------------------------------------------------------------------------------#

# if(0) porque abajo se cambio la funcion para que se revisaran los confounding events solamente en una ventana 
# de traslape determinada
if(0){
  volatility_event_study = function(base.evento, date.col.name, geo.col.name, base.vol, interest.vars, num_lags, AR.m = 20, MA.m = 0,d = 0,
                                    bool = TRUE,metodo = "CSS", es.start,len.ev.window,var.exo,var.exo.pais,bool.paper,bool.cds,garch,
                                    overlap.events = NULL){
    # Crear una nueva clase de objetos para guardar informacion importante de la estimacion ARMA-GARCH
    setClass("ESVolatility",slots=list(coefficients = "numeric",goodness_of_fit = "numeric",res_estandar_estimacion="xts",
                                       res_no_estandar_estimacion="xts",variance_forecast="xts",residuales_evento="xts",
                                       info.evento = 'data.frame'))
    
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
    
    lista_volatilidad <- list()
    for(i in 1:nrow(base.evento)){
      evento <- base.evento[i,]
      pais_evento   <- evento[geo.col.name]
      # Se obtiene primero el indice del pais donde sucedio el desastre
      indice <- matching(as.character(pais_evento),bool.cds,bool.paper)[1] # El 1 solamente se coloca porque con la base de Pagnottoni, USA tiene dos stocks
      # Ya no es necesario escribirlo cuando se utilicen los CDS, cada pais solo tiene un CDS
      
      #if(0) porque se realiza arriba en el vector
      if(0){
        # Loop que genera la posicion de desastre respecto al indice de <base.vol>. Si la fecha del evento no esta en el indice de 
        # <base.vol>,se revisara hasta <nrow(base.vol)> dias despues del desastre para ser considerado como el inicio del evento
        for(j in 0:nrow(base.vol)){
          if((evento[,date.col.name]+j) %in% index(base.vol[,indice])){ 
            # Generacion del dia del desastre (o j dias despues del desastre, si el dia del desastre no esta en el indice de 
            # <asset.returns>)
            dia_evento  <- evento[,date.col.name]+j
            # Generacion de la posicion del dia de desastre en el indice de fechas de <base.vol>
            # (o j dias despues del desastre, si el dia del desastre no esta en el indice de <base.vol>)
            indice_del_evento <- which(index(base.vol[,indice]) == dia_evento)
            break
          }
        }
      }
      # Variable que indica el inicio del evento
      indice_del_evento <- base.evento[i, 'indices2']
      
      # <fin_estimacion> indica que la ventana de estimacion va hasta el dia anterior al dia de evento. Si se desea que termine mucho antes
      # faltaria parametrizarlo
      fin_estimacion      <- indice_del_evento - 1
      inicio_estimacion   <- indice_del_evento - es.start
      
      # Estimacion APARCH con modelo de media ARMA -------------------------------
      
      # Se obtienen los ordenes para el modelo ARMA(p,q) del indice
      p   <- lags_database[indice,"p"]
      q   <- lags_database[indice,"q"]
      
      # Variables exogenas que dependen del pais
      variables_pais <- paste(var.exo.pais,pais_evento,sep="_")
      
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
        subset_data <- suppressMessages(anti_join(subset_data, base.evento[i,]))
        # <subset_data> es un data.frame que contiene todos los desastres de <overlap.events> que se encuentran anterior a nuestro desastre
        # de interes. La idea entonces es generar una variable dummy donde 1 sea en los dias que hubo uno de estos eventos
        # con el fin de controlar el posible confounding effect. 
        # Tambien seria interesante agregar a la dummy ciertos dias despues de cada desastre en <subset_data>, para lo cual se utilizara
        # la media de duracion por el tipo de desastre
        overlap.dummy <- rep(0, fin_estimacion)
        subset_data_list <- split(subset_data, subset_data$Disaster.Subgroup)
        # Recordemos que del analisis descriptivo, la media de duracion de los desastres geofisicos es 2, mientras que de los hidrologicos es 8
        # y de los meteorologicos es 4
        df.mediaduracion <- data.frame(c('Geophysical','Hydrological','Meteorological'), c(2,8,4))
        colnames(df.mediaduracion) <- c('Tipo.desastre','Media.duracion')
        for(df.m in seq_along(subset_data_list)) {
          for(num in subset_data_list[[df.m]]$indices2) overlap.dummy[num:(num+df.mediaduracion$Media.duracion[df.mediaduracion == names(subset_data_list)[df.m]])] <- 1 
        }
        # <overlap.dummy> es del tamaño de <fin_estimacion>, por lo que en la estimacion se restringira a <inicio_estimacion>- <fin_estimacion>
      }
      
      while (TRUE) {
        warning_dummy <- FALSE
        # <tryCatch> corre el codigo, pero si encuentra algun warning o error, realiza un codigo especifico.
        tryCatch({
          # Antigua especificacion GARCH, reemplazada en la linea 2058
          if(0){
            # Especificacion sGARCH
            spec <- ugarchspec(
              variance.model = list(model = garch, garchOrder = c(1, 1)),
              mean.model = list(
                armaOrder = c(p, q),
                # Para la primera iteracion del loop <While> se utilizan los datos de la ventana de estimacion
                external.regressors = as.matrix(base.vol[(inicio_estimacion:fin_estimacion), c(var.exo, variables_pais)])
              ),
              distribution.model = "std"
            )
          }
          
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
        cat('Las fechas del modelo GARCH corresponden con la ventana de estimacion','\n')
      }
      
      gof_p_values        <- gof(fit,c(20,30,40,50))[,"p-value(g-1)"]
      names(gof_p_values) <- c("20 bins","30 bins","40 bins","50 bins")
      if(any(as.logical(gof_p_values < 0.05))) warning("Los residuales estandarizados no siguen la distribucion seleccionada.")
      cat(i,"\n")
      
      # if(0)porque el forecast de la voaltilidad condicional se realiza usando <ugarchforecast>
      if(0){
        # Forecast volatilidad condicional
        # Usando la ecuacion de Bialkowski (2008) para realizar el forecast de sigma^2
        # h_t sale de fit@fit$var, \varepsilon_t sale de fit@fit$residuals y los errores estandarizados 
        # salen de fit@fit!z
        omega         <- coef(fit)["omega"]
        alpha         <- coef(fit)["alpha1"]
        beta          <- coef(fit)["beta1"]
        fcast_var     <- c()
        for(k in 1:len.ev.window){
          j <- 0:(k-1)
          fcast_var_ti  <- omega*sum((beta+alpha)^j) + (beta+alpha)^(k-1)*beta*(tail(as.numeric(sigma(fit))^2,1))+
            (beta+alpha)^(k-1)*alpha*(tail((residuals(fit))^2,1))
          fcast_var <- c(fcast_var, fcast_var_ti)
        }
      }
      
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
      lista_volatilidad[[paste(pais_evento,i,sep="_")]] <- object
    }
    return(lista_volatilidad)
  }

}
#------------------------------   25. volatility_event_study   -------------------------------#
# Para cada evento estima un modelo apARCH(1,1) y calcula el forecast de la volatilidad condicional.
# Por el momento la funcion solo corre con apARCH(1,1) y modelo de distribucion t. Faltaria parametrizarlos si se desean cambiar
# El objeto que retorna es una lista de objetos "ESVolatility", donde cada uno de ellos incluye los coeficientes del modelo, los 
# p_values de tests de bondad de ajuste de los errores estandarizados a distribucion t, junto al forecast de la volatilidad y los
# errores durante la semana de evento.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#--   base.evento   : base de datos que contiene los eventos con los que se hara el estudio de volatilidad
#--   date.col.name : nombre de la columna de <base.evento> que indica la fecha del evento
#--   geo.col.name  : nombre de la columna de <base.evento> que indica la geolocalizacion del evento (generalmente pais)
#--   base      : base de datos que contiene las series financieras con las que se hace el estudio de volatilidad
#--   interest.vars : vector con nombres de las columnas de <base> que corresponden a las series financieras
#--   num_lags      : numero de rezagos a considerar. Si es un numero, se calcula ese numero de rezagos
#                     para todas las <interest.vars>. Si es una lista con numeros, a la i-esima variable de 
#                     <interest.vars> se le calcula el i-esimo elemento de no.lags.
#                     El default es <NULL>, en cuyo caso para cada variable se le calculan los rezagos optimos
#                     siguiendo el criterio de informacion de Akaike
#--   AR.m          : rezago maximo de la parte autorregresiva
#--   MA.m          : rezago maximo de la parte de promedio movil
#--   d             : orden de diferenciacion
#--   bool          : booleano que indica si realizar la estimacion arima con constante
#--   metodo        : metodo por el cual se hara la estimación ARIMA (existe CS, ML y CSS-ML)
#--   es.start      : numero que indica cuantos dias antes del evento comienza la ventana de estimacion
#--   len.ev.window : numero que indica el tamanho de la venta de evento
#--   var.exo       : strings que indican nombres de columnas de <base> que corresponden a variables exogenas que no 
#--                   dependen del pais (o ciudad o region)
#--   var.exo.pais  : strings que indican parte del nombre de alguna columna de <base> que corresponde, concatenado al pais,
#--                   una variable exogena
#--   bool.paper    : booleano donde 1 indica que se usaran los datos del paper, se necesita para la funcion <matching> que esta incluida en la
#                     presente funcion
#--   bool.cds      : booleano donde 1 indica que se usan datos de cds y 0 indica que son datos de indices de bolsa. 
#--   garch         : el tipo de garch que se quiere estimar
#--   overlap.events: dataframe de posibles eventos que se traslapan con la ventana de estimacion de los desastres
#--   overlap.max: ventana maxima para la duracion de un evento que este traslapando
# ----Argumentos de salida  ----#
#--   lista_volatilidad : lista de objetos clase "ESVolatility". Cada objeto de esta clase incluye los parametros estimados durante el
#                         modelo ARMA(p,q)-apARCH(1,1), junto con p_values de pruebas de bondad de ajuste de los errores estandarizados
#                         a la distribucion t. Ademas, incluye el forecast de la volatilidad para los <len.ev.window> dias siguientes
#                         al evento. Por ultimo, incluye el error durante la ventana de evento
#---------------------------------------------------------------------------------------#
volatility_event_study = function(base.evento, date.col.name, geo.col.name, base.vol, interest.vars, num_lags, AR.m = 20, MA.m = 0,d = 0,
                                  bool = TRUE,metodo = "CSS", es.start,len.ev.window,var.exo,var.exo.pais,bool.paper,bool.cds,garch,
                                  overlap.events = NULL,overlap.max){
  # Crear una nueva clase de objetos para guardar informacion importante de la estimacion ARMA-GARCH
  setClass("ESVolatility",slots=list(coefficients = "numeric",goodness_of_fit = "numeric",res_estandar_estimacion="xts",
                                     res_no_estandar_estimacion="xts",variance_forecast="xts",residuales_evento="xts",
                                     info.evento = 'data.frame'))
  
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
  
  lista_volatilidad <- list()
  for(i in 1:nrow(base.evento)){
    evento <- base.evento[i,]
    pais_evento   <- evento[geo.col.name]
    # Se obtiene primero el indice del pais donde sucedio el desastre
    indice <- matching(as.character(pais_evento),bool.cds,bool.paper)[1] # El 1 solamente se coloca porque con la base de Pagnottoni, USA tiene dos stocks
    # Ya no es necesario escribirlo cuando se utilicen los CDS, cada pais solo tiene un CDS
    
    # Variable que indica el inicio del evento
    indice_del_evento <- base.evento[i, 'indices2']
    
    # <fin_estimacion> indica que la ventana de estimacion va hasta el dia anterior al dia de evento. Si se desea que termine mucho antes
    # faltaria parametrizarlo
    fin_estimacion      <- indice_del_evento - 1
    inicio_estimacion   <- indice_del_evento - es.start
    
    # Estimacion APARCH con modelo de media ARMA -------------------------------
    
    # Se obtienen los ordenes para el modelo ARMA(p,q) del indice
    p   <- lags_database[indice,"p"]
    q   <- lags_database[indice,"q"]
    
    # Variables exogenas que dependen del pais
    variables_pais <- paste(var.exo.pais,pais_evento,sep="_")
    
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
      subset_data <- suppressMessages(anti_join(subset_data, base.evento[i,]))
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
      cat('Las fechas del modelo GARCH corresponden con la ventana de estimacion','\n')
    }
    
    gof_p_values        <- gof(fit,c(20,30,40,50))[,"p-value(g-1)"]
    names(gof_p_values) <- c("20 bins","30 bins","40 bins","50 bins")
    if(any(as.logical(gof_p_values < 0.05))) warning("Los residuales estandarizados no siguen la distribucion seleccionada.")
    cat(i,"\n")
    
    # if(0)porque el forecast de la voaltilidad condicional se realiza usando <ugarchforecast>
    if(0){
      # Forecast volatilidad condicional
      # Usando la ecuacion de Bialkowski (2008) para realizar el forecast de sigma^2
      # h_t sale de fit@fit$var, \varepsilon_t sale de fit@fit$residuals y los errores estandarizados 
      # salen de fit@fit!z
      omega         <- coef(fit)["omega"]
      alpha         <- coef(fit)["alpha1"]
      beta          <- coef(fit)["beta1"]
      fcast_var     <- c()
      for(k in 1:len.ev.window){
        j <- 0:(k-1)
        fcast_var_ti  <- omega*sum((beta+alpha)^j) + (beta+alpha)^(k-1)*beta*(tail(as.numeric(sigma(fit))^2,1))+
          (beta+alpha)^(k-1)*alpha*(tail((residuals(fit))^2,1))
        fcast_var <- c(fcast_var, fcast_var_ti)
      }
    }
    
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
    lista_volatilidad[[paste(pais_evento,i,sep="_")]] <- object
  }
  return(lista_volatilidad)
}

#---------------------------------- 26. bootstrap.volatility  ------------------------------------#
# Para una lista de eventos estimados, genera la volatilidad anormal acumulada, junto al pvalue 
# asociado al bootstrap
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- umbral     : numero que indica la longitud de la ventana libre de eventos 
#-- base       : base que sera utilizada para la estimacion en <estimation.event.study>
#-- eventos    : matriz de eventos
#-- col.fecha  : nombre de la columna de fechas de eventos
#-- col.grupo  : nombre de la columna con la cual se va a desagrupar los eventos
#-- col.filtro : nombre de la columna con la cual se quiere filtrar los eventos significativos
# ----Argumentos de salida  ----#
#-- data.droped.events : dataframe de eventos filtrados. En este df ya no estan los eventos que no cuentan con una 
#                        ventana minima de estimacion ni con una ventana minima de evento
#---------------------------------------------------------------------------------------#
bootstrap.volatility <- function(volatility.list,es.window.length,ev.window.length,bootstrap_vol_iterations){ 
  # Detener la funcion si hay algun elemento que no tenga la clase 'ESmean'
  if (any(!sapply(volatility.list, inherits, "ESVolatility"))) {
    stop('La lista contiene elementos que no fueron creados con la funcion estimation.event.study.')
  }
  
  # Calculo Mt y CAV --------------------------------------------------------
  # El calculo de tanto Mt como CAV sale de Bourdeau (2017)
  # Por simplicidad de calculos, guardar los residuales observados (epsilon) y los pronosticos de la varianza condicional
  epsilon      <- data.frame(purrr::map(volatility.list, ~ coredata(.x@residuales_evento)))[1:ev.window.length,]
  sigma_cuad   <- data.frame(purrr::map(volatility.list, ~ coredata(.x@variance_forecast)))[1:ev.window.length,]
  
  # <if(0)> porque el codigo fue mejorado en el siguiente  bloque de codigo
  if(0){
    Mt <- c()
    for(i in 1:ev.window.length){
      termino_sumatoria <- c()
      for(j in 1:length(volatility.list)){
        numerador         <- (length(volatility.list)*epsilon[i,j]-sum(epsilon[i,]))^2
        denominador       <- (length(volatility.list))*(length(volatility.list)-2)*sigma_cuad[i,j]+sum(sigma_cuad[i,])
        termino_sumatoria <- c(termino_sumatoria, numerador/denominador)
      }
      Mt[i] <- (1/(length(volatility.list)-1))*sum(termino_sumatoria)
    }
  }
  
  # Es el mismo codigo anterior pero con operaciones vectorizadas (chatGPT). Si generan los mismos resultados
  # El proceso se encuentra en la funcion <mt_function>
  Mt <- mt_function(epsilon,sigma_cuad)
  
  # La volatilidad anormal acumulada (CAV) esta definida como
  cav <- sum(Mt) - length(Mt)
  
  # Bootstrap Volatility ----------------------------------------------------
  
  # El siguiente procedimiento de bootstrap sigue la formulacion desarrollada por Mnasri y Nechi (2016)
  # Generar matriz kxN (<es.window.length>x<length(volatility.list>) de los residuales de las ecuaciones GARCH
  
  E_matrix <- matrix(nrow=es.window.length,ncol=length(volatility.list)) 
  for(i in 1:length(volatility.list)) E_matrix[,i] <- coredata(volatility.list[[i]]@res_estandar_estimacion)
  
  # Reescalar la matriz para que cada columna tenga media 0 y varianza 1
  E_matrix <- scale(E_matrix)
  
  # Generar un array de <lenght(volatility.list)> matrices, cada una con tamaño 
  # (<es.window.length>x<ev.window.length>)
  residual_array <- array(dim=c(es.window.length,ev.window.length,length(volatility.list)))
  
  # Multiplicar cada columna de la matriz <E_matrix> por el vector de desviacion estandar condicional estimada
  for(i in 1:length(volatility.list)){
    forecasted_sd       <- sqrt(coredata(volatility.list[[i]]@variance_forecast))[1:ev.window.length]
    residual_array[,,i] <- E_matrix[,i] %*% t(forecasted_sd)
  }
  # El codigo anterior genera un array de dimensiones <es.window.length>x<ev.window.length>x<length_volatility.list>
  # La primera dimension indica el numero de filas de cada matriz, siendo igual a la longitud de la ventana de estimacion
  # La segunda dimension indica el numero de columnas de cada matriz, siendo igual a la longitud de la ventana de evento
  # La tercera dimension indica el numero de matrices en el array, siendo igual al numero de eventos
  
  # Realizar el bootstrap 
  cav_empiric_vector <-c()
  
  for(b in 1:bootstrap_vol_iterations){
    set.seed(b)
    # Seleccionar <length(volatility.list)> numeros aleatorios
    random_nums <- sample(1:es.window.length,length(volatility.list),replace = T)
    # Seleccionar la <random_num>-esima fila de cada matriz dentro del array y organizarlas en una matriz para calcular Mt
    epsilon_boot <- matrix(NA,nrow=ev.window.length,ncol=length(volatility.list))
    for(k in 1:length(volatility.list)) epsilon_boot[,k] <- residual_array[random_nums[k],,k]
    Mt_boot  <- mt_function(epsilon_boot,sigma_cuad)
    cav_boot <- sum(Mt_boot) - length(Mt_boot)
    cav_empiric_vector <- c(cav_empiric_vector,cav_boot)
  }
  
  # El p-value esta definido por Bourdeau (2017), quien dice que es la proporcion de valores que son mayores al
  # cav calculado originalmente
  
  pvalue_bootstrap <- sum(cav_empiric_vector > cav)/bootstrap_vol_iterations
  significancia    <- ''
  if(pvalue_bootstrap<=0.1) significancia <- '*'
  if(pvalue_bootstrap<=0.05) significancia <- '**'
  if(pvalue_bootstrap<=0.01) significancia <- '***'
  resultado        <- data.frame('CAV'=round(cav,3),'Significancia' = significancia,'p_value'=pvalue_bootstrap)
}

#------------------------------   27. mt_function   -------------------------------#
# Genera el vector de Mt (efecto multiplicativo en la volatilidad) para una ventana de evento, utilizando
# errores observados y varianza condicional pronosticada.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#--   epsilon_matrix        : matriz de errores observados. El numero de filas es igual a la longitud de la ventana de evento
#--                              El numero de columnas es igual al numero de eventos a considerar
#--   variance_forecast_matrix : matriz de varianza condicional pronosticada. Las dimensiones son iguales que en <epsilon_matrix>
# ----Argumentos de salida  ----#
#--   Mt_vector : vector de Mt (efecto multiplicativo en la volatilidad). La longitud del vector es igual al numero de dias en la 
#--               ventana de evento
#---------------------------------------------------------------------------------------#

mt_function = function(epsilon_matrix, variance_forecast_matrix){
  numerador <- (ncol(epsilon_matrix) * epsilon_matrix - rowSums(epsilon_matrix))^2
  denominador <- (ncol(epsilon_matrix)) * (ncol(epsilon_matrix) - 2) * variance_forecast_matrix + rowSums(variance_forecast_matrix)
  Mt_vector <- (1 / (ncol(epsilon_matrix) - 1)) * rowSums(numerador / denominador)
  return(Mt_vector)
}

#---------------------------------------------------------------------------------------#

#------------------------------   28. bmp_savickas   -------------------------------#
# Genera el estadistico de Savickas (2003), junto a su p_value. La formula del estadistico se realiza siguiendo
# a Demirer y Kutan (2010) 
#---------------------------------------------------------------------------------------#
#-- data.list        : lista generada por la funcion <estimation.event.study>, que incluye para cada par evento-indice 
#--                    un dataframe con retornos anormales. 
#                      Es necesario que en la funcion <estimation.event.study>, <GARCH> no sea null para que funcione <bmp_savickas>
#-- es.window.length : tamaño ventana de estimacion
#-- ev.window.length : tamaño ventana de evento
#-- tail             : a que cola se quiere la hipotesis alternativa, donde -1 significa cola izquierda, 1 significa cola derecha y 2 significa a dos colas. 
#                      el default es a dos colas
# ----Argumentos de salida  ----#
#-- resultado        : dataframe con el estadistico GARCH y su significancia (* para 10%, ** para 5% y *** para 1%)   
#---------------------------------------------------------------------------------------#

bmp_savickas = function(data.list,es.window.length,ev.window.length,ev.window.begin,tail){
  # Para el calculo del CAR se toma la serie <Abnormal> a partir de la obs <es.window.length> + 1 hasta 
  # <es.window.length> + <ev.window.length>
  
  # Los retornos anormales se encuentran en <@retornos$Abnormal>. Seleccionarlos solamente para la ventana de evento 
  # El forecast de la varianza se encuentra en <@variance_forecast>
  # Generar el termino ScT que se encuentra en la pagina 1470 de Demirer(2010)
  retornos.anormales     <- data.frame(purrr::map(data.list,~coredata(.x@retornos$Abnormal[(es.window.length+1+ev.window.begin):(es.window.length+ev.window.length+ev.window.begin)])))
  var.condicional.evento <- data.frame(purrr::map(data.list,~coredata(.x@variance_forecast)[(1+ev.window.begin):ev.window.length+ev.window.begin]))
  # ScT es un vector de longitud <length(data.list)>
  ScT                    <- as.numeric(colMeans(retornos.anormales)/sqrt(colMeans(var.condicional.evento)))
  
  n               <- length(ScT)
  garch_statistic <- (mean(ScT))/(sqrt((1/(n*(n-1)))*sum((ScT - mean(ScT))^2)))
  
  if(0){
    # Comparar con una T con n-1 grados de libertad
    significancia <- ""
    significancia[garch_statistic <= qt(0.1/2, df = (n-1)) | garch_statistic >= qt(1- 0.1/2, df = (n-1))] <- "*"
    # Significancia al 5%
    significancia[garch_statistic <= qt(0.05/2,df = (n-1)) | garch_statistic >= qt(1- 0.05/2,df = (n-1))] <- "**"
    # Significancia al 1%
    significancia[garch_statistic <= qt(0.01/2,df = (n-1)) | garch_statistic >= qt(1- 0.01/2,df = (n-1))] <- "***"
  }
  
  # Hallar el pvalue a dos colas, dado que <tail> == '2' implica qu
  if(tail == 2)   p_value <- pt(abs(garch_statistic),df=(n-1),lower.tail = F)*2 
  # Hallar el pvalue para cola izquierda
  if(tail == -1 ) p_value <- pt(garch_statistic,df=(n-1),lower.tail = T)
  # Hallar el pvalue para cola derecha
  if(tail == 1 )  p_value <- pt(garch_statistic,df=(n-1),lower.tail = F)
  
  # Comparar con una T con n-1 grados de libertad
  significancia <- ""
  significancia[p_value < 0.10] <- "*"
  # Significancia al 5%
  significancia[p_value < 0.05] <- "**"
  # Significancia al 1%
  significancia[p_value < 0.01] <- "***"
  
  resultado <- data.frame('Estadistico'=round(garch_statistic,4),'Significancia'= significancia,
                          'p_value' = round(p_value,4))
  return(resultado)
}

bmp.bootstrap = function(data.list,es.window.length,ev.window.length,iterations.bootstrap){
  # Para el calculo del CAR se toma la serie <Abnormal> a partir de la obs <es.window.length> + 1 hasta 
  # <es.window.length> + <ev.window.length>
  
  # Los retornos anormales se encuentran en <@retornos$Abnormal>. Seleccionarlos solamente para la ventana de evento 
  # El forecast de la varianza se encuentra en <@variance_forecast>
  # Generar el termino ScT que se encuentra en la pagina 1470 de Demirer(2010)
  retornos.anormales     <- data.frame(purrr::map(data.list,~coredata(.x@retornos$Abnormal[(es.window.length+1):(es.window.length+ev.window.length)])))
  var.condicional.evento <- data.frame(purrr::map(data.list,~coredata(.x@variance_forecast)[1:ev.window.length]))
  # ScT es un vector de longitud <length(data.list)>
  ScT                    <- as.numeric(colMeans(retornos.anormales)/sqrt(colMeans(var.condicional.evento)))
  
  n               <- length(ScT)
  garch_statistic <- (mean(ScT))/(sqrt((1/(n*(n-1)))*sum((ScT - mean(ScT))^2)))
  
  # Comparar con una T con n-1 grados de libertad
  significancia <- ""
  significancia[garch_statistic <= qt(0.1/2, df = (n-1)) | garch_statistic >= qt(1- 0.1/2, df = (n-1))] <- "*"
  # Significancia al 5%
  significancia[garch_statistic <= qt(0.05/2,df = (n-1)) | garch_statistic >= qt(1- 0.05/2,df = (n-1))] <- "**"
  # Significancia al 1%
  significancia[garch_statistic <= qt(0.01/2,df = (n-1)) | garch_statistic >= qt(1- 0.01/2,df = (n-1))] <- "***"
  
  p_value   <- pt(abs(garch_statistic),df=(n-1),lower.tail = F)*2  
  resultado <- data.frame('Estadistico'=round(garch_statistic,4),'Significancia'= significancia,
                          'p_value' = round(p_value,4))
  
  # Bootstrap parecido al de Mnasri y Nechi (2016)
  E_matrix <- as.matrix(purrr::map_df(data.list, ~coredata(.x@res_estandar_estimacion)))
  # Reescalar la matriz
  E_matrix <- scale(E_matrix)
  
  # Generar un array de <lenght(data.list)> matrices, cada una con tamaño 
  # (<es.window.length>x<ev.window.length>)
  residual_array <- array(dim=c(es.window.length,ev.window.length,length(data.list)))
  
  # Multiplicar cada columna de la matriz <E_matrix> por el vector de desviacion estandar condicional estimada
  for(i in 1:length(data.list)){
    forecasted_sd       <- sqrt(coredata(data.list[[i]]@variance_forecast))[1:ev.window.length]
    residual_array[,,i] <- E_matrix[,i] %*% t(forecasted_sd)
  }
  # El codigo anterior genera un array de dimensiones <es.window.length>x<ev.window.length>x<length_data.list>
  # La primera dimension indica el numero de filas de cada matriz, siendo igual a la longitud de la ventana de estimacion
  # La segunda dimension indica el numero de columnas de cada matriz, siendo igual a la longitud de la ventana de evento
  # La tercera dimension indica el numero de matrices en el array, siendo igual al numero de eventos
  
  # Realizar el bootstrap 
  garch.stat.vector <- c()
  for(b in 1:iterations.bootstrap){
    set.seed(b)
    # Seleccionar <length(data.list)> numeros aleatorios
    random_nums <- sample(1:(es.window.length- ev.window.length+1),length(data.list),replace = T)
    # Seleccionar la <random_num>-esima fila de cada matriz dentro del array y organizarlas en una matriz para calcular Mt
    # Seleccionar residuales cronologicamente, el primer residual sera el <random_nums[k]> numero, y luego para los siguientes dias 
    # son los residuales <random_nums[k+1]>,<random_nums[k+2]>,...
    residuales.aleatorios <- matrix(NA,nrow=ev.window.length,ncol=length(data.list))
    for(k in 1:length(data.list)) for(m in 0:(ev.window.length-1)) residuales.aleatorios[(m+1),k] <- residual_array[(random_nums[k]+m),(m+1),k]
    
    ScT <- as.numeric(colMeans(residuales.aleatorios)/sqrt(colMeans(var.condicional.evento))) 
    n   <- length(ScT)
    garch.stat.vector[b] <- (mean(ScT))/(sqrt((1/(n*(n-1)))*sum((ScT - mean(ScT))^2)))
  }
  pvalue.garchst <- sum(abs(garch.stat.vector) > abs(garch_statistic))/iterations.bootstrap  # (Va con valor absoluto?)
  resultado$p_valuebootstrap <- pvalue.garchst
  
  return(resultado)
}

#---------------------------------------------------------------------------------------#

#------------------------------   29. j_statistic   -------------------------------#
# Genera el estadistico J que se utiliza en Nakai et al (2016), Zhao et al. (2018), Yamahuchi (2008)
#---------------------------------------------------------------------------------------#
#-- data.list        : lista generada por la funcion <estimation.event.study>, que incluye para cada par evento-indice 
#--                    un dataframe con retornos anormales. 
#                      Es necesario que en la funcion <estimation.event.study>, <GARCH> no sea null para que funcione <bmp_savickas>
#-- es.window.length : tamaño ventana de estimacion
#-- ev.window.length : tamaño ventana de evento
# ----Argumentos de salida  ----#
#-- resultado        : dataframe con el estadistico GARCH y su significancia (* para 10%, ** para 5% y *** para 1%)   
#---------------------------------------------------------------------------------------#

j_statistic = function(data.list,es.window.length,ev.window.length){
  
  # Para el calculo del CAR se toma la serie <Abnormal> a partir de la obs <es.window.length> + 1 hasta 
  # <es.window.length> + <ev.window.length>
  car_vector <- c()
  h_vector   <- c()
  for(element in data.list){
    car <- (sum(element@retornos$Abnormal[(es.window.length+1):(es.window.length+ev.window.length)]))
    h   <- (sum(element@variance_forecast[(1):(1+ev.window.length)])/(ev.window.length+1))
    car_vector <- c(car_vector,car)
    h_vector   <- c(h_vector, h)
  }
  
  j_statistic <- sum(car_vector)/(sqrt(sum(h_vector)))
  
  # Comparar con una normal estandar
  significancia <- ""
  significancia[j_statistic <= qnorm(0.1/2) | j_statistic >= qnorm(1- 0.1/2)] <- "*"
  # Significancia al 5%
  significancia[j_statistic <= qnorm(0.05/2) | j_statistic >= qnorm(1- 0.05/2)] <- "**"
  # Significancia al 1%
  significancia[j_statistic <= qnorm(0.01/2) | j_statistic >= qnorm(1- 0.01/2)] <- "***"
  
  resultado <- cbind(j_statistic, significancia)
  colnames(resultado) <- c("Estadistico","Significancia")
  return(resultado)
}

#---------------------------------------------------------------------------------------#

#---------------------------------- 30. reducir.eventos  ------------------------------------#
# Filtrar una base de eventos para que los eventos no se traslapen. La funcion en primer lugar genera una ventana de 
# estimacion por cada evento con el fin de que no hayan otros eventos dentro de la ventana. Tambien busca elegir los 
# eventos mas significativos segun el valor que tenga cada desastre en una columna ingresada por el usuario.
# Por ultimo es posible hacer el procedimiento para cada pais por separado, no necesariamente para todos
# los eventos al tiempo, es decir, desagrupar los eventos.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- umbral     : numero que indica la longitud de la ventana libre de eventos 
#-- base       : base que sera utilizada para la estimacion en <estimation.event.study>
#-- eventos    : matriz de eventos
#-- col.fecha  : nombre de la columna de fechas de eventos
#-- col.grupo  : nombre de la columna con la cual se va a desagrupar los eventos
#-- col.filtro : nombre de la columna con la cual se quiere filtrar los eventos significativos
# ----Argumentos de salida  ----#
#-- data.droped.events : dataframe de eventos filtrados. En este df ya no estan los eventos que no cuentan con una 
#                        ventana minima de estimacion ni con una ventana minima de evento
#---------------------------------------------------------------------------------------#
reducir.eventos <- function(umbral, base, eventos, col.fecha, col.grupo, col.filtro){
  # Encontrar el maximo de eventos que debe haber por cada valor en <col.grupo>. generalmente paises
  # Lo primero es buscar el indice de <base> correspondiente al evento mas antiguo y el del evento mas 
  # reciente. Si sucedio en un dia no habil, el indice es el primer dia habil posterior al evento
  for(j in 0:nrow(base)){
    index.max <- which(index(base)==(max(eventos[,col.fecha])+j))
    if(length(index.max)==1) break
  }
  for(j in 0:nrow(base)){
    index.min <- which(index(base)==(min(eventos[,col.fecha])+j))
    if(length(index.min)==1) break
  }
  # El maximo de eventos por pais es <index.max>-<index.min> / <umbral>, para asegurar que no haya traslapes
  # entre los eventos
  maximo.eventos.por.pais <- floor((index.max-index.min)/umbral)
  
  # Separar la base de datos <eventos> por cada valor en <col.grupo>, generando asi una lista de bases de 
  # datos. 
  eventos.separado <- eventos %>% 
    split(.[col.grupo])
  
  # Para cada base en <eventos.separado>, eliminar NA de <col.filtro> y ordenar de mayor a menor la columna
  # <col.filtro>
  eventos.separado <- lapply(eventos.separado, function(df) {
    df <- df[!is.na(df[[col.filtro]]), ]
    df <- df[order(df[[col.filtro]], decreasing = TRUE), ]
    return(df)
  })
  
  # Guardar solamente los df que contegan al menos un evento
  eventos.separado <- purrr::keep(eventos.separado, ~ nrow(.) > 0)
  
  # Encontrar los indices de los eventos de cada base de <eventos.separado>
  for(k in seq_along(eventos.separado)){
    indices <- integer()
    for(i in seq_along(eventos.separado[[k]][,col.fecha])){
      date <- as.Date(eventos.separado[[k]][i,col.fecha])
      j    <- 0
      while(T){
        indice <- which(index(base)==(date+j))
        if(length(indice)==1) break
        j <- j +1
      }
      indices[i] <- indice
    }
    # Colocar los indices en la base de datos original
    new_df <- data.frame(indices = indices)
    eventos.separado[[k]] <- cbind(eventos.separado[[k]], new_df)
  }
  
  # Conservar <maximo.eventos.por.pais> eventos. El que genera mas afectados siempre se conserva
  for(i in seq_along(eventos.separado)){
    indice.eventos <- integer()
    filas          <- c(1)
    indice.eventos[1] <- eventos.separado[[i]][1,'indices']
    
    # En orden del numero de afectados, revisar evento por evento si se encuentra entre los umbral dias
    # de algun evento incluido en <indice.eventos>. En caso de no estar, agregar su indice a <indice.eventos>
    if(nrow(eventos.separado[[i]]) > 1){
      for(k in 2:nrow(eventos.separado[[i]])){
        # El siguiente codigo verifica si el indice del evento esta dentro del umbral de algun otro
        # evento ya incluido en <indice.eventos>
        dentro.de.rango <- any(abs(indice.eventos - eventos.separado[[i]][k,'indices']) <= umbral)
        if(!dentro.de.rango){
          indice.eventos <- c(indice.eventos,eventos.separado[[i]][k,'indices'])
          filas          <- c(filas,k)  
        }
      }
    }
    # Por ultimo, asegurar que el vector <indice.eventos> tenga menor longitud que <maximo.eventos.por.pais>,
    # de lo contrario solo conservar los primeros <maximo.eventos.por.pais> eventos
    eventos.separado[[i]] <- eventos.separado[[i]][filas,]
    
    if(length(indice.eventos)>maximo.eventos.por.pais) 
      eventos.separado[[i]] <- eventos.separado[[i]] %>% top_n(col.filtro,maximo.eventos.por.pais)
  }
  eventos.reducidos <- bind_rows(eventos.separado)
  return(eventos.reducidos)
}

#---------------------------------- 31. grafico_car  ------------------------------------#
# Graficar los retornos anormales promedio acumulados (ACAR) relativos al dia de evento.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- events.list      : lista prveniente de la funcion <estimation.event.study>
#-- es.window.length : longitud ventana estimacion
#-- ev.window.length : longitud ventana evento 
# ----Argumentos de salida  ----#
#-- NA
#---------------------------------------------------------------------------------------#
grafico_car <- function(events.list, es.window.length, ev.window.length,ev.window.begin){
  # Detener la funcion si hay algun elemento que no tenga la clase 'ESmean'
  if (any(!sapply(events.list, inherits, "ESmean"))) {
    stop('La lista contiene elementos que no fueron creados con la funcion estimation.event.study.')
  }
  
  # Obtener los retornos anormales para todos los eventos en <events.list> y para la <ev.window.length> deseada
  anormales.evento <- data.frame(purrr::map(events.list, ~ .x@retornos$Abnormal[(es.window.length + 1+ev.window.begin):(es.window.length + ev.window.length)]))
  # Obtener el retorno anormal promedio AAR
  aar <- rowMeans(anormales.evento)
  # Obtener el retorno anormal acumulado promedio (ACAR) para cada dia de la ventana de evento
  caar <- cumsum(aar)
  names(caar) <- 0:(length(caar)-1)
  escala.grafico <- max(abs(min(caar)),abs(max(caar)))
  # Graficar el CAAR relativo al dia de evento
  plot(x=names(caar),y=caar,type='l',col='red',lwd=1.7,ylim=c((-1*escala.grafico),escala.grafico),
       main='CAAR relativo al dia del evento',ylab='CAAR',xlab='t')
  abline(h = 0, col = "black", lty=2,lwd = 1.7)
}

#---------------------------------- 31. grafico_cav  ------------------------------------#
# Graficar la volatilidad anormal acumulada relativa al dia de evento.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- events.list      : lista prveniente de la funcion <volatility_event_study>
#-- es.window.length : longitud ventana estimacion
#-- ev.window.length : longitud ventana evento 
# ----Argumentos de salida  ----#
#-- NA
#---------------------------------------------------------------------------------------#
grafico_cav <- function(events.list, es.window.length,ev.window.length,serie.rm){
  # Detener la funcion si hay algun elemento que no tenga la clase 'ESmean'
  if (any(!sapply(events.list, inherits, "ESVolatility"))) {
    stop('La lista contiene elementos que no fueron creados con la funcion estimation.event.study.')
  }
  
  # Calculo CAV
  epsilon      <- data.frame(purrr::map(events.list, ~ coredata(.x@residuales_evento)))[1:ev.window.length,]
  sigma_cuad   <- data.frame(purrr::map(events.list, ~ coredata(.x@variance_forecast)))[1:ev.window.length,]
  
  Mt           <- mt_function(epsilon,sigma_cuad)
  cav.relativo <- cumsum(Mt) #- (1:length(Mt))
  names(cav.relativo) <- 0:(length(Mt)-1)
  
  # Graficar el CAAV relativo al dia de evento
  plot(x=names(cav.relativo),y=cav.relativo,type='l',col='red',lwd=1.7,
       main=paste0('CAV relativo al dia del evento. Para ',serie.rm),ylab='CAV',xlab='t')
  abline(a = 0, b = 1, col = "black",lty=2,lwd=1.7)
  abline(h = 0, col = "black", lty=2,lwd = 1.7)
  legend("topleft", legend = c("Under Null Hypothesis", "Observed Volatility"),
         col = c("black", "red"), lty = c(2, 1),bty='n')

}

#---------------------------------- 32. grafico_cav_agregado  ------------------------------------#
# Graficar la volatilidad anormal acumulada relativa al dia de evento para distintos tipos de desastre en
# la misma grafica
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- aggregated.events.list      : lista prveniente de la funcion <volatility_event_study>, que contiene todos los desastres
#-- disagg.events.list          : lista compuesta por listas provenientes de la funcion <volatility_event_study>, que contiene cada una
#                                 los desastres de un tipo específico ( geofisico, hidrologico o meteorologico)
#-- es.window.length : longitud ventana estimacion
#-- ev.window.length : longitud ventana evento 
# ----Argumentos de salida  ----#
#-- NA
#---------------------------------------------------------------------------------------#
grafico_cav_agregado <- function(aggregated.events.list, disagg.events.list, es.window.length,ev.window.length,serie.rm, significancia = 0.05){
  # Detener la funcion si hay algun elemento que no tenga la clase 'ESVolatility' en <aggregated.events.list>
  if (any(!sapply(aggregated.events.list, inherits, "ESVolatility"))) stop('La lista contiene elementos que no fueron creados con la funcion estimation.event.study.')
  # Detener la funcion si dentro de cualquier elemento de <disagg.events.list> hay algun elemento que no sea "ESVolatility"
  stop_flag <- lapply(disagg.events.list, function(x) {
    any(!sapply(x, inherits, "ESVolatility"))
  })
  if (any(unlist(stop_flag))) {
    stop('La lista contiene elementos que no fueron creados con la funcion estimation.event.study.')
  }
  
  # Generar una sola lista, que incluira las listas separadas por tipo de desastre y la lista que contiene todos los desastres
  full.list                        <- disagg.events.list 
  full.list[[length(full.list)+1]] <- aggregated.events.list
  names(full.list)                 <- c(names(disagg.events.list),'Todos')
  
  # Calculo CAV para cada elemento de <full.list> junto a intervalo de confianza de la distribucion
  cavs.relativos           <- list()
  sup.intervalos.confianza <- list()
  for(k in seq_along(full.list)) {
    epsilon      <- data.frame(purrr::map(full.list[[k]], ~ coredata(.x@residuales_evento)))[1:ev.window.length,]
    sigma_cuad   <- data.frame(purrr::map(full.list[[k]], ~ coredata(.x@variance_forecast)))[1:ev.window.length,]
    Mt           <- mt_function(epsilon,sigma_cuad)
    cav.relativo <- cumsum(Mt) #- (1:length(Mt))
    names(cav.relativo) <- 0:(length(Mt)-1)
  
    # Para comprender mejor la distribucion ver Mnasri y Nechi (2016), quienes expresan que \sum_{t=n_1}^{n_2} Mt(N-1) sigue una chi
    # cuadrado con (N-1)(n_2-n_1+1) grados de libertad
    # Primero es necesario obtener N, que es el numero de desastres
    N               <- length(full.list[[k]])
    degrees.freedom <- (N-1)*(1:length(cav.relativo)) # Grados de libertad para cada ventana de evento segun Mnasri y Nechi
    # Generamos el pvalue a un nivel de significancia <significancia>, recordando que el test es de cola derecha
    sup.intervalo.confianza        <- (qchisq((1- significancia), degrees.freedom))/(N-1)
    names(sup.intervalo.confianza) <- names(cav.relativo)
    
    sup.intervalos.confianza[[k]] <- sup.intervalo.confianza
    cavs.relativos[[k]]           <- cav.relativo
  }
  names(cavs.relativos)           <- names(full.list)
  names(sup.intervalos.confianza) <- names(full.list)
  
  # Sin embargo, para las graficas queremos que la lista tenga de primer lugar a 'Todos' y luego si el resto, para poder graficar
  # <Todos> con mas grosor
  ordered.names            <- c('Todos',names(disagg.events.list))
  cavs.relativos           <- cavs.relativos[ordered.names]
  sup.intervalos.confianza <- sup.intervalos.confianza[ordered.names]
  
  # Graficar el CAAV relativo al dia de evento
  # Por tema de escala, lo mejor es buscar el maximo de los maximos de <cavs.relativos>
  maximo.escalay <- max(unlist(lapply(cavs.relativos, max)))
  
  # Definir los colores
  colors <- c('#000000',brewer.pal(n=(length(cavs.relativos)-1), name='Set1'))
  plot(x=names(cavs.relativos[[1]]),y=cavs.relativos[[1]],type='l',col=colors[1],lwd=3,
       main=paste0('CAV relativo al dia del evento. Para ',serie.rm),ylab='CAV',xlab='t',
       ylim = c(0,maximo.escalay))
  if(length(cavs.relativos)>1) for(p in 2:length(cavs.relativos)){
    lines(x = names(cavs.relativos[[1]]), cavs.relativos[[p]],type='l',col=colors[[p]],lwd=2)
  }
  
  # Rellenar la zona de los intervalos de confianza
  shading.alpha = 0.15 #<<<--- parametro para hacer mas transparente las areas de IC
  for(m in seq_along(sup.intervalos.confianza)){
    polygon(x = c(names(cavs.relativos[[1]]), rev(names(cavs.relativos[[1]]))),
            y = c(sup.intervalos.confianza[[m]], rev((0:(length(cavs.relativos[[1]])-1)))),
            col = adjustcolor(colors[m], alpha.f = shading.alpha), lty=2)
  }
  
  # anadir rectas del intervalo de confianza con un nivel de <significancia>
  for(l in seq_along(sup.intervalos.confianza)){
    lines(x = names(cavs.relativos[[1]]), sup.intervalos.confianza[[l]],type='l',lty = 2,col=colors[[l]],lwd=1)
  }
  
  # Anadir la recta de la hipotesis nula
  abline(a = 0, b = 1, col = "black",lty=2,lwd=1.5)
  # legend("topleft", legend = c("Under Null Hypothesis (No Effect)", paste0("Observed Volatility: ",names(cavs.relativos))),
  #        col = c("black", colors), lty = c(2,rep(1,length(cavs.relativos))),bty='n',cex = 0.9, pt.cex = 0.9,
  #        y.intersp = 1)
  legend("topleft", 
         legend    = c("Under Null Hypothesis (No Effect)", paste0("Observed Volatility: ",names(cavs.relativos)),
                               paste0((1 - significancia)*100,'% C.I: ', names(cavs.relativos))),
         col       = c("black", colors, rep(NA,length(cavs.relativos))), 
         lty       = c(2,rep(1,length(cavs.relativos)),rep(NA,length(cavs.relativos))),
         fill      = c(rep(NA,(1+length(cavs.relativos))),adjustcolor(colors, alpha.f = shading.alpha)), 
         border    = c(rep(NA,(1+length(cavs.relativos))),colors),
         pch       = c(rep(NA,(1+length(cavs.relativos))),rep(15,length(cavs.relativos))),
         bty       ='n', 
         y.intersp = 1)
  
}

# Revision de la funcion bootstrap.volatility ------------------------------
bootstrap.volatility2 <- function(volatility.list,es.window.length,ev.window.length,bootstrap_vol_iterations){ 
  # Detener la funcion si hay algun elemento que no tenga la clase 'ESmean'
  if (any(!sapply(volatility.list, inherits, "ESVolatility"))) {
    stop('La lista contiene elementos que no fueron creados con la funcion estimation.event.study.')
  }
  
  # Calculo Mt y CAV --------------------------------------------------------
  # El calculo de tanto Mt como CAV sale de Bourdeau (2017)
  # Por simplicidad de calculos, guardar los residuales observados (epsilon) y los pronosticos de la varianza condicional
  epsilon      <- data.frame(purrr::map(volatility.list, ~ coredata(.x@residuales_evento)))[1:ev.window.length,]
  sigma_cuad   <- data.frame(purrr::map(volatility.list, ~ coredata(.x@variance_forecast)))[1:ev.window.length,]
  
  # <if(0)> porque el codigo fue mejorado en el siguiente  bloque de codigo
  if(0){
    Mt <- c()
    for(i in 1:ev.window.length){
      termino_sumatoria <- c()
      for(j in 1:length(volatility.list)){
        numerador         <- (length(volatility.list)*epsilon[i,j]-sum(epsilon[i,]))^2
        denominador       <- (length(volatility.list))*(length(volatility.list)-2)*sigma_cuad[i,j]+sum(sigma_cuad[i,])
        termino_sumatoria <- c(termino_sumatoria, numerador/denominador)
      }
      Mt[i] <- (1/(length(volatility.list)-1))*sum(termino_sumatoria)
    }
  }
  
  # Es el mismo codigo anterior pero con operaciones vectorizadas (chatGPT). Si generan los mismos resultados
  # El proceso se encuentra en la funcion <mt_function>
  Mt <- mt_function(epsilon,sigma_cuad)
  
  # La volatilidad anormal acumulada (CAV) esta definida como
  cav <- sum(Mt) - length(Mt)
  estadistico.bialkowski <- sum(Mt)*(length(volatility.list)-1)
  pvalue.bialkowski      <- pchisq(estadistico.bialkowski,
                                   df = ((length(volatility.list)-1)*(ev.window.length+1)),lower.tail = F)
  
  # Bootstrap Volatility ----------------------------------------------------
  
  # El siguiente procedimiento de bootstrap sigue la formulacion desarrollada por Mnasri y Nechi (2016)
  # Generar matriz kxN (<es.window.length>x<length(volatility.list>) de los residuales de las ecuaciones GARCH
  
  E_matrix <- matrix(nrow=es.window.length,ncol=length(volatility.list)) 
  for(i in 1:length(volatility.list)) E_matrix[,i] <- coredata(volatility.list[[i]]@res_estandar_estimacion)
  
  # Reescalar la matriz para que cada columna tenga media 0 y varianza 1
  E_matrix <- scale(E_matrix)
  
  # Generar un array de <lenght(volatility.list)> matrices, cada una con tamaño 
  # (<es.window.length>x<ev.window.length>)
  residual_array <- array(dim=c(es.window.length,ev.window.length,length(volatility.list)))
  
  # Multiplicar cada columna de la matriz <E_matrix> por el vector de desviacion estandar condicional estimada
  for(i in 1:length(volatility.list)){
    forecasted_sd       <- sqrt(coredata(volatility.list[[i]]@variance_forecast))[1:ev.window.length]
    residual_array[,,i] <- E_matrix[,i] %*% t(forecasted_sd)
  }
  # El codigo anterior genera un array de dimensiones <es.window.length>x<ev.window.length>x<length_volatility.list>
  # La primera dimension indica el numero de filas de cada matriz, siendo igual a la longitud de la ventana de estimacion
  # La segunda dimension indica el numero de columnas de cada matriz, siendo igual a la longitud de la ventana de evento
  # La tercera dimension indica el numero de matrices en el array, siendo igual al numero de eventos
  
  # Realizar el bootstrap 
  cav_empiric_vector <-c()
  set.seed(15)
  for(b in 1:bootstrap_vol_iterations){
    # Seleccionar <length(volatility.list)> numeros aleatorios (no pueden estar dentro de los ultimos <ev.window.length> dias)
    random_nums <- sample((1:(es.window.length- ev.window.length+1)),length(volatility.list),replace = T)
    # Seleccionar la <random_num>-esima fila de cada matriz dentro del array y organizarlas en una matriz para calcular Mt
    epsilon_boot <- matrix(NA,nrow=ev.window.length,ncol=length(volatility.list))
    for(k in 1:length(volatility.list)){
      for(u in 0:(nrow(epsilon_boot)-1)) epsilon_boot[(u+1),k] <- residual_array[(random_nums[k]+u),(u+1),k]
    }
    Mt_boot  <- mt_function(epsilon_boot,sigma_cuad)
    cav_boot <- sum(Mt_boot) - length(Mt_boot)
    cav_empiric_vector <- c(cav_empiric_vector,cav_boot)
  }
  
  # El p-value esta definido por Bourdeau (2017), quien dice que es la proporcion de valores que son mayores al
  # cav calculado originalmente
  
  pvalue_bootstrap <- sum(cav_empiric_vector > cav)/bootstrap_vol_iterations
  significancia    <- ''
  if(pvalue_bootstrap<=0.1) significancia <- '*'
  if(pvalue_bootstrap<=0.05) significancia <- '**'
  if(pvalue_bootstrap<=0.01) significancia <- '***'
  
  significancia.bialkowski <- '/ '
  if(pvalue.bialkowski<=0.1) significancia.bialkowski <- '/ *'
  if(pvalue.bialkowski<=0.05) significancia.bialkowski <- '/ **'
  if(pvalue.bialkowski<=0.01) significancia.bialkowski <- '/ ***'
  
  resultado        <- data.frame('CAV'=round(cav,3),'Significancia' = significancia,'p_value'=pvalue_bootstrap,
                                 'Significanciabialkowski' = significancia.bialkowski)
  return(resultado)
}

#---------------------------------- 32. kernel.cav  ------------------------------------#
# Funcion que genera el kernel de la densidad para todos los tipos de desastres y tambien para la totalidad
# de desastres. Es necesaria una lista agregada, y otra desagregada a un nivel de tipo.desastre-pais
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- tipos.de.desastres : vector que indicara el nombre de los tipos de desastres a tener en cuenta
#-- lista.desagregada  : lista de estimaciones del modelo arima-garch desagregada a nivel tipo.desastre-pais, 
#                        que incluye varias listas de elementos "ESVolatility"
#-- lista.agregada     : lista agregada de todos los desastres a tener en cuenta
#-- columna.pais       : string que indica el nombre de la columna de paises. Por construccion del elemento "ESVolatility" 
#                        se tomara el default como 'Country'
#-- ev.window.length   : tamano maximo ventana de evento
#-- estimation.window  : tamano de la ventana de estimacion
#-- overlap.window     : tamano ventana traslape
#-- series             : serie a graficar
#-- market.variable    : variable de retorno de mercado
# ----Argumentos de salida  ----#
#-- 
#---------------------------------------------------------------------------------------#
kernel.cav <- function(tipos.de.desastres, lista.desagregada, lista.agregada,columna.pais = 'Country',
                       estimation.window, overlap.window, series, market.variable) {
  
  setClass("ESVolatility",slots=list(coefficients = "numeric",goodness_of_fit = "numeric",res_estandar_estimacion="xts",
                                     res_no_estandar_estimacion="xts",variance_forecast="xts",residuales_evento="xts",
                                     info.evento = 'data.frame'))
  
  # Primero vamos a generar una lista, donde cada elemento va a contener listas de los desastres ocurridos en cada pais
  # y tambien en todos los paises
  lista.listas        <- list()
  for(l in seq_along(tipos.de.desastres)){
    lista.listas[[l]] <- lista.desagregada[grepl(tipos.de.desastres[l],names(lista.desagregada))]
  }
  lista.listas[[(length(lista.listas)+1)]] <- split(lista.agregada, 
                                                    sapply(lista.agregada, function(x) x@info.evento[[columna.pais]]))
  names(lista.listas) <- c(tipos.de.desastres,'Todos')
  
  # Ahora toca encontrar la volatilidad anormal acumulada para cada lista del objeto <lista.listas>, pero antes se debe calcular el numero
  # maximo de la ventana de evento. Los objetos mas desagregados en lista.listas son de clase <ESVolatility>, los cuales cuentan con un slot llamado
  # volatility_forecast, que tiene la longitud del tamano maximo de la ventana de evento. 
  # Por tanto, toca usar <lapply> para acceder a los objetos 'ESVolatility' y poder obtener la ventana de evento que se encuentra en todos los desastres
  tamano.ventanas.evento <- lapply(lista.listas, function(x){
    object <- lapply(x, function(vol.obj){
      lista <- purrr::map(vol.obj, ~.x@variance_forecast)
      tamano.ventana.evento <- unique(unlist(lapply(lista, length)))
      return(tamano.ventana.evento)
    })
    return(unique(unlist(object)))
  })
  
  ev.window.length <- unique(unlist(tamano.ventanas.evento))
  
  cavs.relativos <- list()
  for(p in seq_along(lista.listas)){
    cavs.relativos[[p]] <- lapply(lista.listas[[p]], function(x){
      epsilon      <- data.frame(purrr::map(x, ~ coredata(.x@residuales_evento)))[1:ev.window.length,]
      sigma_cuad   <- data.frame(purrr::map(x, ~ coredata(.x@variance_forecast)))[1:ev.window.length,]
      Mt           <- mt_function(epsilon,sigma_cuad)
      cav.relativo <- cumsum(Mt)
      names(cav.relativo) <- 0:(length(Mt)-1)
      return(cav.relativo)
    })
  }
  names(cavs.relativos) <- names(lista.listas)
  # <cavs.relativos> es una lista de vectores. Cada vector es de tamaño <ev.window.length> y mide la volatilidad anormal
  # acumulada para cada dia en la ventana de evento. Por ejemplo, el primer elemento de cada vector es la volatilidad anormal
  # del dia del evento, el segundo elemento de cada vector es la volatilidad anormal acumulada para el dia del evento y el dia 
  # siguiente ...
  
  # Para poder calcular el kernel para un dia determinado de la ventana de evento, se debe crear un objeto que agrupe el CAV asociado
  # a un dia especifico para todos los paises.  Por ejemplo, si hubo eventos geofisicos en brazil, colombia y china, y se quiere revisar el efecto
  # de una ventana desde el dia del evento a 4 dias despues, se debe crear un vector del CAV de brazil, colombia y china, para esa ventana de evento
  # Por otro lado, como tenemos multiples ventanas de evento, desde 1 dia de duracion hasta <ev.window.length> dias de duracion, en vez de crear varios 
  # vectores, podemos crear una matriz para cada tipo de desastre, donde la fila signifique la longitud de la ventana, y la columna indique el pais
  cavs.ventana.evento <- lapply(cavs.relativos, function(x){
    ventanas.evento <- names(x[[1]])
    return.element  <- matrix(as.numeric(unlist(purrr::map_df(x,~.x[ventanas.evento]))),
                              nrow=length(ventanas.evento),ncol=length(x),byrow=T)
    return(return.element)
  })
  
  # Ya con las matrices dentro de <cavs.ventana.evento>, se puede hallar el kernel de los CAV, teniendo en cuenta que cada fila indica una longitud
  # distinta de la ventana de evento, y seran los indicados para los kernels

  # Hallamos el kernel para todas las longitudes de ventana de evento
  for(ventana.evento in 1:ev.window.length){
    # Hallar el ancho de banda optimo para cada elemento de <cavs.ventana.evento>
    anchos.optimos <- lapply(cavs.ventana.evento, function(x){
      ancho.banda <- 1
      while(T){
        # Encontrar una sola moda siguiendo el criterio de la segunda derivada, la cual tiene que pasar de positivo a negativo
        # una sola vez
        if(length(which(abs(diff(sign(diff(density(x[ventana.evento,],bw=ancho.banda)$y)))) == 2)) == 1){
          break
        }
        ancho.banda <- ancho.banda + 1
      }
      return(ancho.banda)
    })
    
    # Sacar las densidades kernel con el ancho de banda optimo para cada elemento en <cavs.ventana.evento>
    densidades <- list()
    for(y in seq_along(cavs.ventana.evento)) densidades[[y]] <- density(cavs.ventana.evento[[y]][ventana.evento,], bw = anchos.optimos[[y]])
    names(densidades) <- names(cavs.ventana.evento)
    
    # Establecer los limites de plot de acuerdo con las densidades
    maximo.x <- max(unlist(lapply(densidades, function(x) max(x$x))))
    minimo.x <- min(unlist(lapply(densidades, function(x) min(x$x))))
    maximo.y <- max(unlist(lapply(densidades, function(x) max(x$y))))
    minimo.y <- min(unlist(lapply(densidades, function(x) min(x$y))))
  
    # Colores para graficar
    # Se determinan los colores para graficar
    colors <- brewer.pal(n=length(densidades), name='Set1')
    png(filename=paste0(cd.kernel.cav,tipo.serie,'_',market,'_CAV_Est_',ventana.estimacion,'_tra_',ventana.traslape,'_t',ventana.evento,'.png'),
        width = 800,height = 800)
    plot(densidades[[1]], col=colors[1],lwd=2,
         main = paste0('Cumulative Abnormal Volatility (CAV) Kernel relative to the disaster date. For CDS. Event window: [0,',(ventana.evento-1),']'),
         xlim = c(minimo.x, maximo.x), ylim = c(minimo.y,maximo.y), xlab = 'Cumulative Abnormal Return (CAV)')
    # Agregar los demas plots
    if(length(densidades)>1) for(k in 2:length(densidades)){
      lines(densidades[[k]], col= colors[k],lwd=2)
    }
    abline(v=ventana.evento,lty=2)
    
    legend("topright", 
           legend    = c("Under Null Hypothesis (No Effect on Volatility)", paste0("Kernel CAV: ",c('Hydrological','Meteorological','Geophysical','All'))),
           col       = c("black",colors), 
           lty       = c(2,rep(1,length(cavs.relativos))),bty = 'n')
    title(paste0('Estimation window: ',estimation.window,' days. Overlap window: ',overlap.window,' days. For ',series, ' with ', market.variable),line=0.75)
    dev.off()
  }
}
