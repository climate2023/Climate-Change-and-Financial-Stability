# El siguiente codigo reemplaza las lineas 3652 a 3687 de funtions_Climate_Change

# Colocar los intervalos y los CAV en escala y = 0
sup.intervalos.confianza <- lapply(sup.intervalos.confianza, function(x) x - 0:(vol_ev_window-1))
cavs.relativos           <- lapply(cavs.relativos, function(x) x - 0:(vol_ev_window-1))

# Graficar el CAAV relativo al dia de evento
# Por tema de escala, lo mejor es buscar el maximo de los maximos de <cavs.relativos>
maximo.escalay <- max(unlist(lapply(cavs.relativos, max))) + 0.5
minimo.escalay <- 0

# Definir los colores
colors <- c('#000000',brewer.pal(n=(length(cavs.relativos)-1), name='Set1'))
# Cambiar el idioma de <rm>
rm.english <- ifelse(rm == 'Promedio Movil', 'Moving Average', rm)
rm.english <- ifelse(serie == 'Cds', '', paste0(' with ', rm.english))
serie.cap  <- ifelse(serie == 'Cds', 'CDS', serie) 
serie.cap  <- ifelse(serie.cap == 'Indices', 'Stock Indexes', serie.cap) 
plot(x=names(cavs.relativos[[1]]),y=cavs.relativos[[1]],type='l',col=colors[1],lwd=3,
     main=paste0('Cumulative Abnormal Volatility (CAV) relative to the disaster date. \nFor ',
                 serie.cap, rm.english,'. \n',extra.title),
     ylab='Cumulative Abnormal Volatility (CAV)',xlab='Day relative to the disaster date',
     ylim = c(minimo.escalay,maximo.escalay), xaxs='i', yaxs='i')
if(length(cavs.relativos)>1) for(p in 2:length(cavs.relativos)){
  lines(x = names(cavs.relativos[[1]]), cavs.relativos[[p]],type='l',col=colors[[p]],lwd=2)
}

# Rellenar la zona de los intervalos de confianza
shading.alpha = 0.15 #<<<--- parametro para hacer mas transparente las areas de IC
for(m in seq_along(sup.intervalos.confianza)){
  # polygon(x = c(names(cavs.relativos[[1]]), rev(names(cavs.relativos[[1]]))),
  #         y = c(sup.intervalos.confianza[[m]], rev((0:(length(cavs.relativos[[1]])-1)))),
  #         col = adjustcolor(colors[m], alpha.f = shading.alpha), border = F)
  polygon(x = c(names(cavs.relativos[[1]]), rev(names(cavs.relativos[[1]]))),
          y = c(sup.intervalos.confianza[[m]], rep(-0, length(cavs.relativos[[1]]))),
          col = adjustcolor(colors[m], alpha.f = shading.alpha), border = F)
}

# anadir rectas del intervalo de confianza con un nivel de <significancia>
for(l in seq_along(sup.intervalos.confianza)){
  lines(x = names(cavs.relativos[[1]]), sup.intervalos.confianza[[l]],type='l',lty = 2,col=colors[[l]],lwd=1)
}

# Anadir la recta de la hipotesis nula
abline(a = 1, b = 0, col = "black",lty=3,lwd=1.5)
