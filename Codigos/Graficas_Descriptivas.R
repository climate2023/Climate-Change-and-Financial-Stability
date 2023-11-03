############# FALTA ARREGLAR EL CODIGO PARA QUE GRAFIQUE CDS / INDICES MSCI /INDICES PM
##########################################################
# Codigo de las graficas de resultados de SUR y de los tests. Tambien hay graficos de mapamundis
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
  # Cargamos la base de datos que salio del codigo <emdat_base>
  load(paste0(getwd(),'/Bases/EMDAT_PAPER.RData'))
  # Cambiar el nombre de <Korea> a <South Korea>
  emdat_base[which(emdat_base$Country == 'Korea'),]$Country <- 'South Korea'
}

# Este procedimiento solo se realiza con la base de datos que utiliza Pagnottoni, ya que la del paper ya fue tratada para corregir los eventos
# en los que no se tiene datos de inicio del mes o del dia
if(!bool_paper){
  #Se eliminan las filas (desastres) que contengan <NA> en el mes de inicio, ya que no se puede suponer el mes en que empezo el desastre.
  emdat_final <- emdat_interest_variables %>% 
    dplyr::filter(!is.na(Start.Month)) %>% 
    # Tambien se van a eliminar los desastres sin dia de inicio exacto
    dplyr::filter(!is.na(Start.Day))
}else{
  emdat_final <- emdat_base %>% dplyr::rename('region' = 'Country')
}

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
if(!bool_paper){
  diff.world <- c("Bahamas","Bolivia", "Cape Verde", "Canary Islands","Cayman Islands", "Comoros", 
                "Democratic Republic of the Congo","Republic of Congo", "Cook Islands", "Ivory Coast", "Czech Republic",
                "Dominican Republic","Swaziland", "Gambia", "Iran", "North Korea" ,"South Korea", "Laos", "North Macedonia",
                "Marshall Islands", "Micronesia", "Moldova","Netherlands", "Niger" , "Northern Mariana Islands" ,"Palestine",
                "Philippines","Reunion","Russia" ,"Saint Barthelemy","Saint Helena", "Saint Kitts","Saint Martin" ,"Saint Vincent",
                "Sint Maarten","Sudan","Syria", "Taiwan","Tanzania", "Turks and Caicos Islands","United Arab Emirates",
                "UK","USA","Venezuela","Vietnam")
}else{diff.world <- c('South Africa')}

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

# Generar los deciles
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

#### Graficas 1, A.1, A.6 y A.7 (mapamundi) ============================

# Tabla explicativa de los desastres en la muestra

summary.table <- emdat_final %>% 
  group_by(region,Disaster.Subgroup,Disaster.Type) %>% 
  dplyr::summarize(Count = n()) %>% 
  pivot_wider(names_from = Disaster.Type,values_from=Count,values_fill = 0) %>% 
  adorn_totals('row') %>%  # Obtener fila de totales
  adorn_totals('col') # Obtener columna de totales 

summary.table.latex <- xtable(summary.table,include.rownames=F); summary.table.latex # Obtener la tabla en formato LateX

# Tabla con los subtipos de desastres
dataframe.eventos <- emdat_base #<<<--- dataframe de eventos a los que se quiere sacar la tabla descriptiva.
# <emdat_interest_variables> es la base con todos los eventos y <emdat_base> es la base despues 
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

mapamundi.complete <- ggplot() +
  geom_polygon(data = merged_data, aes(x = long, y = lat, group = group, fill = breaks), color = "gray")  +
  scale_fill_manual(values = palette, name = 'Frequency') +
  coord_fixed(1.2) +
  ggtitle("The Geography of Natural Disasters")+
  plain + 
  geom_path(data = world, aes(x = long, y = lat, group = group), 
            color = "black", linewidth = 0.5)

# Se guarda el mapamundi en la carpeta  
ggsave(filename = paste0(cd.graficos, 'Descriptive/mapamundi_completo.png'), plot = mapamundi.complete, scale = 2, height = 3.5, 
       width = 6)

# Para diversos grupos tipos de desastre
# Paleta de colores
palette2 <- brewer.pal(grupos, "RdPu")

plots <- lapply(names(merged_data_disasters), function(name) {
  if(name == 'Biological') return(NULL) # Skip the 'Biological' category
  if(name == 'Climatological') return(NULL) # Skip the 'Climatological' category
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
    scale_fill_manual(values = palette2, name = 'Frequency') +
    coord_fixed(1.2) +
    ggtitle(paste("The Geography of ",name,' Disasters'))+
    plain + 
    geom_path(data = world, aes(x = long, y = lat, group = group), 
              color = "black", linewidth = 0.5)
})
names(plots) <- names(merged_data_disasters)
plots <- Filter(Negate(is.null), plots) # Eliminar el objeto <NULL>

# Guardar los objetos de <plots> en archivos separados
lapply(names(plots), function(x){
  ggsave(filename = paste0(cd.graficos, 'Descriptive/mapamundi_',x,'.png'), plot = plots[[x]], scale = 2, height = 3.5, 
         width = 6)
})

# Crear una grilla de los mapamundis por tipo desastre --------------------
# Combinar los mapamundis por tipo de desastre en un solo grafico
grilla.mapamundis <-gridExtra::grid.arrange(grobs = plots, ncol = 2)
# Guardar la grilla en formato png
ggsave(filename = paste0(cd.graficos, 'Descriptive/grilla_mapamundis.png'), plot = grilla.mapamundis, scale = 2, height = 3.5, 
       width = 7)

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

