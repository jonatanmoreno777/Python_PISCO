#*****************************************************************************************#
#           CODIGO PARA LA EXTRACCIÓN DE DATOS DE PP,TMAX,TMIN,ETP A NIVEL DE ESTACION    #
#                                       Y CUENCA DE LA DATA PISCO-SENAMHI                 

#*****************************************************************************************#
# https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/overview.html?fbclid=

# Configurar directorio de trabajo
rm(list=ls())
setwd("D:/T_JONA/TESIS_PISCO/")
getwd()
library(pacman)#cargar el paquete
p_load(sp, sf, raster, rgdal, didyverse, 
       rasterVis,ncdf4, laticce, laticceExtra)

#  ----------------------------------------------------------------------------
#  LEER DATOS DE PISCO_ESTACION - PP,TMAX,TMIN,ETP DEL PERIODO 1981 - 2016-----
#  ----------------------------------------------------------------------------

## XX Longitud e YY Latitud
long_lat <- read.csv("Entrada/Longitud_latitud/long_lat.csv.csv", header = T)
long_lat <- read.csv("Entrada/long_lat - copia.csv", header = T)
### Ensamblamos los datos *.nc
raster_pp <- raster::brick("Entrada/Pisco_Pp/Precday.nc")
raster_Tmax <- raster::brick("Entrada/Pisco_Temp_day/tmax.nc")
raster_Tmin <- raster::brick("Entrada/Pisco_Temp_day/tmin.nc")
raster_Etpm <- raster::brick("Entrada/Pisco_Etp/Etpm.nc")
## Asignamos las coordenadas 
sp::coordinates(long_lat) <- ~XX+YY
# Igualamos las proyecciones del raster y de los puntos a extraer
raster::projection(long_lat) <- raster::projection(raster_pp)
raster::projection(long_lat) <- raster::projection(raster_Tmax )
raster::projection(long_lat) <- raster::projection(raster_Tmin)
raster::projection(long_lat) <- raster::projection(raster_Etpm)
# Extraemos los valores de pp
points_long_lat_pp <- raster::extract(raster_pp[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_pp <- t(raster_pp[points_long_lat_pp])
colnames(data_long_lat_pp ) <- as.character(long_lat$NN)
write.csv(data_long_lat_pp , "Salida/Pisco_ppdlstoB.csv", quote = F)
# Extraemos los valores de Tmax
points_long_lat_tmax <- raster::extract(raster_Tmax[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_tmax <- t(raster_Tmax[points_long_lat_tmax ])
colnames(data_long_lat_tmax ) <- as.character(long_lat$NN)
write.csv(data_long_lat_tmax , "Salida/Pisco_Tmax.csv", quote = F)
# Extraemos los valores de Tmin
points_long_lat_tmin <- raster::extract(raster_Tmin[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_tmin <- t(raster_Tmin[points_long_lat_tmin])
colnames(data_long_lat_tmin ) <- as.character(long_lat$NN)
write.csv(data_long_lat_tmin , "Salida/Pisco_Tmin.csv", quote = F)
# Extraemos los valores de Etpm
points_long_lat_Etpm <- raster::extract(raster_Etpm[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_Etpm <- t(raster_Etpm[points_long_lat_Etpm ])
colnames(data_long_lat_Etpm ) <- as.character(long_lat$NN)
write.csv(data_long_lat_Etpm , "Salida/Pisco_Etpm.csv", quote = F)

# Tmp_media
tmp_mean = (data_long_lat_tmax + data_long_lat_tmin)/2
write.csv(tmp_mean , "Salida/Pisco_tmp_mean.csv", quote = F)

#convertir datos de pp,tmax,tmin:
# de vertical en horizontal
datos <- read.csv("Salida/ESTA_VIRT_MONTH2.csv")
datos
dat=datos[,9]
mdat=t(matrix(dat,nrow = 12))
mdat
length(mdat)
colnames(mdat)
# cambiando los nombres de columnas y filas
colnames(mdat) <- c('ENE', 'FEB', 'MAR', 'ABRI','MAY', 'JUN', 'JUL', 'AGO','SEP', 'OCT', 'NOV', 'DIC')
rownames(mdat)<-1981:2016 
mdat
write.csv(mdat,"Salida/C_Cac_PM_ordenado5.csv")
huamanga <- read.csv("Salida/Pisco_ppM3.csv")


#  --------------------------------------------------------------------------
#  LEER DATOS DE PISCO_CUENCA - PP,TMAX,TMIN,ETP DEL PERIODO 1981 - 2016-----
#  --------------------------------------------------------------------------
Pisco.prec.brick <- brick("Entrada/Pisco_Pp/PISCOpm.nc")# leemos archivo netcdf con brick 
nlayers(Pisco.prec.brick) 
spplot(Pisco.prec.brick[[1:12]]) 
splom(Pisco.prec.brick[[1:12]])
#densityplot(Pisco.prec.brick[[1:12]])
#bwplot(Pisco.prec.brick[[1:12]])
# range(Pisco.prec.brick)
# extraer datos desde raster
# importar polygonos

library(rgdal)
cuenca.wgs <- readOGR(dsn=".", layer="cuen_ca18")
class(cuenca.wgs)# verificar la clase
str(cuenca.wgs)# verificar si tiene sistema de proyeccion
plot(cuenca.wgs, col="cyan", main="cuenca_cachi")
plot(cuenca.wgs,axes=T,asp=1,col="cyan", main="cuenca_cachi")

# Cambiando de sistema de coordenadas (reproyectando)
cuenca.wgs <- spTransform(cuenca.wgs, CRS("+proj=longlat +ellps=WGS84"))
plot(cuenca.wgs, axes=T, asp=1)
plot(cuenca.wgs,axes=T,asp=1,col="green", main="cuenca cachi")

# Estacones meteorologcas
estaciones<-readOGR(".","PONT_ESTAC") #estaciones metereol?gicas
summary(estaciones)
str(estaciones)

# sp.layout.
l1 = list("sp.points", estaciones, pch = 3, col="blue",zise=0.5)
spplot(cuenca.wgs,"ID", sp.layout = list(l1),
       axes=T,asp=1,col="green", main="cuenca cachi")
spplot(cuenca.wgs,sp.layout = list(l1), ylab="long",axes=T,
     xlab = "lat", main="Estacones Meteorologcas")

# Extraer los valores prec. promedio areal para las cuencas CACHI
pp.cuenca.mensual <- extract(Pisco.prec.brick, cuenca.wgs, fun=mean) #para extraer la data
pp.cuenca.mensual
row.names(pp.cuenca.mensual) <- cuenca.wgs@data$NOMB_UH_N4
View(pp.cuenca.mensual)
range(pp.cuenca.mensual)
plot(pp.cuenca.mensual[1,], type="o", col= "blue", ylim=c(0,215), ylab="Prec. [mm]",
     xlab = "Meses", main="Prec. prom areal - Cuenca Cachi [mm]")
legend(.18, 200, legend=c("Prec. prom areal - Cuenca Cachi [mm]"),
       col=c("blue"), lty=1:2, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')
#write.csv(t(pp.cuenca.mensual),'C_Cachi_PM_3.csv')

# Cortar raster para la cuenca en estudio
plot(Pisco.prec.brick[[1]]) 
plot(cuenca.wgs, add=T)
r.Cachi <- crop(Pisco.prec.brick[[1:12]], cuenca.wgs,snap="out") #dividir entre 10 grados??
plot(r.Cachi)
r.Cachi <- mask(r.Cachi, cuenca.wgs)
#plot(r.Cachi)
names(r.Cachi)
names(r.Cachi) <- month.abb 
plot(r.Cachi,main = "Precipitation")
projection(r.Cachi)

library(rasterVis)
#levelplot(r.Cachi,par.settings = RdBuTheme,contour=TRUE,under = TRUE,col = "grey50")
levelplot(r.Cachi, layers = 1, margin = list(FUN = 'median'), contour=TRUE)
histogram(r.Cachi,main = expression("Precipitation" ~ (mm ~ mes^{-1})))
levelplot(r.Cachi, margin = FALSE, 
          main = expression("Precipitation" ~ (mm ~ mes^{-1})),contour=TRUE)
densityplot(r.Cachi,xlab= "Temperatura m?nima [C?]", ylab="Densidad")
bwplot(r.Cachi)
histogram(r.Cachi, FUN = as.yearqtr)
vectorplot(r.Cachi) #vectorplot(r.Cachi, par.settings=RdBuTheme())

# Plot....
#ras_forcing=brick("Entrada/Pisco_Pp/Precday.nc")
#levelplot(crop(ras_forcing,cuenca.wgs,snap='out')[[1]],margin=list(draw=F),colorkey=list(space='right'),
          #par.settings=YlOrRdTheme(),main='prec_day')+layer(sp.polygons(cuenca.wgs))

# Plot utilizando paquete lattice y latticeExtra
library(lattice)
library(latticeExtra)
spplot(r.Cachi, col.regions = terrain.colors(100))
spplot(r.Cachi, col.regions = rev(terrain.colors(100)))
range(r.Cachi)
spplot(r.Cachi, col.regions = rev(topo.colors(100)), #spplot(r.Cachi, col.regions = rev(terrain.colors(100)),
       at=seq(0,53,2), scales = list(draw=TRUE) ) +
  layer(sp.polygons(cuenca.wgs, lwd=1, col='darkgray')) 
boxplot(r.Cachi, xlab= "Meses", ylab="Precipitación(mm)", border= "blue",
        main="Variabilidad y estacionalidad de precipitacion - Cachi [mm]")

#convertir datos de precipitacion
# de vertical en horizontal
datos <- read.csv("Salida/C_Cachi_PM.csv")
datos
dat=datos[,2]
mdat=t(matrix(dat,nrow = 12))
mdat
length(mdat)
colnames(mdat)
# cambiando los nombres de columnas y filas
colnames(mdat) <- c('ENE', 'FEB', 'MAR', 'ABRI','MAY', 'JUN', 'JUL', 'AGO','SEP', 'OCT', 'NOV', 'DIC')
rownames(mdat)<-1981:2016 
mdat
#write.csv(mdat,"Salida/C_Cac_PM_ordenado.csv")

