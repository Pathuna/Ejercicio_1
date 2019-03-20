# EJERCICIO UNO
MI_SEQ<-seq(10,20)
?seq # para solicitar ayuda
matriz <- matrix(seq(1,9),nrow=3,ncol=3)
matriz

numeros <- c(1,2,3)
texto <- c("hola","como","estas")
booleanos <- c(TRUE,FALSE,TRUE)
data_frame <- data.frame(numeros,texto,booleanos)

matriz[2,1]
data_frame[2,1]
condicion <- matriz[,1]>1
condicion

library("raster")
library("sp")
matriz_raster <- raster(matriz)
matriz_raster
plot(matriz_raster)
plot(matriz_raster)
points(0.25,0.75,pch=21,bg="red",cex=2)

library("ggplot2")
library("rasterVis")
gplot(matriz_raster) + geom_tile(aes(fill=values(matriz_raster))) + 
  scale_colour_brewer(palette="Blues")  +
  coord_equal() + 
  geom_text(aes(label=sprintf("%02.0f",values(matriz_raster))),color="white",size=5)
matriz_raster_por2_menos1 <- matriz_raster * 2 - 1

cellStats(matriz_raster,stat="mean")
chiapas1 <- brick("C:/Users/Sara/Desktop/curso_r_conabio/1crop.tif")
VIS <- subset(chiapas1,subset=3)

NIR <- subset(chiapas1,subset=5)

par(mfrow=c(1,2))

plot(VIS,main="VIS")
plot(NIR,main="NIR")

chiapas1_ndvi <- (NIR-VIS)/(NIR+VIS)

plot(chiapas1_ndvi,main="NDVI")
chiapas1_ndvi_st <- (chiapas1_ndvi-cellStats(chiapas1_ndvi,stat="mean"))/(cellStats(chiapas1_ndvi,stat="sd"))

plot(chiapas1_ndvi_st,main="NDVI estandarizado")
chiapas2 <- brick("C:/Users/Sara/Desktop/curso_r_conabio/2crop.tif")

VIS <- subset(chiapas2,subset=3)

NIR <- subset(chiapas2,subset=5)

chiapas2_ndvi <- (NIR-VIS)/(NIR+VIS)

chiapas2_ndvi_st <- (chiapas2_ndvi-cellStats(chiapas2_ndvi,stat="mean"))/(cellStats(chiapas2_ndvi,stat="sd"))

plot(chiapas2_ndvi_st,main="NDVI estandarizado, 1 año después")

D <- chiapas1_ndvi_st - chiapas2_ndvi_st

plot(D,main="Diferencias en NDVI")

hist(D,breaks=100,main="Diferencias en NDVI",xlab="Diferencias",ylab="Conteos")
umbral_positivo <- cellStats(D,stat="mean")+ 2*cellStats(D,stat="sd")

umbral_negativo <- cellStats(D,stat="mean")- 2*cellStats(D,stat="sd")
D[(D>umbral_negativo) & (D<umbral_positivo)] <-NA 
projection(D)
D_reproj <- projectRaster(D,crs=CRS("+proj=longlat"))
KML(D_reproj,"C:/Users/Sara/Desktop/curso_r_conabio/cambios_kml",maxpixels=1000000,overwrite=TRUE)
library("rgdal")
puntos <- readOGR(dsn="C:/Users/Sara/Desktop/curso_r_conabio/puntos.shp",layer="puntos")
lineas <- readOGR(dsn="C:/Users/Sara/Desktop/curso_r_conabio/lineas.shp",layer="lineas")
poligonos <- readOGR(dsn="C:/Users/Sara/Desktop/curso_r_conabio/poligonos.shp",layer="poligonos")
par(mfrow=c(1,3))

plot(puntos)
plot(lineas)
plot(poligonos)

class(puntos)
class(lineas)
class(poligonos)
head(puntos)
head(lineas)
head(poligonos)
poligono <- poligonos[1,]

class(poligono)

plot(poligono)
poligonos$intensidad <- c(1,2,2)

head(poligonos)

colores <- c("red","blue")

plot(poligonos,col=colores[poligonos$intensidad])
overlay <- over(puntos,poligonos)

overlay

plot(poligonos,col=colores[poligonos$intensidad])

plot(puntos,add=TRUE)

poligonos2 <- readOGR(dsn="C:/Users/Sara/Desktop/curso_r_conabio/cruces.shp",layer="cruces")
plot(poligonos2)

plot(poligonos,col=colores[poligonos$intensidad],add=TRUE)
intersecciones <- poligonos + poligonos2
data.frame(intersecciones)
plot(intersecciones,col=blues9)
altitud <- raster("C:/Users/Sara/Desktop/curso_r_conabio/dem30_mean1000.tif")

plot(altitud)
plot(poligonos,col=colores[poligonos$intensidad],add=TRUE)
plot(puntos,add=TRUE)
extraccion_puntos <- extract(altitud,puntos)

# valor de altitud de cada punto
extraccion_puntos
extraccion_poligonos <- extract(altitud,poligonos,fun=mean,na.rm=TRUE)

# valor de altitud promedio por polígono
extraccion_poligonos


extShape <- extent(poligonos)
img <- crop(altitud, extShape, snap='out')
img.NA <- setValues(img, NA)
img.mask <- rasterize(poligonos, img.NA)
img.crop <- mask(x=img, mask=img.mask)

plot(img.crop,main="Altitud en polígonos")

# raster multiespectral vacío
brik <- brick()

# asignemos altitud como primera banda
brik <- addLayer(brik,altitud)

v_boscosa <- raster("C:/Users/Sara/Desktop/curso_r_conabio/MOD44B_2014-03-06.Percent_Tree_Cover.tif")
v_noboscosa <- raster("C:/Users/Sara/Desktop/curso_r_conabio/MOD44B_2014-03-06.Percent_NonTree_Vegetation.tif")
v_no <- raster("C:/Users/Sara/Desktop/curso_r_conabio/MOD44B_2014-03-06.Percent_NonVegetated.tif")

# plot v_boscosa
plot(v_boscosa,main="% vegetación boscosa")
# ¿hay un match entre los metadatos de estas capas y el raster altitud?
projection(v_boscosa)==projection(altitud)
extent(v_boscosa)==extent(altitud)
res(v_boscosa)==res(altitud)
# resolución de v_boscosa?
res(v_boscosa)


# agregación factor *4 con base en función promedio
v_boscosa <- aggregate(v_boscosa,fact=4,fun=mean,na.rm=TRUE)
v_noboscosa <- aggregate(v_noboscosa,fact=4,fun=mean,na.rm=TRUE)
v_no <- aggregate(v_no,fact=4,fun=mean,na.rm=TRUE)

# filtrado de capas: si es NA en altitud que sea NA en capas de vegetación
v_boscosa[is.na(altitud)]<-NA
v_noboscosa[is.na(altitud)]<-NA
v_no[is.na(altitud)]<-NA

# Introduzcamos estas capas a nuestro raster multiespectral
brik <- addLayer(brik,v_boscosa)
brik <- addLayer(brik,v_noboscosa)
brik <- addLayer(brik,v_no)

dim(brik)

# Agregamos las bandas que faltan
brik <- addLayer(brik,raster("C:/Users/Sara/Desktop/curso_r_conabio/temp_max.tif"))

brik <- addLayer(brik, raster("C:/Users/Sara/Desktop/curso_r_conabio/temp_min.tif"))

brik <- addLayer(brik, raster("C:/Users/Sara/Desktop/curso_r_conabio/precipitacion.tif"))

tabla_datos <- rasterToPoints(brik)

# descartemos las filas no completas
tabla_datos_clean <- tabla_datos[complete.cases(tabla_datos),]

head(tabla_datos_clean)
componentes <- prcomp(tabla_datos_clean[,2:9],center=TRUE,scale. =TRUE)

# varianza explicada
summary(componentes)
# k-medias, k=10
kmedias <- kmeans(componentes$x[,1:4],centers=10,iter.max=50)
clustered_data <- data.frame(x=tabla_datos_clean[,1],
                             y=tabla_datos_clean[,2],
                             clust=kmedias$cluster)

# convertir esta tabla en un objeto espacial
coordinates(clustered_data)=~x+y
gridded(clustered_data)=TRUE

clustered_data<-raster(clustered_data)
projection(clustered_data)<-projection(altitud)
#poligonos_clusters <- rasterToPolygons(clustered_data,n=4,dissolve=TRUE)

#writeOGR(poligonos_clusters,"./data/regionalizacion.shp",
#                        "regionalizacion",driver="ESRI Shapefile")