library(readxl)
library(sf)
library(writexl)
library(tidyverse)
library(sp)
library(rgdal)
library(automap)



datos <- read_xlsx("Reporte de datos Saldana.xlsx")
summary(datos)
datos.head()

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

datos.sf <- st_as_sf(x = datos,                         
                   coords = c("LONGITUD", "LATITUD"),
                   crs = projcrs)

plot(st_geometry(datos.sf))

saldana <- st_read("./Shp",
                     layer="borde_saldana")

plot(st_geometry(saldana))

st_crs(saldana)

datos.sf <- st_transform(datos.sf, 32618) 

plot(st_geometry(datos.sf), add=TRUE)

###Exportar

#write_sf(datos.sf, "./ShpPuntos/puntosSaldana", driver="ESRI Shapefile")

plano <- datos.sf %>% 
  st_drop_geometry() %>% 
  bind_cols(data.frame(st_coordinates(datos.sf))) %>% 
  rename(temp = "Temperatura °C")

#write_xlsx(plano, "./ShpPuntos/datosTransformados.xlsx")

plano <- plano[-3,]

dism <- plano
dism <-dism [-15,]
             
# Interpolacion -----------------------------------------------------------

coordinates(plano) = ~ X + Y
spplot(plano,"temp",colorkey=TRUE)

#shp <- readOGR(dsn = file.path("Shp/borde_saldana.shp"), stringsAsFactors = F)
#plot(shp)
# Create an empty grid where n is the total number of cells
### Paths
bddPath <- file.path("..", "Shp")
shp <- readOGR("C:/Users/Milo/Desktop/ShpPuntos/Shp", "borde_saldana")

grd              <- as.data.frame(spsample(shp, "regular", n=3000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

#plot(grd)
#points(plano,pch=20)

####################################################################################
#                                   TEMPERATURA  IDW                                        #


P.idw <- gstat::idw(temp ~ 1, plano, newdata=grd, idp=2.0)

plot(P.idw)
points(plano$X, plano$Y, col ="black", pch = 19)

# Convert to raster object then clip 
r       <- raster::raster(P.idw)
r.m     <- raster::mask(r, shp)

plot(r.m)
points(plano$X, plano$Y, col ="red", pch = 1)

####################################################################################
#                                   TEMPERATURA  KRIGING                         #


kriging_result = autoKrige(temp~1, plano, grd)
plot(kriging_result)

####################################################################################
coordinates(dism) = ~ X + Y
spplot(dism,"temp",colorkey=TRUE)
kriging_result = autoKrige(temp~1, dism, grd)
plot(kriging_result)
#####################################################################################
dism1 <- dism
dism1 <-dism1 [-5,]
coordinates(dism) = ~ X + Y
spplot(dism1,"temp",colorkey=TRUE)
kriging_result = autoKrige(temp~1, dism1, grd)
plot(kriging_result)

#####################################################################################
dism2 <- dism1
dism2 <-dism2 [-5,]
coordinates(dism) = ~ X + Y
spplot(dism2,"temp",colorkey=TRUE)
kriging_result = autoKrige(temp~1, dism2, grd)
plot(kriging_result)


####################################################################################
#                                   NITRATO                                         #

plano <- datos.sf %>% 
  st_drop_geometry() %>% 
  bind_cols(data.frame(st_coordinates(datos.sf))) %>% 
  rename(NO3 = "NO3 (mv)")

#write_xlsx(plano, "./ShpPuntos/datosTransformados.xlsx")

coordinates(plano) = ~ X + Y
spplot(plano,"NO3",colorkey=TRUE)

#                                   NITRATO   IDW

P.idw <- gstat::idw(NO3 ~ 1, plano, newdata=grd, idp=1.0)

plot(P.idw)
points(plano$X, plano$Y, col ="black", pch = 19)

# Convert to raster object then clip 
r       <- raster::raster(P.idw)
r.m     <- raster::mask(r, shp)

plot(r.m)
points(coords$Easting, coords$Northing, col ="red", pch = 4)

#                                   NITRATO   KRIGING
kriging_result = autoKrige(NO3~1, plano, grd)
plot(kriging_result)



####################################################################################
#                                   POTASIO                                       #


plano <- datos.sf %>% 
  st_drop_geometry() %>% 
  bind_cols(data.frame(st_coordinates(datos.sf))) %>% 
  rename(Potasio = "Potasio (mv)")

#write_xlsx(plano, "./ShpPuntos/datosTransformados.xlsx")

coordinates(plano) = ~ X + Y
spplot(plano,"Potasio",colorkey=TRUE)

#                                   POTASIO   IDW

P.idw <- gstat::idw(Potasio ~ 1, plano, newdata=grd, idp=1.0)

plot(P.idw)
points(plano$X, plano$Y, col ="black", pch = 19)

# Convert to raster object then clip 
r       <- raster::raster(P.idw)
r.m     <- raster::mask(r, shp)

plot(r.m)
points(coords$Easting, coords$Northing, col ="red", pch = 4)
#                                   POTASIO   KRIGING

kriging_result = autoKrige(Potasio~1, plano, grd)
plot(kriging_result)



####################################################################################
#                                   HUMEDAD                                       #


plano <- datos.sf %>% 
  st_drop_geometry() %>% 
  bind_cols(data.frame(st_coordinates(datos.sf))) %>% 
  rename(Humedad = "Humedad de Suelo % ")

#write_xlsx(plano, "./ShpPuntos/datosTransformados.xlsx")

coordinates(plano) = ~ X + Y
spplot(plano,"Potasio",colorkey=TRUE)


#                                   HUMEDAD   IDW
P.idw <- gstat::idw(Potasio ~ 1, plano, newdata=grd, idp=1.0)

plot(P.idw)
points(plano$X, plano$Y, col ="black", pch = 19)

# Convert to raster object then clip 
r       <- raster::raster(P.idw)
r.m     <- raster::mask(r, shp)

plot(r.m)
points(coords$Easting, coords$Northing, col ="red", pch = 4)
#                                   HUMEDAD   kRIGING

kriging_result = autoKrige(Potasio~1, plano, grd)
plot(kriging_result)



############################ Validacion Temperatura
# Leave-one-out validation routine
IDW.out <- vector(length = length(plano))
for (i in 1:length(plano)) {
  IDW.out[i] <- gstat::idw(temp ~ 1, plano[-i,], plano[i,], idp=2.0)$var1.pred
}
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ plano$temp, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ plano$temp), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

# Compute RMSE
sqrt( sum((IDW.out - plano$temp)^2) / length(plano$temp))
sqrt(mean((IDW.out - plano$temp)^2) ) 



############################ Validacion NO3
# Leave-one-out validation routine
IDW.out <- vector(length = length(plano))
for (i in 1:length(plano)) {
  IDW.out[i] <- gstat::idw(NO3~ 1, plano[-i,], plano[i,], idp=2.0)$var1.pred
}
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ plano$NO3, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ plano$NO3), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

# Compute RMSE
sqrt( sum((IDW.out - plano$NO3)^2) / length(plano))
sqrt(mean((IDW.out - plano$NO3)^2) ) 
