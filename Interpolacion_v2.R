
library(sf)
library(gstat)
library(automap)
library(sp)
#install.packages('rgeos')

#install.packages('rgdal')
library(rgeos)
library(rgdal) 



#install.packages("remotes")
#remotes::install_github("MariekeDirk/GeoInterpolation")

library("GeoInterpolation")

puntos <- read_sf("C:/Users/Damian Martinez/Desktop/Interpolacion/puntosSaldana/",
                  layer="puntosSaldana")

saldana1 <- read_sf("C:/Users/Damian Martinez/Desktop/Interpolacion/Shp",
                   layer="borde_saldana")

puntos <- as_Spatial(puntos)
saldana <- as_Spatial(saldana1)

saldana <- spTransform(saldana,CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "))

grilla <- expand.grid(seq(from=saldana@bbox[1,1], to=saldana@bbox[1,2], length.out = 200),
                      seq(from=saldana@bbox[2,1], to=saldana@bbox[2,2], length.out = 200))

coordinates(grilla) <- ~Var1+Var2
proj4string(grilla) <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs ")

grillaPred <- gIntersection(grilla,saldana)

plot(saldana, col="red")
plot(grillaPred, add=T)

####Temperatura

#Kriging

krig <- autoKrige(Tmprt.C~1, puntos, grillaPred)

Temp.krig.pred <- krig$krige_output$var1.pred
Temp.krig.sd <- krig$krige_output$var1.stdev

#Interpolación ponderada de distancia inversa

p.idw <- gstat::idw(Tmprt.C ~ 1, puntos, newdata=grillaPred, idp=2.0)

Temp.idw.pred <- p.idw$var1.pred

#Vecinos naturales

vc <- doNearestNeighbor(Tmprt.C ~ 1, data=puntos, newdata = grillaPred)

Temp.vc.pred <- vc$var1.pred

######Humedad

#Kriging

krigH <- autoKrige(HmdddS.~1, puntos, grillaPred)

Hum.krig.pred <- krigH$krige_output$var1.pred
Hum.krig.sd <- krigH$krige_output$var1.stdev

#Interpolación ponderada de distancia inversa

p.idwH <- gstat::idw(HmdddS.~ 1, puntos, newdata=grillaPred, idp=2.0)

Hum.idw.pred <- p.idwH$var1.pred

#Vecinos naturales

vcH <- doNearestNeighbor(HmdddS. ~ 1, data=puntos, newdata = grillaPred)

Hum.vc.pred <- vcH$var1.pred


####

interpolacion <- data.frame("Temp.krig.pred"=Temp.krig.pred,
                            "Temp.krig.sd"=Temp.krig.sd,
                            "Temp.idw.pred"=Temp.idw.pred,
                            "Temp.vc.pred"=Temp.vc.pred,
                            "Hum.krig.pred"=Hum.krig.pred,
                            "Hum.krig.sd"=Hum.krig.sd,
                            "Hum.idw.pred"=Hum.idw.pred,
                            "Hum.vc.pred"=Hum.vc.pred)

#grillaPred@data <- interpolacion

grillaPred.df <- SpatialPointsDataFrame(coordinates(grillaPred), 
                                     proj4string = CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "),
                                     data=interpolacion,
                                     match.ID=FALSE)

writeOGR(grillaPred.df, "C:/Users/mpdue/OneDrive/Documents/Sergio/Interpolacion",
         layer = "interpolacionSaldana.shp", driver="ESRI Shapefile")

plot(saldana, col="red")
plot(grillaPred, add=T)


# grillaPred.sf <- st_as_sf(grillaPred.df)
# 
# plot(st_geometry(saldana1), col="red", type="l")
proj4string(saldana)
plot(p.idw)
