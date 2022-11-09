library(sf)
library(gstat)
library(automap)
library(sp)
library(leaflet)
library(RColorBrewer)
library(raster)
library(rgdal)
library(rgeos)
33

# install.packages("remotes")
# remotes::install_github("MariekeDirk/GeoInterpolation")

library("GeoInterpolation")

puntos <- read_sf("C:/Users/mpdue/OneDrive/Documents/Sergio/puntosSaldana/puntosSaldana/",
                  layer="puntosSaldana")

saldana1 <- read_sf("C:/Users/mpdue/OneDrive/Documents/Sergio/ShpSaldana/Shp/",
                   layer="borde_saldana")

puntos <- as_Spatial(puntos)
saldana <- as_Spatial(saldana1)

saldana <- spTransform(saldana,CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"))



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


grillaPred.df <- SpatialPointsDataFrame(coordinates(grillaPred), 
                                     proj4string = CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs "),
                                     data=interpolacion,
                                     match.ID=FALSE)

writeOGR(grillaPred.df, "C:/Users/mpdue/OneDrive/Documents/Sergio/Interpolacion",
         layer = "interpolacionSaldana.shp", driver="ESRI Shapefile")


#############
#####Mapas####
###############

ras_dom<-raster(xmn=saldana@bbox[1,1], xmx=saldana@bbox[1,2],
                ymn=saldana@bbox[2,1], ymx=saldana@bbox[2,2],
                crs="+proj=longlat +datum=WGS84 +no_defs ",
                resolution=c(1,1), vals=NA)

##Temperatura kriging

r <- rasterize(grillaPred.df, ras_dom, "Temp.krig.pred", update = TRUE)

plot(r, main="Temperatura kriging")


##Temperatura IDW

r2 <- rasterize(grillaPred.df, ras_dom, "Temp.idw.pred", update = TRUE)

plot(r2, main="Temperatura IDW")

#Temperatura vecino más cercano

r3 <- rasterize(grillaPred.df, ras_dom, "Temp.vc.pred", update = TRUE)

plot(r3, main="Temperatura vecino más cercano")

##Humedad kriging


r4 <- rasterize(grillaPred.df, ras_dom, "Hum.krig.pred", update = TRUE)

plot(r4, main="Humedad kriging")

##Humedad IDW

r5 <- rasterize(grillaPred.df, ras_dom, "Hum.idw.pred", update = TRUE)

plot(r5, main="Humedad IDW")

###Humedad vecino más cercano

r6 <- rasterize(grillaPred.df, ras_dom, "Hum.vc.pred", update = TRUE)

plot(r6, main="Humedad vecino más cercano")

