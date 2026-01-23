

# rgeos https://cran.r-project.org/src/contrib/Archive/rgeos/
#rgdal https://cran.r-project.org/src/contrib/Archive/rgdal/
# Installing and loading packages
if(!require(devtools)){
  install.packages("devtools")
}
if(!require(ellipsenm)){
  devtools::install_github("marlonecobos/ellipsenm")
}

library(ellipsenm)
library(sf)
library(terra)
library(readr)
library(raster)

#Load occs
Pp <- read_delim('Plinia_peruviana.csv', delim = ';', col_names = T) |> as.data.frame()

#Remove extreme points
Pp <- Pp[- c(10, 19, 20, 59, 71), ]

#Load shape WWF
ecor <- "Other_files/vectors/WWF_ecoregions/official/wwf_terr_ecos.shp"
ecor = vect(ecor)
ecor <- as(ecor, "Spatial")
#plot(ecor)

#Convex area (Including buffer 60 Km)
M_convex <- convex_area(Pp, longitude = "Longitude", latitude = "Latitude",
                        buffer_distance = 60)

#Concave area (Including buffer 60 Km)
M_conc <- concave_area(Pp, longitude = "Longitude", latitude = "Latitude", length_threshold = 5,
                       buffer_distance = 60)  
#M_conc <- concave_area(Pp, longitude = "Longitude", latitude = "Latitude",
                      # buffer_distance = 100)  
  
# Areas by selecting polygons (Including buffer 30 Km)
M_ecorreg <- polygon_selection(Pp, longitude = "Longitude", latitude = "Latitude",
                               polygons = ecor, buffer_distance = 0)

M_convexv<-vect(M_convex)
M_concv <- vect(M_conc)
M_ecorrev <- vect(M_ecorreg) 

# intersection
M_intersect <- intersect(M_ecorrev, M_concv)
M_inter <- intersect(M_ecorrev, M_convexv)



#X11()
dev.off()
par(mfrow = c(2, 2), cex = 0.5, mar = rep(0.6, 4))
plot(M_convexv); points(Pp[, 2:3], col = 'black'); legend("topleft", legend = "Convex hull", bty = "n")
plot(M_concv); points(Pp[, 2:3], col = 'black'); legend("topleft", legend = "Concave", bty = "n")
plot(M_ecorrev); points(Pp[, 2:3], col = 'black'); legend("topleft", legend = "Ecorregions", bty = "n")
plot(M_intersect); points(Pp[, 2:3], col = 'black'); legend("topleft", legend = "Intersect", bty = "n")
dev.off()

#Save shapefiles
writeVector(M_convexv, "M/M_convexv.shp", overwrite=T)
writeVector(M_ecorrev, "M/M_ecorrev.shp", overwrite=T)
writeVector(M_intersect, "M/M_intersect.shp", overwrite=T)
writeVector(M_concv, "M/M_concv.shp", overwrite=T)





#Include ecoregions not considered in the polygon_selection

M_AltoPar <- ecor[ecor$OBJECTID=="2942",] #Alto Paraná Atlantic forests 
#M_AltoPar1 <- ecor[ecor$OBJECTID=="2942",]
M_Campos <- ecor[ecor$OBJECTID== "2969",] #Campos Rupestres montane savanna 1, 2 3 "2969" "2963"
M_Campos1 <- ecor[ecor$OBJECTID== "2963",] #Campos Rupestres montane savanna 1, 2 3 "2969" "2963"
M_Campos2 <- ecor[ecor$OBJECTID== "2961",]

M_AltoParv<-vect(M_AltoPar)
#M_AltoPar1v<-vect(M_AltoPar1)
M_Camposv <-vect(M_Campos)
M_Campos1v <-vect(M_Campos1)
M_Campos2v <-vect(M_Campos2)

M_union <- union(M_AltoParv, M_Camposv)
M_union1 <- union(M_Campos1v, M_union)
M_union11 <- union(M_Campos2v, M_union1)

plot(M_union1)

M_union2 <- union(M_intersect, M_union11)
plot(M_union2)

#Write final shape
writeVector(M_union2, "M/M_Plinia1.shp", overwrite=T)


#End of the script