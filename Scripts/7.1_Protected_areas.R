##.                              HERE !!!!!!!!!!!!!!!!!!!!!!!

## Future distribution change in area
# Change in altitude
## Convert the map to binary to compute connectivity: Zero-dispersal


# connectivity 
# evaluating protected areas 


# Protected areas ---------------------------------------------------------

# set wd to properly import PA shp

setwd("./uc_brasil/")
ucs <- readOGR("./unidade_protecao_integralPolygon.shp")
#Define proper CRS
projection(ucs) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# imoport PAs in Brazil
ucs_suste <- readOGR("./unidade_uso_sustentavelPolygon.shp") 
#Define proper CRS
projection(ucs_suste) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# combine both spdf
ucs_full_2 <- rbind(ucs,ucs_suste)
# crop it for study area
ucs_ucs_fim <- crop(ucs_full_2,mascarateste2)
projection(ucs_ucs_fim) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(ucs_ucs_fim, add=T, col="green")

# which cells are not NA? These ones:
# notna <- which(!is.na(values(object)))   lines 1119

##back to proper WD
###### generate new maps using IUCN.eval for future lines 1175



# generate a map for current occurrence inside PAs current ------------------



conRtable_current <- read.csv(paste0('./Araucaria/outputs/ca_samp_conR.csv'),
                              header=T)
conRtable_current <- conRtable_current[,-1]

MyData_samp_current <- conRtable_current
mapa_pa_samp_current <- IUCN.eval(MyData_samp_current, Cell_size_locations = 1, 
                                  protec.areas = ucs_full, ID_shape_PA = "nome", 
                                  method_protected_area = "no_more_than_one",
                                  DrawMap = T,write_shp = T)

write.csv(mapa_pa_samp_current,paste0("./Araucaria/outputs/currentinPA.csv"))


### Connectivity among predicted occurrence areas

connect <- raster(paste0("./Araucaria/outputs/",rcp[j],"_binary_",yrs[m],"_",dispersal[q],".tif"))
connect_cur <- raster("./Araucaria/outputs/binary_current.tif")


### Create a matrix from rasters
matrix_connect <- as.matrix(connect)  # future data (ensemble all)
matrix_connect_cur <- as.matrix(connect_cur)

SDA.future <- sum(values(connect)>0,na.rm=TRUE) # dá na mesma se colocar >=1
SDA.current <- sum(values(connect_cur)>=1,na.rm=TRUE)

bb_future <- bbox(connect) #fornece uma caixa delimitadora
bb_current <- bbox(connect_cur) #fornece uma caixa delimitadora

cs <- c(0.15, 0.15)  # cell size 
cc_future <- bb_future[, 1] + (cs/2)  # cell offset
cc_current <- bb_current[, 1] + (cs/2)  # cell offset

dd_future <- ceiling(diff(t(bb_future))/cs)  # number of cells per direction - transforma argumento em vetor
dd_current <- ceiling(diff(t(bb_current))/cs)  # number of cells per direction - transforma argumento em vetor

grd_future <- GridTopology(cellcentre.offset=cc_future, cellsize=cs, cells.dim=dd_future)
grd_current <- GridTopology(cellcentre.offset=cc_current, cellsize=cs, cells.dim=dd_current)

## Conversion from topology to poligon (shape)
sp_grd_future <- SpatialGridDataFrame(grd_future, data=data.frame(id=1:prod(dd_future)))
sp_grd_current <- SpatialGridDataFrame(grd_current, data=data.frame(id=1:prod(dd_current)))                        

sp_grd_future <- as(sp_grd_future, "SpatialPixels")
sp_grd_current <- as(sp_grd_current, "SpatialPixels")                                       

sp_grd_future <- as(sp_grd_future, "SpatialPolygons")
sp_grd_current <- as(sp_grd_current, "SpatialPolygons")

# set CRS

proj4string(sp_grd_future) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
proj4string(sp_grd_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

names(connect)<- "binary"
names(connect_cur)<- "binary"

shp_future <- rasterToPolygons(connect, fun=function(x){x>0})
shp_current<- rasterToPolygons(connect_cur, fun=function(x){x>0})

proj4string(shp_future) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(shp_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

over_future <- over(sp_grd_future, shp_future) # gathering ucs points 
over_current <- over(sp_grd_current, shp_current) # gathering ucs points 

sp_grd_grdDF_future <- SpatialGridDataFrame(grd_future, data=data.frame(id=1:prod(dd_future)))
sp_grd_grdDF_current <- SpatialGridDataFrame(grd_current, data=data.frame(id=1:prod(dd_current)))

sp_grd_coord_future <- as(sp_grd_grdDF_future, "SpatialPixels")
sp_grd_coord_current <- as(sp_grd_grdDF_current, "SpatialPixels")

sp_grd_coord_DF_future <- as.data.frame(sp_grd_coord_future)
sp_grd_coord_DF_current <- as.data.frame(sp_grd_coord_current)

df_coords_future <- data.frame(over_future, sp_grd_coord_DF_future)
df_coords_current <- data.frame(over_current, sp_grd_coord_DF_current)

names(df_coords_future)<-c("binary", "lonDD", "latDD")
names(df_coords_current)<-c("binary", "lonDD", "latDD")

df_coords_future <-subset(df_coords_future, binary == 1)
df_coords_current <-subset(df_coords_current, binary == 1)

names(df_coords_future)<-c("binary", "lonDD", "latDD")

names(df_coords_current)<-c("binary", "lonDD", "latDD")


# # creating graphs using k-nearest neighbours ----------------------------

# Plot (Future connectiviy)

# Table to compare connectivity 


# testing Protected Areas above
#### remove land-use change above presence/absence maps

# convert planted-forests where at least 30% was converted to forests plantation

# convert antropic areas where at least 50% was converted to forests plantation

### setting land use above presence/absence maps
setwd("set_it")

### RCP 4.5 2080 - ZERO


# Plot (Current distribution land-use)
pdf(paste0("./Araucaria/outputs/presence_absence_land_use_current.pdf"),width=14,height=10)
plot(dados_finais_current,col= viridis_pal(option = "D")(4),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-2,1))
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Current binary distribution",
                          "Tropic of Capricorn - Dashed Line",
                          "Land-use (agriculture,forest plantation,urban structure)"),
       col=c("black","black","black"),
       fill=c("#35B779FF","black","#440154FF"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()


#### create table and compare habitat loss due to land-use
## No land-use

# Calculate suitable climatic areas with land-use

# Connectivity among predicted occurrence areas

## Table to compare connectivity 

# Plot (Future connectiviy

### Connectivity among predicted occurrence areas


# 
pdf(paste0("./Araucaria/outputs/future_full_85_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_85_full,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_85_Full_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 8.5 Full Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Plot (Future land=use 4.5 2080 Zero) ------------------------------------

# Plot (Current final_figure land_use + pa within)
pdf(paste0("./Araucaria/outputs/final_distribution_current_land_use_pa_within.pdf"),width=14,height=10)

# Legend
binary_current_teste <- BinaryTransformation(binary_current_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_current_teste) <- "+proj=longlat +datum=WGS84 +no_defs"
crs(dados_finais_current) <- "+proj=longlat +datum=WGS84 +no_defs"
crs(estados) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

breakpoints <- seq(-1,1,by=1)
a.arg <- list(at=seq(-1,1,length.out=3), labels=c("","",""), cex.axis=1.2)
l.arg <- list(text="Absence / Presence",side=2, line=0.5, cex=1.5)
# Plot (Future land=use 4.5 2080)
pdf(paste0("./Araucaria/outputs/current_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_current,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_current_teste,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Current distribution with land-use",
                          "Tropic of Capricorn - Dashed Line",
                          "Current distribution within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Plot future scenarios

# Legend
binary_45_Full_Ucs_within <- BinaryTransformation(binary_45_Full_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_45_Full_Ucs_within) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

breakpoints <- seq(-1,1,by=1)
a.arg <- list(at=seq(-1,1,length.out=3), labels=c("","",""), cex.axis=1.2)
l.arg <- list(text="Absence / Presence",side=2, line=0.5, cex=1.5)
# Plot (Future land=use 4.5 2080 Full)
pdf(paste0("./Araucaria/outputs/future_full_45_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_45_full,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_45_Full_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
#plot(ucs_ucs_fim,col="transparent",contour="white",add=T)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 4.5 Full Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Future 4.5 Zero 2085
# Legend
binary_45_Zero_Ucs_within <- BinaryTransformation(binary_45_Zero_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_45_Zero_Ucs_within) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Plot (Future land=use 4.5 2080 Zero)
pdf(paste0("./Araucaria/outputs/future_zero_45_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_45_zero,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_45_Zero_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 4.5 Zero Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Legend
binary_85_Full_Ucs_within <- BinaryTransformation(binary_85_Full_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_85_Full_Ucs_within) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Plot (Future land=use 4.5 2080 Zero)
pdf(paste0("./Araucaria/outputs/future_full_85_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_85_full,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_85_Full_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 8.5 Full Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Legend
binary_85_Zero_Ucs_within <- BinaryTransformation(binary_85_Zero_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_85_Zero_Ucs_within) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Plot (Future land=use 4.5 2080 Zero)
pdf(paste0("./Araucaria/outputs/future_zero_85_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_85_zero,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_85_Zero_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 8.5 Zero Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()


### Connectivity among predicted occurrence areas
