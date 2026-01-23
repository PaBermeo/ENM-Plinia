

pacman::p_load(cartography, cowplot, dplyr, geobr, grid, ggplot2, ggpattern, ggspatial, ggnewscale, sf, sp, tmap, terra, raster, readr, rgdal, rnaturalearth, viridis)

GCM <- c("AC", 'CM', "MI", "MP")
SSP <- c('2', "3")
year <- c('50', '70', '90')

for (GC in 1: length(GCM)) {
  for (Sp in 1: length(SSP)) {
    for (ye in 1: length(year)) {
      
      pat = paste0(SSP[Sp],'_',year[ye],'noSavann.shp') 
      
      shape <- paste0('Final_maps/Vectors/', pat)
      shape = vect(shape)
      
      L <- rast('Projection_threshold/current_noSavann.tif') 
      
      fut <- mask(L, shape, inverse= T)  |> stack()
      
      pol <- rasterToPolygons(fut, dissolve = T, fun=function(x) {x>0}) 
      
      # Convert SpatialPolygonsDataFrame to sf
      fut_shape <- st_as_sf(pol)
      
      st_write(fut_shape, paste0('Final_maps/Vectors/fut-loss', '_', pat))
    }
  } 
}



# Bio14 future map ---------------------------------------------------

tmap::tmap_mode(mode = "plot")

#Set map layers
#Load states and countries polygons
SA <- st_read('Other_files/Vectors/World_Administrative_Divisions/World_Administrative_Divisions.shp')
states <- c('482', '491', '492', '496', '497', '498', '506', '510', '514', '523', '528', '529', '533', '535')
states1 <- c('476', '482', '489', '491', '492', '498', '506', '510', '514', '523', '528', '529', '533')
Br_state <- SA[SA$OBJECTID %in% states, ]
Br_state1<- SA[SA$OBJECTID %in% states1, ]

COUN <- st_read('Other_files/Vectors/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp')
country <- c('Argentina', 'Paraguay', 'Uruguay')
SA_coun <- COUN[COUN$COUNTRY %in% country, ]


#Graphs in loop

 SSP <- c(2L, 3L)
 year <- c(50L, 70L, 90L)


for (Sp in 1: length(SSP)) {
  for (ye in 1: length(year)) {
    
    Sp <- 2
    ye <- 3
    SY <- paste0(SSP[Sp],'_',year[ye])

#Realized future niche
RN <- st_read(paste0('Final_maps/Vectors/', SY,'noSavann.shp'))

#Area loss 
NoSuita <- st_read(paste0('Final_maps/Vectors/fut-loss_', SY, 'noSavann.shp'))
N <- hatchedLayer(x = NoSuita, mode = "sfc", pattern = 'right2left', density = 5)
NoSuit <- as_Spatial(N)

#Load and crop raster data from current Realized Niche
Bio14p <- rast(paste0('Final_maps/Bio_14/', SY, '_14.asc'))
ReN <- 'Final_maps/Vectors/current_suitability_noSavann.shp'
ShapeRN = vect(ReN)
#SA <- as(ShapeRN, "Spatial")

Bio14 <- terra::mask(crop(Bio14p, ShapeRN), ShapeRN)
palette <- viridis_pal(begin = 0, end = 1, direction = -1)(10)

# change to tmap mode

    tm_shape(Bio14, bbox = c(-57.8, -31, -36.6, -8)) +
    tm_raster(style = "cont", palette = palette, legend.show =F, breaks=seq(0, 180, 30)) +
    tm_shape(SA_coun) +
    tm_borders(col = "gray25", lwd = 0.5, lty = 'dashed') +
    tm_shape(Br_state) +
    tm_borders(col = 'gray40', lwd = 0.4) +
    tm_shape(NoSuit) +
    tm_lines(col = "gray0", lwd = 0.7) +
    tm_shape(RN) +
    tm_borders(col = "red", lwd = 0.4) +
    tm_graticules(lines = FALSE, labels.rot = c(0, 90)) + #lines = FALSE,
    tm_layout(title = paste0('SSP ', SSP[Sp], ' 20', year[ye]),
              title.size = 1.3,
              title.fontface = "bold",
              inner.margins = c(0, 0, 0, 0),
              legend.text.size = 1.2,
              legend.width = 1)
    
    BiomF14 <- grid.grab()

assign(paste0('S',SY), BiomF14)
    
}
 }



#Complete map
{  tm_shape(Bio14, bbox = c(-57.8, -31, -36.6, -8)) +
   tm_raster(style = "cont", legend.reverse= T, legend.format = list(text.align = 'center'), palette = palette, title = 'Pp driest\nmonth (mm)', breaks=seq(0, 180, 30)) +
    tm_shape(SA_coun) +
    tm_borders(col = "gray25", lwd = 0.5, lty = 'dashed') +
    tm_shape(Br_state1) +
    tm_borders(col = 'gray40', lwd = 0.4) +
    tm_shape(NoSuit) +
    tm_lines(col = "gray0", lwd = 0.7) +
    tm_shape(RN) +
    tm_borders(col = "red", lwd = 0.4) +  
    tm_compass(position = c(0.85, 0.5), size= 4, text.size = 1) + #North arrow  
    tm_scale_bar(text.size = 0.45, position = c(0.45, 0)) +
    tm_graticules(lines = FALSE, labels.rot = c(0, 90)) +
    tm_add_legend(type = "line", 
                  labels = "Realized future niche", 
                  col = "red", 
                  lwd = 3, 
    ) +
    tm_add_legend(type = "line", 
                  labels = "Area loss", 
                  col = "gray0", 
                  lwd = 3,
    ) +
    tm_add_legend(type = "fill",
                  border.col = 'red',
                  col = 'white',
                  labels = "Area gain"
    ) +
    tm_layout(legend.position = c('left', 'top'),
      title = paste0('SSP ', SSP[Sp], ' 20', year[ye]),
      title.size = 1.3,
      title.fontface = "bold",
              legend.text.size = 1.2,
              #legend.hist.height = 0.1, 
              inner.margins = c(0, 0, 0, 0), 
              legend.width = 1)
              #legend.outside = T,
              #legend.outside.position = "right")
  
  S3_90 <- grid.grab()

final_plotF <-  plot_grid(S2_50, S2_70, S2_90, S3_50, S3_70, S3_90, labels = 'AUTO', align= 'hv')


}
 #final_plotF
 
 ggsave(final_plotF, filename = "Final_maps/Figure9.var-14fut.png", width =18, heigh= 13.5, units= 'in', dpi = 800)

 
 


# Assesing MG loss proportion in 2050 -------------------------------------

 
CRN <- rast('Projection_threshold/current_noSavann.tif')
 
Br_MG <- SA[SA$OBJECTID == '498', ]
MG_CRN <- terra::mask(crop(CRN, Br_MG), Br_MG)

#Future raster 2050
S2RN <- rast('Projection_threshold/2_50noSavann.tif')
S3RN <- rast('Projection_threshold/3_50noSavann.tif')

MG_2RN <- terra::mask(crop(S2RN, Br_MG), Br_MG)
MG_3RN <- terra::mask(crop(S3RN, Br_MG), Br_MG)


FreqC <- as.data.frame(raster::freq(MG_CRN))

Freq2 <- as.data.frame(raster::freq(MG_2RN))
Freq3 <-  as.data.frame(raster::freq(MG_3RN))

#I Compared the value cells of the value 1 from current to future scenarios


#Assess SSP3 2090 dynamics for MG

S3RN2090 <- rast('Projection_threshold/3_90noSavann.tif')
MG_3RN90 <- terra::mask(crop(S3RN2090, Br_MG), Br_MG)

as.data.frame(raster::freq(MG_3RN90))
  
#Now compare current and SSP3 2090 shits in ES, SP and BA states

Br_BA <- SA[SA$OBJECTID == '482', ]
BA_CRN <- terra::mask(crop(CRN, Br_BA), Br_BA)
as.data.frame(raster::freq(BA_CRN))

BA_3RN90 <- terra::mask(crop(S3RN2090, Br_BA), Br_BA)
as.data.frame(raster::freq(BA_3RN90))  

#For ES state
Br_ES <- SA[SA$OBJECTID == '491', ]
ES_CRN <- terra::mask(crop(CRN, Br_ES), Br_ES)
as.data.frame(raster::freq(ES_CRN))

ES_3RN90 <- terra::mask(crop(S3RN2090, Br_ES), Br_ES)
as.data.frame(raster::freq(ES_3RN90)) 

#For RJ state
Br_RJ <- SA[SA$OBJECTID == '514', ]
RJ_CRN <- terra::mask(crop(CRN, Br_RJ), Br_RJ)
as.data.frame(raster::freq(RJ_CRN))

RJ_3RN90 <- terra::mask(crop(S3RN2090, Br_RJ), Br_RJ)
as.data.frame(raster::freq(RJ_3RN90)) 

#For  state
Br_SP <- SA[SA$OBJECTID == '529', ]
SP_CRN <- terra::mask(crop(CRN, Br_SP), Br_SP)
as.data.frame(raster::freq(SP_CRN))

SP_3RN90 <- terra::mask(crop(S3RN2090, Br_SP), Br_SP)
as.data.frame(raster::freq(SP_3RN90))


# End of the script
