#Cargarregamento de pacotes
pacman::p_load(ggplot2, raster, rasterVis, tidyterra, ggtext, readr, terra, sf, geodata, scales, readr, stars)



# Use coverage ------------------------------------------------------------

Cov <- rast('Other_files/MapBiomas/bosque_coverage_2022.tif')

shape <- 'Final_maps/Vectors/current_suitability_noSavann.shp'
ca = vect(shape)

Cov.ca <- mask(crop(Cov, ca), ca)

#Total frequency
Freq <- as.data.frame(raster::freq(Cov.ca))

#
Total <- (sum(Freq) * (9/10000))

#Land-use 
Forest.coverage <- (sum(Freq[c(2, 3, 4), 2] ) * (9/10000))#3: Forest, 11: flooded grasslands 
Novegetation.coverage <- (Freq[9, 2] * (9/10000)) 
Agriculture.coverage <- (sum(Freq[c(3, 6, 7, 8, 11, 12), 2] ) * (9/10000)) #9: Silvi
Water.coverage <- (Freq[10, 2] * (9/10000))
#

Forest <- Cov.ca == c(3, 11, 12)

Agrop <- Cov.ca == c(9, 15, 19, 21, 46, 48)
#plot(Agrop)

agua <- Cov.ca == 33
#plot(agua)

Urban <- Cov.ca == 22
#plot(Urban)

#Set new dataframe
Present <- as.data.frame(c(Forest, Agrop, Urban))





# Future areas ------------------------------------------------------------

SSP <- c('2', '3')
year <- c('50', '70', '90')

res <- list()

for(Sp in 1: length(SSP)){
  for(ye in 1: length(year)){
    
    fut <- paste0(SSP[Sp], '_', year[ye])

shape <- paste0("Final_maps/Vectors/", fut, "noSavann.shp")
ca = vect(shape)

Cov.ca <- mask(crop(Cov, ca), ca)

#Total frequency
Freq <- as.data.frame(raster::freq(Cov.ca))

Total <- (sum(Freq) * (9/10000))

assign(paste0('F',fut), Total)


  }
}

res[[]] 
