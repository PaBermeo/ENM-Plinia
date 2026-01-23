# Ecological niche analysis --------------------------------------------------


#Cargarregamento de pacotes
pacman::p_load(ggplot2, raster, rasterVis, tidyterra, ggtext, readr, terra, sf, geodata, scales, readr, stars)


# Abiotic variables -------------------------------------------------------

#Download rasters to characterize the ecological niche

#Elevation and Soils
#geodata::elevation_global(res=0.5, path = "Other_files/others_wc/") 
#geodata::soil_world(res = 0.5, var=c('clay','sand','silt', 'phh2o', 'soc'), depth=c(5,15,30), path = "Other_files/soil_world") 


#Import current niche shape
CURR <- c('_Total', '_noSavann', 'Savann')

for (curr in 1: length(CURR)) { 

sp <- paste0('Final_maps/Vectors/current_suitability', CURR[curr], '.shp')

shape <- sp
ca = vect(shape)
#plot(ca)

#Import Worldclim variables
wc <- list.files(path='Other_files/wc2.1_30s_bio/', pattern= '.tif$', full.names = T, recursive=F) |> rast()
#plot(wc[[1]])

#Mask variables to the ca
Wc <- mask(crop(wc, ca), ca) 
Crs <- crs(Wc)

#plot(Wc[[1]])

#Import soil variables
soil <- list.files(path='Other_files/soil_world', pattern= '.tif$', full.names = T, recursive=F) |> rast()
#plot(soil[[1]])


#Mask variables to the ca
Soil <- mask(crop(soil, ca), ca)
#plot(Soil[[1]])
res(Soil)

#The data from soilgrids has a lot of missing values. Let's filled this NA values considering the values of the neighboor cells
P <- focal(Soil, w= matrix(1, 3,3), fun=mean, na.policy="only", na.rm=T)
#plot(P[[1]])


#Average rasters from different depths
Clay_5_30 <- mean(P$`clay_5-15cm`, P$`clay_15-30cm`) #P$`clay_0-5cm` sem info
#plot(Clay_5_30, col = viridis::viridis(10))

Sand_0_30 <- mean(P$`sand_0-5cm`, P$`sand_5-15cm`, P$`sand_15-30cm`)
#plot(Sand_0_30, col = viridis::viridis(10))

Silt_0_30 <- mean(P$`silt_0-5cm`, P$`silt_5-15cm`, P$`soc_15-30cm`)
#plot(Silt_0_30, col = viridis::viridis(10))

pH_0_30 <- mean(P$`phh2o_0-5cm`, P$`phh2o_5-15cm`, P$`phh2o_15-30cm`)
#plot(pH_0_30, col = viridis::viridis(10))

SOC_0_30 <- mean(P$`soc_0-5cm`, P$`soc_5-15cm`, P$`soc_15-30cm`)
#plot(SOC_0_30, col = viridis::viridis(10))

#Cocatenar e exportar
Final_soils <- c(Clay_5_30, Sand_0_30, Silt_0_30, pH_0_30, SOC_0_30)
names(Final_soils) <- c('Clay', 'Sand', 'Silt', 'pH', 'SOC')
Final_solis <- rast(Final_soils)
Final_solis <- project(Final_solis, Crs)


#Importar dados raster
Envirem <- rast(list.files(path = "Other_files/ENVIREM/", pattern = '.tif', full.names = T, recursive=F))
#plot(Envirem[[1]], col = viridis::viridis(10))

Env.ca <- mask(crop(Envirem, ca), ca) 
Env.ca <- project(Env.ca, Crs)
#plot(Env.ca[[1]])

Abiotic <- stack(c(Wc, Final_soils, Env.ca))


# Statistical values
Abiotic_mean <- as.data.frame(raster::cellStats(x = Abiotic, stat = mean))
Abiotic_sd <- as.data.frame(raster::cellStats(x = Abiotic, stat = sd))
names(Abiotic_sd) <- 'sd'

Abiotic_mean[, 2] <- Abiotic_sd$sd
names(Abiotic_mean) <- c('Mean', 'Sd')

write.csv(Abiotic_mean, paste0('Other_files/NicheStats',  CURR[curr], '.csv'), row.names = T)  
}


# Elevation 

for (curr in 1: length(CURR)) { 
  
  sp <- paste0('Final_maps/Vectors/current_suitability', CURR[curr], '.shp')
  
  shape <- sp
  ca = vect(shape)
  
  Elev <- raster('Other_files/elevation.tif')
  ca <- as(ca, "Spatial")
  Elev.ca <- terra::mask(crop(Elev, ca), ca)
  Elev.ca <- stack(Elev.ca)

  Elev_mean <- as.data.frame(raster::cellStats(x = Elev.ca, stat = mean))
  Elev_sd <- as.data.frame(raster::cellStats(x = Elev.ca, stat = sd))
  names(Elev_sd) <- 'sd'

  Elev_mean[, 2] <- Elev_sd$sd
  names(Elev_mean) <- c('Mean', 'Sd')

write.csv(Elev_mean, paste0('Other_files/NicheStatsElev',  CURR[curr], '.csv'), row.names = T)  
}


# Future niche characterization -------------------------------------------


#First create future shapefiles

SSP <- c('2', '3')
year <- c('50', '70', '90')

  for (Sp in 1: length(SSP)) {
    for (ye in 1: length(year)){ 

fut <- paste0(SSP[Sp], '_', year[ye])
      
tif <- read_stars(paste0("Projection_threshold/", fut, "noSavann.tif"))
sf <- st_as_sf(tif, merge = T)

names(sf) <- c('a', 'geometry')
suit <- sf[sf$a == 1, ]

st_write(suit, paste0('Final_maps/Vectors/', fut, 'noSavann.shp'))

    }
  }


#Exrtact values from shapefiles

#gcms <- c("AC", 'CM', "MI", "MP")

rcps <- c(2L , 3L)
years <- c("50", "70", '90')
bands <- c(seq(2, 7, 1), seq(10, 15, 1)) # bands <- 1L 

results <- list()
M_list <- list()

  for (rc in 1: length(rcps)) {
    for (ye in 1: length(years)) {
      for (band in 1: length(bands)) {

  
 esc <- paste0(rcps[rc], '_', years[ye])
              
 wcF <- rast(list.files(path='Other_files/Rasters_wc/Future', pattern = paste0(esc, '.tif$'), full.names = TRUE))
 
 names(wcF) <- gsub('wc2.1_30s_bioc_','',names(wcF))
 names(wcF) <- gsub('ACCESS-CM2', 'AC',names(wcF))
 names(wcF) <- gsub('MIROC6', 'MI',names(wcF))
 names(wcF) <- gsub('MPI-ESM1-2-HR', 'MP',names(wcF))
 names(wcF) <- gsub('CMCC-ESM2', 'CM',names(wcF))
 names(wcF) <- gsub('ssp245', '2',names(wcF))
 names(wcF) <- gsub('ssp370', '3',names(wcF))
 names(wcF) <- gsub("2041-2060",'50',names(wcF))
 names(wcF) <- gsub("2061-2080",'70',names(wcF))
 names(wcF) <- gsub("2081-2100",'90',names(wcF))
 
 
 shape <- paste0("Final_maps/Vectors/", esc, 'noSavann.shp')
 G = vect(shape)
 
 wcF.ca <- mask(crop(wcF, G), G)
 
   lis <- c(wcF.ca[paste0('AC_', esc, '_', bands[band])], 
            wcF.ca[paste0('CM_', esc, '_', bands[band])], 
            wcF.ca[paste0('MI_', esc, '_', bands[band])], 
            wcF.ca[paste0('MP_', esc, '_', bands[band])])
   
 #indices <- grep("_1$", names(wcF.ca)) Just for the case of band 1L
 #lis <- wcF.ca[[indices]] Just for the case of band 1L
   
      }
      
      
   M <- stack(mean(lis)) 
   
   Bio_Fu <- paste0(rcps[rc],'_',years[ye],'_',bands[band])
   
   M_list[[Bio_Fu]] <- M
    
    }
  }
      
      for (i in 1:length(M_list)) {
        
        # Calculate mean
        WC_mean <- as.data.frame(raster::cellStats(x = M_list[[i]], stat = "mean"))
        
        # Calculate standard deviation
        WC_sd <- as.data.frame(raster::cellStats(x = M_list[[i]], stat = "sd"))
        
        # Rename SD column
        names(WC_sd) <- "Sd"
        
        # Combine mean and sd
        WC_mean$Sd <- WC_sd$Sd
        
        # Rename columns
        names(WC_mean) <- c("Mean", "Sd")
        
        # Assign to Bio_F
        Bio_F <- names(M_list)[i]  # Assuming M_list is named appropriately
        assign(Bio_F, WC_mean)
        
        # Add to results list
        results[[Bio_F]] <- WC_mean
        
      }
  
df <- do.call(rbind, results)

write.csv(df, 'Other_files/NicheStats_Future.csv', row.names = T)


# Use coverage ------------------------------------------------------------

Cov <- raster('Other_files/MapBiomas/bosque_coverage_2022.tif')

shape <- 'Final_maps/Vectors/current_suitability_noSavann.shp'
ca = vect(shape)

ca <- as(ca, "Spatial")
Cov.ca <- mask(crop(Cov, ca), ca)


#Frequency
Freq <- as.data.frame(raster::freq(Cov.ca))

Forest.coverage <- (sum(Freq[c(2, 4, 5), 2] ) * (9/10000))#3: Forest, 11: flooded grasslands 
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

#End of the script



