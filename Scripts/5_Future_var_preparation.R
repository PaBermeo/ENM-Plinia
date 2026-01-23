######################################
#### Future variables preparation ####
######################################

pacman::p_load(dplyr, geodata, sp, raster, terra)

# Download raster future --------------

geodata::cmip6_world("ACCESS-CM2", "245", "2041-2060", var = "bioc", res = 0.5, path = 'Rasters_wc')

gcms <- c("ACCESS-CM2", 'CanESM5', "MIROC6", "MPI-ESM1-2-HR")  !!!!!!!!!!!!!!!
rcps <- c("245", "370")
years <- c("2041-2060", "2061-2080", '2081-2100')


for (gc in 1:length(gcms)) {
  for (rc in 1: length(rcps)) {
    for (ye in 1: length(years)) {

            geodata::cmip6_world(model = gcms[gc], ssp = rcps[rc], time = years[ye], var = "bioc", res = 0.5, path = 'Other_files/Rasters_wc/Future')
    }
  }
}



# Geoprocessing calibration area ------------------------------------------

wcF <- rast(list.files(path='Other_files/Rasters_wc/Future', pattern = ".tif$", full.names = TRUE))

#Shapefile G area
shape <- "M/G_Plinia.shp"
G = vect(shape)
plot(G)

## Future distribution with worldclim Data 
wcF.ca <- mask(crop(wcF, G), G)
plot(wcF.ca[[1]])
res(wcF.ca)

#Change names
wcF1 <- wcF.ca

names(wcF1) <- gsub('wc2.1_30s_bioc_','',names(wcF1))
names(wcF1) <- gsub('ACCESS-CM2', 'AC',names(wcF1))
names(wcF1) <- gsub('MIROC6', 'MI',names(wcF1))
names(wcF1) <- gsub('MPI-ESM1-2-HR', 'MP',names(wcF1))
names(wcF1) <- gsub('CMCC-ESM2', 'CM',names(wcF1))
names(wcF1) <- gsub('ssp245', '2',names(wcF1))
names(wcF1) <- gsub('ssp370', '3',names(wcF1))
names(wcF1) <- gsub("2041-2060",'50',names(wcF1))
names(wcF1) <- gsub("2061-2080",'70',names(wcF1))
names(wcF1) <- gsub("2081-2100",'90',names(wcF1))


#Export results in 1 file
#terra::writeRaster(wcF, 'Other_files/Rasters_wc/Future/wcFca.tif', overwrite=T)

#Export results in multiple files
#stk <- stack(wcF)
#raster::writeRaster(stk, filename= file.path('Other_files/Rasters_wc/Future/', names(wcF1)), bylayer=TRUE,format="GTiff")

#setwd
setwd("Other_files/Rasters_wc/Future")

gcms <- c("AC", 'CM', "MI", "MP")
ssps <- c('2', "3")
years <- c('50', '70', '90')
bands <- c('3', '5', '14')

for (gc in 1:length(gcms)) {
  for (rc in 1: length(ssps)) {
    for (ye in 1: length(years)) {
      for (band in 1: length(bands)) {
   #wcF <- rast(list.files(pattern = "tif$", full.names = TRUE))
        
   bio = wcF1[paste0(gcms[gc],'_',ssps[rc],'_',years[ye],'_',bands[band])]
 
  assign(paste0('bio',bands[band]), bio)
    }
    
    Result <- stack(c(bio3, bio5, bio14))
    names(Result) <- c('bio_3', 'bio_5', 'bio_14')
    
    dir.create(paste0(gcms[gc],'_',ssps[rc],'_',years[ye]))
    
    dir <- (paste0(gcms[gc],'_',ssps[rc],'_',years[ye]))
    
    raster::writeRaster(Result, filename= file.path(dir, names(Result)), bylayer=TRUE, format="ascii", overwrite=T)
    
    }
  }
}
     
 
# Geoprocessing current ------------------------------------------
setwd("/Volumes/Elements")

wcC <- rast(list.files(path='Other_files/Rasters_wc/wc2.1_30s_bio', pattern = ".tif$", full.names = TRUE))

wcC <- c(wcC$wc2.1_30s_bio_3, wcC$wc2.1_30s_bio_5, wcC$wc2.1_30s_bio_14) 

#mask crop
wcC.G <- mask(crop(wcC, G), G)
plot(wcC.G[[1]])
res(wcC.G)

names(wcC.G) <- c('bio_3', 'bio_5', 'bio_14')

G_var <- stack(wcC.G)

dir.create('G_variables/current')
dir <- 'G_variables/Set_4/current'

wcC.G <- stack(wcC.G)

raster::writeRaster(wcC.G, filename= file.path(dir, names(wcC.G)), bylayer=TRUE, format="ascii", overwrite=T)


#End of the script

