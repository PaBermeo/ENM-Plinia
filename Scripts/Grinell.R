
if (!require("remotes")) {
  install.packages("remotes")
}

remotes::install_github("fmachados/grinnell")

library(grinnell)
library(readr)
library(raster)
library(terra)
library(sp)


# environmental layers
cv <- list.files(path='M_Grinnell/current', pattern= '.tif$', full.names = T, recursive=F) |> rast() 
#names(cv)= "wc2.1_2.5m_bio_1"  "wc2.1_2.5m_bio_10" "wc2.1_2.5m_bio_11" "wc2.1_2.5m_bio_12" "wc2.1_2.5m_bio_13" "wc2.1_2.5m_bio_14" "wc2.1_2.5m_bio_15" "wc2.1_2.5m_bio_16" "wc2.1_2.5m_bio_17" "wc2.1_2.5m_bio_2" "wc2.1_2.5m_bio_3"  "wc2.1_2.5m_bio_4"  "wc2.1_2.5m_bio_5"  "wc2.1_2.5m_bio_6"  "wc2.1_2.5m_bio_7" 

#Crop máscara
cv <- mask(crop(cv, G), G)
plot(cv[[1]])
#res(cv)
cv <- stack(cv)

#Save results
raster::writeRaster(cv, filename= 'M_Grinnell/current.tif', format="GTiff", overwerite=T)

################################!!! VERIFICAR QUE NO ESTÉN BIO 8,9,18,19!!!!!!!!!!!!!!!!!!!

#Máscara G_Plinia
G <- "M/G_Plinia.shp"
G = vect(G)
#plot(G)  

#Past data prepariton
files <- list.files(path = "M_Grinnell/2_5m/", pattern = ".bil$", full.names = TRUE, recursive = TRUE)|> rast() |> stack()
plot(files[[5]])

dir <- 'M_Grinnell/2_5m'
#Save results
raster::writeRaster(files, filename= file.path(dir, names(files)), bylayer=TRUE, format="GTiff", overwerite=T)

#Import  
file <- list.files(path = "M_Grinnell/2_5m", pattern = ".tif$", full.names = TRUE)|> rast() # |> stack()

#Crop máscara
pv <- mask(crop(file, G), G)
plot(pv[[1]])
#res(pv)
pv <- stack(pv)

#Save results
raster::writeRaster(pv, filename= 'M_Grinnell/past.tif', format="GTiff", overwerite=T)



# Arguments ---------------------------------------------------------------

#In the case of 2.5 min
#current_variables <- rast(list.files("M_Grinnell", pattern= 'current.tif$', full.names = TRUE))
#names(current_variables) <- c('bio_1', 'bio_10', 'bio_11', 'bio_12','bio_13', 'bio_14', 'bio_15', 'bio_16', 'bio_17', 'bio_2', 'bio_3', 'bio_4', 'bio_5', 'bio_6', 'bio_7')

#past_variables <- rast(list.files("M_Grinnell", pattern= 'past.tif$', full.names = TRUE))
#names(past_variables) <- c('bio_1', 'bio_10', 'bio_11', 'bio_12','bio_13', 'bio_14', 'bio_15', 'bio_16', 'bio_17', 'bio_2', 'bio_3', 'bio_4', 'bio_5', 'bio_6', 'bio_7')

#In the case of 30 seconds
current_variables <- rast(list.files("M_Grinnell/", pattern= 'c.tif$', full.names = TRUE))
#names(current_variables) <- c('bio_1', 'bio_10', 'bio_11', 'bio_12','bio_13', 'bio_14', 'bio_15', 'bio_16', 'bio_17', 'bio_2', 'bio_3', 'bio_4', 'bio_5', 'bio_6', 'bio_7')

data <-  read_delim('Plinia_peruviana.csv', delim = ';', col_names = T) |> as.data.frame()
output.directory <- 'M_Gr'

m <- M_simulationR(data, current_variables, starting_proportion = 0.5, 
              sampling_rule = "random", 
              barriers = NULL,# gerar as barreiras de dispersão
              scale = TRUE,
              center = TRUE, project = F, projection_variables = past_variables,
              dispersal_kernel = "normal", kernel_spread = 2, #kernel SD = 2
              max_dispersers = 4, # not 10.1016/j.gecco.2023.e02668
              suitability_threshold = 5,
              replicates = 10, dispersal_events = 25,
              access_threshold = 5, 
              simulation_period = 30, #roject = yes 
              stable_lgm = 10, #if project = yes 10.1016/j.gecco.2023.e02668
              transition_to_lgm = 5, #if project = yes
              lgm_to_current = 3, #if project = yes 
              stable_current = 10, #if project = yes
              scenario_span = 1, #if project = yes
              out_format = "GTiff", set_seed = 1,
              write_all_scenarios = FALSE, output_directory = output.directory,
              overwrite = T)

#map frequency of accesibility
terra::plot(m$A_mean, main='Mean accessibiliy frequency')

## accessible areas in vector format
lims <- terra::ext(m$A_mean)[]
terra::plot(m$A_polygon, main = "Accessed areas", xlim = lims[1:2], 
            ylim = lims[3:4])

M <- "M_Grinn_4/acc_area.shp"
M = vect(M)
plot(M)

plot(m$A_polygon)

R <- raster('M_Grinn/Suitability_results/suitability.tif')
plot(R)
