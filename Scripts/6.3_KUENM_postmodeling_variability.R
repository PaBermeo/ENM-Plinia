
### Model Variability map


#Variability SSP analysis per year

#Future mean threshold (excluding Uruguaian Savanna)
GCM <- c('AC', 'CM', 'MI', 'MP')
SSP <- c('2', '3')
year <- c('50', '70', '90')

for (GC in 1: length(GCM)) {
  for (Sp in 1: length(SSP)) {
    for (ye in 1: length(year)) {
      
      pat = paste0(SSP[Sp],'_',year[ye],'.asc') 
      
      L <- rast(list.files(path="Final_Models/M_1_F_lqp_Set_4_E/", pattern =pat, full.names = T)) 
      
      M <- sd(L)
      
      stack(M_b)
      
      raster::writeRaster(fut, filename = paste0("Projection_threshold/",SSP[Sp],'_',year[ye],"SD.tif"), overwrite=T)
    }
  } 
}



# Option 2: modvar --------------------------------------------------------

sp_name <- "Plinia_peruviana"
fmod_dir <- "Final_Models"
is_swd <- FALSE
rep <- TRUE
format <- "asc"
project <- TRUE
curr <- "current"
emi_scenarios <- c("2", "3")
c_mods <- c('AC', 'CM', 'MI', 'MP')
ext_type <- c("E")
split <- 100
periods <- c('70', '90')
out_dir2 <- "Variation_from_sou"

kuenm_modvar(sp.name = sp_name, fmod.dir = fmod_dir, is.swd = is_swd,
             replicated = rep, format = format, project = project,
             current = curr, emi.scenarios = emi_scenarios,
             clim.models = c_mods, ext.type = ext_type, split.length = split,
             out.dir = out_dir2, time.periods= periods)

# End of the script
