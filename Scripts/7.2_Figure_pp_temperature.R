
pacman::p_load(ggbreak, gridExtra, ggplot2, dplyr, patchwork, readr, raster, terra)


# Precipitation plot ------------------------------------------------------


# Read data
df <- read_delim("Other_files/wc_bion/pp_bion.csv", delim = ";")

relevant_parameters <- c("Wettest month", "Driest month", "Montly mean") #"Annual precipitation", 

df_filtered <- df %>% filter(Parameter %in% relevant_parameters)



# Year as a factor
df_filtered$Year <- factor(df_filtered$Year, levels= c('Current', '2050', '2070', '2090'))

df_filtered$Parameter2 <- reorder(df_filtered$Parameter, df_filtered$mm)

# plot
pp <- ggplot(df_filtered, aes(x = Year, y = mm, group = interaction(SSP2, Parameter), color = SSP2)) +
  geom_line(aes(linetype = SSP2), show.legend = F) +
  geom_point() +
  scale_linetype_manual(values=c('blank', "solid", "dashed")) +
  geom_errorbar(aes(ymin = mm - sd, ymax = mm + sd), width = 0.2) +
  labs(title = " ",
       x = " ",
       y = "Precipitation (mm)",
       color = "SSP Scenario") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_blank()) +
  guides(linetype = guide_legend(title = "Parameter")) +
  #scale_y_break(c(300, 1250) ) +
  scale_color_discrete(guide = FALSE) +
  facet_wrap( ~ Parameter2, axes = "all") #scale's= 'free',
  #
pp


# Temperature plot --------------------------------------------------------


# Read data
data <- read_delim("Other_files/wc_bion/temp_bion.csv", delim = ";")

param <- c("Mean",  "Max warmest month", "Min coldest month")

filt <- data %>% filter(Parameter %in% param)



# Year as a factor
filt$Year <- factor(filt$Year, levels= c('Current', '2050', '2070', '2090'))

#Reorder
filt$Parameter2 <- reorder(filt$Parameter, filt$C)

# plot
temp <- ggplot(filt, aes(x = Year, y = C, group = interaction(SSP2, Parameter), color = SSP2)) +
  geom_line(aes(linetype = SSP2),  show.legend = T) +
  geom_point() +
  scale_linetype_manual(values=c('blank', "solid", "dashed")) +
  geom_errorbar(aes(ymin = C - sd, ymax = C + sd), width = 0.2) +
  labs(title = " ",
       x = "Year",
       y = "Temperature (°C)",
       color = "SSP Scenario") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(linetype = guide_legend(title = " ")) +
  scale_color_discrete(guide = FALSE) +
  facet_wrap( ~ Parameter2)
  
temp


# Final plot --------------------------------------------------------------


p_t <- grid.arrange(pp, temp, nrow=2)

#Save plot
ggsave(plot = p_t, filename= 'Plots/pp_temp.png', dpi = 600, width =10 , height = 9, units = 'in')




# Plot mean and median ----------------------------------------------------

rcps <- c(2L , 3L)
years <- c("50", "70", '90')
bands <- c(5L, 6L, 12L, 13L, 14L) # 1L

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
      
      #indices <- grep("_1$", names(wcF.ca)) #Just for the case of band 1L
      #lis <- wcF.ca[[indices]] #Just for the case of band 1L
      
    M <- stack(mean(lis)) 
    
    Bio_Fu <- paste0(rcps[rc],'_',years[ye],'_',bands[band])
    
    M_list[[Bio_Fu]] <- M
  
    }  
  }
}


for (name in names(M_list)) {
  raster_obj <- M_list[[name]]
  

  output_path <- file.path("Other_files/Rasters_wc/Future/Niche_caract", paste0(name, ".tif"))
  
  writeRaster(raster_obj, filename = output_path, format = "GTiff", overwrite = TRUE)
  
}



# Load bioclim var future -------------------------------------------------


SSP <- c(2L, 3L)
YR <- c(50L, 70L, 90L)


for (Sp in 1: length(SSP)) {
  for (ye in 1: length(YR)) {
    
    sp_yr <- paste0(SSP[Sp], '_', YR[ye])
    sl <- paste0('20',YR[ye])
    
    Bio14 <- raster(paste0('Final_maps/Bio_14/', sp_yr, '_14.asc'))
    
    ca <- raster(paste0('Projection_threshold/', sp_yr, 'noSavann.tif'))
    ex2 <- extent(ca)
    
    resolution <- 0.008333333
    
    r <- raster(extent(ex2), res = resolution)
    projection(r) <- projection(Bio14)
    
    Bio14r <- resample(ca, Bio14, method = "bilinear")
    
    binary_points <- as.data.frame(rasterToPoints(Bio14r, fun = function(x) x == 1)) 
    
    set.seed(17)
    bp <- sample_n(binary_points, size = 2000, replace = F)
    
    coordinates(bp) <- ~x + y
    
    projection(bp) <- projection(Bio14r)
    Bio14_E <- extract(Bio14, bp)
    Bio14_r <- data.frame(coordinates(bp), bio14 = Bio14_E)
    
    Bio14_r[ , 4] <- sl   
    Bio14_r[ , 5] <- paste0('SSP', SSP[Sp])
    
    assign(paste0('F', sp_yr), Bio14_r)
    
  }
}

Bio14data <- rbind(Bio14_p, F2_50, F3_50, F2_70, F3_70, F2_90, F3_90)

#Bio14data$V4 <- factor(Bio14data$V4, levels= c('Current', '2050', '2070', '2090'))

#Bio14data$V5 <- factor(Bio14data$V5, levels= c('Current', 'SSP3', 'SSP2'))

ggplot(Bio14data, aes(x = V4, y = bio14, fill = V5)) +
  introdataviz::geom_split_violin(alpha = .4) +
  geom_boxplot(width = .2, alpha = .6, show.legend = F, outliers=F) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, position = position_dodge(.175)) +
  scale_x_discrete(name = "Year") +
  theme_minimal()






