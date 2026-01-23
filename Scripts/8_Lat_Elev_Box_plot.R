
pacman::p_load(dplyr, ggplot2, grid, gridExtra, raster, terra)

#Raster current latitude
{
  model <- raster('Projection_threshold/current_noSavann.tif') |> rasterToPoints()
  
  modelp1 <- as.data.frame(model[complete.cases(model), ] ) |> filter(mean == 1) #Restrict only southern region
  
  set.seed(17)
  P_per <- sample_n(modelp1, size = 2000, replace = F)
  
  P_per[ , 4] <- 'Current'
}

# Future testing latitude

SSP <- c(2, 3)
YR <- c(50, 70, 90)

for (sp in 1: length(SSP)) {
  for (yr in 1: length(YR)) {
    
    sp_yr <- paste0(SSP[sp], '_', YR[yr])
    sl <- paste0('20',YR[yr])
    
    modelF <- raster(paste0("Projection_threshold/", sp_yr, "noSavann.tif")) |> rasterToPoints()
    modelF1 <- as.data.frame(modelF[complete.cases(modelF), ] ) |> filter(mean == 1)
    
    set.seed(17)
    P_perF <- sample_n(modelF1, size = 2000, replace = F)
    
    P_perF[ , 4] <- sl
    
    assign(paste0('L', sp_yr), P_perF)
    
    
  }
}

LSP2 <- rbind(P_per, L2_50, L2_70, L2_90)



# Latitude plot

LSP2$V4 <- factor(LSP2$V4, levels= c('Current', '2050', '2070', '2090'))

set.seed(17)
LSSP2 <- ggplot(LSP2) +
  aes(y= y, x= V4) +
  geom_jitter(alpha =.5,
              height = 0,
              width = .25) +
  aes(col = V4) +
  geom_boxplot(alpha= .25) +
  #geom_violin(linewidth=0.5, alpha= .5) +
  aes(fill= V4) +
  scale_colour_manual(values = c('#7b3294',"#6a51a3",'#008837',"#6DCD59FF")) +
  scale_fill_manual(values = c('#7b3294',"#6a51a3",'#008837',"#6DCD59FF")) +
  geom_point(stat = 'summary', fun = 'mean', shape = 4, size = 2, color = 'black') +
  xlab("") +
  ylab("Latitude (WGS84)\nSSP2") +
  theme_bw() +
  labs(col = "") +
  #annotate("text", x  = 0.5, y = 8650000 , size=7, label = "(b)") +
  theme(axis.title.x = element_text(size = rel(1.65),colour="black")) +
  theme(axis.title.y = element_text(size = rel(1.65),colour="black")) +
  theme(axis.text.x = element_text(size = rel(1.65),colour="black")) +
  theme(axis.text.y = element_text(size = rel(1.65), colour="black")) +   
  theme(legend.position = "none")


LSP3 <- rbind(P_per, L3_50, L3_70, L3_90)

LSP3$V4 <- factor(LSP3$V4, levels= c('Current', '2050', '2070', '2090'))


{set.seed(17)
  LSPP3 <- ggplot(LSP3) +
    aes(y= y, x= V4) +
    geom_jitter(alpha =.5,
                height = 0,
                width = .25) +
    aes(col = V4) +
    geom_boxplot(alpha= .25) +
    #geom_violin(linewidth=0.5, alpha= .5) +
    aes(fill= V4) +
    scale_colour_manual(values = c('#7b3294',"#6a51a3",'#008837',"#6DCD59FF")) +
    scale_fill_manual(values = c('#7b3294',"#6a51a3",'#008837',"#6DCD59FF")) +
    geom_point(stat = 'summary', fun = 'mean', shape = 4, size = 2, color = 'black') +
    xlab("") +
    ylab("Latitude (WGS84)\nSSP3") +
    theme_bw() +
    labs(col = "") +
    #annotate("text", x  = 0.5, y = 8650000 , size=7, label = "(b)") +
    theme(axis.title.x = element_text(size = rel(1.65),colour="black")) +
    theme(axis.title.y = element_text(size = rel(1.65),colour="black")) +
    theme(axis.text.x = element_text(size = rel(1.65),colour="black")) +
    theme(axis.text.y = element_text(size = rel(1.65), colour="black")) +   
    theme(legend.position = "none")
  LSPP3
}

final_plot <- grid.arrange(LSSP2, LSPP3, nrow=2)

#ggsave(filename = 'Plots/Boxplot_altitude.png', plot = final_plot, width = 21, height = 16, units = "in", dpi = 500)



# Raster current altitude -------------------------------------------------

#Import Raster altitude
#Elev <- raster('Other_files/elevation.tif')
#Elev <- raster("http://www.dpi.inpe.br/amb_data/Brasil/altitude_br.asc") # Ambdata website 30 arc-sec res
{ set.seed(17)
  Elev <- raster('Other_files/wc2.1_30s_bio/wc2.1_30s_elev.tif')
  
  ca <- raster('Projection_threshold/current_noSavann.tif')
  ex2 <- extent(ca)
  
  resolution <- 0.008333333
  
  r <- raster(extent(ex2), res = resolution)
  projection(r) <- projection(Elev)
  
  Elevr <- resample(ca, Elev, method = "bilinear")
  
  binary_points <- as.data.frame(rasterToPoints(Elevr, fun = function(x) x == 1)) ##|> filter( y <= -25) 
  
  set.seed(17)
  bp <- sample_n(binary_points, size = 2000, replace = F)
  
  coordinates(bp) <- ~x + y
  
  projection(bp) <- projection(Elevr)
  Elev_v <- extract(Elev, bp)
  Elev_p <- data.frame(coordinates(bp), elevation = Elev_v)
  
  Elev_p[ , 4] <- 'Current' 
  
}

# Future testing altitude

SSP <- c(2, 3)
YR <- c(50, 70, 90)

for (sp in 1: length(SSP)) {
  for (yr in 1: length(YR)) {
    
    sp_yr <- paste0(SSP[sp], '_', YR[yr])
    sl <- paste0('20',YR[yr])
    
    ca <- raster(paste0('Projection_threshold/', sp_yr, '.tif'))
    ex2 <- extent(ca)
    
    resolution <- 0.008333333
    
    r <- raster(extent(ex2), res = resolution)
    projection(r) <- projection(Elev)
    
    Elevr <- resample(ca, Elev, method = "bilinear")
    
    binary_points <- as.data.frame(rasterToPoints(Elevr, fun = function(x) x == 1)) # |> filter( y <= -25) 
    
    set.seed(17)
    bp <- sample_n(binary_points, size = 2000, replace = F)
    
    coordinates(bp) <- ~x + y
    
    projection(bp) <- projection(Elevr)
    Elev_v <- extract(Elev, bp)
    Elev_f <- data.frame(coordinates(bp), elevation = Elev_v)
    
    Elev_f[ , 4] <- sl        
    assign(paste0('F', sp_yr), Elev_f)
    
  }
}


ESP2 <- rbind(Elev_p, F2_50, F2_70, F2_90)


  # Altitude plot

ESP2$V4 <- factor(ESP2$V4, levels= c('Current', '2050', '2070', '2090'))

{set.seed(17)
  ESSP2 <- ggplot(ESP2) +
    aes(y= elevation, x= V4) +
    geom_jitter(alpha =.5,
                height = 0,
                width = .25) +
    aes(col = V4) +
    geom_boxplot(alpha= .25) +
    # geom_violin(linewidth=0.5, alpha= .5) +
    aes(fill= V4) +
    scale_colour_manual(values = c('#7b3294',"#6a51a3",'#008837',"#6DCD59FF")) +
    scale_fill_manual(values = c('#7b3294',"#6a51a3",'#008837',"#6DCD59FF")) +
    geom_point(stat = 'summary', fun = 'mean', shape = 4, size = 2, color = 'black') +
    xlab("") +
    ylab("Elevation (m)\nSSP2") +
    theme_bw() +
    labs(col = "") +
    #annotate("text", x  = 0.5, y = 8650000 , size=7, label = "(b)") +
    theme(axis.title.x = element_text(size = rel(1.65),colour="black")) +
    theme(axis.title.y = element_text(size = rel(1.65),colour="black")) +
    theme(axis.text.x = element_text(size = rel(1.65),colour="black")) +
    theme(axis.text.y = element_text(size = rel(1.65), colour="black")) +   
    theme(legend.position = "none")
}

ESP3 <- rbind(Elev_p, F3_50, F3_70, F3_90)

ESP3$V4 <- factor(ESP3$V4, levels= c('Current', '2050', '2070', '2090'))


{set.seed(17)
  ESPP3 <- ggplot(ESP3) +
    aes(y= elevation, x= V4) +
    geom_jitter(alpha =.5,
                height = 0,
                width = .25) +
    aes(col = V4) +
    #geom_violin(linewidth=0.5, alpha= .5) +
    geom_boxplot(alpha= .25) +
    aes(fill= V4) +
    scale_colour_manual(values = c('#7b3294',"#6a51a3",'#008837',"#6DCD59FF")) +
    scale_fill_manual(values = c('#7b3294',"#6a51a3",'#008837',"#6DCD59FF")) +
    geom_point(stat = 'summary', fun = 'mean', shape = 4, size = 2, color = 'black') +
    xlab("") +
    ylab("Elevation (m)\nSSP3") +
    theme_bw() +
    labs(col = "") +
    #annotate("text", x  = 0.5, y = 8650000 , size=7, label = "(b)") +
    theme(axis.title.x = element_text(size = rel(1.65),colour="black")) +
    theme(axis.title.y = element_text(size = rel(1.65),colour="black")) +
    theme(axis.text.x = element_text(size = rel(1.65),colour="black")) +
    theme(axis.text.y = element_text(size = rel(1.65), colour="black")) +   
    theme(legend.position = "none")
  ESPP3
}

final_plot2 <- grid.arrange(ESSP2, ESPP3, nrow=2)

ggsave(filename = 'Plots/Boxplot_elevation.png', plot = final_plot, width = 21, height = 16, units = "in", dpi = 500)

plot_L_A <- grid.arrange(LSSP2, ESSP2, LSPP3, ESPP3, nrow=2, ncol=2)

ggsave(filename = 'Plots/Boxplot_Lat_Elevat.png', plot = plot_L_A, width = 14.1, height = 10.9, units = "in", dpi = 500)
