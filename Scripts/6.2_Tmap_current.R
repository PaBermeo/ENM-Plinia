#Carregamento de pacotes
pacman::p_load(dplyr, cartography, cowplot, geobr, ggplot2, ggpattern, ggspatial, gridGraphics, sf, sp, tmap, terra, raster, readr, rnaturalearth, viridis)

# Niche map ---------------------------------------------------

#Realized niche
RN <- st_read('Final_maps/Vectors/current_suitability_noSavann.shp')

#No biologically suitable 
NoSuit <- st_read('Final_maps/Vectors/current_suitability_Savann.shp')

N <- hatchedLayer(x = NoSuit, mode = "sfc", pattern = 'right2left', density = 5)
NS <- as_Spatial(N)
#NSu <- spTransform(NS, CRSobj = crs(4326))

#NSuits <- spTransform(NSuit, CRSobj = crs(4326))

#Load Occurrences data
Peruviana <- read_delim('Plinia_peruviana.csv', delim = ';', col_names = T) |> as.data.frame()
Pp <- Peruviana[- c(10, 19, 20, 59, 71), ]
wgs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
WGS84 <- sp::CRS(wgs)
occ <- sp::SpatialPointsDataFrame(Pp[, c('Longitude', 'Latitude')], Pp, proj4string = WGS84)

#Load states and countries polygons
SA <- st_read('Other_files/Vectors/World_Administrative_Divisions/World_Administrative_Divisions.shp')
states <- c('482', '491', '492', '496', '497', '498', '506', '510', '514', '523', '528', '529', '533', '535')
Br_state <- SA[SA$OBJECTID %in% states, ]

COUN <- st_read('Other_files/Vectors/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp')
country <- c('Argentina', 'Paraguay', 'Uruguay')
SA_coun <- COUN[COUN$COUNTRY %in% country, ]
AR <- COUN[COUN$COUNTRY == 'Argentina', ]

#Load Brazilian biomes
#BB <- st_read('Other_files/Vectors/Brazil_biomes/Brazil_biomes.shp')
#Outbiom <- c('Caatinga', 'Cerrado', "Mata Atlântica")
#biom <- BB[BB$name %in% Outbiom, ]

#MA biome
#MA <- st_read('Other_files/Vectors/Atlantic_Forest/MAmerged.shp')

# change to tmap mode
tmap::tmap_mode(mode = "plot")
tmap_options(check.and.fix = TRUE)


#Niche Map
{   tm_shape(RN, bbox = c(-57.8, -31, -36.6, -8)) +
    tm_fill(col = "blue", lwd = 1, alpha= 0.3) +
    tm_borders(col = "darkblue", lwd = 0.5) +
    #tm_text('ISO', size = 0.9, col = 'black', xmod=1) +
    #tm_shape(NSuit) +
    #tm_lines(col= 'grey30') +
    tm_shape(occ) +
    tm_symbols(size= 0.2, col= 'orange', alpha= 0.8) +
    tm_shape(Br_state) +
    tm_borders(col = 'gray50', lwd = 0.5) +
    tm_text('ISO_SUB', size = 1, col = 'black', fontfamily = 'sans', fontface='bold') +
    tm_shape(SA_coun) + #57.8, -31, -35, -7.8
    tm_borders(col = "black", lwd = 1, lty = 'dashed') +
    tm_text('ISO', size = 1, col = 'gray30', fontfamily = 'sans', fontface='bold', xmod =1.6) + 
  tm_shape(AR) +
   # tm_borders(col = "black") +
    tm_text('ISO', size = 1, col = 'gray30', fontfamily = 'sans', fontface='bold', xmod =10, ymod =9 ) +  
  tm_compass(position = c(0.9, 0.7), size= 3.2) + 
  tm_scale_bar(text.size = 0.6, position = c(0.4, 0), width = 0.2) + #scale bar
  tm_graticules(lines = FALSE, labels.rot = c(0, 90), labels.size = 0.8) +
  tm_add_legend(type = "symbol", 
                  labels = "Plinia filtered occurrences", 
                  col = "orange", 
                  lwd = 3,
                  size = 0.5
    ) +
  tm_add_legend(type = "line", 
                  labels = "Realized niche", 
                  col = "darkblue", 
                  lwd = 3
                ) +
    tm_layout(legend.position = c(0.55, 0.15),
              legend.hist.height = -0.5,
              inner.margins = c(0, 0, 0, 0),
              legend.title.size = 0.9,
              legend.text.size = 0.9,
              legend.width = 1
    )
  
Niche <- grid.grab()
}

SAC <- st_read('Other_files/Vectors/SouthAm/SouthAmerica.shp')

# Secondary map
mapa_SouthA <- tm_shape(SAC, box= c(-40, -41, -30, 12)) +
  tm_borders(col = 'gray40', lwd = 0.5) +
  tm_shape(RN, box= c(-40, -41, -30, 12)) +
  tm_fill(col = "blue", lwd = 1, alpha= 0.5) +
  tm_borders(col = "darkblue", lwd = 0.5) +
  tmap_options(check.and.fix = TRUE)

#Merge the two maps
Niche
print(mapa_SouthA, vp = grid::viewport(.1, .8, wi = .18, he = .2))


##Create shapefile
#extt <- ext(c(-57.8, -31, -36.6, -81))
#p <- as.polygons(extt)
#crs(p) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#writeVector(p, 'Other_files/Vectors/SouthAm/BR1.shp', overwrite=T)



#Altitude map
{Alt <- raster('Other_files/others_wc/wc2.1_30s/wc2.1_30s_elev.tif')

ShapeSA <- 'Other_files/Vectors/SouthAm/BR1.shp'
ShapeSA = vect(ShapeSA)
SA <- as(ShapeSA, "Spatial")


Altm <- terra::mask(crop(Alt, RN), RN)

  palette <- viridis_pal(begin = 0, end = 1, option = 'C', direction = -1)(10)
  tm_shape(Altm, , bbox = c(-57.8, -31, -36.6, -8)) +
  tm_raster(style = "cont", legend.reverse= T,
            #legend.format = list(text.align = 'right'),
            palette = palette, title = 'Elevation (m)', breaks=seq(0, 2000, 500)) +
  tm_shape(Br_state) +
  tm_borders(col = 'gray40', lwd = 0.5) +
  tm_shape(SA_coun) +
  tm_borders(col = "black", lwd = 1, lty = 'dashed') +
  tm_shape(RN) +
  tm_borders(col = "darkblue", lwd=1) +
  tm_graticules(lines = FALSE, labels.rot = c(0, 90), labels.size = 0.8) +
  tm_layout(legend.position = c("left", "top"),
            #legend.hist.height = 0, 
            inner.margins = c(0, 0, 0, 0), 
            legend.width = 2,
            legend.height = -0.5,
            legend.title.size = 0.9,
            legend.text.size = 0.9
  ) +
  tm_add_legend(type = "line", 
                labels = "Realized niche", 
                col = "darkblue", 
                lwd = 3 
  )
  
Altp <- grid.grab()

}



#Bio14 map
{
 Bio14 <- rast('G_variables/Set_4/current/bio_14.asc')
  Bio14 <- terra::mask(crop(Bio14, RN), RN)

  palette <- viridis_pal(begin = 0, end = 1, direction = -1)(10)
  tm_shape(Bio14, bbox = c(-57.8, -31, -36.6, -8)) +
  tm_raster(style = "cont", legend.reverse= T, palette = palette, title = 'Pp driest\nmonth (mm)', breaks=seq(0, 160, 40)) +
  tm_shape(Br_state) +
  tm_borders(col = 'gray40', lwd = 0.5) +
  tm_shape(SA_coun) +
  tm_borders(col = "black", lwd = 1, lty = 'dashed') +
  tm_shape(RN) +
  tm_borders(col = "red", lwd=1) +
    #tm_compass(position = c(0.9, 0.7)) + #North arrow size= 1.5, text.size = 0.55, 
    #tm_scale_bar(text.size = 0.7, position = c(0.3, 0.05)) + #scale bar
    tm_graticules(lines = FALSE, labels.rot = c(0, 90), labels.size = 0.8) +
    tm_layout(legend.position = c("left", "top"),
              #legend.hist.height = 0, 
              inner.margins = c(0, 0, 0, 0), 
              legend.width = 1.1,
              legend.height = -0.5,
              legend.title.size = 1,
              legend.text.size = 1
    ) +
    tm_add_legend(type = "line", 
                  labels = "Realized niche", 
                  col = "red", 
                  lwd = 3 
    )
  Biom14 <- grid.grab()
}

#Bio5 map
{Bio5 <- rast('G_variables/Set_4/current/bio_5.asc')
 Bio5 <- terra::mask(crop(Bio5, RN), RN)

    palette <- viridis_pal(begin = 0, end = 1, option = 'C', direction = 1)(10)
    tm_shape(Bio5, bbox = c(-57.8, -31, -36.6, -8)) +
    tm_raster(style = "cont",palette = palette, title = 'Max Temp warmest\nmonth (°C)', legend.reverse= T, breaks=seq(20, 40, 5)) +
    tm_shape(Br_state) +
    tm_borders(col = 'gray60', lwd = 0.5) +
    tm_shape(SA_coun) +
    tm_borders(col = "black", lwd = 1, lty = 'dashed') +
    tm_shape(RN) +
    tm_borders(col = "darkblue", lwd=1) +
    tm_graticules(lines = FALSE, labels.rot = c(0, 90), labels.size = 0.8) +
    tm_layout(legend.position = c("left", "top"),
              #legend.hist.height = 0, 
              inner.margins = c(0, 0, 0, 0), 
              legend.width = 1.1,
              legend.height = -0.5,
              legend.title.size = 1,
              legend.text.size = 1
    ) +
    tm_add_legend(type = "line", 
                  labels = "Realized niche", 
                  col = "darkblue", 
                  lwd = 3, 
    )
  Biom5 <- grid.grab()
}

#Bio3 map
{Bio3 <- rast('G_variables/Set_4/current/bio_3.asc')
  Bio3 <- terra::mask(crop(Bio3, RN), RN)
  
palette <- viridis_pal(begin = 0, end = 1, option = 'G')(10)

  tm_shape(Bio3) +
    tm_raster(style = "cont",palette = palette, title = 'Isothermality', legend.reverse= T, legend.format = list(text.align = 'center')) +
    tm_shape(RN, bbox = c(57.8, -31, -37, -7.8)) +
    tm_borders(col = "red", lwd=1) +
    tm_graticules(lines = FALSE,labels.col='white') +
    tm_layout(legend.position = c(0.5, 0),
              legend.title.size = 1.4,
              legend.text.size = 1.1,
              legend.hist.height = 0.1, #!!!!!!!!!!!!!
              inner.margins = c(0, 0, 0, 0), 
              legend.width = 1.0
    ) +
    tm_add_legend(type = "line", 
                  labels = "Realized niche", 
                  col = "red", 
                  lwd = 3, 
    )
  Biom3 
}

Figure_bio <- plot_grid(Niche, Altp, Biom14, Biom5, labels = 'auto', align= 'hv',  label_size = 15) #, Biom14, Biom5, ) #, sync=T

Figure_bio

ggsave(Figure_bio, filename = "Final_maps/Figure8.var-imp1.png", width = 12.2, height = 13.5, units = "in", dpi = 600)

#tmap_save(Figure_bio, "Final_maps/Bio_current.png", width = 17.6, heigh = 23.4, units='"cm"',  dpi = 600)





# Total pp & PET ----------------------------------------------------------


{
  Pp <- raster('Other_files/wc2.1_30s_bio/wc2.1_30s_bio_12.tif')

ShapeSA <- 'Other_files/Vectors/SouthAm/BR1.shp'
ShapeSA = vect(ShapeSA)
SA <- as(ShapeSA, "Spatial")

PPS <- terra::mask(crop(Pp, SA), SA)
palette <- viridis_pal(begin = 0, end = 1, direction = -1)(10)

tm_shape(PPS, bbox = c(-57.8, -31, -36.6, -8)) +
  tm_raster(style = "cont", legend.reverse= T, legend.format = list(text.align = 'center'), palette = palette, title = 'Annual precipitation\n(mm)') +
  tm_shape(Br_state, bbox = c(-57.8, -31, -36.6, -8)) +
  tm_borders(col = 'gray40', lwd = 0.5) +
  tm_shape(SA_coun, bbox = c(-57.8, -31, -36.6, -8)) +
  tm_borders(col = "black", lwd = 1, lty = 'dashed') +
  tm_shape(RN, bbox = c(-57.8, -31, -36.6, -8)) +
  tm_borders(col = "red", lwd=1) +
  #tm_compass(position = c(0.9, 0.7)) + #North arrow size= 1.5, text.size = 0.55, 
  #tm_scale_bar(text.size = 0.7, position = c(0.3, 0.05)) + #scale bar
  tm_graticules(lines = FALSE, labels.col='white', labels.rot = c(0, 90)) +
  tm_layout(legend.position = c(0.6, 0.02),
            legend.text.size = 0.9,
            legend.hist.height = 0.1, #!!!!!!!!!!!!!
            inner.margins = c(0, 0, 0, 0), 
            legend.width = 1
            #bg.color = 'black', attr.color = 'white', outer.bg.color = 'white'
  ) +
  tm_add_legend(type = "line", 
                labels = "Realized niche", 
                col = "red", 
                lwd = 3 
  )
Bio12 <- grid.grab()
}

{
  PET <- raster('Other_files/ENVIREM/current_30arcsec_annualPET.tif')
  
  ShapeSA <- 'Other_files/Vectors/SouthAm/BR1.shp'
  ShapeSA = vect(ShapeSA)
  SA <- as(ShapeSA, "Spatial")
  
  PETS <- terra::mask(crop(PET, SA), SA)
  palette <- viridis_pal(begin = 0, end = 1, direction = -1)(10)
  
  tm_shape(PETS, bbox = c(-57.8, -31, -36.6, -8)) +
    tm_raster(style = "cont", legend.reverse= T, legend.format = list(text.align = 'center'), palette = palette, title = 'Annual PET\n(mm)') +
    tm_shape(Br_state, bbox = c(-57.8, -31, -36.6, -8)) +
    tm_borders(col = 'gray40', lwd = 0.5) +
    tm_shape(SA_coun, bbox = c(-57.8, -31, -36.6, -8)) +
    tm_borders(col = "black", lwd = 1, lty = 'dashed') +
    tm_shape(RN, bbox = c(-57.8, -31, -36.6, -8)) +
    tm_borders(col = "red", lwd=1) +
    #tm_compass(position = c(0.9, 0.7)) + #North arrow size= 1.5, text.size = 0.55, 
    #tm_scale_bar(text.size = 0.7, position = c(0.3, 0.05)) + #scale bar
    tm_graticules(lines = FALSE, labels.col='white', labels.rot = c(0, 90)) +
    tm_layout(legend.position = c(0.6, 0.02),
              legend.text.size = 0.9,
              legend.hist.height = 0.1, #!!!!!!!!!!!!!
              inner.margins = c(0, 0, 0, 0), 
              legend.width = 1
              #bg.color = 'black', attr.color = 'white', outer.bg.color = 'white'
    ) +
    tm_add_legend(type = "line", 
                  labels = "Realized niche", 
                  col = "red", 
                  lwd = 3 
    )
  PETm <- grid.grab()
}

App_fig <- plot_grid(Bio12, PETm, labels = 'AUTO', align= 'h') #, Biom14, Biom5, ) #, sync=T

App_fig

ggsave(App_fig, filename = "Final_maps/APP_Bio12-PET.png", width = 13, height = 8.1, units = "in", dpi = 600)

