
#Carregamento de pacotes
pacman::p_load(cowplot, dplyr, geobr,ggplot2, ggspatial, ggnewscale, grid, sf, sp, tmap, terra, raster, readr, rnaturalearth)


# Option 1 ggplot ---------------------------------------------------------

#Load shapefile South America countries
C <- st_read('Other_files/Vectors/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp')
countries <- cbind(C, st_coordinates(st_centroid(C)))

#Load biomas shape
biomas <- geobr::read_biomes(showProgress = FALSE) |> 
  dplyr::filter(name_biome != "Sistema Costeiro") |>  
  dplyr::rename(nome_bioma = name_biome,
                codigo_bioma = code_biome,
                ano = year)

# Load occs sf points
Data <- read_delim("Other_files/Occs/Plinia_peruviana_complete.csv", delim = ";", escape_double = FALSE, col_types = cols(Lat = col_number(), Long = col_number()), na = "null", trim_ws = TRUE)

wgs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
WGS84 <- sp::CRS(wgs)

occ <- st_as_sf(sp::SpatialPointsDataFrame(Data[, c('Long', 'Lat')], Data, proj4string = WGS84))

Occs <- ggplot() +
  geom_sf(data = occ, mapping = aes(fill = Forest), color = "black", size=1) +
  scale_fill_manual(values = c("coral4", "darkgreen")) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true",
       pad_x = unit(0, "cm"), pad_y = unit(.5, "cm"),
      style = north_arrow_fancy_orienteering) +
  labs(title = "Current suitability", fill = "Legend", x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.background = element_rect(colour = "black"),
        axis.title = element_text(size = 10, face = "plain"),
        axis.text.y = element_text(angle = 90, hjust = .4),
        legend.position.inside = c(0.8, .2)) +
  coord_sf() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0)))



# Option 2 tmap -----------------------------------------------------------


#Load biomas shape
biomas <- geobr::read_biomes(showProgress = FALSE) |> 
  dplyr::filter(name_biome != "Sistema Costeiro") |>  
  dplyr::rename(nome_bioma = name_biome,
                codigo_bioma = code_biome,
                ano = year)

#Load South America shape
SouthA <- st_read('Other_files/Vectors/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp')
country <- c('Argentina', 'Paraguay', 'Uruguay')
SA_coun <- SouthA[SouthA$COUNTRY %in% country, ]
BR <- SouthA[SouthA$COUNTRY == 'Brazil', ]

#
SA <- st_read('Other_files/Vectors/SouthAm/SouthAmerica.shp')

# Load occs sf points
Data <- read_delim("Other_files/Occs/Plinia_peruviana_complete.csv", delim = ";", escape_double = FALSE, col_types = cols(Lat = col_number(), Long = col_number()), na = "null", trim_ws = TRUE)

wgs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
WGS84 <- sp::CRS(wgs)
occ <- sp::SpatialPointsDataFrame(Data[, c('Long', 'Lat')], Data, proj4string = WGS84)

#load Elevation raster
Bio1 <- rast('Other_files/wc2.1_30s_bio/wc2.1_30s_bio_1.tif')

#Create shapefile
#extt <- ext(c(-62, -34, -33, 1))
#p <- as.polygons(extt)
#crs(p) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#writeVector(p, 'Other_files/Vectors/SouthAm/BR.shp', overwrite=T)


Bio1m <- mask(crop(Bio1, ShapeSA), ShapeSA)
palette <- hcl.colors(n = 10, palette = 'Viridis')

#
ShapeSA <- st_read('Other_files/Vectors/SouthAm/BR.shp')
#plot(ShapeSA)

# change to tmap mode
tmap::tmap_mode(mode = "plot")

#Principal Map
{tm_shape(Bio1m) +
  tm_raster(style = "cont", palette = palette, title = 'T mean (°C)', legend.reverse= T, legend.format = list(text.align = 'center')) +
  tm_shape(biomas, bbox = c(-61, -34, -32, 0)) +
  tm_borders(col = "grey20", lwd=0.7) +
  tm_shape(occ) +
  tm_symbols(col= 'Occurrences', size=0.2, pal = c("orange", "white"), shape= 21) +
  tm_shape(SA_coun, bbox = c(-61, -34, -32, 0)) +
  tm_borders(col = "black", lwd = 0.7, lty = 'dashed') +
  tm_compass(position = c(0.9, 0.9)) + #North arrow
  tm_scale_bar(position = c(0.4, 0), text.size = .6) + #scale bar
  tm_graticules(lines = F, labels.rot = c(0, 90)) +
  tm_layout(legend.position = c(0.78, 0.01),
            legend.text.size = 0.9,
            legend.hist.height = 0.5,
            inner.margins = c(0, 0, 0, 0),
            legend.width = 1.0
  )
occs <- grid.grab()
}

#tmap::tmap_save(tm = occs, filename = 'Final_maps/Occs.png', dpi = 600)

#mapa_SouthA <- tm_shape(SA, box= c()) +
#  tm_borders('grey30', lwd= 0.9) +
#  tm_shape(BR) +
#  tm_fill("#00B358") +
#  tm_shape(ShapeSA) +
#  tm_borders('black')


  tm_shape(biomas, bbox = c(-78, -35, -33, 10)) +
  tm_polygons(col = "nome_bioma",
              pal = c("#8c510a", "#d8b365", "#f6e8c3", "#1b7837", "#5ab4ac", '#7fbf7b'),
              border.col = "black",
              lwd= 0.07,
              title = 'Brazilian biomes') +
  tm_shape(SouthA) +
  tm_borders(col = "grey", lwd = 0.5) +
  tm_shape(ShapeSA) +
  tm_borders('black') +
  #tm_compass() +
  #tm_scale_bar(text.size = .6) +
  tm_graticules(lines = F, labels.rot = c(0, 90)) +
  tm_layout(legend.frame = F,
            legend.position = c("left", "bottom"),
            legend.title.fontface = "bold") +
  tm_add_legend(type = "line", 
                  labels = "National boundaries", 
                  col = "grey", 
                  lwd = 2.5 
    )

mapbiomes <- grid.grab()


#Save map

App_fig <- plot_grid(mapbiomes, occs, labels = 'AUTO', align= 'h')
App_fig

ggsave(App_fig, filename = "Plots/Fig_biomes-occs.png", width = 12.5, height = 8.1, units = "in", dpi = 500)



