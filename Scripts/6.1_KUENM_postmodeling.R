#Post-modeling
pacman::p_load(cowplot, dplyr, ggplot2, ggnewscale, ggpattern, grid, gridExtra, ggspatial, readr, readxl, raster, sf,  stars, terra)


# Current threshold-decision and current threshold process --------------------------------
InRast <- raster('Final_models/M_1_F_lqp_Set_4_E/Plinia_peruviana_current_avg.asc')
Occur <- read_excel('Plinia.xlsx')
Occur <-  Occur[,-1]
ExtRast <- extract(InRast, Occur) #Extract values from occs points
Occur1 <- sort(ExtRast) #numeric order
PercentThreshold <- 0.30 #Excluding 30% of total presence data
RclVal  <-  Occur1[round(length(Occur1) * PercentThreshold) + 1]
RclVal


C <- rast(list.files(path="Final_Models/M_1_F_lqp_Set_4_E/", pattern ='current.asc', full.names = T)) 

M <- mean(C)

M_b <- M >= RclVal
plot(M_b)
stack(M_b)
raster::writeRaster(M_b, filename = "Projection_threshold/current_Total.tif", overwrite=T)


#Mask real ecoregions where Plinia is not biologically suitable

#Load shape
ecor <- 'Other_files/Vectors/WWF_ecoregions/official/wwf_terr_ecos.shp'
ecor = vect(ecor)
ecor <- as(ecor, 'Spatial')

Sav <- vect(ecor[ecor$OBJECTID=="3027",]) 
current <- mask(M_b, Sav) 


stack(current)
raster::writeRaster(current, filename = "Projection_threshold/current_Savann.tif", overwrite=T) 


# Future mean threshold ---------------------------------------------------

#Total area
GCM <- c('AC', 'CM', 'MI', 'MP')
SSP <- c('2', '3')
year <- c('50', '70', '90')

for (GC in 1: length(GCM)) {
  for (Sp in 1: length(SSP)) {
   for (ye in 1: length(year)) {

pat = paste0(SSP[Sp],'_',year[ye],'.asc') 
  
L <- rast(list.files(path="Final_Models/M_1_F_lqp_Set_4_E/", pattern =pat, full.names = T)) 

M <- mean(L)

M_b <- M >= RclVal

stack(M_b)

raster::writeRaster(M_b, filename = paste0("Projection_threshold/",SSP[Sp],'_',year[ye],".tif"))
   }
  } 
 }


#Future mean threshold (excluding Uruguaian Savanna)


for (GC in 1: length(GCM)) {
  for (Sp in 1: length(SSP)) {
    for (ye in 1: length(year)) {
      
      pat = paste0(SSP[Sp],'_',year[ye],'.asc') 
      
      L <- rast(list.files(path="Final_Models/M_1_F_lqp_Set_4_E/", pattern =pat, full.names = T)) 
      
      M <- mean(L)
      
      M_b <- M >= RclVal
      
      fut <- mask(M_b, Sav, inverse= T) 
      
      stack(fut)
      
      raster::writeRaster(fut, filename = paste0("Projection_threshold/",SSP[Sp],'_',year[ye],"noSavann.tif"))
    }
  } 
}




# Suitability evaluation --------------------------------------------------

### Biomas Br map
{geo_vetor_biomas <- geobr::read_biomes(showProgress = FALSE) %>%
  dplyr::filter(name_biome != "Sistema Costeiro") %>% 
  dplyr::rename(nome_bioma = name_biome,
                codigo_bioma = code_biome,
                ano = year)

##Import shapefile South_Am
shape <- 'Other_files/Vectors/SouthAm/SouthAmerica.shp'
SouthA = vect(shape)
crs(SouthA) <- '+proj=longlat +datum=WGS84 +no_defs'
Br <- SouthA[SouthA$Name == 'BRAZIL']
#'ARGENTINA'
}


# Create current suitability shape ----------------------------------------

#Create niche current shape 
tif <- read_stars("Projection_threshold/current_Total.tif")
sf <- st_as_sf(tif, merge = T)
suit <- sf[sf$current_Total.tif == 1, ]
#plot(suit)

#dir.create('Final_maps')
#dir.create('Final_maps/Vectors')

st_write(suit, 'Final_maps/Vectors/current_suitability_Total.shp')

shape <- 'Final_maps/Vectors/current_suitability_Total.shp' 
ca = vect(shape)

#Create niche current Savanna shape 
tif <- read_stars("Projection_threshold/current_Savann.tif")
sf <- st_as_sf(tif, merge = T)
suit <- sf[sf$current_Savann.tif == 1, ]
#plot(suit)

st_write(suit, 'Final_maps/Vectors/current_suitability_Savann.shp')



# Creating map suitability ------------------------------------------------


#Load shapefile
{SA <- st_read('Other_files/Vectors/World_Administrative_Divisions/World_Administrative_Divisions.shp')

#Select BR states
BA <- SA[SA$OBJECTID == '482', ]
ES <- SA[SA$OBJECTID == '489', ]
MG <- SA[SA$OBJECTID == '498', ]
PR <- SA[SA$OBJECTID == '506', ]
RJ <- SA[SA$OBJECTID == '514', ]
RS <- SA[SA$OBJECTID == '523', ]
SC <- SA[SA$OBJECTID == '528', ]
SP <- SA[SA$OBJECTID == '529', ]

#Select countries
C <- st_read('Other_files/Vectors/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp')
#BR <- C[C$COUNTRY == 'Brazil', ]
countries <- cbind(C, st_coordinates(st_centroid(C)))

#Select total shapefile

#Select total shapefile
#Total <- st_read('Final_maps/Vectors/current_suitability_Total.shp')
Sav <- st_read('Final_maps/Vectors/current_suitability_Savann.shp')

# Mapping current suitability
tif <- read_stars("Projection_threshold/current_noSavann.tif")
sf <- st_as_sf(tif, merge = T)
#plot(sf)

sf <- sf |> mutate(current_noSavann.tif = ifelse(current_noSavann.tif == 0, 'Biologically unsuitable', 'Realized niche')) #modify according to the raster!

sf <- sf |> filter(current_noSavann.tif == 'Realized niche')


# Load occs sf points
#Data <- read_delim("Other_files/Occs/Plinia_peruviana_Forest.csv", delim = ";", escape_double = FALSE, col_types = cols(Lat = col_number(), Long = col_number()), na = "null", trim_ws = TRUE)

Data <- read_delim('Plinia_peruviana.csv', delim = ';', col_names = T) |> as.data.frame()
#Remove extreme points
Data <- Data[- c(10, 19, 20, 59, 71), ]

wgs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
WGS84 <- sp::CRS(wgs)

occ <- st_as_sf(sp::SpatialPointsDataFrame(Data[, c('Longitude', 'Latitude')], Data, proj4string = WGS84))

occ <- occ %>%
  mutate(Species = ifelse(Species == "Plinia peruviana", "Occurrences", Species))
}


# Future map suitability full distribution --------------------------------


tif <- read_stars("Projection_threshold/current_noSavann.tif")
sf <- st_as_sf(tif, merge = T)
#plot(sf)

sf <- sf |> mutate(current.tif = ifelse(current_noSavann.tif == 1, 'Current', ' '))

#Graphs in loop
SSP <- c('2', '3')
year <- c('50', '70', '90')


  for (Sp in 1: length(SSP)) {
    for (ye in 1: length(year)) {
      
SYt <- paste0(SSP[Sp],'_',year[ye],'noSavann.tif')         
SY <- paste0(SSP[Sp],'_',year[ye])


FUT <- st_as_sf(read_stars(paste0('Projection_threshold/',SYt)), merge = T)
#plot(sf)

names(FUT) <- c('a', 'geometry')

FUT <- FUT |> mutate(a = ifelse(a == 1, 'Suitable', 'Unsuitable'))

FUT <- FUT %>%
  mutate(border_color = ifelse(a == "green", "green", "white"))

suit <- ggplot() +
  geom_sf(data = sf, mapping = aes(fill = current.tif), color = "gray100", linewidth=1e-20) +
  scale_fill_manual(values = c("00FFFFFF", "gray30")) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = FUT, mapping = aes(fill = a, color = border_color), linewidth=1e-20, alpha=0.5) +
  scale_fill_manual(values = c("green", "gray100")) +  
  labs(title = paste0('SSPs ',SSP[Sp],' 20',year[ye]), fill = "Legend", x = " ", y = " ") +
  scale_color_identity() +
  theme_bw() +
  theme(title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = .4),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 10, face = "plain"),
        legend.position = 'none',
        panel.grid.major = element_blank()
        ) +#c(0.8, 0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0)))

#Adding BR states
state <- suit + geom_sf(data=BA, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=ES, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=MG, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=PR, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=RJ, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=RS, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=SC, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=SP, fill = NA, color = 'gray40', linewidth=0.4) 

#adding country boundaries and create last object
future_sutability <- state + geom_sf(data=countries, fill = NA, color= 'black', linewidth=0.4) + coord_sf(default_crs = sf::st_crs(4326), xlim = c(-57, -38), ylim = c(-32, -12), expand = T)

assign(paste0('S',SSP[Sp],'_',year[ye]), future_sutability)
    } 
  }


 #Adding scale and north arrow
suitm <- ggplot() +
  geom_sf(data = sf, mapping = aes(fill = current.tif), color = "gray100", linewidth=1e-20) +
  scale_fill_manual(values = c("00FFFFFF", "gray30")) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = FUT, mapping = aes(fill = a, color = border_color), linewidth=1e-20, alpha=0.5) +
  scale_fill_manual(values = c("green", "gray100")) +  
  labs(title = 'SSPs 3 2090', fill = "Legend", x = " ", y = " ") +
  scale_color_identity() +
  theme_bw() +
  theme(title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = .4),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 10, face = "plain"),
        legend.position = 'none',
        panel.grid.major = element_blank()) +#c(0.8, 0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0))) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.5, "cm"),
                         style = north_arrow_fancy_orienteering)

state <- suitm + geom_sf(data=BA, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=ES, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=MG, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=PR, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=RJ, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=RS, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=SC, fill = NA, color = 'gray40', linewidth=0.4) + geom_sf(data=SP, fill = NA, color = 'gray40', linewidth=0.4) 
  
S3_90 <- state + geom_sf(data=countries, fill = NA, color= 'black', linewidth=0.4) + coord_sf(default_crs = sf::st_crs(4326), xlim = c(-57, -38), ylim = c(-32, -12), expand = T)

title <- textGrob("Future suitability", gp=gpar(fontsize=17, fontface="bold"))


final_plot <- grid.arrange(S2_50, S2_70, S2_90, S3_50, S3_70, S3_90, nrow=2, ncol=3)



#Create legend

tif <- read_stars("Projection_threshold/current_noSavann.tif")
sf <- st_as_sf(tif, merge = T)
#plot(sf)

sf <- sf |> mutate(current.tif = ifelse(current_noSavann.tif == 1, 'Current', 'Overlapped suitable area'))

FUT <- st_as_sf(read_stars(paste0('Projection_threshold/',SYt)), merge = T)
#plot(sf)

names(FUT) <- c('a', 'geometry')

FUT <- FUT |> mutate(a = ifelse(a == 1, 'New suitable area', ''))

suit <- ggplot() +
  geom_sf(data = sf, mapping = aes(fill = current.tif), color = "gray100", linewidth=1e-20) +
  scale_fill_manual(values = c("gray30", "00FFFFFF")) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = FUT, mapping = aes(fill = a), linewidth=1e-20, alpha=0.5) +
  scale_fill_manual(values = c("gray100", "green")) +  
  labs(title = paste0('SSPs ',SSP[Sp],' 20',year[ye]), fill = "Legend", x = " ", y = " ") +
  scale_color_identity() +
  theme_bw() +
  theme(title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = .4),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size = 10, face = "plain"),
        legend.position = 'none',
        panel.grid.major = element_blank()
  ) #c(0.8, 0.2)) +

legend_p <- suit +
  theme(legend.position = 'right',
        legend.text = element_text(size = 12)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

legend <- cowplot::get_legend(legend_p)
plot(legend)


fp <- ggdraw() +
  draw_plot(final_plot) +
  draw_grob(legend, x = 0.82, y = 0.03, width = 0.2, height = 0.2)

ggsave(filename = 'Final_maps/future_suitability_Total_leg.png', plot = fp, width = 21, height = 16, units = "in", dpi = 500)



# End of the script
