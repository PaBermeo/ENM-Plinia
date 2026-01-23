
#Install pachages
pacman::p_load(ggbreak, ggplot2, ggspatial, gg.gap, plotrix, raster, readr, rgdal, sf, sp, tmap, viridis)

#Load data
Y <- read_delim("Other_files/Occs/Year_F_NF.csv", delim = ";", col_names = T) |> as.data.frame() #|> t() 

#Figure occs over years
occs <- ggplot(Y, aes(x = Year, y = Occ, group = Option)) +
  geom_line(aes(color= Option)) +
  geom_point(aes(color = Option)) +
  labs(title = " ",
       x = "Year",
       y = "Occurrences") +
  theme_linedraw() +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  scale_y_break(c(70, 110), space = 0.2 ) +
  scale_color_manual(values = c('orange', 'black', 'gray')) +
  scale_x_continuous(breaks = seq(min(Y$Year), max(Y$Year), by = 4), sec.axis = dup_axis(breaks = NULL))
occs

#Save plot
ggsave('Plots/occs.png', occs, dpi = 600, width = 12, height = 6, units= 'in')
