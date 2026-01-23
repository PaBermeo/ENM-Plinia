#############################################################################################
####################                      3. Sp Thinning                 ####################
#############################################################################################


library(spThin)
library(readr)
library(here)

Pperuviana <- read_delim("Plinia_p_all_regions.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Long = col_number(),                                              Lat = col_number()), trim_ws = TRUE) |> na.omit()


# Run spatial thinning, using 1 km distance
thinnned_Ppspp <-
  thin(loc.data = Pperuviana,
       lat.col = "Lat", long.col = "Long",
       spec.col = "Accepted name",
       thin.par = 1, reps = 100,
       locs.thinned.list.return = TRUE,
       out.base = 'Plinia_peruviana',
       write.files = TRUE, max.files=1, out.dir=here(),
       write.log.file = FALSE)
# Have a look at the first thinned data set
View(thinnned_Pp2spp[[1]])

# Plot the first thinned data set over the full data set to see thinning
points(thinnned_Pp2spp[[1]]$Longitude, thinnned_Pp2spp[[1]]$Latitude, col = "red", pch = 20)


