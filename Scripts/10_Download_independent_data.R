
#install.packages("rgbif")
#install.packages("Taxonstand")
#install.packages("CoordinateCleaner")
#install.packages("maps")

library(rgbif)
library(pbapply)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)

# Getting data ---------------------------------------------------------

#packageVersion("rgbif")
#?rgbif
#install.packages("usethis")
usethis::edit_r_environ()

gbif_user <- 'your_user'
gbif_pwd <- 'your_password'
gbif_email <- 'your_email'

occ_download(pred_in("taxonKey", c(5415658, 5415573, 8182428, 5415574, 3173685, 3173688, 3173691, 3173812, 7925669, 3173690, 5415571, 5415572, 5415661, 5415572)),       
                  pred("hasGeospatialIssue", FALSE),
                  pred("hasCoordinate", TRUE),
                  pred("occurrenceStatus","PRESENT"),
                  format = "SIMPLE_CSV", user = gbif_user, pwd = gbif_pwd, email = gbif_email)

occ_download_wait('0030950-240321170329656')

