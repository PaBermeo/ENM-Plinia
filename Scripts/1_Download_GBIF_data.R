#############################################################################################
####################                    1. Download GBIF data            ####################
#############################################################################################

library(rgbif)
library(pbapply)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)


# Getting data ---------------------------------------------------------


occ_download(pred_in("taxonKey", c(5415658, 5415573, 8182428, 5415574, 3173685, 3173688, 3173691, 3173812, 7925669, 3173690, 5415571, 5415572, 5415661, 5415572)), #Includes basydionyms and synonyms of Plinia peruviana and Plinia trunciflora.
                  pred("hasGeospatialIssue", FALSE),
                  pred("hasCoordinate", TRUE),
                  pred(na.omit("decimalLatitude")),
                  pred("occurrenceStatus","PRESENT"), 
                  format = "SIMPLE_CSV")

occ_download_wait('0033994-230810091245214')

