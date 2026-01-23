#############################################################################################
####################         4. Current variables preparation            ####################
#############################################################################################


#Load packages
library(readr)
library(raster)
library(dplyr)
library(terra)
library(sp)

pacman::p_load(corrplot, ggplot2, usdm)


# Abiotic variables -------------------------------------------------------
#Download 19 bioclimatic variables 30 secs. https://www.worldclim.org/data/worldclim21.html

#Import calibration area (ca)
shape <- 'M/M_Plinia1.shp'
ca = vect(shape)
#plot(ca)

#Occs
Pp <- read_delim('Plinia_peruviana.csv', delim = ';', col_names = T) |> as.data.frame()

#Remove extreme points
Peruviana <- Pp[- c(10, 19, 20, 59, 71), ]

Peruviana  <- Peruviana[,-1]

#List Worldclim (wc) rasters
wc <- list.files(path='Other_files/Rasters_wc/wc2.1_30s_bio', pattern= '.tif$', 
                 full.names = T, recursive=F) |> rast()
crs(wc) <- "epsg:4326"
#Change raster names
names(wc) <- c('Bio_1', 'Bio_10', 'Bio_11', 'Bio_12', 'Bio_13', 'Bio_14', 'Bio_15', 'Bio_16', 'Bio_17', 'Bio_18', 'Bio_19', 'Bio_2', 'Bio_3', 'Bio_4', 'Bio_5', 'Bio_6', 'Bio_7', 'Bio_8', 'Bio_9')


wc[order(wc)]
names(wc)

#Mask variables to the ca
wc.ca <- mask(crop(wc, ca), ca)
plot(wc.ca[[1]])
res(wc.ca)

#PET <- rast(list.files(path='Other_files/PET', pattern= '.tif$', full.names = T, recursive=F))
#crs(PET) <- "epsg:4326"
#PET <- resample(PET, wc.ca,"bilinear")
#PET.ca <- mask(crop(PET, ca), ca)
#pred <- c(wc.ca, PET.ca)

#Extracting values from rasters
Pointrast <- raster::extract(wc.ca, Peruviana)
PointRast <- Pointrast[,-1]
sdmdataP <- data.frame(PointRast)

#dir.create("predictors_enm")

#Save extract values
saveRDS(sdmdataP, "predictors_enm/sdm.Rds")


# Table clean ------------------------------------------------------------

#Check if there is not error in the Data
#sdmdataP <- readRDS(file.path("predictors_enm/sdm.RDS"))
View(sdmdataP)

#Table cleaning
valores_soma <- rowSums(sdmdataP)
valores_soma_validos <- 1:nrow(sdmdataP)
valores_soma_validos <- ifelse(is.na(valores_soma), NA, valores_soma_validos)
valores_soma_validos <- subset(valores_soma_validos, valores_soma_validos >0)

sdmdata_validosP <- sdmdataP[valores_soma_validos, ]


# Collinearity ------------------------------------------------------------

cor(sdmdata_validosP, method="spearman") #Spearman method
round(cor(sdmdata_validosP),2)
par(mfrow=c(1,1))
hist(cor(sdmdata_validosP), main="Correlation matrix", col="gray90")

write.table(round(cor(sdmdata_validosP), 2), 'predictors_enm/Correlation/cor_Plinia_BIO.xls', row.names = T, sep = '\t')

write.table(ifelse(cor(sdmdata_validosP) >= 0.7, 'Yes', 'No'), 'predictors_enm/Correlation/Plinia_Y-N_BIO.xls', row.names = T, 
            sep = '\t')

tiff('predictors_enm/Correlation/cor_Plinia.tif', width = 20, height = 20, units = 'cm', res = 300)#, compression = 'lzw')
corrplot(cor(sdmdata_validosP), type = 'lower', diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
         title = 'Environmental Variables')
dev.off()

# figure export
#tiff('predictors_enm/correlacao/corPlinia.tiff', width = 8, height = 9, units = 'in', res = 900, compression = 'lzw')
#corrplot(cor(sdmdata_validosP), type = "lower", diag = F, title = 'predictors_enm/correlacao/Correlation Between Environmental Variables', 
 #       mar = c(3, 0.5, 2, 1), tl.srt = 45)
#dev.off()


# PCA ---------------------------------------------------------------------
#dir.create('pca') 

# pca do pacote 'stats'
pca <- prcomp(sdmdata_validosP, scale = T)

# contribuicao de cada eixo (eigenvalues - autovalores)
summary(pca)

# grafico de barras com as contribuicoes
screeplot(pca, main = 'Autovalores')
abline(h = 1, col = 'red', lty = 2)

tiff('predictors_enm/pca/screeplotPlinia.tif', width = 20, height = 20, units = 'cm', res = 300)
screeplot(pca, main = 'Autovalores')
abline(h = 1, col = 'red', lty = 2)
dev.off()

# valores de cada eixo (eigenvectors - autovetores - escores)
pca$x
plotPCA <- biplot(prcomp(sdmdata_validosP, scale. = T, ))
biplot(pca, col = c('darkblue', 'red'),
       scale = 0, xlabs = rep("*", 89)
       )

# relacao das variaveis com cada eixo (loadings - cargas)
pca$rotation[, 1:4]
abs(pca$rotation[, 1:4])

# exportar tabela com a contribuicao
write.table(abs(pca$rotation[, 1:4]), 'predictors_enm/pca/contrib_pcaPlinia_BIO.xls', row.names = T, sep = '\t')

# plot
biplot(pca)
dev.off()

# VIF ---------------------------------------------------------------------
#dir.create('vif') 

#vifSdmPlinia <- usdm::vifstep(sdmdataP, th=10, keep = c('anPET', 'PETseason'))
vifSdmPlinia <- usdm::vifstep(sdmdata_validosP, th=10, keep = 'Bio_14')

#save.output
capture.output(vifSdmPlinia, file='predictors_enm/vif/VIFPlinia.txt')

Plinia <- as.data.frame(vifSdmPlinia)
VIF <- c( 4.382054, 4.074933, 5.796421,  3.771799,  4.9014, 5.796345, 3.345689, 6.645152)

Variables <- c('Bio_2', 'Bio_3','Bio_5', 'Bio_8', 'Bio_9', 'Bio_13', 'Bio_14', 'Bio_18')
#Var <- Variables[order(Variables)]
dVIF <- as.data.frame(cbind(Variables, as.numeric(VIF)))

bVIF <- ggplot2::ggplot(dVIF, mapping = aes(x=Variables, y=VIF)) +
  geom_bar(stat='identity', fill= c('grey50'), width = 0.7) +
  labs(title='Variance Inflation Factor', x= 'Potential predictors', y='VIF') +
  theme_classic() +
  theme( plot.title= element_text(hjust = 0.5),
        axis.title.y = element_text(size=12, family='times', color='black'), 
        axis.text =element_text(size=12, family = 'times', color= 'black')) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))
bVIF

ggsave("VIFbars.png", path = 'predictors_enm/vif/', dpi=500)


#Save final variables



pred <- c(pred$bio5, pred$bio13, pred$bio14, pred$anPET) #Apagar bio2
plot(wc.ca[[1]])
raster::writeRaster(wc.ca, "Other_files/Rasters_wc/wccurrent_ca.tiff", overwrite=TRUE)

plot(pred$bio5)

pred <- stack(pred)
#wc.fv <- stack("Other_files/Rasters_wc/wccurrent_ca.tiff")

raster::writeRaster(pred$bio3, "Variables/bio_3.asc",  format="ascii", overwrite=TRUE)
raster::writeRaster(pred$bio5, "Variables/bio_5.asc",  format="ascii", overwrite=TRUE)
raster::writeRaster(pred$bio13, "Variables/bio_13.asc",  format="ascii", overwrite=TRUE)
raster::writeRaster(pred$bio14, "Variables/bio_14.asc",  format="ascii", overwrite=TRUE)

# End of the script
