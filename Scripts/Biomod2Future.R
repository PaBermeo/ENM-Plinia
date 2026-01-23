# BIOMOD_Projections future  -------------------------------------------

#Limpiar environment
rm(list=ls())

#Cargarregamento de pacotes
pacman::p_load(biomod2, ggplot2, gridExtra, raster, rasterVis, tidyterra, ggtext, readr, terra, sp, ggpubr)


#Loading de arquivos
BiomodData <- readRDS('Biomod/BiomodData.rds')
BiomodEM <- readRDS('Biomod/BiomodEM.rds')
Biomod_models <- readRDS('Biomod/Biomod_models.rds')
BiomodProj <- readRDS("Biomod/BiomodProj.rds")
BiomodEF <- readRDS('Biomod/BiomodEF.rds')


#AC_2_50 <-
#stack(
#c(
 # bio2 = "dados/rasterFuture/AC_2_50_b2.asc",
  #bio3 =  "dados/rasterFuture/AC_2_50_b3.asc",
  #bio5 = "dados/rasterFuture/AC_2_50_b5.asc",
  #bio13 = "dados/rasterFuture/AC_2_50_b13.asc"))


#Load bioclim variables

### Fazer o processo em loop
gcms <- c("AC","MI","MP")
rcps <- c("2","3")
years <- c("50", '70', '90')
ban <- c("b2", 'b3', 'b5', 'b13')

for (gc in 1:length(gcms)) {
  for (rc in 1: length(rcps)) {
    for (ye in 1: length(years)) {
      for (ba in 1: length(ban)) {
                     
  asc_fut <- list.files(path='dados/rasterFuture', pattern = ".asc$", full.names = TRUE, recursive = FALSE)    
    
  
        bio = paste0('dados/rasterFuture/',gcms[[gc]],'_',rcps[[rc]],'_',years[[ye]],'_',ban[[ba]],'.asc')
        
        assign(paste0(ban[ba]), bio)
      }
      
      result <- stack( c(bio2 = b2, bio3= b3, bio5 = b5, bio13 = b13))
      
#Projetions
BiomodProj_F <- 
  BIOMOD_Projection(bm.mod=Biomod_models,
            new.env= result, #result,
            proj.name= paste0(gcms[[gc]],'_',rcps[[rc]],'_',years[[ye]]),
            models.chosen= grep("_allData_", get_built_models(Biomod_models),
                                value=TRUE),
            metric.binary= "ROC",
            metric.filter= "ROC",
            compress=T,
            build.clamping.mask=T, 
            omi.na=T,
            on_0_1000=T,
            output.format=".grd")

BiomodEF_Future <- BIOMOD_EnsembleForecasting(
  bm.em=BiomodEM, ## Rules for assembling
  bm.proj=BiomodProj_F, # Individual model projection
  metric.binary="ROC", #analiza
  metric.filter="ROC",
  on_0_1000=T,
  output.format = ".img",
  compress=T,
  do.stack = T,
  on_0_1000=T
  #a logical value defining whether all projections are to be saved as one SpatRaster object or several SpatRaster files
)
   
      
    }
  }
}


## check how projections looks like
plot(BiomodEF_Future, str.grep = "EMca|EMwmean")



# # Load current and future binary projections ----------------------------

CurrentProj <- get_predictions(BiomodProj_F, metric.binary = "ROC")
FutureProj <- get_predictions(BiomodProj_FP, metric.binary = "ROC")


# Compute differences
myBiomodRangeSize <- BIOMOD_RangeSize(proj.current = CurrentProj, 
                                      proj.future = FutureProj)

myBiomodRangeSize$Compt.By.Models
plot(myBiomodRangeSize$Diff.By.Pixel)

# Represent main results 
gg = bm_PlotRangeSize(bm.range = myBiomodRangeSize, 
                      do.count = TRUE,
                      do.perc = TRUE,
                      do.maps = TRUE,
                      do.mean = TRUE,
                      do.plot = TRUE,
                      row.names = c("Species", "Dataset", "Run", "Algo"))
str(gg)









#PROVAA

AC_3_50 <-
stack(
c(
 bio2 = "dados/rasterFuture/AC_3_50_b2.asc",
 bio3 =  "dados/rasterFuture/AC_3_50_b3.asc",
 bio5 = "dados/rasterFuture/AC_3_50_b5.asc",
 bio13 = "dados/rasterFuture/AC_3_50_b13.asc"))

BiomodProj_FP <- 
  BIOMOD_Projection(bm.mod=Biomod_models,
                    new.env= AC_3_50, #result,
                    proj.name= 'AC_3_50.prova',
                    models.chosen= grep("_allData_", get_built_models(Biomod_models),
                                        value=TRUE),
                    metric.binary= "ROC",
                    metric.filter= 'ROC',
                    compress=T,
                    build.clamping.mask=T, 
                    omi.na=T,
                    on_0_1000=T,
                    output.format=".grd")

BiomodEF_FutureP <- BIOMOD_EnsembleForecasting(
  bm.em=BiomodEM, ## Rules for assembling
  bm.proj=BiomodProj_FP, # Individual model projection
  metric.binary="ROC", #analiza
  metric.filter="ROC",
  on_0_1000=T,
  output.format = ".img",
  compress=T,
  do.stack = T #a logical value defining whether all projections are to be saved as one SpatRaster object or several SpatRaster files
)


asc_fut <- list.files(path='dados/rasterFuture', pattern = ".asc$", full.names = TRUE, recursive = FALSE)    


bio = paste0('dados/rasterFuture/',gcms[[gc]],'_',rcps[[rc]],'_',years[[ye]],'_',ban[[ba]],'.asc')

assign(paste0(ba[i]), bio)


result <- stack( b2, b3, b5, b13)

