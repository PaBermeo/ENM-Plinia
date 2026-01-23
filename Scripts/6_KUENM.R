#-------------------------------------------------------------------------------
#                              ENM 2020
# kuenm R package
# Author: Marlon E. Cobos
#-------------------------------------------------------------------------------

pacman::p_load(kuenm, readr)

# Importing data for KUENM initial analysis -------------------------------

Peruviana <- read_delim('Plinia_peruviana.csv', delim = ';', col_names = T) |> as.data.frame()

#Remove extreme points
Pp <- Peruviana[- c(10, 19, 20, 59, 71), ]


#set.seed(17)
split <- kuenm_occsplit(occ = Pp, train.proportion = 0.7, method = "random", save = TRUE, name = "occ")

## preparing sets of variables (complete the code)
help(kuenm_varcomb)

vs <- kuenm_varcomb(var.dir = "Variables", out.dir = "M_var", min.number = 3,  in.format = "ascii", out.format = "ascii")


#Markdown
kuenm_start(file.name= 'PliniaKUENM')

# Calibration process

set.seed(17)
occ_joint <- "occ_joint.csv"
occ_tra <- "occ_train.csv"
M_var_dir <- "M_variables"
batch_cal <- "Candidate_models"
out_dir <- "Candidate_Models"
reg_mult <- c(seq(0.1, 1, 0.1), seq(2, 6, 1))
f_clas <- c("l", "lq","lqp") # 
maxent_path <-  "E:/" # 
wait <- F
run <- T
#max.memory = 4000
args = "maximumbackground=20000"

kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir, batch = batch_cal,
          out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas, args = args,
          maxent.path = maxent_path, wait = wait, run = run)



#Models evaluation
set.seed(17)
occ_test <- "occ_test.csv"
out_eval <- "Calibration_results"
threshold <- 5
rand_percent <- 50
iterations <- 500
kept <- F
selection <- "OR_AICc"
paral_proc <- FALSE 

cal_eval <- kuenm_ceval(path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, batch = batch_cal,
                        out.eval = out_eval, threshold = threshold, rand.percent = rand_percent, iterations = iterations,
                        kept = kept, selection = selection, parallel.proc = paral_proc)


#Final model creation
set.seed(17)
batch_fin <- "Final_models"
mod_dir <- "Final_Models"
rep_n <- 10
rep_type <- "Bootstrap"
jackknife <- FALSE
out_format <- "logistic"
project <- T
G_var_dir <- "G_variables"
ext_type <- 'ext'
write_mess <- FALSE
write_clamp <- FALSE
wait <- T
run <- TRUE
args <- "maximumbackground=20000" #for increasing the number of pixels in the background or
# "outputgrids=false" which avoids writing grids of replicated models and only writes the 
# summary of them (e.g., average, median, etc.) when rep.n > 1
# note that some arguments are fixed in the function and should not be changed
# Again, some of the variables used here as arguments were already created for previous functions

set.seed(17)
kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, rep.n = rep_n,
          rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir, out.format = out_format, project = project,
          G.var.dir = G_var_dir, ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, max.memory = max.memory,
          maxent.path = maxent_path, args = args, wait = wait, run = run)


#Final model evaluation
set.seed(17)
occ_joint <- "occ_joint.csv"
occ_ind <- 'ind_test.csv'
replicates <- TRUE
out_feval <- "Final_Models_evaluation"
mod_dir <- 'Final_models'
# Most of the variables used here as arguments were already created for previous functions
set.seed(17)
#occ1 <- read.csv(occ.ind, fill=T, header = T, sep=';')
#

fin_eval <- kuenm_feval(path = mod_dir,
  occ.joint = occ_joint, occ.ind = occ_ind, replicates = replicates,
                        out.eval = out_feval, threshold = threshold, rand.percent = rand_percent,
                        iterations = iterations)
   

#Extrapolation risk analysis
set.seed(17)
sets_var <- "Set_4" # a vector of various sets can be used
out_mop <- "MOP_results"
percent <- 5
paral <- FALSE
is.swd <- F
M_var_dir <- 'M_variables'
# make this true to perform MOP calculations in parallel, recommended
# only if a powerfull computer is used (see function's help)
# Two of the variables used here as arguments were already created for previous functions



