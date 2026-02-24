#setwd("")

library(svglite)
library(OUwie)
require(OUwie)
require(corHMM)
require(parallel)
require(phytools)
require(expm)
require(POUMM)
require(geiger)
require(data.table)
library(ggplot2)
#install.packages('openxlsx')
library(openxlsx)
#install.packages('dplyr')
library(dplyr)

################### ELEVATION #######################################################################################################################
################### posture #######################################################################################################################
#mydata <- read.csv("posture.csv",header=TRUE,row.names=1)
mydata <- read.xlsx('calochortus_hOUwie_elevation.xlsx', sheet = 'posture', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison

#double check to make sure that taxa all match with tree and data
name.check(phy=mytree,data=mydata)
comparison <- name.check(phy=mytree,data=mydata)
comparison

#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))

model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
elev_post_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(elev_post_params)

folder_name <- "Elevation_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(elev_post_params, file = file.path(folder_name, "model_avg_parameters_elev_posture.csv"), row.names = FALSE)


##plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_posture_elevation_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

################### trichomes #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_elevation.xlsx', sheet = 'trichomes', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
elev_tri_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(elev_tri_params)

folder_name <- "Elevation_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(elev_tri_params, file = file.path(folder_name, "model_avg_parameters_elev_trichomes.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_trichomes_elevation_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()


################### perianth #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_elevation.xlsx', sheet = 'perianth', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
elev_peri_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(elev_peri_params)

folder_name <- "Elevation_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(elev_peri_params, file = file.path(folder_name, "model_avg_parameters_elev_perianth.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_perianth_elevation_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

################### functional petal pigmentation #######################################################################################################################
mydata <- read.xlsx('calochortus_hOUwie_elevation.xlsx', sheet = 'funct_pet_pig', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison

#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000) ##cannot have special characters like '_, /, :, -' in code
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
elev_pig_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(elev_pig_params)

folder_name <- "Elevation_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(elev_pig_params, file = file.path(folder_name, "model_avg_parameters_elev_pigment.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_funct_pet_pig_elevation_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

################### MEAN DIURNAL RANGE - BIO2 #######################################################################################################################
################### posture #######################################################################################################################
mydata <- read.xlsx('calochortus_hOUwie_temp_bio2.xlsx', sheet = 'posture', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison

# prune taxa that don't have data but are present in the tree
#mytree <- drop.tip(mytree,comparison$tree_not_data)

#double check to make sure that taxa all match with tree and data
name.check(phy=mytree,data=mydata)
comparison <- name.check(phy=mytree,data=mydata)
comparison

#sigma^2 is a constant describing the rate of stochastic evolution around the optimum
#theta trait optima
#alpha rate that a trait is pulled towards the optima (theta)

#"BM1", "BMV", "OU1", "OUM", "OUA", "OUV", "OUMV", "OUMA", "OUVA", or "OUMVA". 
#allowing theta ("OUM") to vary
#sigma_square ("BMV", "OUV") to vary
#alpha ("OUA") to vary

#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1 
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD",  null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))

model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
DiRange_post_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(DiRange_post_params)

folder_name <- "Diurnal_Range_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(DiRange_post_params, file = file.path(folder_name, "model_avg_parameters_Bio2_posture.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

#svglite::
#pdf(file="hOWie_posture_BIO2_11Jul25_03.tiff", 10,5)
#svg(file="hOWie_posture_BIO2_11Jul25_03.svg", 10,5)
tiff(file="hOWie_posture_BIO2_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

#doesn't work now, looking for two traits, one being color
#plot(mytree, show.tip.label = FALSE, x.lim = c(0, 4), no.margin = TRUE)
#tiplabels(pch = 21, col = mydata$posture, cex = 0.75, offset = 0.02)
#tiplabels(pch = 16, col = mydata$elevation, cex = 0.75, offset = 0.05)
#proportions <- dat$data$x/max(dat$data$x)
#plotting_matrix <- matrix(c(rep(3.6, length(tree$tip.label)), 1:length(tree$tip.label), 3.6 + (0.4 * proportions), 1:length(tree$tip.label)), ncol = 4)
#for(i in 1:64){
# lines(t(matrix(plotting_matrix[i,], 2, 2)), lwd = 2, col = "darkgrey")
#}

################### trichomes #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_temp_bio2.xlsx', sheet = 'trichomes', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
DiRange_tri_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(DiRange_tri_params)

folder_name <- "Diurnal_Range_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(DiRange_tri_params, file = file.path(folder_name, "model_avg_parameters_Bio2_trichomes.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_trichomes_BIO2_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()


################### perianth #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_temp_bio2.xlsx', sheet = 'perianth', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
DiRange_peri_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(DiRange_peri_params)

folder_name <- "Diurnal_Range_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(DiRange_peri_params, file = file.path(folder_name, "model_avg_parameters_Bio2_perianth.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_perianth_BIO2_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

################### functional petal pigmentation #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_temp_bio2.xlsx', sheet = 'funct_pet_pig', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
DiRange_pig_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(DiRange_pig_params)

folder_name <- "Diurnal_Range_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(DiRange_pig_params, file = file.path(folder_name, "model_avg_parameters_Bio2_pigmentation.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_funct_pet_pig_BIO2_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()


################### TEMPERATURE ANNUAL RANGE - BIO7 #######################################################################################################################
################### posture #######################################################################################################################
mydata <- read.xlsx('calochortus_hOUwie_temp_bio7.xlsx', sheet = 'posture', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison

# prune taxa that don't have data but are present in the tree
#mytree <- drop.tip(mytree,comparison$tree_not_data)

#double check to make sure that taxa all match with tree and data
name.check(phy=mytree,data=mydata)
comparison <- name.check(phy=mytree,data=mydata)
comparison

#sigma^2 is a constant describing the rate of stochastic evolution around the optimum
#theta trait optima
#alpha rate that a trait is pulled towards the optima (theta)

#"BM1", "BMV", "OU1", "OUM", "OUA", "OUV", "OUMV", "OUMA", "OUVA", or "OUMVA". 
#allowing theta ("OUM") to vary
#sigma_square ("BMV", "OUV") to vary
#alpha ("OUA") to vary

#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD",  null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD",  null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))

model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
TempRange_post_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(TempRange_post_params)

folder_name <- "Temp_Range_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(TempRange_post_params, file = file.path(folder_name, "model_avg_parameters_Bio7_posture.csv"), row.names = FALSE)


#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_posture_BIO7_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

#doesn't work now, looking for two traits, one being color
#plot(mytree, show.tip.label = FALSE, x.lim = c(0, 4), no.margin = TRUE)
#tiplabels(pch = 21, col = mydata$posture, cex = 0.75, offset = 0.02)
#tiplabels(pch = 16, col = mydata$elevation, cex = 0.75, offset = 0.05)
#proportions <- dat$data$x/max(dat$data$x)
#plotting_matrix <- matrix(c(rep(3.6, length(tree$tip.label)), 1:length(tree$tip.label), 3.6 + (0.4 * proportions), 1:length(tree$tip.label)), ncol = 4)
#for(i in 1:64){
# lines(t(matrix(plotting_matrix[i,], 2, 2)), lwd = 2, col = "darkgrey")
#}

################### trichomes #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_temp_bio7.xlsx', sheet = 'trichomes', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
TempRange_tri_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(TempRange_tri_params)

folder_name <- "Temp_Range_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(TempRange_tri_params, file = file.path(folder_name, "model_avg_parameters_Bio7_trichomes.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_trichomes_BIO7_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()


################### perianth #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_temp_bio7.xlsx', sheet = 'perianth', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
TempRange_peri_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(TempRange_peri_params)

folder_name <- "Temp_Range_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(TempRange_peri_params, file = file.path(folder_name, "model_avg_parameters_Bio7_perianth.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_perianth_BIO7_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

################### functional petal pigmentation #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_temp_bio7.xlsx', sheet = 'funct_pet_pig', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
TempRange_pig_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(TempRange_pig_params)

folder_name <- "Temp_Range_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(TempRange_pig_params, file = file.path(folder_name, "model_avg_parameters_Bio7_pigmentation.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_funct_pet_pig_BIO7_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

################### PRECIPITATION SEASONALITY - BIO15 #######################################################################################################################
################### posture #######################################################################################################################
mydata <- read.xlsx('calochortus_hOUwie_precip_bio15.xlsx', sheet = 'posture', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison

# prune taxa that don't have data but are present in the tree
#mytree <- drop.tip(mytree,comparison$tree_not_data)

#double check to make sure that taxa all match with tree and data
name.check(phy=mytree,data=mydata)
comparison <- name.check(phy=mytree,data=mydata)
comparison

#sigma^2 is a constant describing the rate of stochastic evolution around the optimum
#theta trait optima
#alpha rate that a trait is pulled towards the optima (theta)

#"BM1", "BMV", "OU1", "OUM", "OUA", "OUV", "OUMV", "OUMA", "OUVA", or "OUMVA". 
#allowing theta ("OUM") to vary
#sigma_square ("BMV", "OUV") to vary
#alpha ("OUA") to vary

#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD",  null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD",  null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))

model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
Precip_post_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(Precip_post_params)

folder_name <- "Precip_Season_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(Precip_post_params, file = file.path(folder_name, "model_avg_parameters_Bio15_posture.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_posture_BIO15_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

#doesn't work now, looking for two traits, one being color
#plot(mytree, show.tip.label = FALSE, x.lim = c(0, 4), no.margin = TRUE)
#tiplabels(pch = 21, col = mydata$posture, cex = 0.75, offset = 0.02)
#tiplabels(pch = 16, col = mydata$elevation, cex = 0.75, offset = 0.05)
#proportions <- dat$data$x/max(dat$data$x)
#plotting_matrix <- matrix(c(rep(3.6, length(tree$tip.label)), 1:length(tree$tip.label), 3.6 + (0.4 * proportions), 1:length(tree$tip.label)), ncol = 4)
#for(i in 1:64){
# lines(t(matrix(plotting_matrix[i,], 2, 2)), lwd = 2, col = "darkgrey")
#}

################### trichomes #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_precip_bio15.xlsx', sheet = 'trichomes', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
Precip_tri_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(Precip_tri_params)

folder_name <- "Precip_Season_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(Precip_tri_params, file = file.path(folder_name, "model_avg_parameters_Bio15_trichomes.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_trichomes_BIO15_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()


################### perianth #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_precip_bio15.xlsx', sheet = 'perianth', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
Precip_peri_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(Precip_peri_params)

folder_name <- "Precip_Season_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(Precip_peri_params, file = file.path(folder_name, "model_avg_parameters_Bio15_perianth.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_perianth_BIO15_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

################### functional petal pigmentation #######################################################################################################################

mydata <- read.xlsx('calochortus_hOUwie_precip_bio15.xlsx', sheet = 'funct_pet_pig', rowNames = TRUE)
mydata

mytree <- read.tree("pruned_tree_hOUwie.tre")
mytree

states <- mydata[,1]
states
names(states) <- row.names(mydata)


#compares names between the tree and the data to list any discrepancies
comparison <- name.check(phy=mytree,data=mydata)
comparison


#standard models, no hidden states
pp_oum <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OUM",  nSim = 1000)
print(pp_oum) ##NOT WORKING

pp_bm1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BM1",  nSim = 1000)

pp_ou1 <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "OU1",  nSim = 1000) 

pp_bmv <- hOUwie(mytree, mydata, rate.cat = 1, discrete_model = "ARD", continuous_model = "BMV",  nSim = 1000) 

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum)
print(getModelTable(model_set))


#hidden stsates wtih rate.cat more than 1
pp_bm1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BM1",  nSim = 1000)

pp_ou1_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OU1",  nSim = 1000) 

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_oum_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "OUM",  nSim = 1000)

pp_bmv_cid <- hOUwie(mytree, mydata, rate.cat = 2, discrete_model = "ARD", null.model = TRUE, continuous_model = "BMV",  nSim = 1000)

model_set <- list(bm1_fit = pp_bm1, ou1_fit = pp_ou1, bmv_fit = pp_bmv, oum_fit = pp_oum, bmv_cid_fit = pp_bmv_cid, oum_cid_fit = pp_oum_cid, bm1_cid_fit = pp_bm1_cid, ou1_cid_fit = pp_ou1_cid) 

print(getModelTable(model_set))
model_avg_pars <- getModelAvgParams(model_set)
print(head(model_avg_pars))

#mean by tip_state
Precip_pig_params <- model_avg_pars %>%
  group_by(tip_state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(Precip_pig_params)

folder_name <- "Precip_Season_Params"
if (!dir.exists(folder_name)) { dir.create(folder_name)}
write.csv(Precip_pig_params, file = file.path(folder_name, "model_avg_parameters_Bio15_pigmentation.csv"), row.names = FALSE)

#plot_data <- melt(model_avg_pars)
plot_data <- reshape2::melt(model_avg_pars)

tiff(file="hOWie_funct_pet_pig_BIO15_11Jul25_03.tiff", width=2000, height=1200, res=350)
ggplot(plot_data, aes(x = tip_state, y = value, color = tip_state)) +
  geom_point(size = 5, shape = 21) +
  stat_summary(fun=mean,geom="point",aes(group=1, size = 2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group=1), width = 0.15, color = "black") +
  theme_classic() +
  facet_wrap(~variable, scales = "free")
dev.off()

