############# Continuous ancestral state reconstruction ##################
##################Mean Diurnal Range###########################
#SetWD

# Install and load necessary libraries
install.packages('phytools')
install.packages('readxl')
install.packages('dplyr')
install.packages("ggplot2")
library(phytools)
library(readxl)
library(dplyr)
library(ggplot2)

# Read in mytree as a nexus file
mytree <- read.tree("12Aug24_mx_calo.tre")
print(mytree) # Check the tree structure
plot(mytree)

# Read in your CSV file with data
data <- read.csv("12Aug24_bio_2.csv", row.names=1, stringsAsFactors=TRUE)
print(data) # Check the data

# Extract temperature data
bio_2 <- setNames(data$temperature, rownames(data))
print(head(bio_2)) # Check the bio_2 data

# Estimate ancestral states using fastAnc
fit.temp2 <- fastAnc(mytree, bio_2, vars=TRUE, CI=TRUE)
print(fit.temp2) # Print estimated ancestral states

# Plot phylogeny using plotTree
plotTree(mytree, ftype="i", fsize=0.5, lwd=1) # Add node labels for reference
labelnodes(1:mytree$Nnode + Ntip(mytree), 
           1:mytree$Nnode + Ntip(mytree), 
           interactive=FALSE, cex=0.5)

# Compute "contMap" object
temp2.contMap<-contMap(mytree,bio_2,plot=TRUE,lims=c(13,18))

# Change the color gradient to a custom gradient
temp2.contMap <- setMap(temp2.contMap, c("purple", "blue","yellow", "orange", "red"))
plot(temp2.contMap,xlim=c(0,12),lwd=c(3,4),leg.txt="Temperature (ºC)", fsize=c(0.7,0.8))

# Plot "contMap" and printing object
pdf("temperature2_asr_12aug24.pdf",12,12)
plot(temp2.contMap,xlim=c(0,11),lwd=c(3,4),leg.txt="Temperature (ºC)", fsize=c(0.7,0.8))
#errorbar.contMap(temp2.contMap,lwd=8)
dev.off()

#####Box Plot for temperature###################################################################
# read in excel file
temp2_data <- read_excel("12Aug24_bio_2.xlsx")
head(temp2_data)

#Order the y axis to match the tips of the phylogeny
phylo_order <- c("ambiguus", "clavatus", "concolor", "kennedyi", "catalinae", "splendens", "dunnii", "palmeri", "flexuosus", "occidentalis", "ownbeyi", "spatulatus","cernuus","balsensis","hartwegii", "purpureus", "marcellae", "venustulus", "nigrescens","fuscus", "pringlei", "barbatus", "exilis", "ghiesbreghtii","mendozae","weedii")
temp2_data$species <- factor(temp2_data$species, levels = phylo_order)

# Create the box plot with customized whisker and box lines
temp2_plot <-ggplot(temp2_data, aes(x = species, y = temperature)) +
  # Main box plot with solid lines
  geom_boxplot(outlier.size = 1.5, outlier.colour = "grey", 
               fill = "#E0E0E0", colour = "black", linetype = "solid") +
  coord_flip() +  # Flip coordinates to make the box plot horizontal
  labs(#title = "Temperature by Species"
       x = "Species",
       y = "Temperature (ºC)") +
  theme_classic()
plot(temp2_plot)
# Plot "contMap" and printing object
pdf("12aug24temperature2_boxplot.pdf",4,8)
plot(temp2_plot)
#errorbar.contMap(temp2.contMap,lwd=8)
dev.off()

#####Wilcoxon rank-sum test (Mann-Whitney U test)##########################################################
# Load necessary library
library(dplyr)

# ghiesbreghtii/exilis sister pair #p-value = 0.3036
# Extract temperature data for each species
group1 <- temp2_data %>% filter(species == "ghiesbreghtii") %>% pull(temperature)
group2 <- temp2_data %>% filter(species == "exilis") %>% pull(temperature)
# Perform the Wilcoxon rank-sum test (Mann-Whitney U test)
wilcoxon_rank_sum_result <- wilcox.test(group1, group2)
# Print the result
print(wilcoxon_rank_sum_result)

# pringlei/fuscus sister pair p-value = 5.211e-07
group3 <- temp2_data %>% filter(species == "pringlei") %>% pull(temperature)
group4 <- temp2_data %>% filter(species == "fuscus") %>% pull(temperature)
#test =<0.05 is a significant difference
# Perform the Wilcoxon rank-sum test (Mann-Whitney U test)
wilcoxon_rank_sum_result2 <- wilcox.test(group3, group4)
# Print the result
print(wilcoxon_rank_sum_result2)

# venustulus/marcellae sister pair p-value = 8.858e-09
group5 <- temp2_data %>% filter(species == "venustulus") %>% pull(temperature)
group6 <- temp2_data %>% filter(species == "marcellae") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result3 <- wilcox.test(group5, group6)
# Print the result
print(wilcoxon_rank_sum_result3)

# hartwegii/purpureus sister pair p-value = 1.978e-06
group7 <- temp2_data %>% filter(species == "hartwegii") %>% pull(temperature)
group8 <- temp2_data %>% filter(species == "purpureus") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result4 <- wilcox.test(group7, group8)
# Print the result
print(wilcoxon_rank_sum_result4)

# cernuus/spatulatus sister pair p-value = 1.065e-07
group9 <- temp2_data %>% filter(species == "cernuus") %>% pull(temperature)
group10 <- temp2_data %>% filter(species == "spatulatus") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result5 <- wilcox.test(group9, group10)
# Print the result
print(wilcoxon_rank_sum_result5)


# ownbeyi/occidentalis sister pair p-value = 6.652e-07
group11 <- temp2_data %>% filter(species == "ownbeyi") %>% pull(temperature)
group12 <- temp2_data %>% filter(species == "occidentalis") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result6 <- wilcox.test(group11, group12)
# Print the result
print(wilcoxon_rank_sum_result6)

# catalinae/splendens sister pair p-value = 0.5087
group13 <- temp2_data %>% filter(species == "catalinae") %>% pull(temperature)
group14 <- temp2_data %>% filter(species == "splendens") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result7 <- wilcox.test(group13, group14)
# Print the result
print(wilcoxon_rank_sum_result7)

# concolor/clavatus sister pair p-value = 0.02547
group15 <- temp2_data %>% filter(species == "concolor") %>% pull(temperature)
group16 <- temp2_data %>% filter(species == "clavatus") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result8 <- wilcox.test(group15, group16)
# Print the result
print(wilcoxon_rank_sum_result8)

###SPECIES PAIRS
#ghiesbreghtii/exilis and barbatus species pair p-value < 2.2e-16
group17 <- temp2_data %>% 
  filter(species %in% c("ghiesbreghtii", "exilis")) %>% 
  pull(temperature)
group18 <- temp2_data %>% filter(species == "barbatus") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result9 <- wilcox.test(group17, group18)
# Print the result
print(wilcoxon_rank_sum_result9)

#pringlei/fuscus and nigrescens species pair p-value = 0.7409
group19 <- temp2_data %>% 
  filter(species %in% c("pringlei", "fuscus")) %>% 
  pull(temperature)
group20 <- temp2_data %>% filter(species == "nigrescens") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result10 <- wilcox.test(group19, group20)
# Print the result
print(wilcoxon_rank_sum_result10)

#purpureus/hartwegii and balsensis species pair p-value < 2.2e-16
group21 <- temp2_data %>% 
  filter(species %in% c("purpureus", "hartwegii")) %>% 
  pull(temperature)
group22 <- temp2_data %>% filter(species == "balsensis") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result11 <- wilcox.test(group21, group22)
# Print the result
print(wilcoxon_rank_sum_result11)

#catalinae/splendens and dunnii species pair p-value = 0.3494
group23 <- temp2_data %>% 
  filter(species %in% c("catalinae", "splendens")) %>% 
  pull(temperature)
group24 <- temp2_data %>% filter(species == "dunnii") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result12 <- wilcox.test(group23, group24)
# Print the result
print(wilcoxon_rank_sum_result12)

#clavatus/concolor and kennedyi species pair p-value = 7.483e-06
group25 <- temp2_data %>% 
  filter(species %in% c("clavatus", "concolor")) %>% 
  pull(temperature)
group26 <- temp2_data %>% filter(species == "kennedyi") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result13 <- wilcox.test(group25, group26)
# Print the result
print(wilcoxon_rank_sum_result13)