############# Continuous ancestral state reconstruction ##################
##################No Log Transformed Data###########################

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

# Read in your CSV file with data
data <- read.csv("12Aug24_bio_15.csv", row.names=1, stringsAsFactors=TRUE)
print(data) # Check the data

# Extract precipitation seasonality data
precip <- setNames(data$precipitation_seasonality, rownames(data))
print(head(precip)) # Check the precip data

# Estimate ancestral states using fastAnc
fit.elev <- fastAnc(mytree, precip, vars=TRUE, CI=TRUE)
print(fit.elev) # Print estimated ancestral states

# Plot phylogeny using plotTree
plotTree(mytree, ftype="i", fsize=0.5, lwd=1) # Add node labels for reference
labelnodes(1:mytree$Nnode + Ntip(mytree), 
           1:mytree$Nnode + Ntip(mytree), 
           interactive=FALSE, cex=0.5)

# Compute "contMap" object
precip.contMap<-contMap(mytree,precip,plot=FALSE,lims=c(62,115))

# Change the color gradient to a custom gradient
precip.contMap <- setMap(precip.contMap, c("grey", "white", "lightblue","blue", "darkblue"))

# Plot "contMap" and printing object
pdf("precip_asr_6sep24.pdf",12,12)
plot(precip.contMap,xlim=c(0,11),lwd=c(3,4),leg.txt="Mean Precipitation Seasonality (%)", fsize=c(0.7,0.8))
#errorbar.contMap(precip.contMap,lwd=8)
dev.off()
plot(precip.contMap)

#####Box Plot for Precipitation Seasonality#####################################################
# read in excel file
precip_data <- read_excel("12Aug24_bio_15.xlsx")
head(precip_data)

# Order the y axis to match the tips of the phylogeny
phylo_order <- c("ambiguus", "clavatus", "concolor", "kennedyi", "catalinae", "splendens", "dunnii", "palmeri", "flexuosus", "occidentalis", "ownbeyi", "spatulatus", "cernuus", "balsensis", "hartwegii", "purpureus", "marcellae", "venustulus", "nigrescens", "fuscus", "pringlei", "barbatus", "exilis", "ghiesbreghtii", "mendozae", "weedii")
precip_data$species <- factor(precip_data$species, levels = phylo_order)

# Create the box plot with customized whisker and box lines
precip_plot <-ggplot(precip_data, aes(x = species, y = precipitation_seasonality)) +
  # Main box plot with solid lines
  geom_boxplot(outlier.size = 1.5, outlier.colour = "grey", 
               fill = "#E0E0E0", colour = "black", linetype = "solid") +
  coord_flip() +  # Flip coordinates to make the box plot horizontal
  labs(#title = "Precipitation Seasonality by Species"
       x = "Species",
       y = "Precipitation Seasonality (%)") +
  theme_classic()

# Plot "contMap" and printing object
pdf("precip_seasonality_boxplot.pdf",8,6)
plot(precip_plot)
#errorbar.contMap(precip.contMap,lwd=8)
dev.off()

#####WILCOXON RANK-SUM TEST########################################################################
# ghiesbreghtii/exilis sister pair #p-value = 0.009941
# Extract precipitation_seasonality data for each species
group1 <- precip_data %>% filter(species == "ghiesbreghtii") %>% pull(precipitation_seasonality)
group2 <- precip_data %>% filter(species == "exilis") %>% pull(precipitation_seasonality)
# Perform the Wilcoxon rank-sum test (Mann-Whitney U test)
wilcoxon_rank_sum_result <- wilcox.test(group1, group2)
# Print the result
print(wilcoxon_rank_sum_result)

# pringlei/fuscus sister pair p-value = 0.9074
group3 <- precip_data %>% filter(species == "pringlei") %>% pull(precipitation_seasonality)
group4 <- precip_data %>% filter(species == "fuscus") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
# Perform the Wilcoxon rank-sum test (Mann-Whitney U test)
wilcoxon_rank_sum_result2 <- wilcox.test(group3, group4)
# Print the result
print(wilcoxon_rank_sum_result2)

# venustulus/marcellae sister pair p-value < 2.2e-16
group5 <- precip_data %>% filter(species == "venustulus") %>% pull(precipitation_seasonality)
group6 <- precip_data %>% filter(species == "marcellae") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result3 <- wilcox.test(group5, group6)
# Print the result
print(wilcoxon_rank_sum_result3)

# hartwegii/purpureus sister pair p-value < 2.2e-16
group7 <- precip_data %>% filter(species == "hartwegii") %>% pull(precipitation_seasonality)
group8 <- precip_data %>% filter(species == "purpureus") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result4 <- wilcox.test(group7, group8)
# Print the result
print(wilcoxon_rank_sum_result4)

# cernuus/spatulatus sister pair p-value = 0.3783
group9 <- precip_data %>% filter(species == "cernuus") %>% pull(precipitation_seasonality)
group10 <- precip_data %>% filter(species == "spatulatus") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result5 <- wilcox.test(group9, group10)
# Print the result
print(wilcoxon_rank_sum_result5)

# ownbeyi/occidentalis sister pair p-value = 0.7129
group11 <- precip_data %>% filter(species == "ownbeyi") %>% pull(precipitation_seasonality)
group12 <- precip_data %>% filter(species == "occidentalis") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result6 <- wilcox.test(group11, group12)
# Print the result
print(wilcoxon_rank_sum_result6)

# catalinae/splendens sister pair p-value = 0.168
group13 <- precip_data %>% filter(species == "catalinae") %>% pull(precipitation_seasonality)
group14 <- precip_data %>% filter(species == "splendens") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result7 <- wilcox.test(group13, group14)
# Print the result
print(wilcoxon_rank_sum_result7)

# concolor/clavatus sister pair p-value = 0.03772
group15 <- precip_data %>% filter(species == "concolor") %>% pull(precipitation_seasonality)
group16 <- precip_data %>% filter(species == "clavatus") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result8 <- wilcox.test(group15, group16)
# Print the result
print(wilcoxon_rank_sum_result8)

###SPECIES PAIRS
#ghiesbreghtii/exilis and barbatus species pair p-value < 2.2e-16
group17 <- precip_data %>% 
  filter(species %in% c("ghiesbreghtii", "exilis")) %>% 
  pull(precipitation_seasonality)
group18 <- precip_data %>% filter(species == "barbatus") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result9 <- wilcox.test(group17, group18)
# Print the result
print(wilcoxon_rank_sum_result9)

#pringlei/fuscus and nigrescens species pair p-value = 1.499e-06
group19 <- precip_data %>% 
  filter(species %in% c("pringlei", "fuscus")) %>% 
  pull(precipitation_seasonality)
group20 <- precip_data %>% filter(species == "nigrescens") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result10 <- wilcox.test(group19, group20)
# Print the result
print(wilcoxon_rank_sum_result10)

#purpureus/hartwegii and balsensis species pair p-value = 2.615e-11
group21 <- precip_data %>% 
  filter(species %in% c("purpureus", "hartwegii")) %>% 
  pull(precipitation_seasonality)
group22 <- precip_data %>% filter(species == "balsensis") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result11 <- wilcox.test(group21, group22)
# Print the result
print(wilcoxon_rank_sum_result11)

#catalinae/splendens and dunnii species pair p-value = 0.2059
group23 <- precip_data %>% 
  filter(species %in% c("catalinae", "splendens")) %>% 
  pull(precipitation_seasonality)
group24 <- precip_data %>% filter(species == "dunnii") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result12 <- wilcox.test(group23, group24)
# Print the result
print(wilcoxon_rank_sum_result12)

#clavatus/concolor and kennedyi species pair p-value = 0.003478
group25 <- precip_data %>% 
  filter(species %in% c("clavatus", "concolor")) %>% 
  pull(precipitation_seasonality)
group26 <- precip_data %>% filter(species == "kennedyi") %>% pull(precipitation_seasonality)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result13 <- wilcox.test(group25, group26)
# Print the result
print(wilcoxon_rank_sum_result13)

