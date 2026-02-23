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
data <- read.csv("12Aug24_elevation.csv", row.names=1, stringsAsFactors=TRUE)
print(data) # Check the data

# Extract elevation data
elevations <- setNames(data$Elevation, rownames(data))
print(head(elevations)) # Check the elevations data

# Estimate ancestral states using fastAnc
fit.elev <- fastAnc(mytree, elevations, vars=TRUE, CI=TRUE)
print(fit.elev) # Print estimated ancestral states

# Plot phylogeny using plotTree
plotTree(mytree, ftype="i", fsize=0.5, lwd=1) # Add node labels for reference
labelnodes(1:mytree$Nnode + Ntip(mytree), 
           1:mytree$Nnode + Ntip(mytree), 
           interactive=FALSE, cex=0.5)

# Compute "contMap" object
elev.contMap<-contMap(mytree,elevations,plot=FALSE,lims=c(0,3450))

# Change the color gradient to a custom gradient
#elev.contMap <- setMap(elev.contMap, c("#652d0d", "#bd791d", "#e2d8c3", "#89bb8c","#214821"))
elev.contMap <- setMap(elev.contMap, c("#214821", "#89bb8c","lightgreen", "#e2d8c3","darkorange", "#bd791d","brown","#652d0d"))

# Plot "contMap" and printing object
pdf("elevation_asr_6sep24.pdf",12,12)
plot(elev.contMap,xlim=c(0,11),lwd=c(3,4),leg.txt="Elevation (m)", fsize=c(0.7,0.8))
#errorbar.contMap(elev.contMap,lwd=8)
dev.off()

#####Box Plot for Elevation###################################################################
# read in excel file
elev_data <- read_excel("12Aug24_elevation.xlsx")
head(elev_data)

#Order the y axis to match the tips of the phylogeny
phylo_order <- c("ambiguus", "clavatus", "concolor", "kennedyi", "catalinae", "splendens", "dunnii", "palmeri", "flexuosus", "occidentalis", "ownbeyi", "spatulatus", "cernuus", "balsensis", "hartwegii", "purpureus", "marcellae", "venustulus", "nigrescens", "fuscus", "pringlei", "barbatus", "exilis", "ghiesbreghtii", "mendozae", "weedii")
elev_data$species <- factor(elev_data$species, levels = phylo_order)

# Create the box plot with customized whisker and box lines
elev_plot <-ggplot(elev_data, aes(x = species, y = Elevation)) +
  # Main box plot with solid lines
  geom_boxplot(outlier.size = 1.5, outlier.colour = "grey", 
               fill = "#E0E0E0", colour = "black", linetype = "solid") +
  coord_flip() +  # Flip coordinates to make the box plot horizontal
  labs(#title = "Elevation by Species"
       x = "Species",
       y = "Elevation (m)") +
  theme_classic()

# Plot "contMap" and printing object
pdf("elevation_boxplot.pdf",4,8)
plot(elev_plot)
#errorbar.contMap(elev.contMap,lwd=8)
dev.off()

#####WILCOXON RANK-SUM TEST########################################################################
# ghiesbreghtii/exilis sister pair #p-value = 
# Extract Elevation data for each species
group1 <- elev_data %>% filter(species == "ghiesbreghtii") %>% pull(Elevation)
group2 <- elev_data %>% filter(species == "exilis") %>% pull(Elevation)
# Perform the Wilcoxon rank-sum test (Mann-Whitney U test)
wilcoxon_rank_sum_result <- wilcox.test(group1, group2)
# Print the result
print(wilcoxon_rank_sum_result)

# pringlei/fuscus sister pair p-value = 
group3 <- elev_data %>% filter(species == "pringlei") %>% pull(Elevation)
group4 <- elev_data %>% filter(species == "fuscus") %>% pull(Elevation)
#test =<0.05 is a significant difference
# Perform the Wilcoxon rank-sum test (Mann-Whitney U test)
wilcoxon_rank_sum_result2 <- wilcox.test(group3, group4)
# Print the result
print(wilcoxon_rank_sum_result2)

# venustulus/marcellae sister pair p-value =
group5 <- elev_data %>% filter(species == "venustulus") %>% pull(Elevation)
group6 <- elev_data %>% filter(species == "marcellae") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result3 <- wilcox.test(group5, group6)
# Print the result
print(wilcoxon_rank_sum_result3)

# hartwegii/purpureus sister pair p-value =
group7 <- elev_data %>% filter(species == "hartwegii") %>% pull(Elevation)
group8 <- elev_data %>% filter(species == "purpureus") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result4 <- wilcox.test(group7, group8)
# Print the result
print(wilcoxon_rank_sum_result4)

# cernuus/spatulatus sister pair p-value = 
group9 <- elev_data %>% filter(species == "cernuus") %>% pull(Elevation)
group10 <- elev_data %>% filter(species == "spatulatus") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result5 <- wilcox.test(group9, group10)
# Print the result
print(wilcoxon_rank_sum_result5)

# ownbeyi/occidentalis sister pair p-value =
group11 <- elev_data %>% filter(species == "ownbeyi") %>% pull(Elevation)
group12 <- elev_data %>% filter(species == "occidentalis") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result6 <- wilcox.test(group11, group12)
# Print the result
print(wilcoxon_rank_sum_result6)

# catalinae/splendens sister pair p-value =
group13 <- elev_data %>% filter(species == "catalinae") %>% pull(Elevation)
group14 <- elev_data %>% filter(species == "splendens") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result7 <- wilcox.test(group13, group14)
# Print the result
print(wilcoxon_rank_sum_result7)

# concolor/clavatus sister pair p-value = 
group15 <- elev_data %>% filter(species == "concolor") %>% pull(Elevation)
group16 <- elev_data %>% filter(species == "clavatus") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result8 <- wilcox.test(group15, group16)
# Print the result
print(wilcoxon_rank_sum_result8)

###SPECIES PAIRS
#ghiesbreghtii/exilis and barbatus species pair p-value =
group17 <- elev_data %>% 
  filter(species %in% c("ghiesbreghtii", "exilis")) %>% 
  pull(Elevation)
group18 <- elev_data %>% filter(species == "barbatus") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result9 <- wilcox.test(group17, group18)
# Print the result
print(wilcoxon_rank_sum_result9)

#pringlei/fuscus and nigrescens species pair p-value = 
group19 <- elev_data %>% 
  filter(species %in% c("pringlei", "fuscus")) %>% 
  pull(Elevation)
group20 <- elev_data %>% filter(species == "nigrescens") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result10 <- wilcox.test(group19, group20)
# Print the result
print(wilcoxon_rank_sum_result10)

#purpureus/hartwegii and balsensis species pair p-value = 
group21 <- elev_data %>% 
  filter(species %in% c("purpureus", "hartwegii")) %>% 
  pull(Elevation)
group22 <- elev_data %>% filter(species == "balsensis") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result11 <- wilcox.test(group21, group22)
# Print the result
print(wilcoxon_rank_sum_result11)

#catalinae/splendens and dunnii species pair p-value = 
group23 <- elev_data %>% 
  filter(species %in% c("catalinae", "splendens")) %>% 
  pull(Elevation)
group24 <- elev_data %>% filter(species == "dunnii") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result12 <- wilcox.test(group23, group24)
# Print the result
print(wilcoxon_rank_sum_result12)

#clavatus/concolor and kennedyi species pair p-value = 
group25 <- elev_data %>% 
  filter(species %in% c("clavatus", "concolor")) %>% 
  pull(Elevation)
group26 <- elev_data %>% filter(species == "kennedyi") %>% pull(Elevation)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result13 <- wilcox.test(group25, group26)
# Print the result
print(wilcoxon_rank_sum_result13)