############# Continuous ancestral state reconstruction ##################
##################Temperature Annual Range###########################

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
data <- read.csv("12Aug24_bio_7.csv", row.names=1, stringsAsFactors=TRUE)
print(data) # Check the data

# Extract temperature data
bio_7 <- setNames(data$temperature, rownames(data))
print(head(bio_7)) # Check the bio_7 data

# Estimate ancestral states using fastAnc
fit.temp7 <- fastAnc(mytree, bio_7, vars=TRUE, CI=TRUE)
print(fit.temp7) # Print estimated ancestral states

# Plot phylogeny using plotTree
plotTree(mytree, ftype="i", fsize=0.5, lwd=1) # Add node labels for reference
labelnodes(1:mytree$Nnode + Ntip(mytree), 
           1:mytree$Nnode + Ntip(mytree), 
           interactive=FALSE, cex=0.5)

# Compute "contMap" object
temp7.contMap<-contMap(mytree,bio_7,plot=FALSE,lims=c(19,35))

# Change the color gradient to a custom gradient
temp7.contMap <- setMap(temp7.contMap, c("purple", "blue","yellow", "orange", "red"))

# Plot "contMap" and printing object
pdf("temperature7_asr_12aug24.pdf",12,12)
plot(temp7.contMap,xlim=c(0,11),lwd=c(3,4),leg.txt="Temperature (ºC)", fsize=c(0.7,0.8))
#errorbar.contMap(temp7.contMap,lwd=8)
dev.off()

    #####Box Plot for temperature###################################################################
    library(ggplot2)
    library(readxl)
    library(viridis)  # Or scales for custom colors
    
    # Read in excel file
    temp7_data <- read_excel("12Aug24_bio_7.xlsx")
    
    # Order the y axis to match the tips of the phylogeny
    phylo_order <- c("ambiguus", "clavatus", "concolor", "kennedyi", "catalinae", "splendens", "dunnii", "palmeri", "flexuosus", "occidentalis", "ownbeyi", "spatulatus", "cernuus", "balsensis", "hartwegii", "purpureus", "marcellae", "venustulus", "nigrescens", "fuscus", "pringlei", "barbatus", "exilis", "ghiesbreghtii", "mendozae", "weedii")
    temp7_data$species <- factor(temp7_data$species, levels = phylo_order)
    
    
# Create the box plot with customized aesthetics
    temp7_plot <- ggplot(temp7_data, aes(x = species, y = temperature, fill = species)) +
      geom_boxplot(outlier.size = 2, outlier.colour = "lightgray", 
                   fill = "gray", colour = "black", 
                   linetype = "solid", alpha = 0.7) +
      coord_flip() +  # Flip coordinates to make the box plot horizontal
      labs(
        title = "Temperature Annual Range",
        x = "Species",
        y = "Temperature (ºC)"
      ) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.position = "none"  # Hide the legend if not necessary
      )
# Save the plot to a PDF file
    pdf("12aug24temperature7_boxplot.pdf", width = 8, height = 6)
    print(temp7_plot)
    dev.off()
# Plot the plot
plot(temp7_plot)

#####Wilcoxon rank-sum test (Mann-Whitney U test)##########################################################
# Load necessary library
library(dplyr)

# ghiesbreghtii/exilis sister pair #p-value = 0.05933
# Extract temperature data for each species
group1 <- temp7_data %>% filter(species == "ghiesbreghtii") %>% pull(temperature)
group2 <- temp7_data %>% filter(species == "exilis") %>% pull(temperature)
# Perform the Wilcoxon rank-sum test (Mann-Whitney U test)
wilcoxon_rank_sum_result <- wilcox.test(group1, group2)
# Print the result
print(wilcoxon_rank_sum_result)

# pringlei/fuscus sister pair p-value = 6.365e-08
group3 <- temp7_data %>% filter(species == "pringlei") %>% pull(temperature)
group4 <- temp7_data %>% filter(species == "fuscus") %>% pull(temperature)
#test =<0.05 is a significant difference
# Perform the Wilcoxon rank-sum test (Mann-Whitney U test)
wilcoxon_rank_sum_result2 <- wilcox.test(group3, group4)
# Print the result
print(wilcoxon_rank_sum_result2)

# venustulus/marcellae sister pair p-value = 2.596e-06
group5 <- temp7_data %>% filter(species == "venustulus") %>% pull(temperature)
group6 <- temp7_data %>% filter(species == "marcellae") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result3 <- wilcox.test(group5, group6)
# Print the result
print(wilcoxon_rank_sum_result3)

# hartwegii/purpureus sister pair p-value = 0.0001605
group7 <- temp7_data %>% filter(species == "hartwegii") %>% pull(temperature)
group8 <- temp7_data %>% filter(species == "purpureus") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result4 <- wilcox.test(group7, group8)
# Print the result
print(wilcoxon_rank_sum_result4)

# cernuus/spatulatus sister pair p-value = 8.591e-08
group9 <- temp7_data %>% filter(species == "cernuus") %>% pull(temperature)
group10 <- temp7_data %>% filter(species == "spatulatus") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result5 <- wilcox.test(group9, group10)
# Print the result
print(wilcoxon_rank_sum_result5)


# ownbeyi/occidentalis sister pair p-value = 1.721e-06
group11 <- temp7_data %>% filter(species == "ownbeyi") %>% pull(temperature)
group12 <- temp7_data %>% filter(species == "occidentalis") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result6 <- wilcox.test(group11, group12)
# Print the result
print(wilcoxon_rank_sum_result6)

# catalinae/splendens sister pair p-value = 0.6129
group13 <- temp7_data %>% filter(species == "catalinae") %>% pull(temperature)
group14 <- temp7_data %>% filter(species == "splendens") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result7 <- wilcox.test(group13, group14)
# Print the result
print(wilcoxon_rank_sum_result7)

# concolor/clavatus sister pair p-value = 0.2541
group15 <- temp7_data %>% filter(species == "concolor") %>% pull(temperature)
group16 <- temp7_data %>% filter(species == "clavatus") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result8 <- wilcox.test(group15, group16)
# Print the result
print(wilcoxon_rank_sum_result8)

###SPECIES PAIRS
#ghiesbreghtii/exilis and barbatus species pair p-value < 2.2e-16
group17 <- temp7_data %>% 
  filter(species %in% c("ghiesbreghtii", "exilis")) %>% 
  pull(temperature)
group18 <- temp7_data %>% filter(species == "barbatus") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result9 <- wilcox.test(group17, group18)
# Print the result
print(wilcoxon_rank_sum_result9)

#pringlei/fuscus and nigrescens species pair p-value = 0.4317
group19 <- temp7_data %>% 
  filter(species %in% c("pringlei", "fuscus")) %>% 
  pull(temperature)
group20 <- temp7_data %>% filter(species == "nigrescens") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result10 <- wilcox.test(group19, group20)
# Print the result
print(wilcoxon_rank_sum_result10)

#purpureus/hartwegii and balsensis species pair p-value < 2.2e-16
group21 <- temp7_data %>% 
  filter(species %in% c("purpureus", "hartwegii")) %>% 
  pull(temperature)
group22 <- temp7_data %>% filter(species == "balsensis") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result11 <- wilcox.test(group21, group22)
# Print the result
print(wilcoxon_rank_sum_result11)

#catalinae/splendens and dunnii species pair p-value = 0.8491
group23 <- temp7_data %>% 
  filter(species %in% c("catalinae", "splendens")) %>% 
  pull(temperature)
group24 <- temp7_data %>% filter(species == "dunnii") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result12 <- wilcox.test(group23, group24)
# Print the result
print(wilcoxon_rank_sum_result12)

#clavatus/concolor and kennedyi species pair p-value = 2.976e-06
group25 <- temp7_data %>% 
  filter(species %in% c("clavatus", "concolor")) %>% 
  pull(temperature)
group26 <- temp7_data %>% filter(species == "kennedyi") %>% pull(temperature)
#test =<0.05 is a significant difference
wilcoxon_rank_sum_result13 <- wilcox.test(group25, group26)
# Print the result
print(wilcoxon_rank_sum_result13)
