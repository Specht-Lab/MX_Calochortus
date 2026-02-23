setwd("/Users/jharden946/Desktop/Graduate School/Publications/MX_Calochortus_19Feb26/MX_Calochortus_github")

# Load libraries
library(ape)
library(phytools)
library(geiger)
library(lmtest)

# Load tree
mytree <- read.tree("ConservativeTree_12.14.22.tre")
plot(mytree)
nodelabels()

  # Start with an empty simmap tree (no states yet)
states <- setNames(rep("unknown", Ntip(mytree)), mytree$tip.label)
simmap_tree <- mytree

# Paint specific nodes — example: node 90 = N. America, node 102 = Mexico, node 110 = Both
simmap_tree <- paintSubTree(simmap_tree, node = 75, state = "N. America", anc.state = "N. America", stem = FALSE)
simmap_tree <- paintSubTree(simmap_tree, node = 103, state = "Mexico", anc.state = "Mexico")
simmap_tree <- paintBranches(simmap_tree, edge = 1, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 15, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 4, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 5, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 27, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 18, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 17, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 16, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 6, state = "Both", anc.state = "Both")
simmap_tree <- paintBranches(simmap_tree, edge = 72, state = "Both", anc.state = "Both")

# Define colors for the states
location_colors <- setNames(c("black", "orange3", "green3"), c("N. America", "Mexico", "Both")) #if errors occur change the names back to lowercase

# Plot the painted tree
plotSimmap(simmap_tree, colors =location_colors, fsize = 0.8, lwd = 2)

# Add legend
legend("bottomleft", legend = names(location_colors), fill = location_colors, cex = 1)
 

