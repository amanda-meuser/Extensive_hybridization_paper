# script for creating phylogeny with Fishtree (https://fishtreeoflife.org/)
# Amanda Meuser -- May 2023

# install packages
#install.packages("fishtree")

# load packages
library(fishtree)

# create an object to colour the species names
names <- factor(c("in", "out", "out", "out", "out", "out", "out", "out", "out"))
mycol <- c("firebrick2", "black")[names]

# phylogeny of target AMP22 species
phy_AMP22 <- fishtree_phylogeny(species = c("Luxilus cornutus", "Semotilus atromaculatus", "Campostoma anomalum", "Nocomis biguttatus", "Rhinichthys atratulus", "Luxilus chrysocephalus", "Rhinichthys cataractae", "Nocomis micropogon", "Notropis rubellus", "Pimephales notatus", "Pimephales promelas"))
phy_AMP22

#plot phylogeny
plot(phy_AMP22, use.edge.length = T, tip.color = mycol)

#output as PDF
pdf("Leuciscid_phylo_tree_pimeph.pdf")
plot(phy_AMP22, use.edge.length = T)
dev.off()
