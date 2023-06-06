# script for creating phylogeny with Fishtree (https://fishtreeoflife.org/)
# Amanda Meuser -- May 2023

# install packages
install.packages("fishtree")
#install.packages("ape")

# load packages
library(fishtree)


# phylogeny of target AMP22 species
phy_AMP22 <- fishtree_phylogeny(species = c("Luxilus cornutus", "Semotilus atromaculatus", "Campostoma anomalum", "Nocomis biguttatus", "Rhinichthys atratulus", "Luxilus chrysocephalus", "Rhinichthys cataractae", "Nocomis micropogon", "Notropis rubellus"))
phy_AMP22

#output as PDF
pdf("Leuciscid_phylo_tree.pdf")
plot(phy_AMP22)
dev.off()