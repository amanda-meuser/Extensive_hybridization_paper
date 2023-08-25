# investigating inds with "large" CIs
# AM -- Aug 2023

library(dplyr)

CIs <- read.table("AMP22_target_k12_CIs.txt", header = T)
metadata <- read.csv("old_AMP22_genoID_multispecies_k12_Jun2023.csv")

large.CIs <- CIs %>% filter(CI_width > 0.01)
large.CIs

large.CIs.data <- merge(metadata, large.CIs, by = "Mandeville_ID")
table(large.CIs.data$Geno_ID) # most of them have central stoneroller ancestry
