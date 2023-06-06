## Script to filter out non-target species
## Amanda Meuser -- May 3, 2023

library(tidyverse)

indivs <- read.delim("Leuciscid_Metadata_May2023.csv", sep = ",")

#filter out the 23 indivs from non-target species
indivs_target <- filter(indivs, Common_Name == "Central_Stoneroller" | Common_Name == "Common_Shiner" | Common_Name == "Creek_Chub" | Common_Name == "Hornyhead_Chub" | Common_Name == "Longnose_Dace" | Common_Name == "River_Chub" | Common_Name == "Rosyface_Shiner" | Common_Name == "Striped_Shiner" | Common_Name == "Western_Blacknose_Dace")

# make a table of frequency of each species, just to check we kept what we want
FishesSStable <- table("Species"=indivs_target$Common_Name)
FishesSStable

# save the names to a new data frame
names <- data.frame(indivs_target$Mandeville_ID)

#export list of file names as text file (no paths)
write.table(names, "AMP22_target_indivs.txt", sep = "\t", row.names = F, quote = F)


# add the path prefix and suffix to all individual IDs
names$indivs_target.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names$indivs_target.Mandeville_ID, '.sorted.bam')

# remove header
names(names) <- NULL

#export list of file names as text file
write.table(names, "AMP22_target_indivs_paths.txt", sep = "\t", row.names = F, quote = F)

#-------------------------------------------------------------------------------
# Make a text file of indivs for filtering the VCF file for each species pair

#filter out the non-target species
indivs_BNDxCC <- filter(indivs, Common_Name == "Creek_Chub" | Common_Name == "Western_Blacknose_Dace")
indivs_CSRxCC <- filter(indivs, Common_Name == "Central_Stoneroller" | Common_Name == "Creek_Chub")
indivs_CSxCC <- filter(indivs, Common_Name == "Common_Shiner" | Common_Name == "Creek_Chub")
indivs_HHCxCC <- filter(indivs, Common_Name == "Creek_Chub" | Common_Name == "Hornyhead_Chub")
indivs_LNDxCC <- filter(indivs, Common_Name == "Creek_Chub" | Common_Name == "Longnose_Dace")
indivs_RCxCC <- filter(indivs, Common_Name == "Creek_Chub" | Common_Name == "River_Chub")
indivs_RFSxCC <- filter(indivs, Common_Name == "Creek_Chub" | Common_Name == "Rosyface_Shiner")
indivs_SSxCC <- filter(indivs, Common_Name == "Creek_Chub" | Common_Name == "Striped_Shiner")

# save the names to a new data frame
names_BNDxCC <- data.frame(indivs_BNDxCC$Mandeville_ID)
names_CSRxCC <- data.frame(indivs_CSRxCC$Mandeville_ID)
names_CSxCC <- data.frame(indivs_CSxCC$Mandeville_ID)
names_HHCxCC <- data.frame(indivs_HHCxCC$Mandeville_ID)
names_LNDxCC <- data.frame(indivs_LNDxCC$Mandeville_ID)
names_RCxCC <- data.frame(indivs_RCxCC$Mandeville_ID)
names_RFSxCC <- data.frame(indivs_RFSxCC$Mandeville_ID)
names_SSxCC <- data.frame(indivs_SSxCC$Mandeville_ID)

#export list of file names as text file (no paths)
write.table(names_BNDxCC, "names_BNDxCC.txt", sep = "\t", row.names = F, quote = F)
write.table(names_CSRxCC, "names_CSRxCC.txt", sep = "\t", row.names = F, quote = F)
write.table(names_CSxCC, "names_CSxCC.txt", sep = "\t", row.names = F, quote = F)
write.table(names_HHCxCC, "names_HHCxCC.txt", sep = "\t", row.names = F, quote = F)
write.table(names_LNDxCC, "names_LNDxCC.txt", sep = "\t", row.names = F, quote = F)
write.table(names_RCxCC, "names_RCxCC.txt", sep = "\t", row.names = F, quote = F)
write.table(names_RFSxCC, "names_RFSxCC.txt", sep = "\t", row.names = F, quote = F)
write.table(names_SSxCC, "names_SSxCC.txt", sep = "\t", row.names = F, quote = F)


# add the path prefix and suffix to all individual IDs
names_BNDxCC$indivs_BNDxCC.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_BNDxCC$indivs_BNDxCC.Mandeville_ID, '.sorted.bam')
names_CSRxCC$indivs_CSRxCC.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_CSRxCC$indivs_CSRxCC.Mandeville_ID, '.sorted.bam')
names_CSxCC$indivs_CSxCC.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_CSxCC$indivs_CSxCC.Mandeville_ID, '.sorted.bam')
names_HHCxCC$indivs_HHCxCC.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_HHCxCC$indivs_HHCxCC.Mandeville_ID, '.sorted.bam')
names_LNDxCC$indivs_LNDxCC.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_LNDxCC$indivs_LNDxCC.Mandeville_ID, '.sorted.bam')
names_RCxCC$indivs_RCxCC.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_RCxCC$indivs_RCxCC.Mandeville_ID, '.sorted.bam')
names_RFSxCC$indivs_RFSxCC.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_RFSxCC$indivs_RFSxCC.Mandeville_ID, '.sorted.bam')
names_SSxCC$indivs_SSxCC.Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_SSxCC$indivs_SSxCC.Mandeville_ID, '.sorted.bam')

# remove header
names(names_BNDxCC) <- NULL
names(names_CSRxCC) <- NULL
names(names_CSxCC) <- NULL
names(names_HHCxCC) <- NULL
names(names_LNDxCC) <- NULL
names(names_RCxCC) <- NULL
names(names_RFSxCC) <- NULL
names(names_SSxCC) <- NULL

#export list of file names as text file
write.table(names_BNDxCC, "names_BNDxCC_paths.txt", sep = "\t", row.names = F, quote = F)
write.table(names_CSRxCC, "names_CSRxCC_paths.txt", sep = "\t", row.names = F, quote = F)
write.table(names_CSxCC, "names_CSxCC_paths.txt", sep = "\t", row.names = F, quote = F)
write.table(names_HHCxCC, "names_HHCxCC_paths.txt", sep = "\t", row.names = F, quote = F)
write.table(names_LNDxCC, "names_LNDxCC_paths.txt", sep = "\t", row.names = F, quote = F)
write.table(names_RCxCC, "names_RCxCC_paths.txt", sep = "\t", row.names = F, quote = F)
write.table(names_RFSxCC, "names_RFSxCC_paths.txt", sep = "\t", row.names = F, quote = F)
write.table(names_SSxCC, "names_SSxCC_paths.txt", sep = "\t", row.names = F, quote = F)


#-------------------------------------------------------------------------------

# make a list of species IDs for filtered VCF list of 800 indivs

# import list of filtered indivs
filt_inds <- read.delim("./names_files/AMP22_target_indivs_filtered.txt", sep = "\n", header = F)

#merge data frames
filt_details <- merge(indivs, filt_inds, by.x = "Mandeville_ID", by.y = "V1", all.y = T)

# isolate species IDs
filt_species <- (as.data.frame(filt_details$Common_Name))
names(filt_species) <- NULL

# export as text file
write.table(filt_species, "./names_files/AMP22_target_species_filtered.txt", sep = "\t", row.names = F, quote = F)


