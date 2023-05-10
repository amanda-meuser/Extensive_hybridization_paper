## Script to filter out non-target species
## Amanda Meuser -- May 3, 2023

library(tidyverse)

indivs <- read.delim("Leuciscid_Metadata_Apr2023.csv", sep = ",")

#filter out the 23 non-target species
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
