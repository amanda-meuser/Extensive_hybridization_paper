#============================================================
#============================================================

# Comparing phylogenetic distance and hybridization outcomes
# Feb 2025 -- Amanda Meuser

#============================================================
#============================================================
library(tidyverse)
library(hash)
library(psych)

dist_abs <- read.csv("phylo_distance_absolute.csv") # this file is generated in the fishtree_AMP22.R script
fishies <- read.csv("AMP22_genoID_multispecies_k12_Jun2023.csv") # this file is generated in the assign_geno_ID.R script

# divide all phylo distances by two bc it's measuring adding the distance between the branches together
dist_abs[2:11] <- dist_abs[2:11]/2

# get rid of multi sp hybs bc idk how to deal w them in this scenario
fishies <- filter(fishies, Multi_Status == "0") #drops data set from 731 to 691

# copying some of this from the breeding behaviours script
# check what combos are present to make it easier to write the loop
(table_fishies <- as.data.frame(table(fishies$Geno_ID)))


# need to group the hybrids together so that there's fewer groups
# split up the names
fishies <- fishies %>%
  separate(Geno_ID, c('Species_1', 'Species_2'), ' x ', remove=F)
# alphabetize the names
fishies$Geno_ID_order = NA
for (i in 1:nrow(fishies)){
  a <- fishies$Species_1[i]
  b <- fishies$Species_2[i]
  fishies$Geno_ID_order[i] = ifelse(a < b, paste(a, b, sep =' x '), paste(b, a, sep =' x ')) # always writes the names the same way
  fishies$Geno_ID_order[i] = ifelse(is.na(fishies$Geno_ID_order[i]), paste(a),next) # pastes in just a if no b
}
# check again
(table_fishies <- as.data.frame(table(fishies$Geno_ID_order)))


fishies$Abbreviation = NA

# create dictionary to match parentals to abbreviation
dict <- hash()
dict[["Creek_Chub"]] <- "CC"
dict[["Common_Shiner"]] <- "CS"
dict[["Western_Blacknose_Dace"]] <- "BND"
dict[["Longnose_Dace"]] <- "LND"
dict[["Central_Stoneroller"]] <- "CSR"
dict[["Striped_Shiner"]] <- "SS"
dict[["Hornyhead_Chub"]] <- "HHC"
dict[["River_Chub"]] <- "RC"
dict[["Rosyface_Shiner"]] <- "RFS"
dict[["Pimephales_sp"]] <- "PS"

# assign the abbreviations
for(i in rownames(fishies)) {
  if (is.na(fishies[i, 'Species_2'])) {
    fishies[i, 'Abbreviation'] = dict[[fishies[i, 'Species_1']]]
  } else {
    a = dict[[fishies[i, 'Species_1']]]
    b = dict[[fishies[i, 'Species_2']]]
    fishies[i, 'Abbreviation'] = ifelse(a < b, paste(a, b, sep=' x '),
                                        paste(b, a, sep=' x ')) # always writes the names the same way
  }
}

# create a table to look at all unique combos and parentals
(table_fishies_abbr <- as.data.frame(table(fishies$Abbreviation))) # dropped from 52 to 38 entries

# parental only table
parentals <- filter(table_fishies_abbr, str_detect(table_fishies_abbr$Var1,'x') == FALSE) # 9 options out of possible 10 (no pure SS)
parentals$Var1 <- as.character(parentals$Var1)

# make table for math
numbers <- filter(table_fishies_abbr, str_detect(table_fishies_abbr$Var1,'x') == TRUE) # 29 combos out of possible 45
names(numbers) <- c("Abbreviation", "Freq_hybs_only")

# separate parental species into two columns
numbers = numbers %>%
  separate(Abbreviation, c('Species_1', 'Species_2'), ' x ', remove=F)

numbers$Freq_w_parentals = NA
numbers$Prop_hybs = NA

# for loop that does math
for (i in 1:nrow(numbers)) {
  # find matching indices for both parental species
  match1 <- parentals$Freq[which(str_detect(numbers$Species_1[i], parentals$Var1))]
  match2 <- parentals$Freq[which(str_detect(numbers$Species_2[i], parentals$Var1))]
  # sum the matching frequencies and update the column
  numbers$Freq_w_parentals[i] <- numbers$Freq_hybs_only[i] + sum(match1, na.rm = TRUE) + sum(match2, na.rm = TRUE)
  # get proportion of hybrids
  numbers$Prop_hybs[i] <- numbers$Freq_hybs_only[i] / numbers$Freq_w_parentals[i]
}


# connect the above proportion of hybrids with the phylogenetic distance matrix

# create dictionary to match scientific names to abbreviation
dict2 <- hash()
dict2[["Semotilus_atromaculatus"]] <- "CC"
dict2[["Luxilus_cornutus"]] <- "CS"
dict2[["Rhinichthys_atratulus"]] <- "BND"
dict2[["Rhinichthys_cataractae"]] <- "LND"
dict2[["Campostoma_anomalum"]] <- "CSR"
dict2[["Luxilus_chrysocephalus"]] <- "SS"
dict2[["Nocomis_biguttatus"]] <- "HHC"
dict2[["Nocomis_micropogon"]] <- "RC"
dict2[["Notropis_rubellus"]] <- "RFS"
dict2[["Pimephales_notatus"]] <- "PS" # this is bluntnose, but they're the equidistant from all other species
dict2[["Pimephales_promelas"]] <- "PS" # this is fathead, but they're the equidistant from all other species

#change names to abbreviations to match numbers df
dist_abs$Abbreviation = NA

for (i in rownames(dist_abs)){
  dist_abs[i, 'Abbreviation'] = dict2[[dist_abs[i, 'X']]]
}

# move to front of df
dist_abs <- dist_abs %>% relocate(Abbreviation)

names(dist_abs)[2:13] <- c("Scientific_name", "CC", "PS", "PS", "RFS", "CS", "SS", "RC", "HHC", "CSR", "LND", "BND")

# remove second PS column and row
dist_abs <- dist_abs[,-5]
dist_abs <- dist_abs[-3,]

# add a column in numbers for phylo distance
numbers$Phylo_dist = NA
# set row names as col 1 values then remove col 1
rownames(dist_abs) <- dist_abs[,1]
dist_abs <- dist_abs[,-1]

for (i in 1:nrow(numbers)){
  # get coordinates
  match1 <- grep(numbers$Species_1[i], rownames(dist_abs)) # row
  match2 <- grep(numbers$Species_2[i], colnames(dist_abs)) # column
  # sub in value [row,column]
  numbers$Phylo_dist[i] <- dist_abs[match1,match2]
} # it works, idk what the warning messages are about



# FINALLY!! PLOTTING!!

plot(numbers$Phylo_dist, numbers$Prop_hybs)

# idk why it thinks Phylo_dist is a list
numbers$Phylo_dist <- as.numeric(numbers$Phylo_dist)

(phydistplot <- ggplot(numbers, aes(x = Phylo_dist, y = Prop_hybs))+
    geom_point() + 
    geom_text(label=numbers$Abbreviation) +
    xlab("Phylogenetic distance (millions of years)") + ylab("Proportion of hybrids"))

# pdf("Prop_hybs_vs_phylo_dist.pdf", height = 8, width = 12)
# phydistplot
# dev.off()

print(corr.test(numbers$Phylo_dist, y=numbers$Prop_hybs, use = "pairwise",method="pearson"),short=FALSE)

