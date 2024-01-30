# script for assessing amount of hybridization between species based on breeding behaviours
# AM - Aug 2023

library(dplyr)
library(tidyr)
library(hash)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(waffle)
library(patchwork)

# import hybrid data
# I'm going to exclude multi-species fishies from this (for now)
fishies <- read.csv("old_AMP22_genoID_multispecies_k12_Jun2023.csv")
fishies <- fishies %>% filter(Multi_Status == 0)
# also remove any Pimephales bc they weren't sampled properly
fishies <- fishies %>% filter(!str_detect(fishies$Geno_ID,'Pimephales') == TRUE)

# check what combos are present to make it easier to write the loop
(table_fishies <- as.data.frame(table(fishies$Geno_ID)))
#write.csv(table_fishies, "AMP22_Freq_BreedingBehaviours_k12_Jun2023.csv", row.names = F)

# classify breeding behaviour for parentals
fishies$Breeding_Behaviour = NA

# separate parental species into two columns
fishies = fishies %>%
  separate(Geno_ID, c('Species_1', 'Species_2'), ' x ', remove=F)

# create dictionary to match parentals to breeding behaviour
dict <- hash()
dict[["Creek_Chub"]] <- "NGBH"
dict[["Common_Shiner"]] <- "NGBH"
dict[["Western_Blacknose_Dace"]] <- "NGBH"
dict[["Longnose_Dace"]] <- "NGBH"
dict[["Central_Stoneroller"]] <- "NGBH"
dict[["Striped_Shiner"]] <- "NGBH"
dict[["Hornyhead_Chub"]] <- "NB"
dict[["River_Chub"]] <- "NB"
dict[["Rosyface_Shiner"]] <- "NA"
dict[["Pimephales_sp"]] <- "GS"

# assign the breeding behaviours
for(i in rownames(fishies)) {
  if (is.na(fishies[i, 'Species_2'])) {
    fishies[i, 'Breeding_Behaviour'] = dict[[fishies[i, 'Species_1']]]
  } else {
    a = dict[[fishies[i, 'Species_1']]]
    b = dict[[fishies[i, 'Species_2']]]
    fishies[i, 'Breeding_Behaviour'] = ifelse(a < b, paste(a, b, sep=' x '),
                                              paste(b, a, sep=' x ')) # always writes the names the same way
  }
}

# check that everything was assigned properly
(table_breeding <- as.data.frame(table(fishies$Breeding_Behaviour)))
colnames(table_breeding)[1] <- "Breeding_Behaviour"
# sort alphabetically
table_breeding <- with(table_breeding,  table_breeding[order(Breeding_Behaviour) , ])

# add Hybrid_Status back in
table_breeding$Hybrid_Status = NA
for (i in 1:nrow(table_breeding)){
  if (str_detect(table_breeding$Breeding_Behaviour[i],'x') == TRUE){
    table_breeding$Hybrid_Status[i] <- "1" 
  } else if (str_detect(table_breeding$Breeding_Behaviour[i],'x') == FALSE){
    table_breeding$Hybrid_Status[i] <- "0" 
  }
}

# clutstered bar plot (doesn't look great)
ggplot(table_breeding, aes(fill=Breeding_Behaviour, y=Freq, x=Hybrid_Status)) + 
  geom_bar(position="dodge", stat="identity")


# split up hybrids and parentals
hybrids <- filter(fishies, (str_detect(fishies$Breeding_Behaviour,'x') == TRUE))
parentals <- filter(fishies, (str_detect(fishies$Breeding_Behaviour,'x') == FALSE))

(table_hybrids <- as.data.frame(table(hybrids$Breeding_Behaviour)))
(table_parentals <- as.data.frame(table(parentals$Breeding_Behaviour)))
colnames(table_hybrids)[1] <- "Breeding_Behaviour"
colnames(table_hybrids)[2] <- "Observed"
colnames(table_parentals)[1] <- "Breeding_Behaviour"


(waffle_hybrids <- table_hybrids %>%
  ggplot(aes(fill = Breeding_Behaviour, values = Observed)) + 
  geom_waffle(n_rows = 7, size = 0.33, colour = "white", na.rm = FALSE) +
  coord_equal() +
  #theme_enhance_waffle() + labs(title="Breeding Behaviours - Hybrids") + 
  scale_fill_brewer(palette = "Set2", name = "Hybrid crosses"))


(waffle_parentals <- table_parentals %>%
  ggplot(aes(fill = Breeding_Behaviour, values = Freq)) + 
  geom_waffle(n_rows = 20, size = 0.33, colour = "white", na.rm = FALSE) +
  coord_equal() +
  #theme_enhance_waffle() + labs(title="Breeding Behaviours - Parentals") +  
  scale_fill_brewer(palette = "Set2", name = "Parental Behaviour"))


pdf("AMP22_target_Breeding_behaviours_new.pdf", width = 8, height = 8)
waffle_parentals + waffle_hybrids + 
  plot_layout(ncol = 1) +
  #plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')
dev.off()


#---------------------------------------------------------------------------
# Looking for significant differences b/w expected and observed # of hybrids
#---------------------------------------------------------------------------

# treat the # of parentals like allele frequencies, then calculate expected counts of 'genotype' frequencies aka hybrids b/w the parental breeding behaviour types
# p + q + r = 1
# p^2 + 2pq + 2pr + q^2 + 2qr + r^2 = 1

# calculate allele frequencies
propNA <- table_parentals$Freq[1] / sum(table_parentals$Freq)
propNB <- table_parentals$Freq[2] / sum(table_parentals$Freq)
propNGBH <- table_parentals$Freq[3] / sum(table_parentals$Freq)

# calculate genotype frequencies and expected values
totalObsv <- sum(table_hybrids$Observed)
table_hybrids$Expected <- NA
table_hybrids$Expected[1] <- round((2 * propNA * propNB) * totalObsv, digits = 1) # 2pq
table_hybrids$Expected[2] <- round((2 * propNA * propNGBH) * totalObsv, digits = 1) # 2pr
table_hybrids$Expected[3] <- round((propNB^2) * totalObsv, digits = 1) # q^2
table_hybrids$Expected[4] <- round((2 * propNB * propNGBH) * totalObsv, digits = 1) # 2qr
table_hybrids$Expected[5] <- round((propNGBH^2) * totalObsv, digits = 1) # r^2


# chi square test
# H0: no relationship between breeding behaviour and hybridization patterns
# H1: there is a significant difference between expected and observed numbers of hybrids between groups so there is some sort of influence of breeding behaviour on hybridization, as individuals are not assorting independently 
test <- chisq.test(table_hybrids[,2:3])
test # p-value is non-significant, so we do not reject the null hypothesis, and the population is in HWE


#---------------------------------------------------------------------------
# Splitting by site
#---------------------------------------------------------------------------

details <- read.csv("../Leuciscid_Metadata_May2023.csv", header = T)

#edit details
details$Waterbody[details$Waterbody_Code == "OP2"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "OP7"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "Weir DS"] <- "Costello_Creek"
details$Waterbody_Code[details$Waterbody == "Costello_Creek"] <- "COS"

fishies_details <- merge(fishies, details, by = c("Mandeville_ID", "Common_Name"))
fishies_sites <- fishies_details %>% group_by(Waterbody_Code) %>% count(Breeding_Behaviour)

# create frequency column
sums <- fishies_sites %>% 
  group_by(Waterbody_Code) %>%
  summarise(n_sum = sum(n),
            .groups = 'drop')
fishies_sites <- merge(fishies_sites, sums, by='Waterbody_Code')
getproportion <- function(row) {
  return (row$n/row$n_sum)
}
fishies_sites <- fishies_sites %>% mutate(proportion = n / n_sum)

# order by the frequency column
fishies_sites = fishies_sites %>% 
  group_by(Waterbody_Code) %>% 
  arrange((Breeding_Behaviour))# we assumed parentals are the largest proportion all the time, so they'll always come up before the other two types

# create the object w the levels in order
fishies_sites_order = fishies_sites[!duplicated(fishies_sites$Waterbody_Code), ]$Waterbody_Code
# tell it how we want the levels
fishies_sites$Waterbody_Code <- factor(fishies_sites$Waterbody_Code, levels = fishies_sites_order)

# plot 
ggplot(fishies_sites, aes(fill=Breeding_Behaviour, y=n, x=Waterbody_Code)) + 
  geom_bar(position="fill", stat="identity")+ 
  ylab("Proportion of total individuals") +
  xlab("Waterbody") + 
  scale_fill_brewer(palette="Spectral", name="Breeding Behaviour")







