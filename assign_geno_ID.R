# creating a loop for classifying hybrid status

library(stringr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#=======================================================
# SIMULATED DATA
#=======================================================

#import simulated data for testing
sim <- read.csv("entropy_simulated.csv")

# assign species to entropy populations
names(sim)[4:8] <- c("Big_Chub", "Fat_Dace", "Pearl_Chub", "Rainbow_Dace", "Scarlet_Minnow")

# create new columns
# Geno_ID (will contain Common_Name or "hybrid")
# Pheno_correct (binary, 0=correct, 1=incorrect)
# Hybrid_Status (binary, 0=hybrid, 1=parental)
sim$Geno_ID = NA
sim$Pheno_Correct = NA
sim$Hybrid_Status = NA

# assigns hybrids as 'hybrid'
for (i in 1:nrow(sim)){ #assuming the col indexes, might be better to use names
  for (j in 4:8){
    if (sim[i,j] > 0.9) {
      sim$Geno_ID[i] <- colnames(sim)[j]
      break
    }
    else {
      sim$Geno_ID[i] <- "hybrid"
    }
  }
}


#creating a jump table for the hybrid abbreviations


# inserts name of what is in the hybrids
for (i in 1:nrow(sim)){ #assuming the col indexes, might be better to use names
  for (j in 4:8){
    if (sim[i,j] > 0.9) {
      sim$Geno_ID[i] <- colnames(sim)[j]
      break
    }
    else { # pulls the columns names for 4:8, sorts them by cell value, then takes the largest 1:2 (largest 2) 
      holder <- colnames(sim[i, 4:8])[order(sim[i, 4:8], decreasing=TRUE)[1:2]]
      sim$Geno_ID[i] <- paste(holder[1],"x",holder[2])
    }
  }
}

# assign 0 or 1 for whether whether the ind is a hybrid
for (i in 1:nrow(sim)){
  if(str_detect(sim[i,10],'x') == TRUE){
    sim$Hybrid_Status[i] <- 1 #hybrid
  }
  else {
    sim$Hybrid_Status[i] <- 0 #parental
  }
}


# assign 0 or 1 for whether phenotypic ID was correct 
for (i in 1:nrow(sim)){
  if(identical(sim[i,2],sim[i,10]) == FALSE){
    sim$Pheno_Correct[i] <- 1 #mis-ID
  }
  else {
    sim$Pheno_Correct[i] <- 0 #correct ID
  }
}


#=======================================================
# REAL DATA
#=======================================================

x <- read.delim("AMP22_target_entropy_k12_indivs_q.txt", sep = "\t", header = T)
k = 12

# load metadata
metadata <- read.csv("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/Leuciscid_Metadata_May2023.csv")

# get list of and drop 84 inds from plate 13 inds
plate13inds <- filter(metadata, Plate == "AMP22_LP13")
plate13inds <- plate13inds[,4]
plate13inds <- as.data.frame(plate13inds)
names(plate13inds) <- "ind"
x <- anti_join(x, plate13inds)
nrowx <- nrow(x)

# assign species to entropy populations
#k=9
#names(x)[3:11] <- c("Creek_Chub", "Longnose_Dace", "Striped_Shiner", "Rosyface_Shiner", "Central_Stoneroller", "Common_Shiner", "River_Chub", "Western_Blacknose_Dace", "Hornyhead_Chub")
#k=12
names(x)[3:14] <-c("Common_Shiner", "Central_Stoneroller", "Hornyhead_Chub", "Longnose_Dace", "Striped_Shiner", "River_Chub", "V7", "Rosyface_Shiner", "Creek_Chub", "Western_Blacknose_Dace", "V11", "V12")

# create new columns
# Geno_ID (will contain Common_Name or "hybrid")
# Pheno_correct (binary, 0=correct, 1=incorrect)
# Hybrid_Status (binary, 0=hybrid, 1=parental)
x$Geno_ID = NA
x$Pheno_Correct = NA
x$Hybrid_Status = NA
x$Multi_Status = NA


# inserts name of what is in the hybrids (3:11 for k=9, 3:14 for k=12)
# for (i in 1:nrow(x)){ #assuming the col indexes, might be better to use names
#   for (j in 3:14){
#     if (x[i,j] > 0.9) {
#       x$Geno_ID[i] <- colnames(x)[j]
#       break
#     }
#     else if (x[i,j] > 0.1 * 2){
#       holder <- colnames(x[i, 3:14])[order(x[i, 3:14], decreasing=TRUE)[1:2]]
#       x$Geno_ID[i] <- paste(holder[1],"x",holder[2])
#     }
#     else { # pulls the columns names for 3:14 (the fish species), sorts them by cell value, then takes the largest 1:2 (largest 2) 
#       x$Geno_ID[i] <- "Multi"
#     }
#   }
# }

# more options !! - classifies as multi-species hybrid
for (i in 1:nrow(x)){ #assuming the col indexes, might be better to use names
  colmatches = 0
  for (j in 3:14){
    if (x[i,j] >= 0.9) {
      x$Geno_ID[i] <- colnames(x)[j]
      colmatches = 5000 # huge number that's way more than max columns
      break
    }
    else if (x[i,j] >= 0.1 & x[i,j] <= 0.9) {
      colmatches = colmatches + 1
    }
  }
  if (colmatches == 2) {
    #2 species match found
    holder <- colnames(x[i, 3:14])[order(x[i, 3:14], decreasing=TRUE)[1:2]]
    x$Geno_ID[i] <- paste(holder, collapse=' x ')
  }
  else if (colmatches > 2 & colmatches < 20) { # def 20 columns is max
    #multi-species match found
    holder2 <- colnames(x[i, 3:14])[order(x[i, 3:14], decreasing=TRUE)[1:colmatches]]
    x$Geno_ID[i] <- paste(holder2, collapse=' x ')
  }
  else if (colmatches <= 1){
    # there's only one or no species that contribute 0.1-0.9 ancestry
    holder3 <- colnames(x[i, 3:14])[order(x[i, 3:14], decreasing=TRUE)[1]]
    x$Geno_ID[i] <- paste(holder3, "x fishiness")
  }
}

#something that didn't work...
#noquote(paste0(replicate(colmatches, ',"x",holder2[v]')))

# assign 0 or 1 for whether whether the ind is a hybrid (x[i,12] for k=9, x[i,15] for k=12)
for (i in 1:nrow(x)){
  if(str_detect(x[i,15],'x') == TRUE){
    x$Hybrid_Status[i] <- 1 #hybrid
  }
  else if (str_detect(x[i,15],'x fishiness') == TRUE){
    x$Hybrid_Status[i] <- 1 #hybrid
  }
  else {
    x$Hybrid_Status[i] <- 0 #parental
  }
}

# assign 0 or 1 for whether ind is a multi-species hybrid (x[i,12] for k=9, x[i,15] for k=12)
for (i in 1:nrow(x)){
  if(str_detect(x[i,15],'x\\s+\\w+\\s+x') == TRUE){
    x$Multi_Status[i] <- 1 #multi
  }
  else if (str_detect(x[i,15],'x fishiness') == TRUE){
    x$Multi_Status[i] <- 1 #hybrid
  }
  else {
    x$Multi_Status[i] <- 0 #non-multi
  }
}

# assign 0 or 1 for whether phenotypic ID was correct (x[i,12] for k=9, x[i,15] for k=12)
for (i in 1:nrow(x)){
  if(identical(x[i,2],x[i,15]) == FALSE){
    x$Pheno_Correct[i] <- 1 #mis-ID
  }
  else {
    x$Pheno_Correct[i] <- 0 #correct ID
  }
}

# save the new loop info to a csv file (731 inds)
#write.csv(x, "old_AMP22_genoID_multispecies_k12_Jun2023.csv", quote = F, row.names = F)

# Create a data table from the geno IDs and hybrid statuses
table_pheno <- table(x$Pheno_Correct)
table_hybrid <- table(x$Hybrid_Status)
table_multi <- table(x$Multi_Status)
dftable_multi <- as.data.frame(table_multi)
dftable_pheno <- as.data.frame(table_pheno)
dftable_hybrid <- as.data.frame(table_hybrid)
dftable <- rbind(dftable_multi, dftable_hybrid, dftable_pheno)
names(dftable) <- c("No.Yes", "Freq")
dftable$Question <- "Was this ind mis-ID'd?"
dftable$Question[1:2] <- "Is this ind a multi-species hybrid?"
dftable$Question[3:4] <- "Is this ind a hybrid?"
names(dftable) <- c("No.Yes", "Freq", "Question")

# plot
(stacked_plot <- ggplot(dftable, aes(fill=No.Yes, y=Freq, x=Question)) + 
                geom_bar(position="stack", stat="identity") +
                ylab("Number of individuals")+
                scale_fill_manual(values = c("#E69F00", "#56B4E9"), 
                                  name = "Legend", 
                                  labels = c("No", "Yes")) )


# Create a data table from the geno IDs and hybrid statuses that makes more sense
table_pheno <- table(x$Pheno_Correct)
table_hybrid <- table(x$Hybrid_Status)
table_multi <- table(x$Multi_Status)
dftable_multi <- as.data.frame(table_multi)
dftable_pheno <- as.data.frame(table_pheno)
dftable_hybrid <- as.data.frame(table_hybrid)
dfallstats <- data.frame(matrix(NA,    # Create empty data frame
                                nrow = 1,
                                ncol = 1))
names(dfallstats) <- "State"
dfallstats[1,1] <- (dftable_hybrid[2,2] - dftable_multi[2,2])
dfallstats[2,1] <- dftable_multi[2,2]
dfallstats[3,1] <- dftable_pheno[1,2]
dfallstats[4,1] <- (nrowx - (dfallstats[1,1] + dfallstats[2,1] + dfallstats[3,1]))
dfallstats[,2] <- "x"
dfallstats[,3] <- c("1", "2", "3", "4")


# plot
(stacked_plot_single <- ggplot(dfallstats, aes(fill=V3, y=State, x=V2)) + 
    geom_bar(position="stack", stat="identity") +
    ylab("Number of individuals")+
    xlab("")+
    geom_text(aes(label = State), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
    scale_fill_manual(values = c("#FBC4DE", "#FDFA9E", "#E264A0", "#A0E49F"), 
                      name = "Legend", 
                      labels = c("2-species hybrid", "Multi-species hybrid", "Correct Pheno ID (Non-hybrid)", "Incorrect Pheno ID (Non-hybrid)")) )


#pdf(paste0("genomic_IDs_stats_1bar_k",k,".pdf"))
#stacked_plot_single
#dev.off()



#---------------------------------------------------------------------------
# break this down by site !!
#---------------------------------------------------------------------------

#edit metadata
metadata$Waterbody[metadata$Waterbody_Code == "OP2"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "OP7"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "Weir DS"] <- "Costello_Creek"
metadata$Waterbody_Code[metadata$Waterbody == "Costello_Creek"] <- "COS"
# split types by site
meta_names <- metadata$Mandeville_ID
meta_sites <- metadata$Waterbody_Code
meta_namesandsites <- as.data.frame(cbind(meta_names, meta_sites))
colnames(meta_namesandsites) <- c("Mandeville_ID", "Waterbody_Code")
colnames(x)[1:2] <- c("Mandeville_ID", "Common_Name")
x_sites <- merge(x, meta_namesandsites, by.x = "Mandeville_ID", by.y = "Mandeville_ID")

# simplify hybrid type and plot
x_sites$Hybrid_Type_Simple = NA

for (i in 1:nrow(x_sites)){
  if (str_detect(x[i,15],'x\\s+\\w+\\s+x') == TRUE){
    x_sites$Hybrid_Type_Simple[i] <- "Multi-species hybrid"
  }
  else if(str_detect(x[i,15],'x') == TRUE & str_detect(x[i,15],'x fishiness') == FALSE){
    x_sites$Hybrid_Type_Simple[i] <- "2-species hybrid" 
  }
  else if (str_detect(x[i,15],'x fishiness') == TRUE){
    x_sites$Hybrid_Type_Simple[i] <- "Multi-species hybrid"
  }
  else if(str_detect(x[i,15],'x') == FALSE){
    x_sites$Hybrid_Type_Simple[i] <- "Parental"
  }
}

# create counts of hybrid types
x_count <- x_sites %>% group_by(Waterbody_Code) %>% count(Hybrid_Type_Simple)

# create frequency column
sums <- x_count %>% 
  group_by(Waterbody_Code) %>%
  summarise(n_sum = sum(n),
            .groups = 'drop')
x_count <- merge(x_count, sums, by='Waterbody_Code')
getproportion <- function(row) {
  return (row$n/row$n_sum)
}
x_count <- x_count %>% mutate(proportion = n / n_sum)

# order by the frequency column
x_count = x_count %>% 
  group_by(Waterbody_Code) %>% 
  arrange(-proportion)# we assumed parentals are the largest proportion all the time, so they'll always come up before the other two types

# create the object w the levels in order
x_order = x_count[!duplicated(x_count$Waterbody_Code), ]$Waterbody_Code
# tell it how we want the levels
x_count$Waterbody_Code <- factor(x_count$Waterbody_Code, levels = x_order)

# plot 
ggplot(x_count, aes(fill=Hybrid_Type_Simple, y=n, x=Waterbody_Code)) + 
  geom_bar(position="fill", stat="identity")+ 
  ylab("Proportion of total individuals") +
  xlab("Waterbody") + 
  scale_fill_brewer(palette="Spectral", name="Hybrid Type")









# put mis-ID's into a separate data frame and merge  
misIDs <- subset(x, Pheno_Correct %in% 1 & Hybrid_Status %in% 0)
x_metadata <- merge(x, metadata, by.x = "ind", by.y = "Mandeville_ID", all.x = T, all.y = F)
misID_metadata <- merge(misIDs, metadata, by.x = "ind", by.y = "Mandeville_ID", all.x = T, all.y = F)

#export as CSV
#write.csv(misID_metadata, paste0("AMP22_Leuciscid_MisID_k",k,"_Jun2023.csv"), row.names = F)

# misIDs per plate
table_plate <- as.data.frame(table(misID_metadata$Plate))

#export as text file
#write.table(table_plate, file = paste0("MisIDs_per_plate_k",k,".txt"), quote = F, row.names = F, col.names = F)




# put multi-species hybrids in their own table                           
multihybrids <- subset(x,  Multi_Status %in% 1)
multihybrids <- multihybrids %>% mutate_if(is.numeric, round, digits=3)
multihybrids_metadata <- merge(multihybrids, metadata, by.x = "ind", by.y = "Mandeville_ID", all.x = T, all.y = F)
(tableGenoID <- table(multihybrids$Geno_ID)) #12/44 are in the "x fishiness" category...

#4 three-way crosses w the big 3
#Common_Shiner x Western_Blacknose_Dace x Creek_Chub:2 
#Creek_Chub x Western_Blacknose_Dace x Common_Shiner:2
# 1 three-way w V7:
#Common_Shiner x Creek_Chub x V7:1
# so all but one ind w at least 0.1 V7 ancestry were DNA barcoded

#export as CSV
#write.csv(multihybrids_metadata, paste0("AMP22_Leuciscid_multispecies_ONLY_hybrids_k",k,"_Jun2023.csv"), row.names = F)


 

# hybrids per plate
table_plate_hybrid <- as.data.frame(table(hybrids_metadata$Plate))





# segregate the V7 ancestry weirdos
V7_weirdos <- filter(x, str_detect(x[,15],'V7'))

#export as csv file
#write.csv(V7_weirdos, "V7_ancestry_indivs.csv", row.names = F)


# pull out CS, CC, and CSxCC hybrids
CSxCC <- filter(x, Geno_ID == "Creek_Chub" | Geno_ID == "Common_Shiner" | Geno_ID == "Creek_Chub x Common_Shiner" | Geno_ID == "Common_Shiner x Creek_Chub")
inds_per_type_CSxCC <- aggregate(CSxCC$ind, by = list(CSxCC$Geno_ID), length)

# pull out BND, CC, and BNDxCC hybrids
BNDxCC <- filter(x, Geno_ID == "Creek_Chub" | Geno_ID == "Western_Blacknose_Dace" | Geno_ID == "Creek_Chub x Western_Blacknose_Dace" | Geno_ID == "Western_Blacknose_Dace x Creek_Chub")
inds_per_type_BNDxCC <- aggregate(BNDxCC$ind, by = list(BNDxCC$Geno_ID), length)

# pull out BND, CS, and BNDxCS hybrids
BNDxCS <- filter(x, Geno_ID == "Common_Shiner" | Geno_ID == "Western_Blacknose_Dace" | Geno_ID == "Common_Shiner x Western_Blacknose_Dace" | Geno_ID == "Western_Blacknose_Dace x Common_Shiner")
inds_per_type_BNDxCS <- aggregate(BNDxCS$ind, by = list(BNDxCS$Geno_ID), length)

# pull out BND, CS, CC, and BNDxCS, BNDxCC, and CSxCC hybrids (might abandon this due to time idk, but would be good to add the 3-way hybrids in here)
BNDxCSxCC <- filter(x, Geno_ID == "Common_Shiner" | Geno_ID == "Western_Blacknose_Dace" | Geno_ID == "Creek_Chub" | Geno_ID == "Common_Shiner x Western_Blacknose_Dace" | Geno_ID == "Western_Blacknose_Dace x Common_Shiner" | Geno_ID == "Creek_Chub x Common_Shiner" | Geno_ID == "Common_Shiner x Creek_Chub" | Geno_ID == "Creek_Chub x Western_Blacknose_Dace" | Geno_ID == "Western_Blacknose_Dace x Creek_Chub")
inds_per_type_BNDxCSxCC <- aggregate(BNDxCSxCC$ind, by = list(BNDxCSxCC$Geno_ID), length)

### exporting each geno ID as a text file of names

# save the names to a new data frame
names_BNDxCC <- data.frame(BNDxCC$ind)
names_BNDxCS <- data.frame(BNDxCS$ind)
names_CSxCC <- data.frame(CSxCC$ind)
names_BNDxCSxCC <- data.frame(BNDxCSxCC$ind)

#export list of file names as text file (no paths)
#write.table(names_BNDxCC, "names_BNDxCC_genoID.txt", sep = "\t", row.names = F, quote = F)
#write.table(names_BNDxCS, "names_BNDxCS_genoID.txt", sep = "\t", row.names = F, quote = F)
#write.table(names_CSxCC, "names_CSxCC_genoID.txt", sep = "\t", row.names = F, quote = F)
#write.table(names_BNDxCSxCC, "names_BNDxCSxCC_genoID.txt", sep = "\t", row.names = F, quote = F)

# add the path prefix and suffix to all individual IDs
names_BNDxCC$BNDxCC.ind = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_BNDxCC$BNDxCC.ind, '.sorted.bam')
names_BNDxCS$BNDxCS.ind = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_BNDxCS$BNDxCS.ind, '.sorted.bam')
names_CSxCC$CSxCC.ind = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_CSxCC$CSxCC.ind, '.sorted.bam')
names_BNDxCSxCC$BNDxCSxCC.ind = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', names_BNDxCSxCC$BNDxCSxCC.ind, '.sorted.bam')

# remove headers
names(names_BNDxCC) <- NULL
names(names_BNDxCS) <- NULL
names(names_CSxCC) <- NULL
names(names_BNDxCSxCC) <- NULL

# save as text file
#write.table(names_BNDxCC, "names_BNDxCC_genoID_paths.txt", sep = "\t", row.names = F, quote = F)
#write.table(names_BNDxCS, "names_BNDxCS_genoID_paths.txt", sep = "\t", row.names = F, quote = F)
#write.table(names_CSxCC, "names_CSxCC_genoID_paths.txt", sep = "\t", row.names = F, quote = F)
#write.table(names_BNDxCSxCC, "names_BNDxCSxCC_genoID_paths.txt", sep = "\t", row.names = F, quote = F)


# rename first two columns
names(x_metadata)[c(1:2,18,26,32)] <- c("Mandeville_ID", "Common_Name", "Date", "Barcode", "Sex")

x_metadata <- x_metadata[-22]

# export x_metadata
write.csv(x_metadata, paste0("old_AMP22_genoID_k",k,"_Jun2023.csv"), row.names = F, quote = F)


#############################################################################################
      # Pulling in Q data
#############################################################################################

k2q_per_species <- read.delim("AMP22_CSxCC_genoID_entropy_list_k2_Geno_ID_q.txt", header = T, sep = "\t")
k2qQ_values <- read.delim("AMP22_CSxCC_genoID_entropy_list_k2_indivs_q_Q.txt", header = T, sep = "\t")

# filter out plate 13
# load metadata
metadata <- read.csv("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/Leuciscid_Metadata_May2023.csv")
# get list of and drop 84 inds from plate 13 inds
plate13inds <- filter(metadata, Plate == "AMP22_LP13")
plate13inds <- plate13inds[,4]
plate13inds <- as.data.frame(plate13inds)
colnames(plate13inds) <- "Mandeville_ID"
xQ <- anti_join(k2qQ_values, plate13inds)
nrowxQ <- nrow(xQ)


# create new columns
# Geno_ID (will contain Common_Name or "hybrid")
# Pheno_correct (binary, 0=correct, 1=incorrect)
# Hybrid_Status (binary, 0=hybrid, 1=parental)
xQ$Hybrid_Type = NA
xQ$Parental_Status = NA
xQ$Hybrid_Status = NA
xQ$F1_Status = NA
xQ$F2_Status = NA
xQ$F3_Status = NA
xQ$BC1_Status = NA
xQ$BC2_Status = NA
xQ$Other_Status = NA


# classifies as type of hybrid (deciding on these values )
for (i in 1:nrow(xQ)){ 
  if (xQ[i,3] >= 0 & xQ[i,3] <= 0.125 & xQ[i,4] >= 0 & xQ[i,4] <= 0.125) {
    xQ$Hybrid_Type[i] <- "P1" #Parental left side
    xQ$Parental_Status[i] = 1
    xQ$Hybrid_Status[i] = 0
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 0
    xQ$BC2_Status[i] = 0
    xQ$Other_Status[i] = 0
  } else if (xQ[i,3] >= 0.125 & xQ[i,3] <= 0.25 & xQ[i,4] >= 0.125 & xQ[i,4] <= 0.375) {
    xQ$Hybrid_Type[i] <- "BC2P1" #Back cross 2 to left parental
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 1
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 0
    xQ$BC2_Status[i] = 1
    xQ$Other_Status[i] = 0
  } else if (xQ[i,3] >= 0.25 & xQ[i,3] <= 0.375 & xQ[i,4] >= 0.375 & xQ[i,4] <= 0.625) {
    xQ$Hybrid_Type[i] <- "BC1P1" #Back cross 1 to left parental
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 1
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 1
    xQ$BC2_Status[i] = 0
    xQ$Other_Status[i] = 0
  } else if (xQ[i,3] >= 0.375 & xQ[i,3] <= 0.625 & xQ[i,4] >= 0.75 & xQ[i,4] <= 1) {
    xQ$Hybrid_Type[i] <- "F1" #First gen hybrid
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 1
    xQ$F1_Status[i] = 1
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 0
    xQ$BC2_Status[i] = 0
    xQ$Other_Status[i] = 0
  } else if (xQ[i,3] >= 0.375 & xQ[i,3] <= 0.625 & xQ[i,4] >= 0.375 & xQ[i,4] <= 0.75) {
    xQ$Hybrid_Type[i] <- "F2" #Second gen hybrid
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 1
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 1
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 0
    xQ$BC2_Status[i] = 0
    xQ$Other_Status[i] = 0
  } else if (xQ[i,3] >= 0.375 & xQ[i,3] <= 0.625 & xQ[i,4] >= 0.125 & xQ[i,4] <= 0.375) {
    xQ$Hybrid_Type[i] <- "F3" #Third gen hybrid
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 1
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 1
    xQ$BC1_Status[i] = 0
    xQ$BC2_Status[i] = 0
    xQ$Other_Status[i] = 0
  } else if (xQ[i,3] >= 0.625 & xQ[i,3] <= 0.75 & xQ[i,4] >= 0.375 & xQ[i,4] <= 0.625) {
    xQ$Hybrid_Type[i] <- "BC1P2" #Back cross 1 to right parental
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 1
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 1
    xQ$BC2_Status[i] = 0
    xQ$Other_Status[i] = 0
  } else if (xQ[i,3] >= 0.75 & xQ[i,3] <= 0.875 & xQ[i,4] >= 0.125 & xQ[i,4] <= 0.375) {
    xQ$Hybrid_Type[i] <- "BC2P2" #Back cross 2 to right parental
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 1
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 0
    xQ$BC2_Status[i] = 1
    xQ$Other_Status[i] = 0
  } else if (xQ[i,3] >= 0.875 & xQ[i,3] <= 1 & xQ[i,4] >= 0 & xQ[i,4] <= 0.125) {
    xQ$Hybrid_Type[i] <- "P2" #Parental right side
    xQ$Parental_Status[i] = 1
    xQ$Hybrid_Status[i] = 0
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 0
    xQ$BC2_Status[i] = 0
    xQ$Other_Status[i] = 0
  } else {
    xQ$Hybrid_Type[i] <- "Other" #Other
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 0
    xQ$F1_Status[i] = 0
    xQ$F2_Status[i] = 0
    xQ$F3_Status[i] = 0
    xQ$BC1_Status[i] = 0
    xQ$BC2_Status[i] = 0
    xQ$Other_Status[i] = 0
  } 
}

# save data frame
#write.csv(xQ, "AMP22_genoID_k2_BNDxCS_old.csv", row.names = F, quote = F)


#summarize the hybrid types found
(table_types <- table(xQ$Hybrid_Type))
dftable_types <- as.data.frame(table_types)
colnames(dftable_types)[1] <- "Hybrid_Type"

#save to compare to other species crosses
#write.csv(dftable_types, paste0("AMP22_hybrid_types_BNDxCS_old.csv"), row.names = F, quote = F)

#edit metadata
metadata$Waterbody[metadata$Waterbody_Code == "OP2"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "OP7"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "Weir DS"] <- "Costello_Creek"
metadata$Waterbody_Code[metadata$Waterbody == "Costello_Creek"] <- "COS"
# split types by site
meta_names <- metadata$Mandeville_ID
meta_sites <- metadata$Waterbody_Code
meta_namesandsites <- as.data.frame(cbind(meta_names, meta_sites))
colnames(meta_namesandsites) <- c("Mandeville_ID", "Waterbody_Code")
xQ_sites <- merge(xQ, meta_namesandsites)
xQ_count <- xQ_sites %>% group_by(Waterbody_Code) %>% count(Hybrid_Type)



# create frequency column
sumsQ <- xQ_count %>% 
  group_by(Waterbody_Code) %>%
  summarise(n_sum = sum(n),
            .groups = 'drop')
xQ_count <- merge(xQ_count, sumsQ, by='Waterbody_Code')
getproportion <- function(row) {
  return (row$n/row$n_sum)
}
xQ_count <- xQ_count %>% mutate(proportion = n / n_sum)

# order by the frequency column
xQ_count = xQ_count %>% 
  group_by(Waterbody_Code) %>% 
  arrange(desc(Hybrid_Type))# what it sorts by OFTEN NEED TO CHANGE THIS TO EITHER PROPORTION OR HYBRID_TYPE

# create the object w the levels in order
xQ_order = xQ_count[!duplicated(xQ_count$Waterbody_Code), ]$Waterbody_Code
# tell it how we want the levels
xQ_count$Waterbody_Code <- factor(xQ_count$Waterbody_Code, levels = xQ_order)

# plot 
ggplot(xQ_count, aes(fill=Hybrid_Type, y=n, x=Waterbody_Code)) + 
  geom_bar(position="fill", stat="identity")+ 
  ylab("Proportion of total individuals") +
  xlab("Waterbody") + 
  scale_fill_brewer(palette="Spectral", name="Hybrid Type")

#------------------------------------------------------------
# simplify hybrid type and replot
#------------------------------------------------------------
xQ_sites$Hybrid_Type_Simple = NA

for (i in 1:nrow(xQ_sites)){
  if(str_detect(xQ_sites$Hybrid_Type[i],'^P1|^P2') == TRUE){
    xQ_sites$Hybrid_Type_Simple[i] <- "Parental" 
  }
  else if (str_detect(xQ_sites$Hybrid_Type[i],'BC1') == TRUE){
    xQ_sites$Hybrid_Type_Simple[i] <- "BC1"
  }
  else if (str_detect(xQ_sites$Hybrid_Type[i],'BC2') == TRUE){
    xQ_sites$Hybrid_Type_Simple[i] <- "BC2"
  }
  else {
    xQ_sites$Hybrid_Type_Simple[i] <- xQ_sites$Hybrid_Type[i]
  }
}

xQ_count_simple <- xQ_sites %>% group_by(Waterbody_Code) %>% count(Hybrid_Type_Simple)
#save to compare to other species crosses
#write.csv(xQ_count_simple, paste0("AMP22_hybrid_types_persite_BNDxCS.csv"), row.names = F, quote = F)

# create frequency column
sumsQ <- xQ_count_simple %>% 
  group_by(Waterbody_Code) %>%
  summarise(n_sum = sum(n),
            .groups = 'drop')
xQ_count_simple <- merge(xQ_count_simple, sumsQ, by='Waterbody_Code')
getproportion <- function(row) {
  return (row$n/row$n_sum)
}
xQ_count_simple <- xQ_count_simple %>% mutate(proportion = n / n_sum)

# order by the frequency column
xQ_count_simple = xQ_count_simple %>% 
  group_by(Waterbody_Code) %>% 
  arrange(desc(proportion))# what it sorts by

# create the object w the levels in order
xQ_order = xQ_count_simple[!duplicated(xQ_count_simple$Waterbody_Code), ]$Waterbody_Code
# tell it how we want the levels
xQ_count_simple$Waterbody_Code <- factor(xQ_count_simple$Waterbody_Code, levels = xQ_order)

# plot
ggplot(xQ_count_simple, aes(fill=Hybrid_Type_Simple, y=n, x=Waterbody_Code)) + 
  geom_bar(position="fill", stat="identity")+ 
  ylab("Proportion of total individuals") +
  scale_fill_brewer(palette="Spectral")


#------------------------------------------------------------
# remove parentals and replot
#------------------------------------------------------------
xQ_count_noparental <- filter(xQ_count_simple, Hybrid_Type_Simple != "Parental")



# order by the frequency column
xQ_count_noparental = xQ_count_noparental %>% 
  group_by(Waterbody_Code) %>% 
  arrange(desc(Hybrid_Type_Simple))# what it sorts by

# create the object w the levels in order
xQ_order = xQ_count_noparental[!duplicated(xQ_count_noparental$Waterbody_Code), ]$Waterbody_Code
# tell it how we want the levels
xQ_count_noparental$Waterbody_Code <- factor(xQ_count_noparental$Waterbody_Code, levels = xQ_order)

# plot
ggplot(xQ_count_noparental, aes(fill=Hybrid_Type_Simple, y=n, x=Waterbody_Code)) + 
  geom_bar(position="fill", stat="identity") + 
  ylab("Proportion of hybrid individuals") +
  scale_fill_brewer(palette="Spectral")



#------------------------------------------------------------
# merge the 3 csv files of hybrid types for each pairing
#------------------------------------------------------------

BNDxCC <- read.csv("AMP22_genoID_k2_BNDxCC_old.csv")
BNDxCS <- read.csv("AMP22_genoID_k2_BNDxCS_old.csv")
CSxCC <- read.csv("AMP22_genoID_k2_CSxCC_old.csv")

CSxCC$q_col1 <- round(CSxCC$q_col1, digits = 3)
CSxCC$Q_col2 <- round(CSxCC$Q_col2, digits = 3) 

dflist <- list(BNDxCC, BNDxCS, CSxCC)
  
lapply(dflist, function(x) {
  x$q_col1 <- NULL
  x$Q_col2 <- NULL 
})

# run this on all 3 data frames
x <- CSxCC
  for (i in 1:nrow(x)){
    if(str_detect(x$Hybrid_Type[i],'^P1|^P2') == TRUE){
      x$Hybrid_Type[i] <- "Parental" 
    }
    else if (str_detect(x$Hybrid_Type[i],'BC1') == TRUE){
      x$Hybrid_Type[i] <- "BC1"
    }
    else if (str_detect(x$Hybrid_Type[i],'BC2') == TRUE){
      x$Hybrid_Type[i] <- "BC2"
    }
    else {
      x$Hybrid_Type[i] <- x$Hybrid_Type[i]
    }
  }
CSxCC <- x


temp <- merge(BNDxCC, BNDxCS, by.x = c("Mandeville_ID", "Geno_ID", "Hybrid_Type", "Parental_Status", "Hybrid_Status", "F1_Status", "F2_Status", "F3_Status", "BC1_Status", "BC2_Status", "Other_Status"), by.y = c("Mandeville_ID", "Geno_ID", "Hybrid_Type", "Parental_Status", "Hybrid_Status", "F1_Status", "F2_Status", "F3_Status", "BC1_Status", "BC2_Status", "Other_Status"), all.x = T, all.y = T)
BNDxCSxCC <- merge(temp, CSxCC, by.x = c("Mandeville_ID", "Geno_ID", "Hybrid_Type", "Parental_Status", "Hybrid_Status", "F1_Status", "F2_Status", "F3_Status", "BC1_Status", "BC2_Status", "Other_Status"), by.y = c("Mandeville_ID", "Geno_ID", "Hybrid_Type", "Parental_Status", "Hybrid_Status", "F1_Status", "F2_Status", "F3_Status", "BC1_Status", "BC2_Status", "Other_Status"), all.x = T, all.y = T)


nrow(BNDxCSxCC[duplicated(BNDxCSxCC$Mandeville_ID), ]) #10, so there's 10 inds w different outcomes for hybrid_status
dups <- BNDxCSxCC[duplicated(BNDxCSxCC$Mandeville_ID)|duplicated(BNDxCSxCC$Mandeville_ID, fromLast=TRUE),]
# i think i should compare q and Q values in both entropy files and see where the issues are, try grouping all the inds into a category of some sort


