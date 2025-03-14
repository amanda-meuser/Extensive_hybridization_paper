# creating a loop for classifying hybrid status

library(stringr)
library(tidyverse)
library(RColorBrewer)
library(patchwork)

#=======================================================
# SIMULATED DATA
#=======================================================

# #import simulated data for testing
# sim <- read.csv("entropy_simulated.csv")
# 
# # assign species to entropy populations
# names(sim)[4:8] <- c("Big_Chub", "Fat_Dace", "Pearl_Chub", "Rainbow_Dace", "Scarlet_Minnow")
# 
# # create new columns
# # Geno_ID (will contain Common_Name or "hybrid")
# # Pheno_correct (binary, 0=correct, 1=incorrect)
# # Hybrid_Status (binary, 0=hybrid, 1=parental)
# sim$Geno_ID = NA
# sim$Pheno_Correct = NA
# sim$Hybrid_Status = NA
# 
# # assigns hybrids as 'hybrid'
# for (i in 1:nrow(sim)){ #assuming the col indexes, might be better to use names
#   for (j in 4:8){
#     if (sim[i,j] > 0.9) {
#       sim$Geno_ID[i] <- colnames(sim)[j]
#       break
#     }
#     else {
#       sim$Geno_ID[i] <- "hybrid"
#     }
#   }
# }
# 
# 
# #creating a jump table for the hybrid abbreviations
# 
# 
# # inserts name of what is in the hybrids
# for (i in 1:nrow(sim)){ #assuming the col indexes, might be better to use names
#   for (j in 4:8){
#     if (sim[i,j] > 0.9) {
#       sim$Geno_ID[i] <- colnames(sim)[j]
#       break
#     }
#     else { # pulls the columns names for 4:8, sorts them by cell value, then takes the largest 1:2 (largest 2)
#       holder <- colnames(sim[i, 4:8])[order(sim[i, 4:8], decreasing=TRUE)[1:2]]
#       sim$Geno_ID[i] <- paste(holder[1],"x",holder[2])
#     }
#   }
# }
# 
# # assign 0 or 1 for whether whether the ind is a hybrid
# for (i in 1:nrow(sim)){
#   if(str_detect(sim[i,10],'x') == TRUE){
#     sim$Hybrid_Status[i] <- 1 #hybrid
#   }
#   else {
#     sim$Hybrid_Status[i] <- 0 #parental
#   }
# }
# 
# 
# # assign 0 or 1 for whether phenotypic ID was correct
# for (i in 1:nrow(sim)){
#   if(identical(sim[i,2],sim[i,10]) == FALSE){
#     sim$Pheno_Correct[i] <- 1 #mis-ID
#   }
#   else {
#     sim$Pheno_Correct[i] <- 0 #correct ID
#   }
# }


#=======================================================
# REAL DATA
#=======================================================
#setwd("/Users/ameuser/Library/CloudStorage/GoogleDrive-ameuser03@gmail.com/My Drive/PhD Files/Masters_stuff/Thesis_pub")

x <- read.delim("./AMP22_thesis_24feb25/AMP22_thesis_24feb25_entropy_list_k12_indivs_q.txt", sep = "\t", header = T)
k = 12

# load metadata
metadata <- read.csv("Leuciscid_Metadata_May2023.csv", sep = ";")

# get list of and drop 84 inds from plate 13 inds
# plate13inds <- filter(metadata, Plate == "AMP22_LP13")
# plate13inds <- plate13inds[,4]
# plate13inds <- as.data.frame(plate13inds)
# names(plate13inds) <- "Mandeville_ID"
# x <- anti_join(x, plate13inds) #removed 69 inds (not all plate 13 inds made it thru filtering)
# nrowx <- nrow(x)

#save plate 13 inds list
#write.table(plate13inds, "Plate13_inds.txt", quote = F, row.names = F)
# add paths and save again
# plate13inds$Mandeville_ID = paste0('/project/rrg-emandevi/hybrid_ameuser/AMP22/bwa/', plate13inds$Mandeville_ID, '.sorted.bam')
#write.table(plate13inds, "Plate13_inds_paths.txt", quote = F, row.names = F)

#double check plate 13 inds are gone
#RLplates <- metadata[c(4,8)]
#hmm <- merge(x,RLplates, by = "Mandeville_ID")
#table(hmm$Plate)

# assign species to entropy populations
#k=9
#names(x)[3:11] <- c("Creek_Chub", "Longnose_Dace", "Striped_Shiner", "Rosyface_Shiner", "Central_Stoneroller", "Common_Shiner", "River_Chub", "Western_Blacknose_Dace", "Hornyhead_Chub")
#k=12
names(x)[3:14] <-c("Common_Shiner", "Central_Stoneroller", "Hornyhead_Chub", "Longnose_Dace", "Striped_Shiner", "River_Chub", "Pimephales_sp", "Rosyface_Shiner", "Creek_Chub", "Western_Blacknose_Dace", "V11", "V12")

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
    holder <- colnames(x[i, 3:14])[order(colSums(x[i, 3:14]), decreasing=TRUE)[1:2]]
    x$Geno_ID[i] <- paste(holder, collapse=' x ')
  }
  else if (colmatches > 2 & colmatches < 20) { # def 20 columns is max
    #multi-species match found
    holder2 <- colnames(x[i, 3:14])[order(colSums(x[i, 3:14]), decreasing=TRUE)[1:colmatches]]
    x$Geno_ID[i] <- paste(holder2, collapse=' x ')
  }
  else if (colmatches <= 1){
    # there's only one or no species that contribute 0.1-0.9 ancestry
    holder3 <- colnames(x[i, 3:14])[order(colSums(x[i, 3:14]), decreasing=TRUE)[1]]
    x$Geno_ID[i] <- paste(holder3, "x fishiness")
  }
}


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
nrowx <- nrow(x)
dfallstats[4,1] <- (nrowx - (dfallstats[1,1] + dfallstats[2,1] + dfallstats[3,1]))
dfallstats[,2] <- "x"
dfallstats[,3] <- c("1", "2", "3", "4")

palette <- brewer.pal(5, "Spectral")

# plot
(stacked_plot_single <- ggplot(dfallstats, aes(fill=V3, y=State, x=V2)) + 
    geom_bar(position="stack", stat="identity") +
    ylab("Number of individuals")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    geom_text(aes(label = State), size = 3, hjust = 0.5, vjust = 3, position = "stack") +
    scale_fill_manual(values = c(palette[2], palette[3], palette[4], palette[5]), 
                      name = "Legend", 
                      labels = c("2-species hybrid", "Multi-species hybrid", "Correct Pheno ID (Parental)", "Incorrect Pheno ID (Parental)")) )


# pdf(paste0("genomic_IDs_stats_1bar_k",k,".pdf"))
# stacked_plot_single
# dev.off()

#---------------------------------------------------------------------------
# Creating the plot above but with more detail
#---------------------------------------------------------------------------

# identify hybrids where the pheno ID isn't either of the geno ID hybrid inds
# 0 = the phenotype parental is at least one species in the hybrid cross
# 1 = the phenotype is none of the species in the cross
x$Hybrid_Match = NA
for (i in 1:nrow(x)){
  if (str_detect(x[i,15], x[i,2]) == TRUE){
    x$Hybrid_Match[i] <- 0
  }
  else if (str_detect(x[i,15], x[i,2]) == FALSE){
    x$Hybrid_Match[i] <- 1
  }
}

# Create counts for each category in this crazy stacked plot
stackedA <- data.frame(matrix(nrow = 1, ncol = 1))
stackedA$Parental <- sum(x$Hybrid_Status == 0)
stackedA$ProbParent <- sum(str_detect(x[,15], "x fishiness"))
stackedA$ThreespH <- sum(x$Multi_Status) - stackedA$ProbParent
stackedA$TwospH <- sum(x$Hybrid_Status) - (stackedA$ProbParent + stackedA$ThreespH)
stackedA$matrix.nrow...1..ncol...1. = NULL

# flip and make plot A
stackedAt <- as.data.frame(t(stackedA))
stackedAt$V2 <- "x"
stackedAt$V3 <- c("1", "2", "3", "4")

colours <- brewer.pal(10, "Paired")
#"#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A"

(stackedA_plot <- ggplot(stackedAt, aes(fill=V3, y=V1, x=V2)) + 
    geom_bar(position="stack", stat="identity") +
    ylab("Number of individuals")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    geom_text(aes(label = V1), size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c(colours[2], colours[4], colours[8], colours[6]), 
                      labels = c("Parentals", "Uncertain Hybrids", "3-species hybrid", "2-species hybrid")) +
    guides(fill=guide_legend(title="Grouped Genomic IDs")))

# Create counts for each category in the second crazy stacked plot
stackedB <- data.frame(matrix(nrow = 1, ncol = 1))
stackedB$MissP <- sum(x$Pheno_Correct) - sum(x$Hybrid_Status)
stackedB$MatchP <- stackedA$Parental - stackedB$MissP
fishy <- x %>% filter(str_detect(x[,15], "x fishiness"))
stackedB$MatchPP <- sum(str_detect(fishy[,15], fishy[,2]))
stackedB$MissPP <- nrow(fishy) - stackedB$MatchPP
ThreespH <- x %>% filter(x$Multi_Status == 1)
ThreespH <-  ThreespH %>% filter(!str_detect(ThreespH[,15], "x fishiness"))
stackedB$MatchThreespH <- sum(ThreespH$Hybrid_Match == 0) #all match, awesome!
stackedB$MissTwospH <- stackedA$TwospH - (sum(x$Hybrid_Status == 1 & x$Hybrid_Match == 0 & x$Multi_Status == 0))
stackedB$MatchTwospH <- sum(x$Hybrid_Status == 1 & x$Hybrid_Match == 0 & x$Multi_Status == 0) 
stackedB$matrix.nrow...1..ncol...1. = NULL

# flip and create plot B!!
stackedBt <- as.data.frame(t(stackedB))
stackedBt$V2 <- "x"
stackedBt$V3 <- c("1", "2", "3", "4", "5", "6", "7")

nb.cols <- 12
mycolours <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
#"#A6CEE3" "#5097C5" "#4794A8" "#A4D58D" "#6CBC56" "#579E3F" "#D69B85" "#F05F60" "#E52923" "#F59258" "#FDA746" "#FF7F00"

(stackedB_plot <- ggplot(stackedBt, aes(fill=V3, y=V1, x=V2)) + 
    geom_bar(position="stack", stat="identity") +
    ylab("Number of individuals")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_text(aes(label = V1), size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c(mycolours[1], mycolours[3], colours[3], mycolours[5], mycolours[10], colours[5], mycolours[8]), 
                      labels = c("Mismatched Parentals", "Matching Parentals", "Mismatched Uncertain Hybrids", "Matching Uncertain Hybrids", "Matching 3-species Hybrids", "Mismatched 2-species Hybrids", "Matching 2-species Hybrids")) +
    guides(fill=guide_legend(title="Mismatch of Phenotypic \nand Genomic IDs")))

# pdf("genomic_IDs_stats_2bars_k12.pdf")
# stackedA_plot + stackedB_plot +
#   plot_annotation(tag_levels = 'A') +
#   plot_layout(guides = 'collect')
# dev.off()

#---------------------------------------------------------------------------
# Plot number of each parental (phenotypic and genomic) and each hybrid
#---------------------------------------------------------------------------

x_copy <- x
colnames(x_copy)[1:2] <- c("Mandeville_ID", "Common_Name")

x_copy$Geno_ID_simple = NA

for (i in 1:nrow(x_copy)){ 
  if (x_copy$Multi_Status[i] == 1){
    x_copy$Geno_ID_simple[i] <- "Multi"
  } 
  else {
    x_copy$Geno_ID_simple[i] <- x_copy$Geno_ID[i]
  }
}


x_hybrids <- filter(x_copy, x_copy$Hybrid_Status == 1)
x_parentals <- filter(x_copy, x_copy$Hybrid_Status == 0)
x_parentals <- x_parentals %>% count(Geno_ID)

# prelim plots
barplot(x_hybrids$n)
barplot(x_parentals$n)

# species by genomic ID
(parent_plot_geno <- ggplot(data=x_parentals, aes(x=(reorder(Geno_ID, -n)), y=n, fill = (reorder(Geno_ID, -n)))) +
  geom_bar(stat="identity", width=0.85, linewidth = 2)+
  scale_fill_manual(values = c("#A6CEE3","#FDBF6F","#DF65B0","#B2DF8A","#35978F","#FB9A99","#CAB2D6","#8C510A","#969696"), name = "Species by Phenotypic \nIdentity", labels=c("Creek Chub","Common Shiner","Western Blacknose Dace", "River Chub", "Hornyhead Chub", "Rosyface Shiner", "Longnose Dace", "Central Stoneroller", "Pimephales sp."))+
  geom_text(aes(label=n), vjust=-0.3, color="black", linewidth=4)+
  theme(axis.text.x = element_blank(), legend.position.inside = c(0.85, 0.75)) +
  xlab(" ") + 
  ylab("Number of individuals")) 

# species by phenotypic ID
x_pheno <- x_copy %>% count(Common_Name)

(parent_plot_pheno <- ggplot(data=x_pheno, aes(x=(reorder(Common_Name, -n)), y=n, fill = (reorder(Common_Name, -n)))) +
  geom_bar(stat="identity", width=0.85, size = 2)+
  scale_fill_manual(values = c("#A6CEE3","#FDBF6F","#DF65B0","#B2DF8A","#35978F","#FB9A99","#CAB2D6","#8C510A","#E5DF60"), name = "Species", labels=c("Creek Chub","Common Shiner","Western Blacknose Dace", "River Chub", "Hornyhead Chub", "Rosyface Shiner", "Longnose Dace", "Central Stoneroller", "Striped Shiner"))+
  geom_text(aes(label=n), vjust=-0.3, color="black", size=4)+
  theme(axis.text.x = element_blank(), legend.position = "none") +
  xlab(" ") + 
  ylab("Number of individuals")) 

(parent_plot_pheno2 <- ggplot(data=x_pheno, aes(x=(reorder(Common_Name, -n)), y=n, fill = (reorder(Common_Name, -n)))) +
    geom_bar(stat="identity", width=0.85, size = 2)+
    scale_fill_manual(values = c("#A6CEE3","#FDBF6F","#DF65B0","#B2DF8A","#35978F","#FB9A99","#CAB2D6","#8C510A","#E5DF60"), name = "Species", labels=c("Creek Chub","Common Shiner","Western Blacknose Dace", "River Chub", "Hornyhead Chub", "Rosyface Shiner", "Longnose Dace", "Central Stoneroller", "Striped Shiner"))+
    geom_text(aes(label=n), vjust=-0.3, color="black", size=4)+
    theme(axis.text.x = element_blank(), legend.position = "none")+
    labs(x = "",  y = "Number of individuals", title = "Thesis Data Set", subtitle = "731 inds, 12228 SNPs"))

# split pheno ID by year
x_copy$Year = NA

for (i in 1:nrow(x_copy)){
  if (str_detect(x_copy[i,1], "EGM19|AMP19") == TRUE){
    x_copy$Year[i] <- "2019"
  }
  else if (str_detect(x_copy[i,1], "AMP22") == TRUE){
    x_copy$Year[i] <- "2022"
  }
}

x_pheno_year <- x_copy %>% group_by(Year) %>% count(Common_Name)

(parent_plot_year <- ggplot(x_pheno_year, aes(fill=(reorder(Common_Name, -n)), y=n, x=Year)) + 
  geom_bar(position=position_dodge2(width = 0.9, preserve ="single"), stat="identity")+
  scale_fill_manual(values = c("#A6CEE3","#FDBF6F","#DF65B0","#B2DF8A","#35978F","#CAB2D6","#FB9A99","#8C510A","#E5DF60"), name = "Species", labels=c("Creek Chub","Common Shiner","Western Blacknose Dace", "River Chub", "Hornyhead Chub", "Rosyface Shiner", "Longnose Dace", "Central Stoneroller", "Striped Shiner"))+
  geom_text(position = position_dodge2(width = 0.9, preserve ="single"), aes(label=n), vjust=-0.3, color="black", size=4)+
  theme(legend.position = c(0.87, 0.75)) +
  ylab("Number of individuals"))


# pdf(file = "AMP22_number_of_inds_wo_13.pdf", width = 10)
# parent_plot_pheno
# parent_plot_geno
# parent_plot_year
# dev.off()

# pdf("AMP22_number_of_inds_pheno_only.pdf", width = 14, height = 16)
# parent_plot_pheno + parent_plot_year +
#   plot_annotation(tag_levels = 'A') +
#   plot_layout(ncol = 1, guides = 'collect')
# dev.off()

#---------------------------------------------------------------------------
# compare # of inds by pheno ID across data sets
#---------------------------------------------------------------------------

# inds_august <- read.delim("../comparing_datasets/indivs_august.txt", header = F)
# inds_newest <- read.delim("../comparing_datasets/indivs_newest.txt", header = F)
# 
# # merge w whole data set to get metadata
# meta_august <- merge(metadata, inds_august, by.x = "Mandeville_ID", by.y = "V1")
# meta_newest <- merge(metadata, inds_newest, by.x = "Mandeville_ID", by.y = "V1")
# 
# # count inds per species
# count_august <- meta_august %>% count(Common_Name)
# count_newest <- meta_newest %>% count(Common_Name)
# 
# (parent_plot_pheno_august <- ggplot(data=count_august, aes(x=(reorder(Common_Name, -n)), y=n, fill = (reorder(Common_Name, -n)))) +
#     geom_bar(stat="identity", width=0.85, size = 2)+
#     scale_fill_manual(values = c("#A6CEE3","#FDBF6F","#DF65B0","#B2DF8A","#35978F","#FB9A99","#CAB2D6","#8C510A","#E5DF60", "grey66", "grey34"), name = "Species", labels=c("Creek Chub","Common Shiner","Western Blacknose Dace", "River Chub", "Hornyhead Chub", "Longnose Dace", "Rosyface Shiner","Central Stoneroller", "Striped Shiner", "Fathead Minnow", "Bluntnose Minnow"))+
#     geom_text(aes(label=n), vjust=-0.3, color="black", size=4)+
#     theme(axis.text.x = element_blank()) +#, legend.position = "none"
#     labs(x = "",  y = "Number of individuals", title = "August Data Set", subtitle = "802 inds, 8349 SNPs"))
# 
# (parent_plot_pheno_newest <- ggplot(data=count_newest, aes(x=(reorder(Common_Name, -n)), y=n, fill = (reorder(Common_Name, -n)))) +
#     geom_bar(stat="identity", width=0.85, size = 2)+
#     scale_fill_manual(values = c("#A6CEE3","#FDBF6F","#DF65B0","#B2DF8A","#35978F","#FB9A99","#CAB2D6","#8C510A","#E5DF60", "grey66", "grey34"), name = "Species", labels=c("Creek Chub","Common Shiner","Western Blacknose Dace", "River Chub", "Hornyhead Chub", "Longnose Dace", "Rosyface Shiner","Central Stoneroller", "Striped Shiner", "Fathead Minnow", "Bluntnose Minnow"))+
#     geom_text(aes(label=n), vjust=-0.3, color="black", size=4)+
#     theme(axis.text.x = element_blank()) +#, legend.position = "none"
#     labs(x = "",  y = "Number of individuals", title = "Newest Data Set", subtitle = "799 inds, 20631 SNPs"))


# pdf("AMP22_comparing datasets.pdf", width = 10, height = 16)
# parent_plot_pheno2 + parent_plot_pheno_august + parent_plot_pheno_newest +
#   plot_annotation(tag_levels = 'A') +
#   plot_layout(ncol = 1, guides = 'collect')
# dev.off()


#---------------------------------------------------------------------------

# need to group the hybrids together so that there's fewer groups
# split up the names
x_hybrids <- x_hybrids %>%
  separate(Geno_ID_simple, c('Species_1', 'Species_2'), ' x ', remove=F)
# alphabetize the names
x_hybrids$Geno_ID_order = NA
for (i in 1:nrow(x_hybrids)){
  a <- x_hybrids$Species_1[i]
  b <- x_hybrids$Species_2[i]
  x_hybrids$Geno_ID_order[i] = ifelse(a < b, paste(a, b, sep =' x '), paste(b, a, sep =' x ')) # always writes the names the same way
}

# swap NAs back for 'Multi' and remove
x_hybrids <- x_hybrids %>% mutate(Geno_ID_order = replace_na(Geno_ID_order, "Multi"))
x_hybrids <- x_hybrids %>% filter(Geno_ID_order != "Multi")

# drop extra columns and re-sort
x_hybrids <- x_hybrids[-(1:3)]
x_hybrids <- x_hybrids %>% count(Geno_ID_order)

nb.cols <- 30
mycolours <- colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols)

# species by genomic ID PUT LEGEND ON THE PLOT
(hybrid_plot <- ggplot(data=x_hybrids, aes(x=(reorder(Geno_ID_order, -n)), y=n, fill = (reorder(Geno_ID_order, -n)))) +
  geom_bar(stat="identity", width=0.85, size = 2)+
  scale_fill_manual(values = mycolours, name = "Two-species hybrid crosses")+
  geom_text(aes(label=n), vjust=-0.3, color="black", size=4)+
  theme(axis.text.x = element_blank(), legend.text=element_text(size=7), legend.position = c(0.7, 0.65)) +
  xlab(" ") + 
  ylab("Number of individuals"))


# also create heatmap!
# split, reorganize, count, then split again
x_hybrids <- filter(x_copy, x_copy$Hybrid_Status == 1)
x_hybrids_heatmap <- x_hybrids %>% filter(Geno_ID_simple != "Multi")

x_hybrids_heatmap <- x_hybrids_heatmap %>%
  separate(Geno_ID_simple, c('Species_1', 'Species_2'), ' x ', remove=F)

x_hybrids_heatmap$Geno_ID_order = NA
for (i in 1:nrow(x_hybrids_heatmap)){
  a <- x_hybrids_heatmap$Species_1[i]
  b <- x_hybrids_heatmap$Species_2[i]
  x_hybrids_heatmap$Geno_ID_order[i] = ifelse(a < b, paste(a, b, sep =' x '), paste(b, a, sep =' x '))
}

# remove year column (don't need), count occurrences of each hybrid cross, then drop full geno ID
x_hybrids_heatmap <- x_hybrids_heatmap[-23]
x_hybrids_heatmap <- x_hybrids_heatmap %>%
  count(Geno_ID_order) %>%
  separate(Geno_ID_order, c('Species_1', 'Species_2'), ' x ', remove=F)
x_hybrids_heatmap <- x_hybrids_heatmap[-1]

palette <- brewer.pal(8, "Set2")

# created this stupid file in excel cuz i'm frustrated with ggplot
all_options <- read.csv("heatmap_pairwise_comparisons.csv")
heatmap_plus_blanks <- merge(x_hybrids_heatmap, all_options, by.x = c("Species_1", "Species_2"), by.y = c("Species_1", "Species_2"), all.y=T)


(heatmap <- ggplot(data = heatmap_plus_blanks, aes(Species_2, Species_1, fill = n))+
  geom_tile(color = "grey40")+
  theme_bw()+ 
  scale_fill_gradient2(low = palette[8], high = palette[2], mid = palette[7], 
                       midpoint = 22, limit = c(0.5,43), space = "Lab", 
                       name="Number of\nhybrids found", na.value = 'white') +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.85, 
                                   size = 9, hjust = 0.55), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
    coord_fixed() )
  #+ geom_point(data = heatmap_plus_blanks, aes(size=0, shape = NA), fill = "white", colour = "grey40") +
  # guides(size=guide_legend("", override.aes=list(shape=22, size = 8)))) # these two lines make the white box for 0 appear but it wasn't working right last time I tried it

# pdf("AMP22_species_crosses_heatmap_nowhitebox.pdf", width = 10, height = 8)
# heatmap
# dev.off()


#---------------------------------------------------------------------------
# break this down by site !!
#---------------------------------------------------------------------------

#edit metadata
metadata$Waterbody[metadata$Waterbody_Code == "OP2"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "OP7"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "Weir DS"] <- "Costello_Creek"
metadata$Waterbody_Code[metadata$Waterbody == "Costello_Creek"] <- "COS"
# split types by site
meta_namesandsites <- metadata[c(4,3)]
x_sites <- merge(x, meta_namesandsites, by = "Mandeville_ID")

# simplify hybrid type and plot
x_sites$Hybrid_Type_Simple = NA

for (i in 1:nrow(x_sites)){
  if (x_sites$Multi_Status[i] == 1 & str_detect(x_sites[i,15], "x fishiness") == T){
    x_sites$Hybrid_Type_Simple[i] <- "Probable Parental"
  }
  else if(x_sites$Multi_Status[i] == 1 & str_detect(x_sites[i,15], "x fishiness") == F){
    x_sites$Hybrid_Type_Simple[i] <- "3-species Hybrid" 
  }
  else if(x_sites$Multi_Status[i] == 0 & x_sites$Hybrid_Status[i] == 1){
    x_sites$Hybrid_Type_Simple[i] <- "2-species Hybrid" 
  }
  else if(x_sites$Hybrid_Status[i] == 0){
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

nb.cols <- 12
mycolours <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
#"#A6CEE3" "#5097C5" "#4794A8" "#A4D58D" "#6CBC56" "#579E3F" "#D69B85" "#F05F60" "#E52923" "#F59258" "#FDA746" "#FF7F00"

# change "probable parentals" to "uncertain hybrids"
x_count[x_count=="Probable Parental"] <- "Uncertain Hybrid"


# plot 
(hyb_per_site <- ggplot(x_count, aes(fill=Hybrid_Type_Simple, y=n, x=Waterbody_Code)) + 
  geom_bar(position="fill", stat="identity")+ 
  ylab("Proportion of total individuals") +
  xlab("Waterbody") + 
    scale_fill_manual(values = c(mycolours[9], mycolours[11], mycolours[2], mycolours[5]), 
                      name = "Hybrid Type") +
  theme(legend.position = c(0.1, 0.3)))

# pdf("AMP22_target_hybrids_per_site_new.pdf", width = 10)
# hyb_per_site
# dev.off()

#---------------------------------------------------------------------------
# Identify more inds that don't match their pheno ID for barcoding
#---------------------------------------------------------------------------

# pull all mis-IDs into a separate CSV file
misIDs <- x %>% filter(Pheno_Correct == 1)
misID_parental <- misIDs %>% filter(Hybrid_Status == 0)
misID_hybrid <- misIDs %>% filter(Hybrid_Status == 1)

# pull out any hybrids where the pheno ID isn't either of the geno ID hybrid inds
misID_hybrid$Odd = NA

for (i in 1:nrow(misID_hybrid)){
  if (str_detect(misID_hybrid[i,15], misID_hybrid[i,2]) == TRUE){
    misID_hybrid$Odd[i] <- 0
  }
  else if (str_detect(misID_hybrid[i,15], misID_hybrid[i,2]) == FALSE){
    misID_hybrid$Odd[i] <- 1
  }
}

misID_hybrid_odd <- misID_hybrid %>% filter(misID_hybrid$Odd == 1)

# remove the pimephales sp (already barcoded) and the x fishiness
misID_hybrid_odd <- misID_hybrid_odd %>% filter(!str_detect(misID_hybrid_odd$Geno_ID, 'fish|Pimephales_sp'))

(table_hybrids <- table(misID_hybrid_odd$Geno_ID)) # maybe we could do 1 of each unique hybrid pairing (12 inds)

# add the parentals back in
misID_hybrid_odd <- misID_hybrid_odd[-19]
misIDs_barcode <- rbind(misID_hybrid_odd, misID_parental)
misIDs_barcode <- misIDs_barcode[-c(3:14)] #remove extra columns
#write.csv(misIDs_barcode, "AMP22_Leuciscid_OddMisIDs_k12.csv", row.names = F, quote = F)


#-----------------------------------------------------------------------------

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


#-----------------------------------------------------------------------------

# put multi-species hybrids in their own table                           
multihybrids <- subset(x,  Multi_Status %in% 1)
multihybrids <- multihybrids %>% mutate_if(is.numeric, round, digits=3)
multihybrids_metadata <- merge(multihybrids, metadata, by.x = c("Mandeville_ID", "Common_Name"), by.y = c("Mandeville_ID", "Common_Name"), all.x = T, all.y = F)
(tableGenoID <- table(multihybrids$Geno_ID)) #12/44 are in the "x fishiness" category...

#4 three-way crosses w the big 3
#Common_Shiner x Western_Blacknose_Dace x Creek_Chub:2 
#Creek_Chub x Western_Blacknose_Dace x Common_Shiner:2
# 1 three-way w V7:
#Common_Shiner x Creek_Chub x V7:1
# so all but one ind w at least 0.1 V7 ancestry were DNA barcoded

#export as CSV
#write.csv(multihybrids_metadata, paste0("AMP22_Leuciscid_multispecies_ONLY_hybrids_k",k,"_Jun2023.csv"), row.names = F)

# what sites are the fishy multi species hybrids from?
fishy <- multihybrids_metadata %>% filter(str_detect(multihybrids_metadata$Geno_ID, 'fishiness')) 
#write.csv(fishy, paste0("AMP22_Leuciscid_fishy_ONLY_hybrids_k",k,"_Jun2023.csv"), row.names = F)

# hybrids per plate
table_plate_hybrid <- as.data.frame(table(hybrids_metadata$Plate))



#-----------------------------------------------------------------------------

# segregate the V7 ancestry weirdos
V7_weirdos <- filter(x, str_detect(x[,15],'V7'))

#export as csv file
#write.csv(V7_weirdos, "V7_ancestry_indivs.csv", row.names = F)

#-----------------------------------------------------------------------------

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

k2q_per_species <- read.delim("AMP22_CSxCC_woP13_entropy_list_k2_Geno_ID_q.txt", header = T, sep = "\t")
k2qQ_values <- read.delim("AMP22_CSxCC_woP13_entropy_list_k2_indivs_q_Q.txt", header = T, sep = "\t")

# filter out plate 13
# load metadata
metadata <- read.csv("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/Leuciscid_Metadata_May2023.csv")
# get list of and drop 84 inds from plate 13 inds
# plate13inds <- filter(metadata, Plate == "AMP22_LP13")
# plate13inds <- plate13inds[,4]
# plate13inds <- as.data.frame(plate13inds)
# colnames(plate13inds) <- "Mandeville_ID"
# xQ <- anti_join(k2qQ_values, plate13inds)
# nrowxQ <- nrow(xQ)

xQ <- k2qQ_values

# create new columns
# Geno_ID (will contain Common_Name or "hybrid")
# Pheno_correct (binary, 0=correct, 1=incorrect)
# Hybrid_Status (binary, 0=hybrid, 1=parental)
xQ$Hybrid_Type = 0
xQ$Parental_Status = 0
xQ$Hybrid_Status = 0
xQ$F1_Status = 0
xQ$F2_Status = 0
xQ$F3_Status = 0
xQ$BC1_Status = 0
xQ$BC2_Status = 0
xQ$Other_Status = 0


# classifies as type of hybrid (deciding on these values )
for (i in 1:nrow(xQ)){ 
  if (xQ[i,3] >= 0 & xQ[i,3] <= 0.1 & xQ[i,4] >= 0 & xQ[i,4] <= 0.2) {
    xQ$Hybrid_Type[i] <- "P1" #Parental left side
    xQ$Parental_Status[i] = 1
  } 
  else if (xQ[i,3] >= 0.1875 & xQ[i,3] <= 0.3125 & xQ[i,4] >= 0.375 & xQ[i,4] <= 0.625) {
    xQ$Hybrid_Type[i] <- "BC1P1" #Back cross 2 to left parental
    xQ$Parental_Status[i] = 0
    xQ$Hybrid_Status[i] = 1
  } 
  else if (xQ[i,3] >= 0.375 & xQ[i,3] <= 0.625 & xQ[i,4] >= 0.75 & xQ[i,4] <= 1) {
    xQ$Hybrid_Type[i] <- "F1" #First gen hybrid
    xQ$Hybrid_Status[i] = 1
    xQ$F1_Status[i] = 1
  } 
  else if (xQ[i,3] >= 0.375 & xQ[i,3] <= 0.625 & xQ[i,4] >= 0.375 & xQ[i,4] <= 0.625) {
    xQ$Hybrid_Type[i] <- "F2" #Second gen hybrid
    xQ$Hybrid_Status[i] = 1
    xQ$F2_Status[i] = 1
  } 
  else if (xQ[i,3] >= 0.375 & xQ[i,3] <= 0.625 & xQ[i,4] >= 0.25 & xQ[i,4] <= 0.375) {
    xQ$Hybrid_Type[i] <- "F3" #Third gen hybrid
    xQ$Hybrid_Status[i] = 1
    xQ$F3_Status[i] = 1
  } 
  else if (xQ[i,3] >= 0.6875 & xQ[i,3] <= 0.8125 & xQ[i,4] >= 0.375 & xQ[i,4] <= 0.625) {
    xQ$Hybrid_Type[i] <- "BC1P2" #Back cross 1 to right parental
    xQ$Hybrid_Status[i] = 1
    xQ$BC1_Status[i] = 1
  } 
  else if (xQ[i,3] >= 0.9 & xQ[i,3] <= 1 & xQ[i,4] >= 0 & xQ[i,4] <= 0.2) {
    xQ$Hybrid_Type[i] <- "P2" #Parental right side
    xQ$Parental_Status[i] = 1
  } 
  else {
    xQ$Hybrid_Type[i] <- "Other" #Other
  } 
}

# save data frame
#write.csv(xQ, "AMP22_woP13_k2_CSxCC_old.csv", row.names = F, quote = F)


#summarize the hybrid types found
(table_types <- table(xQ$Hybrid_Type))
dftable_types <- as.data.frame(table_types)
colnames(dftable_types)[1] <- "Hybrid_Type"

#save to compare to other species crosses
#write.csv(dftable_types, paste0("AMP22_hybrid_types_CSxCC_old.csv"), row.names = F, quote = F)

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
(alltypes <- ggplot(xQ_count, aes(fill=Hybrid_Type, y=n, x=Waterbody_Code)) + 
  geom_bar(position="fill", stat="identity")+ 
  ylab("Proportion of total individuals") +
  xlab("Waterbody") + 
  scale_fill_brewer(palette="Spectral", name="Hybrid Type"))

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
#write.csv(xQ_count_simple, paste0("AMP22_hybrid_types_persite_CSxCC.csv"), row.names = F, quote = F)

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
  ylab("Proportion of total individuals")+ 
  xlab("Waterbody")+
  scale_fill_brewer(palette="Spectral", name="Hybrid Type")


#------------------------------------------------------------
# remove parentals and replot
#------------------------------------------------------------
xQ_count_noparental <- filter(xQ_count_simple, Hybrid_Type_Simple != "Parental")



# order by the frequency column
xQ_count_noparental = xQ_count_noparental %>% 
  group_by(Waterbody_Code) %>% 
  arrange(desc(Hybrid_Type_Simple), desc(proportion))# what it sorts by

# create the object w the levels in order
xQ_order = xQ_count_noparental[!duplicated(xQ_count_noparental$Waterbody_Code), ]$Waterbody_Code
# tell it how we want the levels
xQ_count_noparental$Waterbody_Code <- factor(xQ_count_noparental$Waterbody_Code, levels = xQ_order)

# plot
(noparentals <- ggplot(xQ_count_noparental, aes(fill=Hybrid_Type_Simple, y=n, x=Waterbody_Code)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("Proportion of hybrid individuals") +
  xlab("Waterbody")+
  scale_fill_brewer(palette="Spectral", name="Hybrid Type"))



#------------------------------------------------------------
# save the with and without parentals plots
#------------------------------------------------------------

pdf("AMP22_CSxCC_woP13_hybrid_types.pdf", width = 10, height = 12)
alltypes + noparentals + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(ncol = 1)
dev.off()




#============================================================
#============================================================

# Comparing phylogenetic distance and hybridization outcomes

#============================================================
#============================================================

library(hash)
library(psych)

dist_abs <- read.csv("phylo_distance_absolute.csv")
dist_rel <- read.csv("phylo_distance_relative.csv")

# isolate columns that I need but get rid of multi sp hybs bc idk how to deal w them in this scenario
fishies <- x[, c(1,15,17:18)]
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








# somehow connect the above proportion of hybrids with the phylogenetic distance matrix

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

ggplot(numbers, aes(x = Phylo_dist, y = Prop_hybs))+
  geom_point() + 
  geom_text(label=numbers$Abbreviation) +
  xlab("Phylogenetic distance (millions of years)") + ylab("Proportion of hybrids")


print(corr.test(numbers$Phylo_dist, y=numbers$Prop_hybs, use = "pairwise",method="pearson"),short=FALSE)

