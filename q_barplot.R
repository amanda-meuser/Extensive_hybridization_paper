## Script to plot q values from HDF5 files as a barplot and creating caterpiller plots
## Created by Jillian Campbell and Liz Mandeville
## Modified by Amanda Meuser -- May 2023 

## USAGE: Rscript q_barplot.R k_value nametag /path/to/names_file.txt path/to/hdf5/files.hdf5 
        ## names_file.txt should contain a list of all indivs in the HDF5 files, without a header
        ## nametag is the chunk of text that will go in the name of the output files

        # Rscript ../../../../q_barplot.R 6 AMP22_humber_100k ../starting_values_entropy/names_humber.txt /project/rrg-emandevi/hybrid_ameuser/AMP22_pub/geographical_groupings/humber/entropy/AMP22_Pimephales_humber_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k6_100k_rep1_qk6inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/AMP22_pub/geographical_groupings/humber/entropy/AMP22_Pimephales_humber_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k6_100k_rep2_qk6inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/AMP22_pub/geographical_groupings/humber/entropy/AMP22_Pimephales_humber_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k6_100k_rep3_qk6inds.hdf5

# install packages
# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")

# BiocManager::install("rhdf5")
# install.packages("RColorBrewer")
# install.packages("abind")
# install.packages("tidyverse")

# load packages
print("Loading packages...")
library(rhdf5)
library(RColorBrewer)
library(abind)
#library(tibble)
library(dplyr)

args <- commandArgs(TRUE)

k <- as.numeric(args[1])
nametag <- args[2]
names_file <- args[3]
hdf5path1 <- args[4]
hdf5path2 <- args[5]	
hdf5path3 <- args[6]

print("k:")
k

print("Nametag:")
nametag

print("Names file: ")
names_file

print("Paths to HDF5 files:")
hdf5path1
hdf5path2
hdf5path3

# manually read in files
k <- 4
names_file <- "../starting_values_entropy/names_algonquin.txt"
hdf5path1 <- "../entropy/AMP22_Pimephales_target_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k4_100k_rep1_qk4inds.hdf5"
hdf5path2 <- "../entropy/AMP22_Pimephales_target_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k4_100k_rep2_qk4inds.hdf5"	
hdf5path3 <- "../entropy/AMP22_Pimephales_target_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_k4_100k_rep3_qk4inds.hdf5"
metadata <- "Leuciscid_Metadata_May2023.csv"

# extracts k colours from set3 and puts in object
colour <- brewer.pal(k, "Set3")

# creating table with ind names
names_list <- read.table(names_file, header=F, col.names = "ind")
print("Here's the first few individuals...")
head(names_list)

# reading in hdf5 files for given k value
q1 <- h5read(hdf5path1, "q") 
q2 <- h5read(hdf5path2, "q")
q3 <- h5read(hdf5path3, "q")

# getting index for the final and middle indivs, for graphing caterpillars
dims <- dim(q1)
last_indiv <- dims[3] 
middle_indiv <- ceiling((last_indiv/2)) #ceiling() rounds up to the nearest integer
print("The index of the middle individual and final individual:")
middle_indiv
last_indiv

print("Creating caterpillar plot...")
# Checking caterpillar plots, 1 row per rep, 3 inds per plot: first, middle, last
# I removed the ylim=c(0,1) to get a closer look at the wiggling, but just add it after each line (9times)
pdf(paste0("caterpillars_default_scale_",nametag,"_k",k,".pdf"), width=11, height=11) #CHECK THAT THIS IS A VIABLE WCY TO CREATE DIFFERENT FILE NAMES
par(mfrow=c(3,3))
        plot(q1[,1,1], ylim=c(0,1))
        plot(q1[,1,middle_indiv], ylim=c(0,1))
        plot(q1[,1,last_indiv], ylim=c(0,1))

        plot(q2[,1,1], ylim=c(0,1))
        plot(q2[,1,middle_indiv], ylim=c(0,1))
        plot(q2[,1,last_indiv], ylim=c(0,1))

        plot(q3[,1,1], ylim=c(0,1))
        plot(q3[,1,middle_indiv], ylim=c(0,1))
        plot(q3[,1,last_indiv], ylim=c(0,1))
dev.off()

# checking for chain switching manually with K=4
# library(scales)
# q1a <- t(apply(q1, 2:3, mean))
# q2a <- t(apply(q2, 2:3, mean))
# q3a <- t(apply(q3, 2:3, mean))
# head(round(q1a, 3))
# head(round(q2a, 3))
# head(round(q3a, 3))
# pdf(paste0("checking_chain_switching_k",k,".pdf"),height=11, width=11)
# par(mfrow=c(2,2))
#         v1rep1 <- q1a[,1][order(q1a[,1])]
#         v1rep2 <- q2a[,1][order(q2a[,1])]
#         v1rep3 <- q3a[,1][order(q3a[,1])]
#         plot(v1rep1, xlab="Individuals, ordered by q value", ylab="q-value", main="V1", col="royalblue2")
#         points(v1rep2, col=alpha("deeppink", 0.6))
#         points(v1rep3, col=alpha("palegreen1", 0.6))
#         legend(x="topleft", legend=c("Rep 1", "Rep 2", "Rep 3"), col=c("royalblue2", alpha("deeppink", 0.6), alpha("palegreen1", 0.6)), pch=1)

#         v2rep1 <- q1a[,2][order(q1a[,2])]
#         v2rep2 <- q2a[,2][order(q2a[,2])]
#         v2rep3 <- q3a[,2][order(q3a[,2])]
#         plot(v2rep1, xlab="Individuals, ordered by q value", ylab="q-value", main="V2", col="royalblue2")
#         points(v2rep2, col=alpha("deeppink", 0.6))
#         points(v2rep3, col=alpha("palegreen1", 0.6))
#         legend(x="topleft", legend=c("Rep 1", "Rep 2", "Rep 3"), col=c("royalblue2", alpha("deeppink", 0.6), alpha("palegreen1", 0.6)), pch=1)
        
#         v3rep1 <- q1a[,3][order(q1a[,3])]
#         v3rep2 <- q2a[,3][order(q2a[,3])]
#         v3rep3 <- q3a[,3][order(q3a[,3])]
#         plot(v3rep1, xlab="Individuals, ordered by q value", ylab="q-value", main="V3", col="royalblue2")
#         points(v3rep2, col=alpha("deeppink", 0.6))
#         points(v3rep3, col=alpha("palegreen1", 0.6))
#         legend(x="topleft", legend=c("Rep 1", "Rep 2", "Rep 3"), col=c("royalblue2", alpha("deeppink", 0.6), alpha("palegreen1", 0.6)), pch=1)
        
#         v4rep1 <- q1a[,4][order(q1a[,4])]
#         v4rep2 <- q2a[,4][order(q2a[,4])]
#         v4rep3 <- q3a[,4][order(q3a[,4])]
#         plot(v4rep1, xlab="Individuals, ordered by q value", ylab="q-value", main="V4", col="royalblue2")
#         points(v4rep2, col=alpha("deeppink", 0.6))
#         points(v4rep3, col=alpha("palegreen1", 0.6))
#         legend(x="topleft", legend=c("Rep 1", "Rep 2", "Rep 3"), col=c("royalblue2", alpha("deeppink", 0.6), alpha("palegreen1", 0.6)), pch=1)
# dev.off()


# take mean of distributions 
allq <- abind(q1,q2,q3, along=1) 
q <- t(apply(allq, 2:3, mean))


dim(q)


# GET SIMPLIFIED ENTROPY OUTPUT FOR GENERATING GENOMIC ID'S
# manually read in species info
metadata <- read.csv("/project/rrg-emandevi/hybrid_ameuser/Leuciscid_Metadata_May2023.csv", header=T)
species <- metadata[c("Mandeville_ID","Common_Name")] # before doing this, filter/merge the metadata by the names file, then contine to bind w the entropy data. saves having to create a filtered metadata file for each dataset

# manually bind species data to names
q_names <- as.data.frame(cbind(names_list, q))

# merge w common names and save as text file
q_species <- merge(species, q_names, by.x = "Mandeville_ID", by.y = "ind")
write.table(q_species, paste0(nametag,"_entropy_list_k",k,"_indivs_q.txt"), sep = "\t", row.names = F, quote = F)

# make sure the numbers are class numeric
for (i in 3:ncol(q_species)){
        q_species[,i] <- as.numeric(q_species[,i])
}

#create a table of mean q values for each column, for each species
(mean_q_species <- q_species %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = mean))))

#save table as a text file
write.table(mean_q_species, paste0(nametag,"_entropy_list_k",k,"_species_q.txt"), sep = "\t", row.names = F, quote = F)




#find row based on AMP ID
#q_species[q_species$ind == 'AMP22_0800',]



#PLOTTING BY LOCATION   
# print("Read in metadata...")
# metadata <- read.csv(metadata, header = T)

# #pull out columns and bind to q values
# q_metadata <- cbind(metadata$Mandeville_ID, metadata$Common_Name, metadata$Waterbody, metadata$Waterbody_Code, q)
# q_metadata <- as.data.frame(q_metadata)

# #rename the new columns and alter the C.Ward waterbody codes
# names(q_metadata)[1:4] <- c("Mandeville_ID", "Common_Name", "Waterbody","Waterbody_Code")
# q_metadata$Waterbody_Code[q_metadata$Waterbody == "Costello_Creek"] <- "COS"
# q_metadata <- q_metadata[-3]

# # make sure the q columns are numeric data, then round values of q to 3 decimal places 
# q_metadata <- type.convert(q_metadata, as.is=T)
# #sapply(q_metadata, class)
# q_metadata_round <- q_metadata %>% mutate_if(is.numeric, round, digits=3)

# #merge ID and common name
# q_metadata_round$ID_species <- paste(q_metadata_round$Mandeville_ID, q_metadata_round$Common_Name, sep="/")

# #split data by waterbody code
# #split_q_metadata_round <- split(q_metadata_round, f=q_metadata_round$Waterbody_Code)

# # using dataframes that are pre-demensionality reduction to get confidence intervals
# allq <- abind(q1, q2, q3, along=1)
# #head(allq)

#looking at CIs - number should be really small!
q.ci <- apply(allq, 2:3, quantile, probs=c(0.025,0.975))
q.ci.width <- q.ci[2,2,]-q.ci[1,2,]
print("Mean width of confidence intervals -- should be quite small:")
mean(q.ci.width)
max(q.ci.width)

# q.ci.width <- as.data.frame(q.ci.width)
# CIs_names <- cbind(metadata$Mandeville_ID,q.ci.width)
# colnames(CIs_names) <- c("Mandeville_ID", "CI_width")
# plate13 <- read.table("../Plate13_inds.txt", header = T)
# CIs_names <- anti_join(as.data.frame(CIs_names), plate13)
#write.table(CIs_names, "AMP22_target_k1_CIs.txt", quote = F, row.names = F)

# pdf(paste("histogram_CIs_k",k,".pdf"))
# hist(as.numeric(CIs_names$CI_width), 
#         xlab = "Credible interval width", 
#         main = " ",
#         col = "steelblue")
# dev.off()

##############################################
# # PLOTTING JUST THE MULTI-SPECIES HYBRIDS
# metadata <- read.csv(metadata, header = T)

# #pull out columns and bind to q values
# q_metadata <- cbind(metadata$Mandeville_ID, metadata$Common_Name, metadata$Waterbody, metadata$Waterbody_Code, q)
# q_metadata <- as.data.frame(q_metadata)

# #rename the new columns and alter the C.Ward waterbody codes
# names(q_metadata) <- c("Mandeville_ID", "Common_Name", "Waterbody","Waterbody_Code", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12")
# q_metadata$Waterbody_Code[q_metadata$Waterbody == "Costello_Creek"] <- "COS"
# q_metadata <- q_metadata[-3]

# # make sure the q columns are numeric data, then round values of q to 3 decimal places 
# q_metadata <- type.convert(q_metadata, as.is=T)
# #sapply(q_metadata, class)
# # actually, i don't need to round the values if i'm ordering by largest ancestry proportion. this will keep the tops more even
# #q_metadata_round <- q_metadata %>% mutate_if(is.numeric, round, digits=3)

# #filter to get just the multi-species inds
# multi_deets <- read.csv("AMP22_Leuciscid_multispecies_ONLY_hybrids_k1_Jun2023.csv", header = T)
# multi <- as.data.frame(multi_deets[,1])
# colnames(multi) <- "Mandeville_ID"
# q_multi <- merge(q_metadata, multi, by = "Mandeville_ID")

# #order data frame by largest proportions to smallest
# q_multi_order <- q_multi[with(q_multi, order(-pmax(q_multi$V1, q_multi$V2, q_multi$V3, q_multi$V4, q_multi$V5, q_multi$V6, q_multi$V7, q_multi$V8, q_multi$V9, q_multi$V10, q_multi$V11, q_multi$V12))),]

##############################################



#combining name file with q file
q.names <- data.frame(cbind(names_list, q))
q.names <- q.names %>% mutate_if(is.numeric, round, digits=3)
head(q.names)


#renaming the columns with species names -- need to find better method than just guessing
#colnames(q.names)[2:(k+1)] <- c("A", "B", "C") #FIND A WAY TO INSERT THE SPECIES NAMES
#head(q.names) 

K <- (k+1) 
#colour <- c("#FDBF6F","#8C510A","#35978F","#CAB2D6","#FFFF99","#B2DF8A","#969696","#FB9A99","#A6CEE3","#DF65B0","#000000","#000000")
rows <- nrow(q.names)

print("Creating barplot...")
#plotting proportion of ancestry
pdf(paste0("q_barplot_",nametag,"_k",k,".pdf"), width = 14, height = 12) 
barplot(t(q.names[order(q.names$X1, q.names$X2),2:K]), 
        beside=F, 
        col=colour,
        names.arg = q.names$ind[order(q.names$X1, q.names$X2)],
        #names.arg = rep("", nrow(q.names)), 
        las=2, 
        cex.names=0.5,
        border=NA, 
        main=paste("Bar plot of",nametag),
        ylab="proportion of ancestry",
        xlab ="Sample ID",
        xlim = c(0,rows),
        space = -0.3,
        width = 1) 
dev.off()

# FOR MAKING NICER VERSIONS, ONCE YOU KNOW WHICH CLUSTER IS WHICH SPECIES
# colour <- c("grey40","#DF65B0","#B2DF8A","#A6CEE3","grey85","#FDBF6F")

# print("Creating barplot...")
# #plotting proportion of ancestry
# pdf(paste0("q_barplot_",nametag,"_k",k,".pdf"), width = 12, height = 10) 
# barplot(t(q.names[order(q.names$X2, q.names$X3),2:K]), 
#         beside=F, 
#         col=colour,
#         #names.arg = q.names$ind[order(q.names$X1, q.names$X2)],
#         names.arg = rep("", nrow(q.names)), 
#         las=2, 
#         #cex.names=0.5,
#         border=NA, 
#         main=paste("Bar plot of",nametag),
#         ylab="proportion of ancestry",
#         legend.text = c("Unknown 1", "Western Blacknose Dace", "River Chub", "Creek Chub", "Unknown 2", "Common Shiner"),
#         xlab ="Individual",
#         xlim = c(0,rows),
#         space = -0.3,
#         width = 1) 
# dev.off()



# FOR PLOTTING MULTI SPECIES HYBRIDS
# L <- k+3
# colour <- c("#FDBF6F","#8C510A","#35978F","#CAB2D6","#FFFF99","#B2DF8A","#969696","#FB9A99","#A6CEE3","#DF65B0","#000000","#000000")
# print("Creating barplot...")
# #plotting proportion of ancestry
# pdf(paste0("q_barplot_AMP22_target_multispecies_k",k,".pdf"), width = 12, height = 12) 
# barplot(t(q_multi_order[4:L]), 
#         beside=F, 
#         col=colour,
#         names.arg = rep("", nrow(q_multi_order)), 
#         las=2, 
#         cex.lab=1.5,
#         border=NA, 
#         ylab="Proportion of ancestry",
#         xlab ="Individuals",
#         legend.text = c("Common Shiner", "Central Stoneroller", "Horneyhead Chub", "Longnose Dace", "Striped Shiner", "River Chub", "Pimephales sp.", "Rosyface Shiner", "Creek Chub", "Western Blacknose Dace", "V11", "V12"),
#         xlim = c(0,100),
#         space = -0.3,
#         width = 2.6) 
# dev.off()


# colour <- c("#8E0152", "#6A3D9A", "#FFFF33", "#E31A1C", "#543005", "#FF7F00", "#33A02C", "#000000", "#1F78B4")
# L <- k+3
# waterbody_names <- c("AT", "BW", "CAC", "CLC", "COS", "EP1", "EP2", "EP3", "EP4", "ERR", "HC", "HT", "HUR", "IRC", "KC", "LAC", "LAP", "LEST", "MAC", "MEF", "PF", "PVTF", "SAR", "SCU", "SIC", "SWP", "UH", "WA", "WC")

# # plot by location individually
# pdf(paste0("q_barplot_AMP22_k1_WA.pdf"), width = 11, height = 8) 
# barplot(t(split_q_metadata_round$WA[order(split_q_metadata_round$WA$V1),4:L]), 
#         beside=F, 
#         col=colour,
#         names.arg =split_q_metadata_round$WA$ID_species[order(split_q_metadata_round$WA$V1)], 
#         las=2, 
#         cex.names=0.25,
#         border=NA, 
#         space=-0.3, 
#         ylab="proportion of ancestry",
#         xlab ="Sample ID",
#         legend = T,
#         xlim = c(0,50))
# dev.off()



# # lapply loop for creating all locations in one file (not individually titled, though)
# pdf(paste0("q_barplot_AMP22_k1_all_sites.pdf"), width = 11, height = 8) 
# lapply(split_q_metadata_round, function(x) {
# barplot(t(x[order(x$V1, x$V2, x$V3, x$V4, x$V5, x$V6, x$V7, x$V8, x$V9),4:L]), 
#         beside=F, 
#         col=colour,
#         names.arg =x$ID_species[order(x$V1, x$V2, x$V3, x$V4, x$V5, x$V6, x$V7, x$V8, x$V9)], 
#         las=2, 
#         cex.names=0.25,
#         border=NA, 
#         space=-0.3, 
#         ylab="proportion of ancestry",
#         xlab ="Sample ID",
#         main = paste("q scores at a given waterbody"),
#         legend = T,
#         xlim = c(0,50))
# })
# dev.off()

#L <- k+3
# for loop for creating all locations in one file (not individually titled, though)
# pdf(paste0("q_barplot_AMP22_",k,"_all_sites_unordered.pdf"), width = 11, height = 8) 
# for (i in split_q_metadata_round){
# barplot(t(i[,4:L]), 
#          beside=F, 
#          col=colour,
#          names.arg =i$ID_species, 
#          las=2, 
#          cex.names=0.25,
#          border=NA, 
#          space=-0.3, 
#          ylab="proportion of ancestry",
#          xlab ="Sample ID",
#          main = paste("q scores at a given waterbody" ),
#          legend.text = c("CS", "CSR", "HHC", "LND", "SS", "RC", "V7", "RFS", "CC", "BND", "V11", "V12"),
#          xlim = c(0,50))
# }
# dev.off()

#drop indiv w incorrect barcode
# q_metadata_round <- q_metadata_round[q_metadata_round$Mandeville_ID != "EGM19_0770",]

# #count number of inds per site
# inds_per_site <- aggregate(q_metadata_round$Mandeville_ID, by=list(q_metadata_round$Waterbody_Code), length)
# min_inds <- as.data.frame(inds_per_site$Group.1[inds_per_site$x >=20])
# colnames(min_inds) <- "Waterbody_Code"


# K = k+3

# pdf(paste0("q_barplot_AMP22_",k,"_all_sites_>20inds.pdf"), width = 12, height = 12) 
# par(mfrow=c(4,4), mar=c(3,2,2,1))

# for (i in 1:nrow(min_inds)){

#   site_inds <- q_metadata_round[q_metadata_round$Waterbody_Code == min_inds$Waterbody_Code[i],]

#   barplot(t(site_inds[order(site_inds$Waterbody_Code, site_inds$V9, site_inds$V2, site_inds$V3, site_inds$V4),4:K]), 
#         beside=F, 
#         col=colour,
#         #names.arg =site_inds$ID_species[order(site_inds$Waterbody_Code, site_inds$V9, site_inds$V2, site_inds$V3, site_inds$V4)], 
#         names.arg = rep("", nrow(site_inds[,1])),
#         las=2, 
#         #cex.names=0.25,
#         border=NA, 
#         #ylab="proportion of ancestry",
#         #xlab ="Sample ID",
#         #legend.text = c("CS", "CSR", "HHC", "LND", "SS", "RC", "V7", "RFS", "CC", "BND", "V11", "V12"),
#         xlim = c(0,100),
#         space = -0.3,
#         width = 3,
#         horiz = T,
#         axes = F)

#   axis(1, at=c(0,0.5,1), labels=c(0,0.5,1))

#   mtext(paste("Site ",min_inds$Waterbody_Code[i], ", n=", inds_per_site$x[inds_per_site$Group.1 == min_inds$Waterbody_Code[i]], sep=""), side=3, cex=0.6)
# }

# plot(1:10,1:10, type="n", axes=F, xlab="", ylab="")
# legend("center", legend = c("CS", "CSR", "HHC", "LND", "SS", "RC", "V7", "RFS", "CC", "BND", "V11", "V12"), col = colour, pch=15, pt.cex=2, cex=1)

# mtext("Proportion of Ancestry", side=1, cex=0.75, outer=T, line=-1)
# mtext("Individuals", side=2, cex=0.75, outer=T, line=-1)


# dev.off()

#k=12
#legend.text = c("CS", "CSR", "HHC", "LND", "SS", "RC", "V7", "RFS", "CC", "BND", "V11", "V12"),
# colour <- c("#FF7F00", "#543005", "#1F78B4", "#6A3D9A", "#FFFF33", "#33A02C", "#ffd9df", "#E31A1C", "#8E0152", "#000000", "#FFC0CB", "#cc99a2")
#new colours
# colour <- c("#FDBF6F","#8C510A","#35978F","#CAB2D6","#FFFF99","#B2DF8A","#969696","#FB9A99","#A6CEE3","#DF65B0","#000000","#000000")

#k=9
#legend.text = c("CC", "LND", "SS", "RFS", "CSR", "CS", "RC", "BND", "HHC"),
# colour <- c("#8E0152", "#6A3D9A", "#FFFF33", "#E31A1C", "#543005", "#FF7F00", "#33A02C", "#000000", "#1F78B4")
        
