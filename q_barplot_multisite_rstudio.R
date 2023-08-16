## Script to plot q values from HDF5 files as a barplot and creating caterpiller plots
## Created by Jillian Campbell and Liz Mandeville
## Modified by Amanda Meuser -- May 2023 

## USAGE: Rscript q_barplot.R k_value nametag /path/to/metadata_file.txt path/to/hdf5/files.hdf5 
        ## names_file.txt should contain a list of all indivs in the HDF5 files, without a header
        ## nametag is the chunk of text that will go in the name of the output files

        # Rscript ../../q_barplot.R 12 AMP22_target Leuciscid_Metadata_OFAT_May2023_filtered.csv /project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep1_qk12inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep2_qk12inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep3_qk12inds.hdf5

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
library(tibble)
library(dplyr)

# manually read in files
k <- 12
hdf5path1 <- "AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep1_qk12inds.hdf5"
hdf5path2 <- "AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep2_qk12inds.hdf5"	
hdf5path3 <- "AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep3_qk12inds.hdf5"
metadata <- "../Leuciscid_Metadata_OFAT_May2023_filtered.csv"

# extracts k colours from set3 and puts in object
#colour <- brewer.pal(k, "Set3")


# creating table with ind names
#names_list <- read.table(names_file, header=F, col.names = "ind")
#print("Here's the first few individuals...")
#head(names_list)

# reading in hdf5 files for given k value
data1.q <- h5read(hdf5path1, "q") 
data2.q <- h5read(hdf5path2, "q")
data3.q <- h5read(hdf5path3, "q")


# takes the mean of all 1000 values for the estimate of q, to essentially create a point estimate from a distribution
q1 <- t(apply(data1.q, 2:3, mean)) 
q2 <- t(apply(data2.q, 2:3, mean))
q3 <- t(apply(data3.q, 2:3, mean))

#combining the three data frames into 1
#step 1:converting from matricies to data frames, then adding a row position ID
dfq1 <- as.data.frame(q1) %>% rowid_to_column("ROW")
dfq2 <- as.data.frame(q2) %>% rowid_to_column("ROW")
dfq3 <- as.data.frame(q3) %>% rowid_to_column("ROW")

#step 2:bind into one dataframe, but they're still numbered by row position in the original 3 dataframes
everyq <- bind_rows(dfq1, dfq2, dfq3)
#head(everyq)
print("Merged 3 data frames into 1. Dimensions:")
dim(everyq)

#step 3:grouping by their row ID, take the mean for each row
q <- everyq %>% 
  group_by(ROW) %>% 
  summarise_all(mean)

#head(q)
print("Summarised and took mean. Dimensions:")
dim(q)


#step 4: remove row number column and turn back into a matrix
q <- q[,-1] 
q <- as.matrix(q)
#head(q)
#class(q)
print("Removed indexing column. Dimensions:")
dim(q)

# manually read in species info
# species <- read.table("AMP22_target_species_filtered.txt", header=F, col.names = c("species"))

# manually bind species data 
#q_species <- cbind(species, q)

# create a table of mean q values for each column, for each species
#(mean_q_species <- q_species %>% group_by(species) %>% summarize(across(everything(), list(mean))))

# save table as a text file
#write.table(mean_q_species, paste0("AMP22_target_entropy_k",k,"_species_q.txt"), sep = "\t", row.names = F, quote = F)

# add names and save this as a text file
#q.sp.names <- cbind(names_list, species, q)
#write.table(q.sp.names, paste0("AMP22_target_entropy_k",k,"_indivs_q.txt"), sep = "\t", row.names = F, quote = F)

# find row based on AMP ID
#q_species[q_species$ind == 'AMP22_0800',]



#PLOTTING BY LOCATION   
print("Read in metadata...")
metadata <- read.csv(metadata, header = T)

#pull out columns and bind to q values
q_metadata <- cbind(metadata$Mandeville_ID, metadata$Common_Name, metadata$Waterbody, metadata$Waterbody_Code, metadata$Plate, metadata$Lab, q)
q_metadata <- as.data.frame(q_metadata)

#rename the new columns and alter the C.Ward waterbody codes
names(q_metadata)[1:6] <- c("Mandeville_ID", "Common_Name", "Waterbody","Waterbody_Code", "Plate", "Lab")
q_metadata$Waterbody_Code[q_metadata$Waterbody == "Costello_Creek"] <- "COS"
#drop indivs from barcode plate 13
q_metadata <- q_metadata[q_metadata$Plate != "AMP22_LP13",]
#check inds per year
inds_per_year <- aggregate(q_metadata$Mandeville_ID, by=list(q_metadata$Lab), length)
#remove columns unecessary for downstream plotting
q_metadata <- q_metadata[-c(3,5,6)]

# make sure the q columns are numeric data, then round values of q to 3 decimal places 
q_metadata <- type.convert(q_metadata, as.is=T)
#sapply(q_metadata, class)
q_metadata_round <- q_metadata %>% mutate_if(is.numeric, round, digits=3)

#merge ID and common name
q_metadata_round$ID_species <- paste(q_metadata_round$Mandeville_ID, q_metadata_round$Common_Name, sep="/")

#split data by waterbody code
#split_q_metadata_round <- split(q_metadata_round, f=q_metadata_round$Waterbody_Code)

# using dataframes that are pre-demensionality reduction to get confidence intervals
allq <- abind(data1.q, data2.q, data3.q, along=1)
#head(allq)

#looking at CIs - number should be really small!
q.ci <- apply(allq, 2:3, quantile, probs=c(0.025,0.975))
q.ci.width <- q.ci[2,2,]-q.ci[1,2,]
print("Mean width of confidence intervals -- should be quite small:")
mean(q.ci.width)



K<-k+3
colour <- c("#FDBF6F","#8C510A","#35978F","#CAB2D6","#FFFF99","#B2DF8A","#969696","#FB9A99","#A6CEE3","#DF65B0","#000000","#000000")



#count number of inds per site
inds_per_site <- aggregate(q_metadata_round$Mandeville_ID, by=list(q_metadata_round$Waterbody_Code), length)
min_inds <- as.data.frame(inds_per_site$Group.1[inds_per_site$x >=15])
colnames(min_inds) <- "Waterbody_Code"


pdf(paste0("q_barplot_AMP22_",k,"_all_sites_min_15inds.pdf"), width = 12, height = 15) 
par(mfrow=c(5,4), mar=c(4,3,2,1))

for (i in 1:nrow(min_inds)){

  site_inds <- q_metadata_round[q_metadata_round$Waterbody_Code == min_inds$Waterbody_Code[i],]

  barplot(t(site_inds[order(round(site_inds$V9, digits = 1), round(site_inds$V10, digits = 1), round(site_inds$V1, digits = 1), round(site_inds$V3, digits = 1), round(site_inds$V6, digits = 1)),4:K]), 
        beside=F, 
        col=colour,
        #names.arg =site_inds$ID_species[order(site_inds$Waterbody_Code, site_inds$V9, site_inds$V2, site_inds$V3, site_inds$V4)], 
        names.arg = rep("", nrow(site_inds)),
        las=2, 
        cex.axis = 2,
        #cex.names=0.25,
        border=NA, 
        #ylab="proportion of ancestry",
        #xlab ="Sample ID",
        #legend.text = c("CS", "CSR", "HHC", "LND", "SS", "RC", "V7", "RFS", "CC", "BND", "V11", "V12"),
        xlim = c(0,1),
        space = -0.2,
        width = 3,
        horiz = T,
        axes = F)

  axis(1, at=c(0,0.5,1), labels=c(0,0.5,1))

  mtext(paste("Site ",min_inds$Waterbody_Code[i], ", n=", inds_per_site$x[inds_per_site$Group.1 == min_inds$Waterbody_Code[i]], sep=""), side=3, cex=1.5)
}

plot(1:10,1:10, type="n", axes=F, xlab="", ylab="")
legend("center", legend = c("Common Shiner", "Central Stoneroller", "Hornyhead Chub", "Longnose Dace", "Striped Shiner", "River Chub", "V7", "Roseyface Shiner", "Creek Chub", "Western Blacknose Dace", "V11", "V12"), col = colour, pch=15, pt.cex=3.75, cex=1.4, ncol = 1)

mtext("Proportion of Ancestry", side=1, cex=1, outer=T, line=-2)
mtext("Individuals", side=2, cex=1, outer=T, line=-2)


dev.off()

#k=12
#legend.text = c("CS", "CSR", "HHC", "LND", "SS", "RC", "V7", "RFS", "CC", "BND", "V11", "V12"),
# colour <- c("#FF7F00", "#543005", "#1F78B4", "#6A3D9A", "#FFFF33", "#33A02C", "#ffd9df", "#E31A1C", "#8E0152", "#000000", "#FFC0CB", "#cc99a2")
#new colours
# colour <- c("#FDBF6F","#8C510A","#35978F","#CAB2D6","#FFFF99","#B2DF8A","#969696","#FB9A99","#A6CEE3","#DF65B0","#000000","#000000")

#k=9
#legend.text = c("CC", "LND", "SS", "RFS", "CSR", "CS", "RC", "BND", "HHC"),
# colour <- c("#8E0152", "#6A3D9A", "#FFFF33", "#E31A1C", "#543005", "#FF7F00", "#33A02C", "#000000", "#1F78B4")
        