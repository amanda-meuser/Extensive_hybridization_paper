## Script to plot q values from HDF5 files as a barplot and creating caterpiller plots
## Created by Jillian Campbell and Liz Mandeville
## Modified by Amanda Meuser -- May 2023 

## USAGE: Rscript q_barplot.R k_value nametag /path/to/names_file.txt path/to/hdf5/files.hdf5 
        ## names_file.txt should contain a list of all indivs in the HDF5 files, without a header
        ## nametag is the chunk of text that will go in the name of the output files

        # Rscript ../../q_barplot.R 7 AMP22_target AMP22_target_indivs_filtered.txt /project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k7_150k_rep1_qk7inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k7_150k_rep2_qk7inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k7_150k_rep3_qk7inds.hdf5


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

args <- commandArgs(TRUE)

k <- as.numeric(args[1])
names_file <- args[3]
nametag <- args[2]
hdf5path1 <- args[4]
hdf5path2 <- args[5]	
hdf5path3 <- args[6]

print("k:")
k

print("names_file:")
names_file

print("nametag:")
nametag

print("Paths to HDF5 files:")
hdf5path1
hdf5path2
hdf5path3

# manually read in files
# k <- 12 
# names_file <- "AMP22_target_indivs_filtered.txt"
# nametag <- AMP22_target
# hdf5path1 <- "/project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep1_qk12inds.hdf5"
# hdf5path2 <- "/project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep2_qk12inds.hdf5"	
# hdf5path3 <- "/project/rrg-emandevi/hybrid_ameuser/AMP22/entropy/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k12_150k_rep3_qk12inds.hdf5"


# extracts k colours from set3 and puts in object
colour <- brewer.pal(k, "Set3")


# creating table with ind names
names_list <- read.table(names_file, header=F, col.names = c("ind"))
print("Here's the first few individuals...")
head(names_list)

# reading in hdf5 files for given k value
data1.q <- h5read(hdf5path1, "q") 
data2.q <- h5read(hdf5path2, "q")
data3.q <- h5read(hdf5path3, "q")

# getting index for the final and middle indivs, for graphing caterpillars
dims <- dim(data1.q)
last_indiv <- dims[3] 
middle_indiv <- ceiling((last_indiv/2)) #ceiling() rounds up to the nearest integer
print("The index of the middle individual and final individual:")
middle_indiv
last_indiv

print("Creating caterpillar plot...")
# Checking caterpillar plots, 1 row per rep, 3 inds per plot: first, middle, last
pdf(paste0("caterpillars_",nametag,"_k",k,".pdf"), width=11, height=11) #CHECK THAT THIS IS A VIABLE WAY TO CREATE DIFFERENT FILE NAMES
par(mfrow=c(3,3))
        plot(data1.q[,1,1], ylim=c(0,1))
        plot(data1.q[,1,middle_indiv], ylim=c(0,1))
        plot(data1.q[,1,last_indiv], ylim=c(0,1))

        plot(data2.q[,1,1], ylim=c(0,1))
        plot(data2.q[,1,middle_indiv], ylim=c(0,1))
        plot(data2.q[,1,last_indiv], ylim=c(0,1))

        plot(data3.q[,1,1], ylim=c(0,1))
        plot(data3.q[,1,middle_indiv], ylim=c(0,1))
        plot(data3.q[,1,last_indiv], ylim=c(0,1))
dev.off()

# improved axis labelling? not working rn...
# pdf(paste0("caterpillars_",nametag,"_k",k,".pdf"), width=11, height=11) #CHECK THAT THIS IS A VIABLE WAY TO CREATE DIFFERENT FILE NAMES
# par(mfrow=c(3,3))
#         plot(data1.q[,1,1], ylim=c(0,1), main="Individual 1")
#         plot(data1.q[,1,middle_indiv], ylim=c(0,1), main="Individual "middle_indiv"")
#         plot(data1.q[,1,last_indiv], ylim=c(0,1), main="Individual "last_indiv"")

#         plot(data2.q[,1,1], ylim=c(0,1))
#         plot(data2.q[,1,middle_indiv], ylim=c(0,1), ylab="Value of q")
#         plot(data2.q[,1,last_indiv], ylim=c(0,1))

#         plot(data3.q[,1,1], ylim=c(0,1))
#         plot(data3.q[,1,middle_indiv], ylim=c(0,1), xlab="Iteration of q")
#         plot(data3.q[,1,last_indiv], ylim=c(0,1))
# dev.off()


# takes the mean of all 1000 values for the estimate of q, to essentially create a point estimate from a distribution
q1 <- t(apply(data1.q, 2:3, mean)) 
q2 <- t(apply(data2.q, 2:3, mean))
q3 <- t(apply(data3.q, 2:3, mean))

# Looking at columns to check if chain switching occured, but thanks to the starting values, it shouldn't 
print("Check for chain switching")
head(q1)
head(q2)
head(q3)

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
#mean_q_species <- q_species %>% group_by(species) %>% summarize(meanV1 = mean(V1), meanV2 = mean(V2), meanV3 = mean(V3), meanV4 = mean(V4), meanV5 = mean(V5), meanV6 = mean(V6), meanV7 = mean(V7), meanV8 = mean(V8), meanV9 = mean(V9))
#(mean_q_species <- q_species %>% group_by(species) %>% summarize(across(everything(), list(mean))))

# save table as a text file
#write.table(mean_q_species, paste0("AMP22_target_entropy_k",k,"_species_q.txt"), sep = "\t", row.names = F, quote = F)

# add names and save this as a text file
#q_sp.names <- cbind(names_list, species, q)
#write.table(q_sp.names, paste0("AMP22_target_entropy_k",k,"_indivs_q.txt"), sep = "\t", row.names = F, quote = F)

# find row based on AMP ID
#q_species[q_species$ind == 'AMP22_0800',]


# using dataframes that are pre-demensionality reduction to get confidence intervals
allq <- abind(data1.q, data2.q, data3.q, along=1)
#head(allq)

#looking at CIs - number should be really small!
q.ci <- apply(allq, 2:3, quantile, probs=c(0.025,0.975))
q.ci.width <- q.ci[2,2,]-q.ci[1,2,]
print("Mean width of confidence intervals -- should be quite small:")
mean(q.ci.width)


#combining name file with q file
q.names <- data.frame(cbind(names_list, q))
#head(q.names)

#renaming the columns with species names -- need to find better method than just guessing
#colnames(q.names)[2:(k+1)] <- c("A", "B", "C") #FIND A WAY TO INSERT THE SPECIES NAMES
#head(q.names) 

K<-(k+1) 


print("Creating barplot...")
#plotting proportion of ancestry
pdf(paste0("q_barplot_",nametag,"_k",k,"_moreorder.pdf"), width = 11, height = 8) #GET THE NAMING CONVENTION SAME AS NAME FOR CATERPILLER PLOTS
barplot(t(q.names[order(q.names$V6, q.names$V1, decreasing = c(T,F)),2:K]), 
        beside=F, 
        col=colour,
        names.arg =q.names$ind[order(q.names$V1)], 
        las=2, 
        cex.names=0.25,
        border=NA, 
        space=-0.3, 
        ylab="proportion of ancestry",
        xlab ="Sample ID",
        legend = T,
        xlim = c(0,600))
dev.off()



