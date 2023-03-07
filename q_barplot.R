## Script to plot q values from HDF5 files as a barplot and creating caterpiller plots
## Created by Jillian Campbell and/or Liz Mandeville?
## Modified by Amanda Meuser -- March 2023 

## USAGE: Rscript q_barplot.R k_value nametag /path/to/names_file.txt path/to/hdf5/files.hdf5 
        ## names_file.txt should contain a list of all indivs in the HDF5 files
        ## nametag is the chunk of text that will go in the name of the output files

        # Rscript q_barplot.R 3 EGM19_target /project/rrg-emandevi/hybrid_ameuser/plotting_entropy/EGM19_target/names_target2.txt /project/rrg-emandevi/hybrid_ameuser/entropy/EGM19_target/EGM19_target_27jul22_miss0.4_mac3_Q30_DP3_ind95_maf001_k3_150k_rep1_qk3inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/entropy/EGM19_target/EGM19_target_27jul22_miss0.4_mac3_Q30_DP3_ind95_maf001_k3_150k_rep2_qk3inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/entropy/EGM19_target/EGM19_target_27jul22_miss0.4_mac3_Q30_DP3_ind95_maf001_k3_150k_rep3_qk3inds.hdf5


# install packages
# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")

# BiocManager::install("rhdf5")
# install.packages("RColorBrewer")
# install.packages("tidyverse")

# load packages
print("Loading packages...")
library(rhdf5)
library(RColorBrewer)
library(tidyverse)

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

# extracts colours from set3 and puts in object, could try pulling in k here
colour <- brewer.pal(k, "Set3")

# creating table with ind names
names_list <- read.table(names_file, header=T, col.names = c("ind"))
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


#looking at confidence intervals - number should be really small! (not working rn)
# q.ci <- apply(q, 2:3, quantile, probs=c(0.025,0.975))
# q.ci.width <- q.ci[2,2,]-q.ci[1,2,]
# mean(q.ci.width)

#combining name file with q file
q.names <- data.frame(cbind(names_list, q))
#head(q.names)

#renaming the columns with species names -- need to find better method than just guessing
#colnames(q.names)[2:(k+1)] <- c("A", "B", "C") #FIND A WAY TO INSERT THE SPECIES NAMES
#head(q.names) 

K<-(k+1) 


print("Creating barplot...")
#plotting proportion of ancestry
pdf(paste0("q_barplot_",nametag,"_k",k,".pdf"), width = 11, height = 8) #GET THE NAMING CONVENTION SAME AS NAME FOR CATERPILLER PLOTS
barplot(t(q.names[order(q.names$V1),2:K]), 
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
        xlim = c(0,150))
dev.off()



