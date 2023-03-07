## Script to plot q and Q values from HDF5 files against one another as a triangle plot
## Created by Jillian Campbell and/or Liz Mandeville?
## Modified by Amanda Meuser -- March 2023 

## USAGE: Rscript Q_triangleplot.R nametag /path/to/names_file.txt path/to/hdf5/files.hdf5
    ## Rscript Q_triangleplot.R EGM19_target /project/rrg-emandevi/hybrid_ameuser/entropy/EGM19_target/EGM19_target_27jul22_miss0.4_mac3_Q30_DP3_ind95_maf001_k3_150k_rep1_qk3inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/entropy/EGM19_target/EGM19_target_27jul22_miss0.4_mac3_Q30_DP3_ind95_maf001_k3_150k_rep2_qk3inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/entropy/EGM19_target/EGM19_target_27jul22_miss0.4_mac3_Q30_DP3_ind95_maf001_k3_150k_rep3_qk3inds.hdf5



# install packages
# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")

# BiocManager::install("rhdf5")
# install.packages("RColorBrewer")
# install.packages("tidyverse")


#load necessary packages
print("Loading packages...")
library(rhdf5)
library(abind)
library(RColorBrewer)
library(tidyverse)


args <- commandArgs(TRUE)

k <- as.numeric(2)
nametag <- as.character(args[1])
hdf5path1 <- args[2]
hdf5path2 <- args[3]	
hdf5path3 <- args[4]

print("k:")
k

print("nametag:")
nametag

print("Paths to HDF5 files:")
hdf5path1
hdf5path2
hdf5path3


# read in files for both q and Q 
q1 <- h5read(hdf5path1, "q")
q2 <- h5read(hdf5path2, "q")
q3 <- h5read(hdf5path3, "q")

Q1 <- h5read(hdf5path1, "Q")
Q2 <- h5read(hdf5path2, "Q")
Q3 <- h5read(hdf5path3, "Q")


# Looking at columns to check if chain switching occured, but thanks to the starting values, it shouldn't 
#head(q1)
#head(q2)
#head(q3)


# takes the mean of all 1000 values for the estimate of q, to essentially create a point estimate from a distribution
# NOT SURE IF THIS DOES WHAT IT SHOULD -- I THINK I DID THIS DIFFERENTLY IN THE q SCRIPT
allq <- abind(q1,q2,q3, along=1) 
q <- t(apply(allq, 2:3, mean))

allQ <- abind(Q1,Q2,Q3, along=1)
Q <- t(apply(allQ, 2:3, mean))


#####################################################################################################################################
# # takes the mean of all 1000 values for the estimate of q, to essentially create a point estimate from a distribution
# data1.q <- t(apply(q1, 2:3, mean)) 
# data2.q <- t(apply(q2, 2:3, mean))
# data3.q <- t(apply(q3, 2:3, mean))


# #combining the three data frames into 1
# #step 1:converting from matricies to data frames, then adding a row position ID
# dfq1 <- as.data.frame(data1.q) %>% rowid_to_column("ROW")
# dfq2 <- as.data.frame(data2.q) %>% rowid_to_column("ROW")
# dfq3 <- as.data.frame(data3.q) %>% rowid_to_column("ROW")

# #step 2:bind into one dataframe, but they're still numbered by row position in the original 3 dataframes
# everyq <- bind_rows(dfq1, dfq2, dfq3)
# #head(everyq)
# print("Merged 3 data frames into 1. Dimensions:")
# dim(everyq)

# #step 3:grouping by their row ID, take the mean for each row
# q <- everyq %>% 
#   group_by(ROW) %>% 
#   summarise_all(mean)

# #head(q)
# print("Summarised and took mean. Dimensions:")
# dim(q)


# #step 4: remove row number column and turn back into a matrix
# q <- q[,-1] 
# q <- as.matrix(q)
# #head(q)
# #class(q)
# print("Removed indexing column. Dimensions:")
# dim(q)


# #allQ <- abind(Q1,Q2,Q3, along=1)
# #Q <- t(apply(allQ, 2:3, mean))

# # takes the mean of all 1000 values for the estimate of q, to essentially create a point estimate from a distribution
# data1.Q <- t(apply(Q1, 2:3, mean)) 
# data2.Q <- t(apply(Q2, 2:3, mean))
# data3.Q <- t(apply(Q3, 2:3, mean))


# #combining the three data frames into 1
# #step 1:converting from matricies to data frames, then adding a row position ID
# dfQ1 <- as.data.frame(data1.Q) %>% rowid_to_column("ROW")
# dfQ2 <- as.data.frame(data2.Q) %>% rowid_to_column("ROW")
# dfQ3 <- as.data.frame(data3.Q) %>% rowid_to_column("ROW")

# #step 2:bind into one dataframe, but they're still numbered by row position in the original 3 dataframes
# everyQ <- bind_rows(dfQ1, dfQ2, dfQ3)
# #head(everyQ)
# print("Merged 3 data frames into 1. Dimensions:")
# dim(everyQ)

# #step 3:grouping by their row ID, take the mean for each row
# Q <- everyQ %>% 
#   group_by(ROW) %>% 
#   summarise_all(mean)

# #head(q)
# print("Summarised and took mean. Dimensions:")
# dim(Q)


# #step 4: remove row number column and turn back into a matrix
# Q <- Q[,-1] 
# Q <- as.matrix(Q)
# #head(Q)
# #class(Q)
# print("Removed indexing column. Dimensions:")
# dim(Q)
########################################################################################################################



# checking credible intervals - results in a ci for each each ind for q1 (proportion ancestry in species 1) and q2 (proportion ancestry in species 2), along with Q12
#not sure if this will work now that i've changed things upstream...
qci <- apply(allq, 2:3, quantile, probs=c(0.025,0.975))
Qci <- apply(allQ, 2:3, quantile, probs=c(0.025,0.975))

qciwidth <- qci[2,2,]-qci[1,2,]
Qciwidth <- Qci[2,2,]-Qci[1,2,]

# mean and median of credible intervals across all individuals -- should be small
print("Mean CI width for q and Q:")
mean(qciwidth)
mean(Qciwidth)

print("Median CI width for q and Q:")
median(qciwidth)
median(Qciwidth)


print("Creating triangle plot...")
# plotting Q and q on triangle
# we need column 2 from all.Q as it's the 12/21 ancestry, but i could plot either columns 1 or 2, it'll just show a different species' proportion of ancestry along the X-axis, but they add up to 1 so it doesn't make a difference for interpreting the graph, but could maybe always plot CC or something
pdf(paste0("Q_triangleplot_",nametag,"_k",k,".pdf"))
    plot(q[,2],Q[,2], xlim=c(0,1), ylim=c(0,1), xlab="q (proportion ancestry)", ylab="Q (interspecific ancestry)", type="n", main="Triangle Plot") #FIGURE OUT HOW TO GET SPECIES NAMES HERE
    arrows(0,0,0.5,1, length=0, col="gray") # Triangle marks
    arrows(0.5,1,1,0, length=0, col="gray")
    arrows(qci[2,2,], Q[,2], qci[1,2,], Q[,2], length=0, col="gray45") # CI on q
    arrows(q[,2], Qci[1,2,], q[,2],  Qci[2,2,], length=0, col="gray45") # CI on Q
    points(q[,2],Q[,2], cex=1.5, col="darkblue")
dev.off()


#########

