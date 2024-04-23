## Script to plot q and Q values from HDF5 files against one another as a triangle plot
## Created by Jillian Campbell and/or Liz Mandeville?
## Modified by Amanda Meuser -- March 2023 

## USAGE: Rscript Q_triangleplot.R nametag /path/to/metadata/file /path/to/names/file /path/to/names_file.txt path/to/hdf5/files.hdf5
    ## Rscript ../../../../Q_triangleplot.R AMP22_CSxCC_woP13 ../../old_AMP22_genoID_k12_Jun2023.csv ../names_CSxCC_genoID.txt /project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/CSxCC/entropy/AMP22_CSxCC_genoID_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_50k_rep1_qk2inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/CSxCC/entropy/AMP22_CSxCC_genoID_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_50k_rep2_qk2inds.hdf5 /project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/CSxCC/entropy/AMP22_CSxCC_genoID_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_50k_rep3_qk2inds.hdf5
  
  # for pheno colour, use: Leuciscid_Metadata_May2023.csv
  # for geno colour, use: old_AMP22_genoID_k12_Jun2023.csv (has Geno_ID column)


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
library(stringr)
#library(RColorBrewer)
library(dplyr)
#library(ggplot2)


args <- commandArgs(TRUE)

k <- 2
nametag <- as.character(args[1])
metadata_file <- args[2]
names_file <- args[3]
hdf5path1 <- args[4]
hdf5path2 <- args[5]	
hdf5path3 <- args[6]

print("k:")
k

print("nametag:")
nametag

print("metadata file:")
metadata_file

print("names list:")
names_file

print("Paths to HDF5 files:")
hdf5path1
hdf5path2
hdf5path3

#read in metadata and names list
metadata <- read.csv(metadata_file, header = T)
names <- read.table(names_file, header = F)

# read in files for both q and Q 
q1 <- h5read(hdf5path1, "q")
q2 <- h5read(hdf5path2, "q")
q3 <- h5read(hdf5path3, "q")

Q1 <- h5read(hdf5path1, "Q")
Q2 <- h5read(hdf5path2, "Q")
Q3 <- h5read(hdf5path3, "Q")

# read in files manually 
# q1 <- h5read("/project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/BNDxCC/entropy_woP13/AMP22_BNDxCC_genoID_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_75k_rep1_qk2inds.hdf5", "q")
# q2 <- h5read("/project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/BNDxCC/entropy_woP13/AMP22_BNDxCC_genoID_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_75k_rep2_qk2inds.hdf5", "q")
# q3 <- h5read("/project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/BNDxCC/entropy_woP13/AMP22_BNDxCC_genoID_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_75k_rep3_qk2inds.hdf5", "q")

# Q1 <- h5read("/project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/BNDxCC/entropy_woP13/AMP22_BNDxCC_genoID_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_75k_rep1_qk2inds.hdf5", "Q")
# Q2 <- h5read("/project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/BNDxCC/entropy_woP13/AMP22_BNDxCC_genoID_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_75k_rep2_qk2inds.hdf5", "Q")
# Q3 <- h5read("/project/rrg-emandevi/hybrid_ameuser/old_AMP22/species_pairs/BNDxCC/entropy_woP13/AMP22_BNDxCC_genoID_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_k2_75k_rep3_qk2inds.hdf5", "Q")
# nametag = "AMP22_BNDxCC_woP13"
# metadata <- read.csv("../../old_AMP22_genoID_k12_Jun2023.csv", header = T)
# names <- read.table("../starting_values_genoID/names_BNDxCC_woP13.txt", header = F)


# Looking at columns to check if chain switching occured, but thanks to the starting values, it shouldn't 
#head(q1)
#head(q2)
#head(q3)


# takes the mean of all 1000 values for the estimate of q, to essentially create a point estimate from a distribution
allq <- abind(q1,q2,q3, along=1) 
q <- t(apply(allq, 2:3, mean))

allQ <- abind(Q1,Q2,Q3, along=1)
Q <- t(apply(allQ, 2:3, mean))


# pull out q and Q values for each ind and save as a text file for assigning geno ID (parental, F1, F2, BC1, BC2)
# manually read in species info
#metadata <- read.csv("/project/rrg-emandevi/hybrid_ameuser/old_AMP22/plotting_entropy/old_AMP22_genoID_k12_Jun2023.csv", header=T)
#names <- read.table("../names_CSxCC_genoID.txt", header = F)
metadata_filt <- merge(metadata, names, by.x = "Mandeville_ID", by.y = "V1")
Geno_ID <- metadata_filt$Geno_ID

#manually bind species data 
q_Geno_ID <- as.data.frame(cbind(Geno_ID, q))
for (i in 2:ncol(q_Geno_ID)){
        q_Geno_ID[,i] <- as.numeric(q_Geno_ID[,i])
}

Q_Geno_ID <- as.data.frame(cbind(Geno_ID, Q))
for (i in 2:ncol(Q_Geno_ID)){
        Q_Geno_ID[,i] <- as.numeric(Q_Geno_ID[,i])
}


#create a table of mean q values for each column, for each species
(mean_q_Geno_ID <- q_Geno_ID %>% group_by(Geno_ID) %>% summarise(across(everything(), (list = mean))))
colnames(mean_q_Geno_ID)[2:3] <- c("mean_q_col1", "mean_q_col2")

#save table as a text file
write.table(mean_q_Geno_ID, paste0(nametag,"_entropy_list_k2_Geno_ID_q.txt"), sep = "\t", row.names = F, quote = F)

#add names and save this as a text file
ID <- as.data.frame(metadata_filt$Mandeville_ID)
colnames(ID) <- "Mandeville_ID"
q.sp.names <- cbind(ID, Geno_ID, q[,1], Q[,2])
colnames(q.sp.names)[3:4] <- c("q_col1", "Q_col2")
write.table(q.sp.names, paste0(nametag,"_entropy_list_k2_indivs_q_Q.txt"), sep = "\t", row.names = F, quote = F)



#filter metadata file by list of indivs in the HDF5 output
#metadata_filt <- merge(metadata, names, by.x = "Mandeville_ID", by.y = "V1", all.x = F)

# # remove plate 13 inds
# plate13 <- read.table("../../Plate13_inds.txt", header = T)
# metadata_filt <- anti_join(metadata_filt, plate13)

# colnames(names) <- "Mandeville_ID"

# q <- as.data.frame(q)
# Q <- as.data.frame(Q)

# q <- cbind(q, names)
# q <- anti_join(q, plate13)
# q <- q[-3]
# Q <- cbind(Q, names)
# Q <- anti_join(Q, plate13)
# Q <- Q[-4]
# dim(q)
# dim(Q)

# q <- as.matrix(q)
# Q <- as.matrix(Q)


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

print("Max CI width for q and Q:")
max(qciwidth)
max(Qciwidth)

pdf(paste("histogram_CIs_",nametag,".pdf"), height = 12, width = 6)
par(mfrow = c(2,1))
hist(qciwidth, 
  xlab = "Credible interval width", 
  main = " ",
  col = "steelblue",
  xlim = c(0,0.9),
  breaks = 4)

hist(Qciwidth, 
  xlab = "Credible interval width", 
  main = " ",
  col = "steelblue",
  xlim = c(0,0.9),
  breaks = 8)
mtext("A", side=3, cex=1.5, outer=T, line=-3, adj = 0.04)
mtext("B", side=3, cex=1.5, outer=T, line=-35, adj = 0.04)
dev.off()

#filter metadata file by list of indivs in the HDF5 output
metadata_filt <- merge(metadata, names, by.x = "Mandeville_ID", by.y = "V1", all.x = F)

#initialize both coloums w CC colour as default
metadata_filt$colour_pheno="#A6CEE3"  
metadata_filt$colour_geno="#A6CEE3" 

print("Assigning colours...")

#add colour column
metadata_filt$colour_pheno[metadata_filt$Common_Name=="Common_Shiner"]="#FDBF6F"
metadata_filt$colour_pheno[metadata_filt$Common_Name=="Western_Blacknose_Dace"]="#DF65B0"
metadata_filt$colour_pheno[metadata_filt$Common_Name=="Hornyhead_Chub"]="#35978F"
metadata_filt$colour_pheno[metadata_filt$Common_Name=="River_Chub"]="#B2DF8A"
metadata_filt$colour_pheno[metadata_filt$Common_Name=="Rosyface_Shiner"]="#FB9A99"
metadata_filt$colour_pheno[metadata_filt$Common_Name=="Longnose_Dace"]="#CAB2D6"
metadata_filt$colour_pheno[metadata_filt$Common_Name=="Striped_Shiner"]="#FFFF99"
metadata_filt$colour_pheno[metadata_filt$Common_Name=="Central_Stoneroller"]="#8C510A"

# eric's idea
#[fish_type1, fish_type_2, fish_type_3...]
#[colour_1, colour_2, colour_3...]
#for i in 1:len(fishtypes)
#    metadata_filt[which(fish_type==fish_types[i])]$colour = colours[i]


# colours for geno ID, does same thing as above but w a for loop that i somehow thought would be fewer lines...
#i think this just won't work for the metadata lacking genoID, so everything in colour_geno will be CC colour but I won't be plotting w that anyways
# for (i in 1:length(metadata_filt)){
#     if (metadata_filt$Geno_ID[i]=="Common_Shiner"){
#     metadata_filt$colour_geno == "#FDBF6F"
#     }else if(metadata_filt$Geno_ID[i]=="Western_Blacknose_Dace"){
#         metadata_filt$colour_geno == "#DF65B0"
#     }else if(metadata_filt$Geno_ID[i]=="Hornyhead_Chub"){
#         metadata_filt$colour_geno == "#35978F"
#     }else if(metadata_filt$Geno_ID[i]=="River_Chub"){
#         metadata_filt$colour_geno == "#B2DF8A"
#     }else if(metadata_filt$Geno_ID[i]=="Rosyface_Shiner"){
#         metadata_filt$colour_geno == "#FB9A99"
#     }else if(metadata_filt$Geno_ID[i]=="Longnose_Dace"){
#         metadata_filt$colour_geno == "#CAB2D6"
#     }else if(metadata_filt$Geno_ID[i]=="Striped_Shiner"){
#         metadata_filt$colour_geno == "#FFFF99"
#     }else if(metadata_filt$Geno_ID[i]=="Central_Stoneroller"){
#         metadata_filt$colour_geno == "#8C510A"
#     }else if(metadata_filt$Geno_ID[i]=="V7"){
#         metadata_filt$colour_geno == "#969696"
#     }else if(metadata_filt$Geno_ID[i]=="V11" | metadata_filt$Geno_ID[i]=="V12"){
#         metadata_filt$colour_geno == "#000000"
#     }else{
#         metadata_filt$colour_geno == "#8E0152"
#     }
# }

#rename all hybrids 
for (i in 1:nrow(metadata_filt)){
  if(str_detect(metadata_filt[i,15],'x') == TRUE){
    metadata_filt$Geno_ID[i] = "Hybrid" #hybrid
  } 
  else {
    metadata_filt$Geno_ID[i] = metadata_filt$Geno_ID[i]
  }
}

metadata_filt$colour_geno[metadata_filt$Geno_ID=="Common_Shiner"]="#FDBF6F"
metadata_filt$colour_geno[metadata_filt$Geno_ID=="Western_Blacknose_Dace"]="#DF65B0"
metadata_filt$colour_geno[metadata_filt$Geno_ID=="Hornyhead_Chub"]="#35978F"
metadata_filt$colour_geno[metadata_filt$Geno_ID=="River_Chub"]="#B2DF8A"
metadata_filt$colour_geno[metadata_filt$Geno_ID=="Rosyface_Shiner"]="#FB9A99"
metadata_filt$colour_geno[metadata_filt$Geno_ID=="Longnose_Dace"]="#CAB2D6"
metadata_filt$colour_geno[metadata_filt$Geno_ID=="Striped_Shiner"]="#FFFF99"
metadata_filt$colour_geno[metadata_filt$Geno_ID=="Central_Stoneroller"]="#8C510A"
metadata_filt$colour_geno[metadata_filt$Geno_ID=="Hybrid"]="#8E0152"


print("Creating triangle plot...")
# plotting Q and q on triangle
# we need column 2 from all.Q as it's the 12/21 ancestry, but i could plot either columns 1 or 2, it'll just show a different species' proportion of ancestry along the X-axis, but they add up to 1 so it doesn't make a difference for interpreting the graph, but could maybe always plot CC or something
pdf(paste0("Q_triangleplot_",nametag,"_k2.pdf"))
    plot(q[,2],Q[,2], xlim=c(0,1), ylim=c(0,1), axes = F, xlab="Admixture Proportion (q)", ylab="Inter-source Ancestry (Q)", type="n", main=" ") 
    axis(1, at = seq(0, 1, by = 0.25))
    axis(2, at = seq(0, 1, by = 0.25))
    box()
    arrows(0,0,0.5,1, length=0, col="gray") # Triangle marks
    arrows(0.5,1,1,0, length=0, col="gray")
    arrows(qci[2,2,], Q[,2], qci[1,2,], Q[,2], length=0, col="gray45") # CI on q
    arrows(q[,2], Qci[1,2,], q[,2],  Qci[2,2,], length=0, col="gray45") # CI on Q
    points(q[,2],Q[,2], cex=1.5, col=metadata_filt$colour_geno, pch = 18)
    legend("topright", legend = unique(metadata_filt$Geno_ID), fill = unique(metadata_filt$colour_geno), cex = 0.75) #pheno: legend(metadata_filt$Common_Name) and fill(metadata_filt$colour_pheno), geno: legend(metadata_filt$Geno_ID) and fill(metadata_filt$colour_geno)
dev.off()


#########

