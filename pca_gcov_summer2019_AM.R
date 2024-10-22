
## Rscript for plotting PCAs from point estimates
# Written originally by Liz Mandeville 2019, Modified by Amanda Meuser 2023

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("rhdf5")

# load packages
library(rhdf5)
library(tidyverse)

# import point estimates file
g <- read.table("pntest_mean_gl_MAF_005.txt", header=F) 

# import .imiss file
#imiss <- read.table("missingness_guelph_bruce.imiss", header=T) 

# import names list
names <- read.table("inds_refilter27jun24.txt", header=F) 
colnames(names) <- "FishID"


# import metadata
details <- read.csv("Leuciscid_Metadata_May2023.csv", header=T) 

# Modifying Charlotte Ward's fish to all say Costello Creek as location
details$Waterbody[details$Waterbody_Code == "OP2"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "OP7"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "Weir DS"] <- "Costello_Creek"

# get list of and drop 84 inds from plate 13 inds
# plate13inds <- filter(details, Plate == "AMP22_LP13")
# plate13inds <- plate13inds[,4]
# plate13inds <- as.data.frame(plate13inds)
# names(plate13inds) <- "Mandeville_ID"
# details <- anti_join(details, plate13inds)

# check for indivs missing species IS
fishinfo_raw <- merge(names, details, by.x="FishID", by.y="Mandeville_ID", all.x=F, all.y=F)
n_data_present <- sum(fishinfo_raw$Species != "NA", na.rm=T)
total_data <- nrow(fishinfo_raw)
percent_data_missing = 1-(n_data_present/total_data)

# replace blank species datat w Ns
fishinfo <- fishinfo_raw
fishinfo$Species <- replace_na(fishinfo$Species, "N")


nind <- dim(g)[2]
nloci <- dim(g)[1]

## calculate N x N genotype covariance matrix
gmn<-apply(g,1,mean, na.rm=T)
gmnmat<-matrix(gmn,nrow=nloci,ncol=nind)
gprime<-g-gmnmat ## remove mean
gcovarmat<-matrix(NA,nrow=nind,ncol=nind)
for(i in 1:nind){
    for(j in i:nind){
        if (i==j){
            gcovarmat[i,j]<-cov(gprime[,i],gprime[,j], use="pairwise.complete.obs")
        }
        else{
            gcovarmat[i,j]<-cov(gprime[,i],gprime[,j], use="pairwise.complete.obs")
            gcovarmat[j,i]<-gcovarmat[i,j]
        }
    }
}



## pca on the genotype covariance matrix
#pcgcov<-prcomp(x=gcovarmatB,center=TRUE,scale=FALSE)
pcgcov<-prcomp(x=gcovarmat,center=TRUE,scale=FALSE)
imp <- summary(pcgcov)


#pc.summary <- data.frame(names, fishinfo$Species, round(pcgcov$x[,1:5], digits=5)) #rounding digits on pcgcov object
#write.csv(pc.summary, file="pc_1to5_2aug17_1286ind_12666loci.csv", row.names=F, col.names=F, quote=F)

# colors <-c(
# "#74336E", #purple
# "#5EAB3E", #green
# "#D05433", #red
# "#6F9AB7",
# "#46542F",
# "#C256D1",
# "#BC897E",
# "#B3943C",
# "#6974C9",
# "#D04576",
# "#6B3128",
# "#6BA684")
# morecolors <- rep(colors,3)


# parentals2 <- c("#DF65B0", #BND
#                 "#969696", #Pimephales
#                 "#A6CEE3", #CC
#                 "#FDBF6F", #CS
#                 "#FB9A99", #RFS
#                 "#B2DF8A", #RC
#                 "#35978F", #HHC
#                 "#8C510A", #CSR
#                 "#969696", #Pimephales
#                 "#E5DF60", #SS
#                 "#CAB2D6", #LND
#                 "#000000", #v11,v12
#                 "#8E0152") #hybrid


symbols <- rep(1:14,3)
parentals2 <- c(
                "#DF65B0", #BND
                "#969696", #FHM
                "#A6CEE3", #CC
                "#FDBF6F", #CS
                "#FB9A99", #RFS
                "#B2DF8A", #RC
                "#35978F", #HHC
                "#8C510A", #CSR
                "#000000", #BM
                "#E5DF60", #SS
                "#CAB2D6", #LND
                #"#000000", #v11,v12
                "#8E0152") #hybrid

#----------------------------------------------------------------------------
#plot PC1 and PC2 for species

# title for all inds
#pdf("PCA_AMP22_Pimephales_target_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001_PHENO_ID.pdf", width = 12, height = 18)


pdf("PCA_AMP22_Pimephales_target_27jun24_miss0.6_mac3_Q30_DP3_ind95_maf_005_PHENO_ID.pdf", width = 12, height = 18)

par(mfrow=c(3,2))
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),1], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 2], pch=symbols[i], col=parentals2[i])
}

legend("bottomright", legend=unique(fishinfo$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=1)


#dev.off()

#----------------------------------------------------------------------------
#plot PC2 and PC3 for species

plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),2], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 3], pch=symbols[i], col=parentals2[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=1)


#dev.off()

#----------------------------------------------------------------------------
#plot PC3 and PC4 for species
#pdf("PCA3,4_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs_final.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),3], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 4], pch=symbols[i], col=parentals2[i])
}

legend("topleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC4 and PC5 for species

plot(pcgcov$x[,4], pcgcov$x[,5], type="n", xlab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""), ylab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),4], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 5], pch=symbols[i], col=parentals2[i])
}

legend("topleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC5 and PC6 for species

plot(pcgcov$x[,5], pcgcov$x[,6], type="n", xlab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""), ylab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),5], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),6], pch=symbols[i], col=parentals2[i])
}

legend("topright", legend=unique(fishinfo$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC6 and PC7 for species

plot(pcgcov$x[,6], pcgcov$x[,7], type="n", xlab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""), ylab=paste("PC7 (",(imp$importance[,7][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),6], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 7], pch=symbols[i], col=parentals2[i])
}

legend("topleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=1)

dev.off()

#----------------------------------------------------------------------------
#plot PC7 and PC8 for species

#plot(pcgcov$x[,7], pcgcov$x[,8], type="n", xlab=paste("PC7 (",(imp$importance[,7][[2]]*100), "% )", sep=""), ylab=paste("PC8 (",(imp$importance[,8][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
#for(i in 1:(length(unique(fishinfo$Common_Name)))){
#points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),7], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),8], pch=symbols[i], col=parentals2[i])
#}

#legend("topright", legend=unique(fishinfo$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=1)

#dev.off()


#====================================================================
# #plot PC1 and PC2 for location
# pdf("PCA_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf", width = 5, height = 15)
# par(mfrow=c(3,1))
# plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""))
# 
# #loop iterates over locations and adds a different colour for each one
# for(i in 1:(length(unique(fishinfo$Waterbody)))){
#   points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),1], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 2], pch=symbols[i], col=morecolors[i])
# }
# 
# legend("bottomleft", legend=unique(fishinfo$Waterbody), pch=symbols, col=morecolors, ncol=1, cex=0.5)
# 
# 
# #dev.off()
# 
# #----------------------------------------------------------------------------
# #plot PC2 and PC3 for location
# #pdf("PCA2,3_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf")
# #par(mfrow=c(1,2))
# plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""))
# 
# #loop iterates over locations and adds a different colour for each one
# for(i in 1:(length(unique(fishinfo$Waterbody)))){
#   points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),2], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 3], pch=symbols[i], col=morecolors[i])
# }
# 
# legend("topleft", legend=unique(fishinfo$Waterbody), pch=symbols, col=morecolors, ncol=1, cex=0.5)
# 
# 
# #dev.off()
# 
# #----------------------------------------------------------------------------
# #plot PC3 and PC4 for location
# #pdf("PCA3,4_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf")
# #par(mfrow=c(1,2))
# plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""))
# 
# #loop iterates over locations and adds a different colour for each one
# for(i in 1:(length(unique(fishinfo$Waterbody)))){
#   points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),3], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 4], pch=symbols[i], col=morecolors[i])
# }
# 
# legend("bottomleft", legend=unique(fishinfo$Waterbody), pch=symbols, col=morecolors, ncol=1, cex=0.5)
# 
# 
# dev.off()

#----------------------------------------------------------------------------


# simple version (no colours/shapes)
plot(pcgcov$x[,1], pcgcov$x[,2])

#----------------------------------------------------------------------------


# missingness vs PC1

# merge metadata with missingness file
missing_fish <- merge(imiss, fishinfo, by.x="INDV", by.y="FishID", all.x=F, all.y=T)


pdf("F_MISSxPC_AMP22_guelph_bruce_11jul23_miss0.5_mac3_Q30_DP3_maf001_ind95_maf001.pdf", width = 8, height = 16)
par(mfrow=c(2,1))
plot(pcgcov$x[,1], missing_fish$F_MISS, type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab="Proportion of missing data")

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(missing_fish$Common_Name)))){
  points(pcgcov$x[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i]),1], missing_fish$F_MISS[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i])], pch=symbols[i], col=parentals2[i])
}

legend("topright", legend=unique(missing_fish$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=0.75)


#dev.off()

#----------------------------------------------------------------------------

# missingness vs PC2

#pdf("F_MISSxPC2_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.pdf")

plot(pcgcov$x[,2], missing_fish$F_MISS, type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab="Proportion of missing data")

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(missing_fish$Common_Name)))){
  points(pcgcov$x[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i]),2], missing_fish$F_MISS[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i])], pch=symbols[i], col=parentals2[i])
}

legend("topleft", legend=unique(missing_fish$Common_Name), pch=symbols, col=parentals2, ncol=1, cex=0.75)


dev.off()

############################################################################################################################################################################################################################################################################################################################################################################################


# species pairs


# import metadata
details <- read.csv("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/Leuciscid_Metadata_May2023.csv", header=T) 

# Modifying Charlotte Ward's fish to all say Costello Creek as location
details$Waterbody[details$Waterbody_Code == "OP2"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "OP7"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "Weir DS"] <- "Costello_Creek"


# import names lists for species pairs
names_pair <- read.table("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/names_files/names_BNDxCSxCC_filtered.txt", header = F)

# import pnt estimate file
g_pair<-read.table("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/PCA/pntest_files/pntest_mean_gl_BNDxCSxCC.txt", header=F)

# change header
names(names_pair) <- "inds"


# merge inds w metadata 
fishinfo_blah <- merge(names_pair, details, by.x="inds", by.y="Mandeville_ID", all.x=T, all.y=F)
# order inds (JUST FOR SSxCC)
fishinfo_pair <- fishinfo_blah[order(fishinfo_blah$Common_Name),]


nind <- dim(g_pair)[2]
nloci <- dim(g_pair)[1]

## calculate N x N genotype covariance matrix
gmn<-apply(g_pair,1,mean, na.rm=T)
gmnmat<-matrix(gmn,nrow=nloci,ncol=nind)
gprime<-g_pair-gmnmat ## remove mean
gcovarmat<-matrix(NA,nrow=nind,ncol=nind)
for(i in 1:nind){
  for(j in i:nind){
    if (i==j){
      gcovarmat[i,j]<-cov(gprime[,i],gprime[,j], use="pairwise.complete.obs")
    }
    else{
      gcovarmat[i,j]<-cov(gprime[,i],gprime[,j], use="pairwise.complete.obs")
      gcovarmat[j,i]<-gcovarmat[i,j]
    }
  }
}

pcgcov_pair<-prcomp(x=gcovarmat,center=TRUE,scale=FALSE)
imp_pair <- summary(pcgcov_pair)

colors <-c(
         #"#35978F", #HHC
         #"#B2DF8A", #RC
         #"#FB9A99", #RFS
         "#FDBF6F", #CS
         "#A6CEE3", #CC
         #"#CAB2D6", #LND
         #"#FFFF99", #SS
         #"#8C510A", #CSR
         "#DF65B0") #BND
         #"#000000", #v11,v12
         #"#969696" ) #v7

symbols <- rep(1:14,3)

fishinfo_pair <- fishinfo_pair[-667,]

jpeg("PCA_AMP22_BNDxCSxCC_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.jpeg")
plot(pcgcov_pair$x[,1], pcgcov_pair$x[,2], type="n", xlab=paste("PC1 (",(imp_pair$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp_pair$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),1], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 2], pch=symbols[i], col=colors[i])
}

legend("bottomleft", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=colors, ncol=1, cex=1)
dev.off()

#plot PC1 and PC2 for species
pdf("PCA_AMP22_BNDxCSxCC_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.pdf", width = 8, height = 8)

plot(pcgcov_pair$x[,1], pcgcov_pair$x[,2], type="n", xlab=paste("PC1 (",(imp_pair$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp_pair$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),1], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 2], pch=symbols[i], col=colors[i])
}

legend("bottomleft", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=colors, ncol=1, cex=1)


#plot PC2 and PC3 for species
plot(pcgcov_pair$x[,2], pcgcov_pair$x[,3], type="n", xlab=paste("PC2 (",(imp_pair$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp_pair$importance[,3][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),2], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 3], pch=symbols[i], col=colors[i])
}

legend("topleft", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=colors, ncol=1, cex=1)


#plot PC3 and PC4 for species
plot(pcgcov_pair$x[,3], pcgcov_pair$x[,4], type="n", xlab=paste("PC3 (",(imp_pair$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp_pair$importance[,4][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),3], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 4], pch=symbols[i], col=colors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=colors, ncol=1, cex=1)


#plot PC4 and PC5 for species
plot(pcgcov_pair$x[,4], pcgcov_pair$x[,5], type="n", xlab=paste("PC4 (",(imp_pair$importance[,4][[2]]*100), "% )", sep=""), ylab=paste("PC5 (",(imp_pair$importance[,5][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),4], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 5], pch=symbols[i], col=colors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=colors, ncol=1, cex=1)


#plot PC5 and PC6 for species
plot(pcgcov_pair$x[,5], pcgcov_pair$x[,6], type="n", xlab=paste("PC5 (",(imp_pair$importance[,5][[2]]*100), "% )", sep=""), ylab=paste("PC6 (",(imp_pair$importance[,6][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),5], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),6], pch=symbols[i], col=colors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=colors, ncol=1, cex=1)


#plot PC6 and PC7 for species
plot(pcgcov_pair$x[,6], pcgcov_pair$x[,7], type="n", xlab=paste("PC6 (",(imp_pair$importance[,6][[2]]*100), "% )", sep=""), ylab=paste("PC7 (",(imp_pair$importance[,7][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),6], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),7], pch=symbols[i], col=colors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=colors, ncol=1, cex=1)


#plot PC7 and PC8 for species
plot(pcgcov_pair$x[,7], pcgcov_pair$x[,8], type="n", xlab=paste("PC7 (",(imp_pair$importance[,7][[2]]*100), "% )", sep=""), ylab=paste("PC8 (",(imp_pair$importance[,8][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),7], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),8], pch=symbols[i], col=colors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=colors, ncol=1, cex=1)

dev.off()



############################################################################################################################################################################################################################################################################################################################################################################################

# BRING IN GENOMIC ID'S

############################################################################################################################################################################################################################################################################################################################################################################################



# import genomic IDs
dfGeno_ID <- read.csv("../entropy_target_old/old_AMP22_genoID_k12_Jun2023.csv")

# create simplified column and extract
dfGeno_ID$Geno_ID_simple = NA

for (i in 1:nrow(dfGeno_ID)){
  if(str_detect(dfGeno_ID$Geno_ID[i],'x') == TRUE){
    dfGeno_ID$Geno_ID_simple[i] <- "Hybrid" 
  }
  else {
    dfGeno_ID$Geno_ID_simple[i] <- dfGeno_ID$Geno_ID[i]
  }
}

ids <- as.data.frame(dfGeno_ID$Mandeville_ID)
genos <- as.data.frame(dfGeno_ID$Geno_ID_simple)
extracted <- cbind(ids,genos)
colnames(extracted) <- c("Mandeville_ID", "Geno_ID_simple")

#merge w details
details_Geno_ID <- merge(details, extracted, by = "Mandeville_ID")



# check for indivs missing species IS
fishinfo_raw <- merge(names, details_Geno_ID, by.x="FishID", by.y="Mandeville_ID", all.x=F, all.y=F)
n_data_present <- sum(fishinfo_raw$Species != "NA", na.rm=T)
total_data <- nrow(fishinfo_raw)
percent_data_missing = 1-(n_data_present/total_data)

# replace blank species datat w Ns
fishinfo <- fishinfo_raw
fishinfo$Species <- replace_na(fishinfo$Species, "N")

fishinfo$Geno_ID_simple[fishinfo$Geno_ID_simple == "V7"] <- "Pimephales_sp"


nind <- dim(g)[2]
nloci <- dim(g)[1]

## calculate N x N genotype covariance matrix
gmn<-apply(g,1,mean, na.rm=T)
gmnmat<-matrix(gmn,nrow=nloci,ncol=nind)
gprime<-g-gmnmat ## remove mean
gcovarmat<-matrix(NA,nrow=nind,ncol=nind)
for(i in 1:nind){
  for(j in i:nind){
    if (i==j){
      gcovarmat[i,j]<-cov(gprime[,i],gprime[,j], use="pairwise.complete.obs")
    }
    else{
      gcovarmat[i,j]<-cov(gprime[,i],gprime[,j], use="pairwise.complete.obs")
      gcovarmat[j,i]<-gcovarmat[i,j]
    }
  }
}


pcgcov<-prcomp(x=gcovarmat,center=TRUE,scale=FALSE)
imp <- summary(pcgcov)


symbols <- rep(1:14,3)

parentals2 <- c("#8E0152", #hybrid
                "#DF65B0", #BND
                "#CAB2D6", #LND
                "#A6CEE3", #CC
                "#35978F", #HHC
                "#FDBF6F", #CS
                "#FB9A99", #RFS
                "#B2DF8A", #RC
                "#8C510A", #CSR
                "#969696", #Pimeph
                "#E5DF60") #SS
                



#plot PC1 and PC2 for species
pdf("PCA_AMP22_target_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_GENO_ID.pdf", width = 12, height = 18)

par(mfrow=c(3,2))
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),1], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 2], pch=symbols[i], col=parentals2[i])
}

legend("bottomleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols, col=parentals2, ncol=1, cex=1)


#dev.off()

#----------------------------------------------------------------------------
#plot PC2 and PC3 for species

plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),2], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 3], pch=symbols[i], col=parentals2[i])
}

legend("topleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols, col=parentals2, ncol=1, cex=1)


#dev.off()

#----------------------------------------------------------------------------
#plot PC3 and PC4 for species

plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),3], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 4], pch=symbols[i], col=parentals2[i])
}

legend("bottomleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols, col=parentals2, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC4 and PC5 for species

plot(pcgcov$x[,4], pcgcov$x[,5], type="n", xlab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""), ylab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),4], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 5], pch=symbols[i], col=parentals2[i])
}

legend("topleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols, col=parentals2, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC5 and PC6 for species

plot(pcgcov$x[,5], pcgcov$x[,6], type="n", xlab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""), ylab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),5], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),6], pch=symbols[i], col=parentals2[i])
}

legend("topleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols, col=parentals2, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC6 and PC7 for species

plot(pcgcov$x[,6], pcgcov$x[,7], type="n", xlab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""), ylab=paste("PC7 (",(imp$importance[,7][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),6], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 7], pch=symbols[i], col=parentals2[i])
}

legend("bottomleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols, col=parentals2, ncol=1, cex=1)

dev.off()





############################################################################################
############################ MISSINGNESS VS PCA ############################################
############################################################################################

# missingness vs PC1

names(plate13inds) <- "INDV"
imiss <- anti_join(imiss, plate13inds)

# merge metadata with missingness file
missing_fish <- merge(imiss, fishinfo, by.x="INDV", by.y="FishID", all.x=F, all.y=T)

parentals2 <- c("#8E0152", #hybrid
                "#DF65B0", #BND
                "#CAB2D6", #LND
                "#A6CEE3", #CC
                "#35978F", #HHC
                "#FDBF6F", #CS
                "#FB9A99", #RFS
                "#B2DF8A", #RC
                "#8C510A", #CSR
                "#969696", #Pimeph
                "#E5DF60") #SS

symbols_geno <- as.integer(c(8,1,9,2,4,3,5,6,7,10))


pdf("F_MISSxPC_AMP22_target_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_GENO_ID.pdf", width = 8, height = 16)
par(mfrow=c(2,1))
plot(pcgcov$x[,1], missing_fish$F_MISS, type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab="Proportion of missing data")

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(missing_fish$Geno_ID_simple)))){
  points(pcgcov$x[which(missing_fish$Geno_ID_simple==unique(missing_fish$Geno_ID_simple)[i]),1], missing_fish$F_MISS[which(missing_fish$Geno_ID_simple==unique(missing_fish$Geno_ID_simple)[i])], pch=symbols_geno[i], col=parentals2[i])
}

legend("topleft", legend=unique(missing_fish$Geno_ID_simple), pch=symbols_geno, col=parentals2, ncol=1, cex=0.75)
mtext("A", side=3, cex=1.5, outer=T, line=-4, adj = 0.05)

#dev.off()

#----------------------------------------------------------------------------

# missingness vs PC2

#pdf("F_MISSxPC2_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.pdf")

plot(pcgcov$x[,2], missing_fish$F_MISS, type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab="Proportion of missing data")

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(missing_fish$Geno_ID_simple)))){
  points(pcgcov$x[which(missing_fish$Geno_ID_simple==unique(missing_fish$Geno_ID_simple)[i]),2], missing_fish$F_MISS[which(missing_fish$Geno_ID_simple==unique(missing_fish$Geno_ID_simple)[i])], pch=symbols_geno[i], col=parentals2[i])
}

legend("topleft", legend=unique(missing_fish$Geno_ID_simple), pch=symbols_geno, col=parentals2, ncol=1, cex=0.75)
mtext("B", side=3, cex=1.5, outer=T, line=-44, adj = 0.05)

dev.off()


parentals_pheno <- c("#DF65B0", #BND
                "#A6CEE3", #CC
                "#FDBF6F", #CS
                "#35978F", #HHC
                "#FB9A99", #RFS
                "#B2DF8A", #RC
                "#8C510A", #CSR
                "#E5DF60", #SS
                "#CAB2D6") #LND

parentals_geno <- c("#8E0152", #hybrid
                "#DF65B0", #BND
                "#CAB2D6", #LND
                "#A6CEE3", #CC
                "#35978F", #HHC
                "#FDBF6F", #CS
                "#FB9A99", #RFS
                "#B2DF8A", #RC
                "#8C510A", #CSR
                "#969696", #Pimeph
                "#E5DF60") #SS

symbols_pheno <- as.integer(c(1,2,3,4,5,6,7,12,9))
symbols_geno <- as.integer(c(8,1,9,2,4,3,5,6,7,10))

####################################################################################################################################################################################################################################################################################################################################################################################
###############                             MERGING GENO AND PHENO                                           ###############
###############                                 PLOTS INTO ONE                                               ###############
####################################################################################################################################################################################################################################################################################################################################################################################

#plot PC1 and PC2 for species PHENO
pdf("PCA_AMP22_target_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_PHENO_AND_GENO_ID_1_2_TO_3_4.pdf", width = 12, height = 18)

par(mfrow=c(3,2))
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),1], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 2], pch=symbols_pheno[i], col=parentals_pheno[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols_pheno, col=parentals_pheno, ncol=1, cex=1)
mtext("A", side=3, cex=1.5, outer=T, line=-4, adj = 0.02)

#dev.off()

#----------------------------------------------------------------------------
#plot PC1 and PC2 for species GENO
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),1], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 2], pch=symbols_geno[i], col=parentals_geno[i])
}

legend("bottomleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols_geno, col=parentals_geno, ncol=1, cex=1)
mtext("B", side=3, cex=1.5, outer=T, line=-4, adj = 0.52)

#dev.off()

#----------------------------------------------------------------------------
#plot PC2 and PC3 for species PHENO

plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),2], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 3], pch=symbols_pheno[i], col=parentals_pheno[i])
}

legend("topleft", legend=unique(fishinfo$Common_Name), pch=symbols_pheno, col=parentals_pheno, ncol=1, cex=1)
mtext("C", side=3, cex=1.5, outer=T, line=-49, adj = 0.02)

#dev.off()

#----------------------------------------------------------------------------
#plot PC2 and PC3 for species GENO

plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),2], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 3], pch=symbols_geno[i], col=parentals_geno[i])
}

legend("topleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols_geno, col=parentals_geno, ncol=1, cex=1)
mtext("D", side=3, cex=1.5, outer=T, line=-49, adj = 0.52)

#dev.off()

#----------------------------------------------------------------------------
#plot PC3 and PC4 for species PHENO

plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),3], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 4], pch=symbols_pheno[i], col=parentals_pheno[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols_pheno, col=parentals_pheno, ncol=1, cex=1)
mtext("E", side=3, cex=1.5, outer=T, line=-95, adj = 0.02)


#----------------------------------------------------------------------------
#plot PC3 and PC4 for species GENO

plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),3], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 4], pch=symbols_geno[i], col=parentals_geno[i])
}

legend("bottomleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols_geno, col=parentals_geno, ncol=1, cex=1)
mtext("F", side=3, cex=1.5, outer=T, line=-95, adj = 0.52)


dev.off()




#----------------------------------------------------------------------------
#plot PC4 and PC5 for species PHENO
pdf("PCA_AMP22_target_woP13_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_PHENO_AND_GENO_ID_4_5_TO_6_7.pdf", width = 12, height = 18)
par(mfrow=c(3,2), cex.lab = 1.5)


plot(pcgcov$x[,4], pcgcov$x[,5], type="n", xlab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""), ylab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),4], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 5], pch=symbols_pheno[i], col=parentals_pheno[i])
}

legend("topleft", legend=unique(fishinfo$Common_Name), pch=symbols_pheno, col=parentals_pheno, ncol=1, cex=1)
mtext("G", side=3, cex=1.5, outer=T, line=-4, adj = 0.02)

#----------------------------------------------------------------------------
#plot PC4 and PC5 for species GENO

plot(pcgcov$x[,4], pcgcov$x[,5], type="n", xlab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""), ylab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),4], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 5], pch=symbols_geno[i], col=parentals_geno[i])
}

legend("topleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols_geno, col=parentals_geno, ncol=1, cex=1)
mtext("H", side=3, cex=1.5, outer=T, line=-4, adj = 0.52)

#----------------------------------------------------------------------------
#plot PC5 and PC6 for species PHENO

plot(pcgcov$x[,5], pcgcov$x[,6], type="n", xlab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""), ylab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),5], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),6], pch=symbols_pheno[i], col=parentals_pheno[i])
}

legend("topleft", legend=unique(fishinfo$Common_Name), pch=symbols_pheno, col=parentals_pheno, ncol=1, cex=1)
mtext("I", side=3, cex=1.5, outer=T, line=-49, adj = 0.02)

#----------------------------------------------------------------------------
#plot PC5 and PC6 for species GENO

plot(pcgcov$x[,5], pcgcov$x[,6], type="n", xlab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""), ylab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),5], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),6], pch=symbols_geno[i], col=parentals_geno[i])
}

legend("topleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols_geno, col=parentals_geno, ncol=1, cex=1)
mtext("J", side=3, cex=1.5, outer=T, line=-49, adj = 0.52)

#----------------------------------------------------------------------------
#plot PC6 and PC7 for species PHENO

plot(pcgcov$x[,6], pcgcov$x[,7], type="n", xlab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""), ylab=paste("PC7 (",(imp$importance[,7][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),6], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 7], pch=symbols_pheno[i], col=parentals_pheno[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols_pheno, col=parentals_pheno, ncol=1, cex=1)
mtext("K", side=3, cex=1.5, outer=T, line=-95, adj = 0.02)

#----------------------------------------------------------------------------
#plot PC6 and PC7 for species GENO

plot(pcgcov$x[,6], pcgcov$x[,7], type="n", xlab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""), ylab=paste("PC7 (",(imp$importance[,7][[2]]*100), "% )", sep=""), cex.lab = 1.5)

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Geno_ID_simple)))){
  points(pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]),6], pcgcov$x[which(fishinfo$Geno_ID_simple==unique(fishinfo$Geno_ID_simple)[i]), 7], pch=symbols_geno[i], col=parentals_geno[i])
}

legend("bottomleft", legend=unique(fishinfo$Geno_ID_simple), pch=symbols_geno, col=parentals_geno, ncol=1, cex=1)
mtext("L", side=3, cex=1.5, outer=T, line=-95, adj = 0.52)

dev.off()


