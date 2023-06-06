
## Rscript for plotting PCAs from point estimates
# Written originally by Liz Mandeville 2019, Modified by Amanda Meuser 2023

# load packages
library(rhdf5)
library(tidyverse)

# import point estimates file
g <- read.table("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/PCA/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_pntest_mean_gl.txt", header=F) 

# import .imiss file
imiss <- read.table("out_03may23_0.6_tidy.imiss", header=T) 

# import names list
names <- read.table("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/PCA/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs_final.txt", header=F) 
colnames(names) <- "FishID"


# import metadata
details <- read.csv("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/Leuciscid_Metadata_May2023.csv", header=T) 

# Modifying Charlotte Ward's fish to all say Costello Creek as location
details$Waterbody[details$Waterbody_Code == "OP2"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "OP7"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "Weir DS"] <- "Costello_Creek"

# check for indivs missing species IS
fishinfo_raw <- merge(names, details, by.x="FishID", by.y="Mandeville_ID", all.x=T, all.y=F)
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

colors <-c(
"#74336E", #purple
"#5EAB3E", #green
"#D05433", #red
"#6F9AB7",
"#46542F",
"#C256D1",
"#BC897E",
"#B3943C",
"#6974C9",
"#D04576",
"#6B3128",
"#6BA684")


morecolors <- rep(colors,3)
symbols <- rep(1:14,3)

#----------------------------------------------------------------------------
#plot PC1 and PC2 for species
pdf("PCA_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.pdf", width = 5, height = 35)

par(mfrow=c(7,1))
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),1], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#dev.off()

#----------------------------------------------------------------------------
#plot PC2 and PC3 for species

plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),2], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 3], pch=symbols[i], col=morecolors[i])
}

legend("topleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#dev.off()

#----------------------------------------------------------------------------
#plot PC3 and PC4 for species
#pdf("PCA3,4_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs_final.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),3], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 4], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC4 and PC5 for species

plot(pcgcov$x[,4], pcgcov$x[,5], type="n", xlab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""), ylab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),4], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 5], pch=symbols[i], col=morecolors[i])
}

legend("topleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC5 and PC6 for species

plot(pcgcov$x[,5], pcgcov$x[,6], type="n", xlab=paste("PC5 (",(imp$importance[,5][[2]]*100), "% )", sep=""), ylab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),5], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),6], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC6 and PC7 for species

plot(pcgcov$x[,6], pcgcov$x[,7], type="n", xlab=paste("PC6 (",(imp$importance[,6][[2]]*100), "% )", sep=""), ylab=paste("PC7 (",(imp$importance[,7][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),6], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 7], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)

#----------------------------------------------------------------------------
#plot PC7 and PC8 for species

plot(pcgcov$x[,7], pcgcov$x[,8], type="n", xlab=paste("PC7 (",(imp$importance[,7][[2]]*100), "% )", sep=""), ylab=paste("PC8 (",(imp$importance[,8][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),7], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),8], pch=symbols[i], col=morecolors[i])
}

legend("topright", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)

dev.off()


#====================================================================
#plot PC1 and PC2 for location
pdf("PCA_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf", width = 5, height = 15)
par(mfrow=c(3,1))
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Waterbody)))){
  points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),1], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Waterbody), pch=symbols, col=morecolors, ncol=1, cex=0.5)


#dev.off()

#----------------------------------------------------------------------------
#plot PC2 and PC3 for location
#pdf("PCA2,3_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Waterbody)))){
  points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),2], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 3], pch=symbols[i], col=morecolors[i])
}

legend("topleft", legend=unique(fishinfo$Waterbody), pch=symbols, col=morecolors, ncol=1, cex=0.5)


#dev.off()

#----------------------------------------------------------------------------
#plot PC3 and PC4 for location
#pdf("PCA3,4_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Waterbody)))){
  points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),3], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 4], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Waterbody), pch=symbols, col=morecolors, ncol=1, cex=0.5)


dev.off()

#----------------------------------------------------------------------------


# simple version (no colours/shapes)
plot(pcgcov$x[,1], pcgcov$x[,2])

#----------------------------------------------------------------------------

# missingness vs PC1
# merge metadata with missingness file
missing_fish <- merge(imiss, fishinfo, by.x="INDV", by.y="FishID", all.x=F, all.y=T)


pdf("F_MISSxPC_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.pdf", width = 12)
par(mfrow=c(1,2))
plot(pcgcov$x[,1], missing_fish$F_MISS, type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab="Proportion of missing data")

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(missing_fish$Common_Name)))){
  points(pcgcov$x[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i]),1], missing_fish$F_MISS[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i])], pch=symbols[i], col=morecolors[i])
}

legend("top", legend=unique(missing_fish$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#dev.off()

#----------------------------------------------------------------------------

# missingness vs PC2

#pdf("F_MISSxPC2_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.pdf")

plot(pcgcov$x[,2], missing_fish$F_MISS, type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab="Proportion of missing data")

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(missing_fish$Common_Name)))){
  points(pcgcov$x[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i]),2], missing_fish$F_MISS[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i])], pch=symbols[i], col=morecolors[i])
}

legend("topleft", legend=unique(missing_fish$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


dev.off()

############################################################################################################################################################################################################################################################################################################################################################################################

#stupid for loop not working
# list.dfs <- list(names_BNDxCC,names_CSRxCC,names_CSxCC,names_HHCxCC,names_LNDxCC,names_RCxCC,names_RFSxCC,names_SSxCC)
# 
# for (a in 1:length(list.dfs)){
#   fishinfo_(paste(a)) <- merge(a, details, by.x="inds", by.y="Mandeville_ID", all.x=T, all.y  =F)
# }

# species pairs


# import metadata
details <- read.csv("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/Leuciscid_Metadata_May2023.csv", header=T) 

# Modifying Charlotte Ward's fish to all say Costello Creek as location
details$Waterbody[details$Waterbody_Code == "OP2"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "OP7"] <- "Costello_Creek"
details$Waterbody[details$Waterbody_Code == "Weir DS"] <- "Costello_Creek"



# import names lists for species pairs
names_pair <- read.table("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/names_files/names_SSxCC_filtered.txt", header = F)

# import pnt estimate file
g_pair<-read.table("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/PCA/pntest_files/pntest_mean_gl_SSxCC_thin.txt", header=F)

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
  "#74336E", #purple
  "#5EAB3E", #green
  "#D05433", #red
  "#6F9AB7",
  "#46542F",
  "#C256D1",
  "#BC897E",
  "#B3943C",
  "#6974C9",
  "#D04576",
  "#6B3128",
  "#6BA684")


morecolors <- rep(colors,3)
symbols <- rep(1:14,3)



#plot PC1 and PC2 for species
pdf("PCA_AMP22_SSxCC_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_thin100_indivs.pdf", width = 8, height = 8)

plot(pcgcov_pair$x[,1], pcgcov_pair$x[,2], type="n", xlab=paste("PC1 (",(imp_pair$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp_pair$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),1], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("bottomright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#plot PC2 and PC3 for species
plot(pcgcov_pair$x[,2], pcgcov_pair$x[,3], type="n", xlab=paste("PC2 (",(imp_pair$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp_pair$importance[,3][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),2], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 3], pch=symbols[i], col=morecolors[i])
}

legend("topleft", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#plot PC3 and PC4 for species
plot(pcgcov_pair$x[,3], pcgcov_pair$x[,4], type="n", xlab=paste("PC3 (",(imp_pair$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp_pair$importance[,4][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),3], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 4], pch=symbols[i], col=morecolors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#plot PC4 and PC5 for species
plot(pcgcov_pair$x[,4], pcgcov_pair$x[,5], type="n", xlab=paste("PC4 (",(imp_pair$importance[,4][[2]]*100), "% )", sep=""), ylab=paste("PC5 (",(imp_pair$importance[,5][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),4], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]), 5], pch=symbols[i], col=morecolors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#plot PC5 and PC6 for species
plot(pcgcov_pair$x[,5], pcgcov_pair$x[,6], type="n", xlab=paste("PC5 (",(imp_pair$importance[,5][[2]]*100), "% )", sep=""), ylab=paste("PC6 (",(imp_pair$importance[,6][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),5], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),6], pch=symbols[i], col=morecolors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#plot PC6 and PC7 for species
plot(pcgcov_pair$x[,6], pcgcov_pair$x[,7], type="n", xlab=paste("PC6 (",(imp_pair$importance[,6][[2]]*100), "% )", sep=""), ylab=paste("PC7 (",(imp_pair$importance[,7][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),6], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),7], pch=symbols[i], col=morecolors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


#plot PC7 and PC8 for species
plot(pcgcov_pair$x[,7], pcgcov_pair$x[,8], type="n", xlab=paste("PC7 (",(imp_pair$importance[,7][[2]]*100), "% )", sep=""), ylab=paste("PC8 (",(imp_pair$importance[,8][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo_pair$Common_Name)))){
  points(pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),7], pcgcov_pair$x[which(fishinfo_pair$Common_Name==unique(fishinfo_pair$Common_Name)[i]),8], pch=symbols[i], col=morecolors[i])
}

legend("topright", legend=unique(fishinfo_pair$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)

dev.off()



