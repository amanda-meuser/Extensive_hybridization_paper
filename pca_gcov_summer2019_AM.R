library(rhdf5)
library(ggplot2)
library(tidyverse)

# import point estimates file
g <- read.table("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/PCA/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_pntest_mean_gl.txt", header=F) 

# import .imiss file
imiss <- read.table("out_03may23_0.6_tidy.imiss", header=T) 

# import names list
names <- read.table("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/PCA/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs_final.txt", header=F) 
colnames(names) <- "FishID"

# import metadata
details <- read.csv("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/Leuciscid_Metadata_Apr2023.csv", header=T) 

# check for indivs missing species IS
fishinfo_raw <- merge(names, details, by.x="FishID", by.y="Mandeville_ID", all.x=T, all.y=F)
n_data_present <- sum(fishinfo_raw$Species != "NA", na.rm=T)
total_data <- nrow(fishinfo_raw)
percent_data_missing = 1-(n_data_present/total_data)

# replace blank species datat w Ns
fishinfo <- fishinfo_raw
fishinfo$Species <- replace_na(fishinfo$Species, "N")

# Modifying Charlotte Ward's fish to all say Costello Creek as location
fishinfo$Waterbody[fishinfo$Waterbody_Code == "OP2"] <- "Costello_Creek"
fishinfo$Waterbody[fishinfo$Waterbody_Code == "OP7"] <- "Costello_Creek"
fishinfo$Waterbody[fishinfo$Waterbody_Code == "Weir DS"] <- "Costello_Creek"


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
pdf("PCA1,2_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs_final.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),1], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


dev.off()

#----------------------------------------------------------------------------
#plot PC2 and PC3 for species
pdf("PCA2,3_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs_final.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),1], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


dev.off()

#----------------------------------------------------------------------------
#plot PC3 and PC4 for species
pdf("PCA3,4_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs_final.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Common_Name)))){
  points(pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]),1], pcgcov$x[which(fishinfo$Common_Name==unique(fishinfo$Common_Name)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


dev.off()


#----------------------------------------------------------------------------
#plot PC1 and PC2 for location
pdf("PCA1,2_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Waterbody)))){
  points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),1], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Waterbody), pch=symbols, col=morecolors, ncol=1, cex=0.5)


dev.off()

#----------------------------------------------------------------------------
#plot PC2 and PC3 for location
pdf("PCA2,3_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,2], pcgcov$x[,3], type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Waterbody)))){
  points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),1], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("bottomleft", legend=unique(fishinfo$Waterbody), pch=symbols, col=morecolors, ncol=1, cex=0.5)


dev.off()

#----------------------------------------------------------------------------
#plot PC3 and PC4 for location
pdf("PCA3,4_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_location.pdf")
#par(mfrow=c(1,2))
plot(pcgcov$x[,3], pcgcov$x[,4], type="n", xlab=paste("PC3 (",(imp$importance[,3][[2]]*100), "% )", sep=""), ylab=paste("PC4 (",(imp$importance[,4][[2]]*100), "% )", sep=""))

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(fishinfo$Waterbody)))){
  points(pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]),1], pcgcov$x[which(fishinfo$Waterbody==unique(fishinfo$Waterbody)[i]), 2], pch=symbols[i], col=morecolors[i])
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


pdf("F_MISSxPC1_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.pdf")

plot(pcgcov$x[,1], missing_fish$F_MISS, type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab="Proportion of missing data")

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(missing_fish$Common_Name)))){
  points(pcgcov$x[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i]),1], pcgcov$x[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("top", legend=unique(missing_fish$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


dev.off()

#----------------------------------------------------------------------------

# missingness vs PC2

pdf("F_MISSxPC2_AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs.pdf")

plot(pcgcov$x[,2], missing_fish$F_MISS, type="n", xlab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), ylab="Proportion of missing data")

#loop iterates over locations and adds a different colour for each one
for(i in 1:(length(unique(missing_fish$Common_Name)))){
  points(pcgcov$x[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i]),1], pcgcov$x[which(missing_fish$Common_Name==unique(missing_fish$Common_Name)[i]), 2], pch=symbols[i], col=morecolors[i])
}

legend("top", legend=unique(missing_fish$Common_Name), pch=symbols, col=morecolors, ncol=1, cex=1)


dev.off()

############################################################################



pdf("trout_26trib_plusref_simplified_color_27july17.pdf")
                                       #par(mfrow=c(1,2))
plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), ylab=paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""))

## NF fish
points(pcgcov$x[which(fishinfo$Tributary != "AuburnHatchery" & fishinfo$Tributary != "TensleepHatchery" & fishinfo$Tributary != "StoryHatchery" & fishinfo$Tributary != "SFOwlCreek" & fishinfo$Tributary != ""),1], pcgcov$x[which(fishinfo$Tributary != "AuburnHatchery" & fishinfo$Tributary != "TensleepHatchery" & fishinfo$Tributary != "StoryHatchery" & fishinfo$Tributary != "SFOwlCreek" & fishinfo$Tributary != ""),2], pch=1, col="darkgray")

## Snake cutthroat
points(pcgcov$x[which(fishinfo$Tributary == "AuburnHatchery"),1], pcgcov$x[which(fishinfo$Tributary == "AuburnHatchery"),2], pch=4, col=colors[1], lwd=1.5)

## YSC ref
points(pcgcov$x[which(fishinfo$Tributary == "SFOwlCreek"),1], pcgcov$x[which(fishinfo$Tributary == "SFOwlCreek"),2], pch=2, col=colors[2], lwd=1.5)
points(pcgcov$x[which(fishinfo$Tributary == "TensleepHatchery"),1], pcgcov$x[which(fishinfo$Tributary == "TensleepHatchery"),2], pch=3, col=colors[2], lwd=1.5)

## RBT ref
points(pcgcov$x[which(fishinfo$Tributary == "StoryHatchery"),1], pcgcov$x[which(fishinfo$Tributary == "StoryHatchery"),2], pch=5, col=colors[3], lwd=1.5)




legend("topleft", legend=c("North Fork", "Tensleep Hatchery (YSC)", "SF Owl Creek (YSC)", "Auburn Hatchery (FSC)", "Story Hatchery (RBT)"), pch=c(1,3,2,4,5), col=c("darkgray", colors[2], colors[2], colors[1], colors[3]))


dev.off()

pdf("PC1_hist_27july17.pdf")
par(mfrow=c(4,4), mar=c(4,4,4,0))

for(i in 1:(length(unique(fishinfo$Tributary)))){
    hist(pcgcov$x[which(fishinfo$Tributary==sort(unique(fishinfo$Tributary))[i]),1], main=sort(unique(fishinfo$Tributary))[i], xlim=c(-2,0.6), breaks=seq(-2,0.6,0.2), col="gray", xlab="", ylab="")
    mtext("PC1 score", side=1, outer=T, line=-1)
    mtext("Frequency", side=2, outer=T, line=-1.5)
}

dev.off()

pdf("trout_PCA_bytrib_27july17.pdf")
par(mfrow=c(4,4), mar=c(3,4,4,1))


for(i in 1:(length(unique(fishinfo$Tributary)))){
    plot(pcgcov$x[,1], pcgcov$x[,2], type="n", xlab="", ylab="", main=sort(unique(fishinfo$Tributary))[i])

    points(pcgcov$x[which(fishinfo$Tributary==sort(unique(fishinfo$Tributary))[i]),1], pcgcov$x[which(fishinfo$Tributary==sort(unique(fishinfo$Tributary))[i]), 2], pch=symbols[i], col=morecolors[i], xlab="", ylab="")
    mtext(paste("PC1 (",(imp$importance[,1][[2]]*100), "% )", sep=""), side=1, outer=T, line=-1)
    mtext(paste("PC2 (",(imp$importance[,2][[2]]*100), "% )", sep=""), side=2, outer=T, line=-1.5)
}

plot(1:10, 1:10, type="n", axes=F, xlab="", ylab="")
legend("center", legend=unique(fishinfo$Tributary), pch=symbols, col=morecolors, ncol=2, cex=0.7)


dev.off()

## Plot PC1 against amount of data

assem <- read.table("assembled_per_ind.txt", header=T)
pc.assem <- merge(assem, pc.summary, by.x="ind", by.y="FishID", all.x=T, all.y=T)

pdf("PC1_assembled_12666loci.pdf")
plot(pc.assem$assembled, pc.assem$PC1, xlab="reads assembled", ylab=paste("PC1 (",(imp$importance[,1][[2]]*100), "% )"))#, xlim=c(0,4*10^6))
dev.off()

##------------------------------------------------------------------------------------
## kmeans and lda
library(MASS)
#k1<-kmeans(pcgcov$x[,1:5],1,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
k2<-kmeans(pcgcov$x[,1:5],2,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
k3<-kmeans(pcgcov$x[,1:5],3,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
## k4<-kmeans(pcgcov$x[,1:5],4,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
## k5<-kmeans(pcgcov$x[,1:5],5,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
## k6<-kmeans(pcgcov$x[,1:5],6,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
## k7<-kmeans(pcgcov$x[,1:5],7,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
## k8<-kmeans(pcgcov$x[,1:5],8,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
## k9<-kmeans(pcgcov$x[,1:5],9,iter.max=10,nstart=10,algorithm="Hartigan-Wong")
## k10<-kmeans(pcgcov$x[,1:5],10,iter.max=10,nstart=10,algorithm="Hartigan-Wong")

## ldak1<-lda(x=pcgcov$x[,1:5],grouping=k1$cluster,CV=TRUE)
ldak2<-lda(x=pcgcov$x[,1:5],grouping=k2$cluster,CV=TRUE)
ldak3<-lda(x=pcgcov$x[,1:5],grouping=k3$cluster,CV=TRUE)
## ldak4<-lda(x=pcgcov$x[,1:5],grouping=k4$cluster,CV=TRUE)
## ldak5<-lda(x=pcgcov$x[,1:5],grouping=k5$cluster,CV=TRUE)
## ldak6<-lda(x=pcgcov$x[,1:5],grouping=k6$cluster,CV=TRUE)
## ldak7<-lda(x=pcgcov$x[,1:5],grouping=k7$cluster,CV=TRUE)
## ldak8<-lda(x=pcgcov$x[,1:5],grouping=k8$cluster,CV=TRUE)
## ldak9<-lda(x=pcgcov$x[,1:5],grouping=k9$cluster,CV=TRUE)
## ldak10<-lda(x=pcgcov$x[,1:5],grouping=k10$cluster,CV=TRUE)

#write.table(round(ldak1$posterior,5),file="ldak_var0.7_1all.txt",quote=F,row.names=F,col.names=F)
write.table(round(ldak2$posterior,5),file="ldak_var0.6_maf0.05_2all.txt",quote=F,row.names=F,col.names=F)
write.table(round(ldak3$posterior,5),file="ldak_var0.6_maf0.05_3all.txt",quote=F,row.names=F,col.names=F)
## write.table(round(ldak4$posterior,5),file="ldak_var0.7_4all.txt",quote=F,row.names=F,col.names=F)
## write.table(round(ldak5$posterior,5),file="ldak_var0.7_5all.txt",quote=F,row.names=F,col.names=F)
## write.table(round(ldak6$posterior,5),file="ldak_var0.7_6all.txt",quote=F,row.names=F,col.names=F)
## write.table(round(ldak7$posterior,5),file="ldak_var0.7_7all.txt",quote=F,row.names=F,col.names=F)
## write.table(round(ldak8$posterior,5),file="ldak_var0.7_8all.txt",quote=F,row.names=F,col.names=F)
## write.table(round(ldak9$posterior,5),file="ldak_var0.7_9all.txt",quote=F,row.names=F,col.names=F)
## write.table(round(ldak10$posterior,5),file="ldak_var0.7_10all.txt",quote=F,row.names=F,col.names=F)

## when you run entropy use provide the input values as, e.g., -q ldak2common.txt
## also set -s to something like 50
