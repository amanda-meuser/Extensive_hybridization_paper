# script for plotting q values averaged across species

#devtools::install_github("gadenbuie/ggpomological")

library(tidyverse)
#library(ggpomological)
library(RColorBrewer)
library(patchwork)


setwd("/Users/ameuser/Library/CloudStorage/GoogleDrive-ameuser03@gmail.com/My Drive/PhD_Files/Masters_stuff/Thesis_pub/AMP22_thesis_24feb25")

speciesk2 <- read.delim("AMP22_thesis_24feb25_entropy_list_k2_species_q.txt", sep = "\t", header = T)
colnames(speciesk2) <- c("Common_Name", "V01", "V02")
speciesk3 <- read.delim("AMP22_thesis_24feb25_entropy_list_k3_species_q.txt", sep = "\t", header = T)
colnames(speciesk3) <- c("Common_Name", "V01", "V02", "V03")
speciesk4 <- read.delim("AMP22_thesis_24feb25_entropy_list_k4_species_q.txt", sep = "\t", header = T)
colnames(speciesk4) <- c("Common_Name", "V01", "V02", "V03", "V04")
speciesk5 <- read.delim("AMP22_thesis_24feb25_entropy_list_k5_species_q.txt", sep = "\t", header = T)
colnames(speciesk5) <- c("Common_Name", "V01", "V02", "V03", "V04","V05")
speciesk6 <- read.delim("AMP22_thesis_24feb25_entropy_list_k6_species_q.txt", sep = "\t", header = T)
colnames(speciesk6) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06")
speciesk7 <- read.delim("AMP22_thesis_24feb25_entropy_list_k7_species_q.txt", sep = "\t", header = T)
colnames(speciesk7) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07")
speciesk8 <- read.delim("AMP22_thesis_24feb25_entropy_list_k8_species_q.txt", sep = "\t", header = T)
colnames(speciesk8) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08")
speciesk9 <- read.delim("AMP22_thesis_24feb25_entropy_list_k9_species_q.txt", sep = "\t", header = T)
colnames(speciesk9) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09")
speciesk10 <- read.delim("AMP22_thesis_24feb25_entropy_list_k10_species_q.txt", sep = "\t", header = T)
colnames(speciesk10) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10")
speciesk11 <- read.delim("AMP22_thesis_24feb25_entropy_list_k11_species_q.txt", sep = "\t", header = T)
colnames(speciesk11) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11")
speciesk12 <- read.delim("AMP22_thesis_24feb25_entropy_list_k12_species_q.txt", sep = "\t", header = T)
colnames(speciesk12) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12")

speciesk13 <- read.delim("AMP22_thesis_24feb25_entropy_list_k13_species_q.txt", sep = "\t", header = T)
colnames(speciesk13) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13")

speciesk14 <- read.delim("AMP22_thesis_24feb25_entropy_list_k14_species_q.txt", sep = "\t", header = T)
colnames(speciesk14) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13","V14")

speciesk15 <- read.delim("AMP22_thesis_24feb25_entropy_list_k15_species_q.txt", sep = "\t", header = T)
colnames(speciesk15) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13","V14","V15")



# # in order for algonquin, london w CS, CC, and BND
# colour <- c("#FDBF6F","#A6CEE3","#DF65B0")
# # in order for algonquin, w CS, CC, RC, and BND
# colour <- c("#FDBF6F","#A6CEE3","#B2DF8A","#DF65B0")
# # in order for all 11 species 
# colour <- c("grey90","#8C510A","#FDBF6F","#A6CEE3","grey30","#35978F","#CAB2D6","#B2DF8A","#FB9A99","#E5DF60","#DF65B0")
# in order for 9 species 
colour <- c("#8C510A","#FDBF6F","#A6CEE3","#35978F","#CAB2D6","#B2DF8A","#FB9A99","#E5DF60","#DF65B0")

# data frame of all the data frames for looping through maybe, but not using this rn...
#dfs <- t(as.data.frame(list("speciesk2", "speciesk3", "speciesk4", "speciesk5", "speciesk6", "speciesk7", "speciesk8", "speciesk9", "speciesk10", "speciesk11", "speciesk12")))
#row.names(dfs) <- seq(1:11)




# k=2
# rearrange the data frame and plot
speciesk2_gathered <- speciesk2 %>% gather("column", "q_value", 2:3)
(plotk2 <- ggplot(speciesk2_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k2") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))


# k=3
speciesk3_gathered <- speciesk3 %>% gather("column", "q_value", 2:4)
(plotk3 <- ggplot(speciesk3_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
  geom_point(size = 3) +
    ggtitle("k3") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))


# k=4
speciesk4_gathered <- speciesk4 %>% gather("column", "q_value", 2:5)
(plotk4 <- ggplot(speciesk4_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k4") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=5
speciesk5_gathered <- speciesk5 %>% gather("column", "q_value", 2:6)
(plotk5 <- ggplot(speciesk5_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k5") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=6
speciesk6_gathered <- speciesk6 %>% gather("column", "q_value", 2:7)
(plotk6 <- ggplot(speciesk6_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k6") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=7
speciesk7_gathered <- speciesk7 %>% gather("column", "q_value", 2:8)
(plotk7 <- ggplot(speciesk7_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k7") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=8
speciesk8_gathered <- speciesk8 %>% gather("column", "q_value", 2:9)
(plotk8 <- ggplot(speciesk8_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k8") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=9
speciesk9_gathered <- speciesk9 %>% gather("column", "q_value", 2:10)
(plotk9 <- ggplot(speciesk9_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k9") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=10
speciesk10_gathered <- speciesk10 %>% gather("column", "q_value", 2:11)
(plotk10 <- ggplot(speciesk10_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k10") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=11
speciesk11_gathered <- speciesk11 %>% gather("column", "q_value", 2:12)
(plotk11 <- ggplot(speciesk11_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k11") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=12
speciesk12_gathered <- speciesk12 %>% gather("column", "q_value", 2:13)
(plotk12 <- ggplot(speciesk12_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k12") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=13
speciesk13_gathered <- speciesk13 %>% gather("column", "q_value", 2:14)
(plotk13 <- ggplot(speciesk13_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k13") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=14
speciesk14_gathered <- speciesk14 %>% gather("column", "q_value", 2:15)
(plotk14 <- ggplot(speciesk14_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k14") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=15
speciesk15_gathered <- speciesk15 %>% gather("column", "q_value", 2:16)
(plotk15 <- ggplot(speciesk15_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k15") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))



# save all as a pdf
pdf("AMP22_thesis_24feb25_mean_q_per_species_ordered.pdf", width = 12, height = 18)
plotk2 + plotk3 + plotk4 +
plotk5 + plotk6 + plotk7 +
plotk8 + plotk9 + plotk10 +
plotk11 + plotk12 + plotk13 + 
plotk14 + plotk15 + 
  plot_annotation(tag_levels = 'A')+
  guide_area() +
  plot_layout(guides = 'collect', ncol = 3)
dev.off()


#----------------------------------------------------------------------------
# read in indivs files and generate median values, rather than means
#----------------------------------------------------------------------------

indivsk2 <- read.delim("AMP22_thesis_24feb25_entropy_list_k2_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk2) <- c("Mandeville_ID", "Common_Name", "V01", "V02")
indivsk3 <- read.delim("AMP22_thesis_24feb25_entropy_list_k3_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk3) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03")
indivsk4 <- read.delim("AMP22_thesis_24feb25_entropy_list_k4_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk4) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04")
indivsk5 <- read.delim("AMP22_thesis_24feb25_entropy_list_k5_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk5) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05")
indivsk6 <- read.delim("AMP22_thesis_24feb25_entropy_list_k6_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk6) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06")
indivsk7 <- read.delim("AMP22_thesis_24feb25_entropy_list_k7_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk7) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07")
indivsk8 <- read.delim("AMP22_thesis_24feb25_entropy_list_k8_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk8) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08")
indivsk9 <- read.delim("AMP22_thesis_24feb25_entropy_list_k9_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk9) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09")
indivsk10 <- read.delim("AMP22_thesis_24feb25_entropy_list_k10_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk10) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10")
indivsk11 <- read.delim("AMP22_thesis_24feb25_entropy_list_k11_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk11) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11")
indivsk12 <- read.delim("AMP22_thesis_24feb25_entropy_list_k12_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk12) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12")
indivsk13 <- read.delim("AMP22_thesis_24feb25_entropy_list_k13_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk13) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13")
indivsk14 <- read.delim("AMP22_thesis_24feb25_entropy_list_k14_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk14) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13","V14")
indivsk15 <- read.delim("AMP22_thesis_24feb25_entropy_list_k15_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk15) <- c("Mandeville_ID", "Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13","V14","V15")

#create a table of MEDIAN q values for each column, for each species
median_k2 <- indivsk2 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k3 <- indivsk3 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k4 <- indivsk4 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k5 <- indivsk5 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k6 <- indivsk6 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k7 <- indivsk7 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k8 <- indivsk8 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k9 <- indivsk9 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k10 <- indivsk10 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k11 <- indivsk11 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k12 <- indivsk12 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k13 <- indivsk13 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k14 <- indivsk14 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))
median_k15 <- indivsk15 %>% group_by(Common_Name) %>% summarise(across(where(is.numeric), (list = median)))

# in order for all 11 species
colour <- c("grey90","#8C510A","#FDBF6F","#A6CEE3","grey30","#35978F","#CAB2D6","#B2DF8A","#FB9A99","#E5DF60","#DF65B0")

# k=2
# rearrange the data frame and plot
median_k2_gathered <- median_k2 %>% gather("column", "q_value", 2:3)
(plotk2 <- ggplot(median_k2_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k2") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))


# k=3
median_k3_gathered <- median_k3 %>% gather("column", "q_value", 2:4)
(plotk3 <- ggplot(median_k3_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k3") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))


# k=4
median_k4_gathered <- median_k4 %>% gather("column", "q_value", 2:5)
(plotk4 <- ggplot(median_k4_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k4") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=5
median_k5_gathered <- median_k5 %>% gather("column", "q_value", 2:6)
(plotk5 <- ggplot(median_k5_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k5") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=6
median_k6_gathered <- median_k6 %>% gather("column", "q_value", 2:7)
(plotk6 <- ggplot(median_k6_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k6") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=7
median_k7_gathered <- median_k7 %>% gather("column", "q_value", 2:8)
(plotk7 <- ggplot(median_k7_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k7") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=8
median_k8_gathered <- median_k8 %>% gather("column", "q_value", 2:9)
(plotk8 <- ggplot(median_k8_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k8") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=9
median_k9_gathered <- median_k9 %>% gather("column", "q_value", 2:10)
(plotk9 <- ggplot(median_k9_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k9") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=10
median_k10_gathered <- median_k10 %>% gather("column", "q_value", 2:11)
(plotk10 <- ggplot(median_k10_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k10") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=11
median_k11_gathered <- median_k11 %>% gather("column", "q_value", 2:12)
(plotk11 <- ggplot(median_k11_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k11") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=12
median_k12_gathered <- median_k12 %>% gather("column", "q_value", 2:13)
(plotk12 <- ggplot(median_k12_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k12") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=13
median_k13_gathered <- median_k13 %>% gather("column", "q_value", 2:14)
(plotk13 <- ggplot(median_k13_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k13") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=14
median_k14_gathered <- median_k14 %>% gather("column", "q_value", 2:15)
(plotk14 <- ggplot(median_k14_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k14") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))

# k=15
median_k15_gathered <- median_k15 %>% gather("column", "q_value", 2:16)
(plotk15 <- ggplot(median_k15_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k15") +
    labs(y = "q  Value", x = "Ancestry Source", color= "Species") +
    theme(plot.title = element_text(size = 20, hjust = 0.5), legend.title = element_text(size=14), legend.text = element_text(size=11))  +
    scale_color_manual(values = colour) + ylim(0,1))



# save all as a pdf
pdf("AMP22_refilter22oct24_median_q_per_species_ordered.pdf", width = 12, height = 18)
plotk2 + plotk3 + plotk4 +
  plotk5 + plotk6 + plotk7 +
  plotk8 + plotk9 + plotk10 +
  plotk11 + plotk12 + plotk13 + 
  plotk14 + plotk15 + 
  plot_annotation(tag_levels = 'A')+
  guide_area() +
  plot_layout(guides = 'collect', ncol = 3)
dev.off()



#--------------------------------------------------
# checking ghost admixture
#--------------------------------------------------

indivsk12 <- read.delim("AMP22_thesis_24feb25_entropy_list_k12_indivs_q.txt", sep = "\t", header = T)
colnames(indivsk12) <- c("Mandeville_ID","Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12")


max(indivsk12$V11)
max(indivsk12$V12)
min(indivsk12$V11)
min(indivsk12$V12)

# isolate the two ghost columns
ghost11 <- as.data.frame(indivsk12[c(1,2,13)])
ghost12 <- as.data.frame(indivsk12[c(1,2,14)])

# retain inds w ancestry above 0.1%
ghost11 <- filter(ghost11, V11 > 0.001)
ghost12 <- filter(ghost12, V12 > 0.001)


