# script for plotting q values averaged across species

#devtools::install_github("gadenbuie/ggpomological")

library(tidyverse)
#library(ggpomological)
library(RColorBrewer)
library(patchwork)


setwd("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/entropy_pub/")

speciesk2 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k2_species_q.txt", sep = "\t", header = T)
colnames(speciesk2) <- c("Common_Name", "V01", "V02")
speciesk3 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k3_species_q.txt", sep = "\t", header = T)
colnames(speciesk3) <- c("Common_Name", "V01", "V02", "V03")
speciesk4 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k4_species_q.txt", sep = "\t", header = T)
colnames(speciesk4) <- c("Common_Name", "V01", "V02", "V03", "V04")
speciesk5 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k5_species_q.txt", sep = "\t", header = T)
colnames(speciesk5) <- c("Common_Name", "V01", "V02", "V03", "V04","V05")
speciesk6 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k6_species_q.txt", sep = "\t", header = T)
colnames(speciesk6) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06")
speciesk7 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k7_species_q.txt", sep = "\t", header = T)
colnames(speciesk7) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07")
speciesk8 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k8_species_q.txt", sep = "\t", header = T)
colnames(speciesk8) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08")
speciesk9 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k9_species_q.txt", sep = "\t", header = T)
colnames(speciesk9) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09")
speciesk10 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k10_species_q.txt", sep = "\t", header = T)
colnames(speciesk10) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10")
speciesk11 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k11_species_q.txt", sep = "\t", header = T)
colnames(speciesk11) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11")
speciesk12 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k12_species_q.txt", sep = "\t", header = T)
colnames(speciesk12) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12")

speciesk13 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k13_species_q.txt", sep = "\t", header = T)
colnames(speciesk13) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13")

speciesk14 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k14_species_q.txt", sep = "\t", header = T)
colnames(speciesk14) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13","V14")

speciesk15 <- read.delim("./algonquin_100k_q_files/AMP22_algonquin_100k_entropy_list_k15_species_q.txt", sep = "\t", header = T)
colnames(speciesk15) <- c("Common_Name", "V01", "V02", "V03", "V04","V05","V06","V07","V08","V09","V10","V11","V12","V13","V14","V15")


# in order for k=12
colour <- c("#8C510A","#FDBF6F","#A6CEE3","#35978F","#CAB2D6","#B2DF8A","#FB9A99","#E5DF60","#DF65B0")
# in order for algonquin, w CS, CC, and BND
colour <- c("#FDBF6F","#A6CEE3","#DF65B0")
# in order for algonquin, w CS, CC, RC, and BND
colour <- c("#FDBF6F","#A6CEE3","#B2DF8A","#DF65B0")

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
pdf("AMP22_algonquin_100k_mean_q_per_species_ordered.pdf", width = 12, height = 18)
plotk2 + plotk3 + plotk4 +
plotk5 + plotk6 + plotk7 +
plotk8 + plotk9 + plotk10 +
plotk11 + plotk12 + plotk13 + 
plotk14 + plotk15 + 
  plot_annotation(tag_levels = 'A')+
  guide_area() +
  plot_layout(guides = 'collect', ncol = 3)
dev.off()
