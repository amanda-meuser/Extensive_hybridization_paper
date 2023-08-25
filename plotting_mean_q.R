# script for plotting q values averaged across species

#devtools::install_github("gadenbuie/ggpomological")

library(tidyverse)
#library(ggpomological)
library(RColorBrewer)
library(patchwork)


#setwd("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/entropy_target/")

speciesk2 <- read.delim("AMP22_target_entropy_k2_species_q.txt", sep = "\t", header = T)
colnames(speciesk2) <- c("Common_Name", "V1", "V2")
speciesk3 <- read.delim("AMP22_target_entropy_k3_species_q.txt", sep = "\t", header = T)
colnames(speciesk3) <- c("Common_Name", "V1", "V2", "V3")
speciesk4 <- read.delim("AMP22_target_entropy_k4_species_q.txt", sep = "\t", header = T)
colnames(speciesk4) <- c("Common_Name", "V1", "V2", "V3", "V4")
speciesk5 <- read.delim("AMP22_target_entropy_k5_species_q.txt", sep = "\t", header = T)
colnames(speciesk5) <- c("Common_Name", "V1", "V2", "V3", "V4","V5")
speciesk6 <- read.delim("AMP22_target_entropy_k6_species_q.txt", sep = "\t", header = T)
colnames(speciesk6) <- c("Common_Name", "V1", "V2", "V3", "V4","V5","V6")
speciesk7 <- read.delim("AMP22_target_entropy_k7_species_q.txt", sep = "\t", header = T)
colnames(speciesk7) <- c("Common_Name", "V1", "V2", "V3", "V4","V5","V6","V7")
speciesk8 <- read.delim("AMP22_target_entropy_k8_species_q.txt", sep = "\t", header = T)
colnames(speciesk8) <- c("Common_Name", "V1", "V2", "V3", "V4","V5","V6","V7","V8")
speciesk9 <- read.delim("AMP22_target_entropy_k9_species_q.txt", sep = "\t", header = T)
colnames(speciesk9) <- c("Common_Name", "V1", "V2", "V3", "V4","V5","V6","V7","V8","V9")
speciesk10 <- read.delim("AMP22_target_entropy_k10_species_q.txt", sep = "\t", header = T)
colnames(speciesk10) <- c("Common_Name", "V1", "V2", "V3", "V4","V5","V6","V7","V8","V9","V10")
speciesk11 <- read.delim("AMP22_target_entropy_k11_species_q.txt", sep = "\t", header = T)
colnames(speciesk11) <- c("Common_Name", "V1", "V2", "V3", "V4","V5","V6","V7","V8","V9","V10","V11")
speciesk12 <- read.delim("AMP22_target_entropy_k12_species_q.txt", sep = "\t", header = T)
colnames(speciesk12) <- c("Common_Name", "V1", "V2", "V3", "V4","V5","V6","V7","V8","V9","V10","V11","V12")

# in order for k=12
colour <- c("#8C510A","#FDBF6F","#A6CEE3","#35978F","#CAB2D6","#B2DF8A","#FB9A99","#E5DF60","#DF65B0")


# data frame of all the data frames for looping through maybe, but not using this rn...
dfs <- t(as.data.frame(list("speciesk2", "speciesk3", "speciesk4", "speciesk5", "speciesk6", "speciesk7", "speciesk8", "speciesk9", "speciesk10", "speciesk11", "speciesk12")))
row.names(dfs) <- seq(1:11)




# k=2
# rearrange the data frame and plot
speciesk2_gathered <- speciesk2 %>% gather("column", "q_value", 2:3)
(plotk2 <- ggplot(speciesk2_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k2") +
    theme(plot.title = element_text(size = 24, hjust = 0.5)) +
    scale_color_manual(values = colour))


# k=3
speciesk3_gathered <- speciesk3 %>% gather("column", "q_value", 2:4)
(plotk3 <- ggplot(speciesk3_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
  geom_point(size = 3) +
    ggtitle("k3") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))


# k=4
speciesk4_gathered <- speciesk4 %>% gather("column", "q_value", 2:5)
(plotk4 <- ggplot(speciesk4_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k4") +
    theme(plot.title = element_text(size = 24, hjust = 0.5)) +
    scale_color_manual(values = colour))

# k=5
speciesk5_gathered <- speciesk5 %>% gather("column", "q_value", 2:6)
(plotk5 <- ggplot(speciesk5_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k5") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))

# k=6
speciesk6_gathered <- speciesk6 %>% gather("column", "q_value", 2:7)
(plotk6 <- ggplot(speciesk6_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k6") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))

# k=7
speciesk7_gathered <- speciesk7 %>% gather("column", "q_value", 2:8)
(plotk7 <- ggplot(speciesk7_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k7") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))

# k=8
speciesk8_gathered <- speciesk8 %>% gather("column", "q_value", 2:9)
(plotk8 <- ggplot(speciesk8_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k8") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))

# k=9
speciesk9_gathered <- speciesk9 %>% gather("column", "q_value", 2:10)
(plotk9 <- ggplot(speciesk9_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k9") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))

# k=10
speciesk10_gathered <- speciesk10 %>% gather("column", "q_value", 2:11)
(plotk10 <- ggplot(speciesk10_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k10") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))

# k=11
speciesk11_gathered <- speciesk11 %>% gather("column", "q_value", 2:12)
(plotk11 <- ggplot(speciesk11_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k11") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))

# k=12
speciesk12_gathered <- speciesk12 %>% gather("column", "q_value", 2:13)
(plotk12 <- ggplot(speciesk12_gathered, mapping = aes(y = q_value, x = column, color = Common_Name)) +
    geom_point(size = 3) +
    ggtitle("k12") +
    theme(plot.title = element_text(size = 24, hjust = 0.5))  +
    scale_color_manual(values = colour))


# save all as a pdf
pdf("AMP22_target_mean_q_per_species.pdf", width = 12, height = 18)
plotk2 + plotk3 + plotk4 +
plotk5 + plotk6 + plotk7 +
plotk8 + plotk9 + plotk10 +
plotk11 + plotk12 + 
  plot_annotation(tag_levels = 'A')+
  guide_area() +
  plot_layout(guides = 'collect', ncol = 3)
dev.off()
