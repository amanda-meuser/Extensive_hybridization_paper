# script for plotting DIC values from entropy runs
# Amanda Meuser -- March 2023

# USAGE: Rscript plotting_DIC.R file 

# load packages
print("Loading packages...")
library(tidyverse)
library(tools)

# import DIC file
args <- commandArgs(TRUE)
file <- args[1]
DIC <- read.delim(file, header=T, sep = "\t")

print("Dimensions of file:")
dim(DIC)
print("Here's a sneak peak:")
head(DIC)

# average across the 3 reps
DIC_avg <- DIC %>% group_by(k) %>% summarize_all(mean)

# remove useless rep column
DIC_avg <- DIC_avg[,-2]

print("Dimensions of file:")
dim(DIC_avg)
print("Here's a sneak peak:")
head(DIC_avg)

print("The optimal value of k is:")
DIC_avg$k[which.min(DIC_avg$Model_DIC)]

basename1 <- basename(file)
basename <- file_path_sans_ext(basename1)
print("Basename is:")
basename

print("Creating plots...")
pdf(paste0(basename,"_plot.pdf"), width=11, height=11)
    ggplot(DIC_avg, aes(k, Model_DIC)) + geom_point()
dev.off()


pdf(paste0(basename,"_plot_extra.pdf"), width=11, height=11)
par(mfrow=c(1,3)) 

    ggplot(DIC_avg, aes(k, Model_deviance)) + geom_point()
    ggplot(DIC_avg, aes(k, Effective_number_of_parameters)) + geom_point()
    ggplot(DIC_avg, aes(k, Model_DIC)) + geom_point()

dev.off()
