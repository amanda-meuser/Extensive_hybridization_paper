#get packages to do stuff
#install.packages("wesanderson")
#install.packages(grid)

library(ggplot2)
library(wesanderson)
library(dplyr)
library(grid)

#-----------------------------------------------------------------
#import the data and create the percent aligned column
ass <- data.frame(read.delim2("assembled_reads_25apr23_final.txt", sep = "\t"))
ass <- ass %>% mutate(percent = (assembled/raw)*100)

#import metadata
metadata <- read.csv("Leuciscid_Metadata_Apr2023.csv")

# identify and remove the snake without ANY sequenced reads
dfNA <- ass[!complete.cases(ass),]
ass <- ass[complete.cases(ass),]

#merge the two files, retaining all data
merged <- merge(ass, metadata, by.x="ind", by.y="Mandeville_ID", all.x=T)

#mean and median alignment percentage
mean(ass$percent)
median(ass$percent)

#mean and median by species (NA**** is PFTV-19-WHOLE)
species_mean <- merged %>% group_by(Common_Name) %>% summarize_all(mean)
species_mean <- species_mean[c(1,5)]
species_mean

species_median <- merged[c(4,9)]
species_median <- species_median %>% group_by(Common_Name) %>% summarize_all(median)
species_median

#extract colours for plots
names(wes_palettes)
col <- wes_palette("GrandBudapest1")
#col <- wes_palette("Darjeeling1")

#-----------------------------------------------------------------

#histogram of number of raw reads

histogram_raw <- ggplot(ass, aes(x=raw)) +
  geom_histogram(binwidth = 300000, color = col[3], fill = col[1]) +
  labs(x = "Number of Raw Reads", y = "Number of Individuals",
       title = "Number of raw reads, all individuals",
       subtitle = "Solid line = mean, Dashed line = median") +
  geom_vline(aes(xintercept = mean(raw)), color = col[2], linetype = "solid", size = 2) +
  geom_vline(aes(xintercept = median(raw)), color = col[2], linetype = "dashed", size = 2) 

histogram_raw


#histogram of number of aligned reads

histogram_aligned <- ggplot(ass, aes(x=assembled)) +
  geom_histogram(binwidth = 300000, color = col[3], fill = col[1]) +
  labs(x = "Number of Aligned Reads", y = "Number of Individuals",
       title = "Number of reads aligned, all individuals",
       subtitle = "Solid line = mean, Dashed line = median") +
  geom_vline(aes(xintercept = mean(assembled)), color = col[2], linetype = "solid", size = 2) +
  geom_vline(aes(xintercept = median(assembled)), color = col[2], linetype = "dashed", size = 2) 

histogram_aligned


#-----------------------------------------------------------------

#scatter plot

scatterplot <- ggplot(ass, aes(x = raw, y = percent)) +
  geom_point(size = 2, color = col[2], fill = col[1]) +
  labs(x = "Number of Raw Reads", y = "Percent of Reads Aligned",
       title = "Percent of reads aligned vs Number of raw reads, all individuals", subtitle = "Solid line = linear regression") +
  ylim(0,100) +
  geom_smooth(method=lm, se=T, color = col[3]) #add regression line, se = confidence interval

scatterplot

#-----------------------------------------------------------------

#line plot of percent aligned reads

lineplot <- ggplot(ass, aes(x=ind, y=percent)) +
  geom_line(color = col[3]) +
  labs(x = "Individuals", 
       y = "Percent of Reads Aligned",
       title = "Percent of reads aligned, all individuals, unsorted") +
  geom_point(color = col[2], size = 1.5)+ 
  theme(axis.text.x = element_blank()) 

lineplot


#-----------------------------------------------------------------

#histogram of percent of aligned reads
#use just the percent column on its own cuz u don't need the other data anyways

histogram_percent <- ggplot(ass, aes(x=percent)) +
  geom_histogram(binwidth = 1, color = col[3], fill = col[1]) +
  labs(x = "Percent of Reads Aligned", 
       y = "Number of Individuals",
       title = "Percent of reads aligned, all individuals",
       subtitle = "Solid line = mean, Dashed line = median") +
  geom_vline(aes(xintercept = mean(percent)), color = col[2], linetype = "solid", size = 2) +
  geom_vline(aes(xintercept = median(percent)), color = col[2], linetype = "dashed", size = 2)

histogram_percent


#histogram of percent aligned for creek chub
merged_noNA <- filter(merged, !is.na(Common_Name))
merged_cc <- filter(merged_noNA, Common_Name == "Creek_Chub")

histo_cc <- ggplot(merged_cc, aes(percent)) +
  geom_histogram(aes(x=percent, fill=Common_Name), alpha=0.5, binwidth = 1, color = col[3], fill = col[2]) +
  labs(x="Percent of reads aligned",
       title="Percent of reads aligned for creek chub, prefiltering",
       subtitle = "Solid line = mean, Dashed line = median") +
  geom_vline(aes(xintercept = mean(percent)), color = col[2], linetype = "solid", size = 2) +
  geom_vline(aes(xintercept = median(percent)), color = col[2], linetype = "dashed", size = 2)
histo_cc


#=================================================================

#density plot of percent reads aligned by species 
theme_set(theme_bw())
merged_noNA <- filter(merged, !is.na(Common_Name))

density <- ggplot(merged_noNA, aes(percent)) +
  geom_density(aes(fill=factor(Common_Name)), alpha=0.5) +
  labs(x="Percent of reads aligned",
       fill="Common_Name", 
       title="Percent of reads aligned by species, prefiltering")
density

#density plot of percent reads aligned by species OF INTEREST
theme_set(theme_bw())
merged_noNA <- filter(merged, !is.na(Common_Name))
merged_SOI <- filter(merged_noNA, Common_Name == "Central_Stoneroller" | Common_Name == "Common_Shiner" | Common_Name == "Creek_Chub" | Common_Name == "Hornyhead_Chub" | Common_Name == "Longnose_Dace" | Common_Name == "River_Chub" | Common_Name == "Rosyface_Shiner" | Common_Name == "Striped_Shiner" | Common_Name == "Western_Blacknose_Dace")

density_SOI <- ggplot(merged_SOI, aes(percent)) +
  geom_density(aes(fill=factor(Common_Name)), alpha=0.5) +
  labs(x="Percent of reads aligned",
       fill="Common_Name", 
       title="Percent of reads aligned by species (for species of interest), prefiltering")
density_SOI

#-----------------------------------------------------------------

# dot/violin plot of mean by species 

dotplot_species <- ggplot(merged, aes(x=Common_Name, y=percent)) + 
  geom_violin(aes(fill=Common_Name))+ 
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") + 
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "blue")+
  geom_point(size = 1)+
  labs(y="Percent of reads aligned", 
       title="Percent of reads aligned by species, prefiltering",
       subtitle="Red bar = mean, Blue bar = median")+
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

dotplot_species


# dot/violin plot of mean by species OF INTEREST
merged_arranged_SOI <- arrange(merged_SOI, Common_Name)

dotplot_species_SOI <- ggplot(merged_arranged_SOI, aes(x=Common_Name, y=percent)) + 
  geom_violin(aes(fill=Common_Name))+ 
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") + 
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "blue")+
  geom_point(size = 1)+
  labs(y="Percent of reads aligned", 
       title="Percent of reads aligned by species of interest, prefiltering",
       subtitle="Red bar = mean, Blue bar = median",
       x="")+
  theme(axis.text.x = element_blank())+ 
  scale_fill_discrete(name="Species")

dotplot_species_SOI

#-----------------------------------------------------------------

# dot/violin plot of mean by barcode plate

merged_arranged_SOI_barcode <- arrange(merged_SOI, Barcode_plate)

dotplot_species_SOI_barcode <- ggplot(merged_arranged_SOI_barcode, aes(x=Barcode_plate, y=percent)) + 
  geom_violin(aes(fill=Barcode_plate))+ 
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") + 
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "blue")+
  geom_point(size = 1)+
  labs(x="Barcode Plate", y="Percent of reads aligned", 
       title="Percent of reads aligned by barcode plate for species of interest, prefiltering",
       subtitle="Red bar = mean, Blue bar = median")+
  theme(axis.text.x = element_blank())

dotplot_species_SOI_barcode


# dot/violin plot of mean by R/L plate
merged_arranged_SOI_plate <- arrange(merged_SOI, Plate)

dotplot_species_SOI_plate <- ggplot(merged_arranged_SOI_plate, aes(x=Plate, y=percent)) + 
  geom_violin(aes(fill=Plate))+ 
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") + 
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "blue")+
  geom_point(size = 1)+
  labs(x="R/L Plate", y="Percent of reads aligned", 
       title="Percent of reads aligned by R/L plate  for species of interest, prefiltering",
       subtitle="Red bar = mean, Blue bar = median")+
  theme(axis.text.x = element_blank())

dotplot_species_SOI_plate


#-----------------------------------------------------------------

#density plot of percent reads aligned by species AFTER FILTERING 

filtered_names <- read.delim("./PCA/AMP22_target_03may23_miss0.6_mac3_Q30_DP3_ind95_maf001_indivs_final.txt", header=F)
names(filtered_names) <- "FishID"
merged_filtered <- merge(filtered_names, merged, by.x="FishID", by.y="ind", all.x=F)


density_filtered <- ggplot(merged_filtered, aes(percent)) +
  geom_density(aes(fill=factor(Common_Name)), alpha=0.5) +
  labs(x="Percent of reads aligned",
       fill="Common_Name", 
       title="Percent of reads aligned by species, post-filtering", 
       subtitle="Red bar = mean, Blue bar = median")
density_filtered


# dot/violin plot of percent reads aligned by species AFTER FILTERING 

merged_filtered_arranged <- arrange(merged_filtered, Common_Name)

dotplot_species_filtered <- ggplot(merged_filtered_arranged, aes(x=Common_Name, y=percent)) + 
  geom_violin(aes(fill=Common_Name))+ 
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") + 
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "blue")+
  geom_point(size = 1)+
  labs(y="Percent of reads aligned", 
       title="Percent of reads aligned by species of interest, post-filtering",
       subtitle="Red bar = mean, Blue bar = median",
       x="")+
  theme(axis.text.x = element_blank())+ 
  scale_fill_discrete(name="Species")
dotplot_species_filtered


#-----------------------------------------------------------------

#output as a PDF overall plots


pdf("alignment_quality_summary_28Apr23_AMP22.pdf", width=10, height=6)

pushViewport(plotViewport(layout=grid.layout(12, 1)))
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=4))
grid.draw(lineplot)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=1))
grid.draw(histogram_raw)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=2))
grid.draw(histogram_aligned)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=3))
grid.draw(scatterplot)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=5))
grid.draw(histogram_percent)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=6))
grid.draw(histo_cc)
popViewport()

pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=7))
grid.draw(density)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=8))
grid.draw(density_SOI)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=9))
grid.draw(dotplot_species)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=10))
grid.draw(dotplot_species_SOI)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=11))
grid.draw(dotplot_species_SOI_barcode)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=12))
grid.draw(dotplot_species_SOI_plate)
popViewport()

dev.off()


pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=1))
grid.draw(density_filtered)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=3))
grid.draw(dotplot_species_filtered)
popViewport()






