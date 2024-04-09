# comparing [DNA] and sequenced reads for each individual (that I have [DNA] data for)
# March 2024 -- Amanda Meuser

library(ggplot2)

setwd("C:/Users/ameus/Documents/McFarlane_Lab_York/seq_reads_vs_DNA_conc")

reads <- read.delim("seq_reads_per_ind.txt", header = F)
metadata <- read.csv("Leuciscid_Metadata_May2023.csv")
concentration <- read.csv("AMP22_Extractions_and_MNRreporting_Updates.csv")

# add name to reads column
names(reads) <- "Reads"

# combine reads with metadata and extract columns of reads and IDs
metadata_reads <- cbind(metadata, reads)
IDs_reads <- data.frame(metadata_reads$Mandeville_ID, metadata_reads$Reads)

# rename columns
names(IDs_reads) <- c("Mandeville_ID", "Reads")

# merge w concentration df and extract columns of interest
conc_reads <- merge(concentration, IDs_reads, by.x = "AMP_ID", by.y = "Mandeville_ID")
# filtering by who did the final extraction
#conc_reads <- conc_reads[conc_reads$Final_extraction_by == "Amanda" | conc_reads$Final_extraction_by == "AmandaAmyTeaghan" | conc_reads$Final_extraction_by == "Teaghan",]
conc_reads_lil <- data.frame(conc_reads$AMP_ID, conc_reads$Final_extraction_concentration, conc_reads$Reads)
names(conc_reads_lil) <- c("Mandeville_ID", "DNA_Concentration", "Reads")

# remove NAs
conc_reads_lil <- conc_reads_lil[complete.cases(conc_reads_lil), ]

# plot it
plot(conc_reads_lil$DNA_Concentration, conc_reads_lil$Reads, 
     ylab = "Sequenced Reads", xlab = "DNA Concentration")

# pull in inds that were lost post-filtering and colour by that, see what kind of trend there is
post_filt <- read.delim("indivs_all_newest.txt", header = F)
names(post_filt) <- "Mandeville_ID"
post_filt$Retained <- "1"

conc_retained <- merge(conc_reads_lil, post_filt, all.x = T)
conc_retained$Retained[is.na(conc_retained$Retained)] <- 0

# plot it

(plot <- ggplot(conc_retained, aes(x=DNA_Concentration, y=Reads, color=Retained)) + 
  geom_point() +
  geom_smooth(method=lm) +
  xlab("DNA Concentration (ng/ul)") + ylab("Number of Sequenced Reads per Individual") + 
  scale_color_manual(values=c("#e56c60", "#56B4E9"), 
                     name="Post-Filtering\nStatus",
                     labels=c("Removed", "Kept")))

# pdf("Seq_Reads_vs_DNA_Concentration.pdf", width = 8, height = 8)
# plot
# dev.off()

# remove indiv in the 900s and plot again
conc_retained2 <- conc_retained[!conc_retained$Mandeville_ID == "AMP22_0240", ]
  
(plot2 <- ggplot(conc_retained2, aes(x=DNA_Concentration, y=Reads, color=Retained)) + 
    geom_point() +
    geom_smooth(method=lm) +
    xlab("DNA Concentration (ng/ul)") + ylab("Number of Sequenced Reads per Individual") + 
    scale_color_manual(values=c("#e56c60", "#56B4E9"), 
                       name="Post-Filtering\nStatus",
                       labels=c("Removed", "Kept")))
 
# pdf("Seq_Reads_vs_DNA_Concentration_outlier_removed.pdf", width = 8, height = 8)
# plot2
# dev.off()

# Eryn's stats suggestions
reg <- lm(conc_retained2$Retained ~ conc_retained2$DNA_Concentration, family="binomial")
reg
plot(reg)


inds_kept <- conc_retained[conc_retained$Retained == 1, ]
inds_lost <- conc_retained[conc_retained$Retained == 0, ]

test <- t.test(inds_kept$DNA_Concentration, inds_lost$DNA_Concentration)
test

# look at "borderline" cases, all [DNA] below 40
borderline <- conc_retained2[conc_retained2$DNA_Concentration <= 40,]

(plot3 <- ggplot(borderline, aes(x=DNA_Concentration, y=Reads, color=Retained)) + 
    geom_point() +
    geom_smooth(method=lm) +
    xlab("DNA Concentration (ng/ul)") + ylab("Number of Sequenced Reads per Individual") + 
    scale_color_manual(values=c("#e56c60", "#56B4E9"), 
                       name="Post-Filtering\nStatus",
                       labels=c("Removed", "Kept")))

inds_kept <- borderline[borderline$Retained == 1, ]
inds_lost <- borderline[borderline$Retained == 0, ]

test <- t.test(inds_kept$DNA_Concentration, inds_lost$DNA_Concentration)
test
