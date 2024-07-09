## Setup -----

setwd("~/documents/Mandeville_Lab/Hybrid_Swarm/R/Data/")

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(scales)
library(patchwork)
theme_set(theme_pubr())

## Load Data -----

# Main dataset
df_hybridswarm <- read_csv("hybridswarm - Cleaned.csv")
#view(df_hybridswarm)

# remove papers without active hybrid swarms
df_hybridswarm <- df_hybridswarm %>% filter(Relevant != "N")

# Author Wordcloud Datasheet
#df_authors <- read_csv("hybridswarm - AuthorFrequency.csv")

## Basic Variable Plots -----

#Shows how many papers used the term 'hybrid swarm' each year
(freq <- ggplot(df_hybridswarm, aes(Year)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean() +
  #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Year") + ylab("Number of Publications")+ 
   scale_y_continuous(breaks = breaks_pretty()))


#Shows how many papers used the term 'hybrid swarm' to describe each type of organism
(organism <- ggplot(data = subset(df_hybridswarm, !is.na(OrganismType)), aes(fct_infreq(OrganismType))) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean() +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Organism Type") + ylab("Number of Publications")+ 
    scale_y_continuous(breaks = breaks_pretty()))


#Shows how many papers used the term 'hybrid swarm' to describe each genus
(genus <- ggplot(data = subset(df_hybridswarm, !is.na(Genus)), aes(fct_infreq(Genus))) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90)) +
  #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)+
  xlab("Genus") + ylab("Number of Publications")+ 
    scale_y_continuous(breaks = breaks_pretty()))

# distribution of loci
Nloci <- na.omit(as.numeric(df_hybridswarm$Nloci))
Nloci <- as.data.frame(Nloci)
names(Nloci) <- "V1"
(loci <- ggplot(data = df_hybridswarm, aes(x=Nloci)) + 
    geom_histogram(fill = "#0073C2FF") +
    xlab("Number of loci") + ylab("Number of Publications"))

# distrib of loci w outlier removed
subset_Nloci <- as.data.frame(Nloci[-46,])
names(subset_Nloci) <- "V1"
(loci2 <- ggplot(data = subset_Nloci, aes(x=V1)) + 
    geom_histogram(fill = "#0073C2FF") +
    xlab("Number of loci w outlier removed") + ylab("Number of Publications"))

#Shows the relationship between Nloci and Nind
(locivsinds <- ggplot(data = subset(df_hybridswarm, !is.na(Nind), !is.na(Nloci)), aes(Nind,Nloci)) +
  geom_point()+
  xlab("Number of loci") + ylab("Number of inds"))
cor.test(df_hybridswarm$Nloci,df_hybridswarm$Nind)

# Plot of SNP usage over time
# proportion of studies
(SNP_prop <- ggplot(data = subset(df_hybridswarm, !is.na(UsedSNPs)), aes(fill=UsedSNPs,x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(SNP_count <- ggplot(data = subset(df_hybridswarm, !is.na(UsedSNPs)), aes(fill=UsedSNPs,x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications"))

# Plots of microsat usage over time
# proportion of studies
(micro_prop <- ggplot(data = subset(df_hybridswarm, !is.na(UsedMicrosatellites)), aes(fill=UsedMicrosatellites,x=Year)) + 
  geom_bar(position="fill", stat="count")+
    xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(micro_count <- ggplot(data = subset(df_hybridswarm, !is.na(UsedMicrosatellites)), aes(fill=UsedMicrosatellites,x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
    xlab("Year") + ylab("Number of Publications"))


# Plots of phenotypic data usage over time
# proportion of studies
(pheno_prop <- ggplot(data = subset(df_hybridswarm, !is.na(PhenotypicEvidence)), aes(fill=PhenotypicEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count")+
    xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(pheno_count <- ggplot(data = subset(df_hybridswarm, !is.na(PhenotypicEvidence)), aes(fill=PhenotypicEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
    xlab("Year") + ylab("Number of Publications"))


# Plots of genetic data usage over time
# proportion of studies
(geno_prop <- ggplot(data = subset(df_hybridswarm, !is.na(GeneticEvidence)), aes(fill=GeneticEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count")+
    xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(geno_count <- ggplot(data = subset(df_hybridswarm, !is.na(GeneticEvidence)), aes(fill=GeneticEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
    xlab("Year") + ylab("Number of Publications"))


# Plots of both phenotypic and genetic data usage over time
# proportion of studies
(both_prop <- ggplot(data = subset(df_hybridswarm, !is.na(GenoAndPhenoEvidence)), aes(fill=GenoAndPhenoEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual(values = c("mediumpurple1", "brown2", "grey", "steelblue1"))+
    xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(both_count <- ggplot(data = subset(df_hybridswarm, !is.na(GenoAndPhenoEvidence)), aes(fill=GenoAndPhenoEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")+
  scale_fill_manual(values = c("mediumpurple1", "brown2", "grey", "steelblue1"))+
    xlab("Year") + ylab("Number of Publications"))


# Data availability
# proportion of studies
(data_prop <- ggplot(data = subset(df_hybridswarm, !is.na(DataAvailable)), aes(fill=DataAvailable,x=Year)) + 
    geom_bar(position="fill", stat="count")+
    xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(data_count <- ggplot(data = subset(df_hybridswarm, !is.na(DataAvailable)), aes(fill=DataAvailable, x=Year)) + 
  geom_bar(position="dodge") +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)+
    xlab("Year") + ylab("Number of Publications"))




## Basics by Taxon -----

# Disturbance
df_disturbance <- df_hybridswarm[!is.na(df_hybridswarm$Disturbance),]
(disturbance <- ggplot(data=subset(df_disturbance, !is.na(OrganismType)), aes(x=OrganismType, fill=Disturbance, colour=Disturbance)) + 
  geom_bar(position="dodge") +
  labs(y = "Number of Publications", x = "Organism Type"))
  #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25))

## Author frequency -----

# #More simplistic model
# wordcloud(words = df_authors$Author, freq = df_authors$Frequency, min.freq = 1,
#           max.words=500, random.order=FALSE, rot.per=0.35,
#           colors=brewer.pal(8, "Dark2"))
# 
# #Newer package with more options
# wordcloud2(data = df_authors, size=0.5, color='random-dark')


## Output as a pdf -----

pdf("Hybrid_swarm_plots.pdf", height = 48, width = 14)
  freq + organism + 
  genus + locivsinds +
  loci + loci2 + 
  SNP_prop + SNP_count + 
  micro_prop + micro_count +
  geno_prop + geno_count +
  pheno_prop + pheno_count +
  both_prop + both_count +
  data_prop + data_count +
  disturbance + 
  plot_layout(ncol = 2)
dev.off()







