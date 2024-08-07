## Setup -----

setwd("~/documents/Mandeville_Lab/Hybrid_Swarm/R/Data/")

library(tidyverse)
library(wordcloud2)
library(RColorBrewer)
library(scales)
library(patchwork)

## Load Data -----

# Main dataset
df_raw <- read_csv("hybridswarm - Cleaned.csv")
#view(df_raw)

# remove papers that aren't relevant
df_relevant <- df_raw %>% filter(Relevant != "N")
df_hybridswarm <- df_relevant %>% filter(ActiveHybSwarm != "N")

# Author Wordcloud Datasheet
#df_authors <- read_csv("hybridswarm - AuthorFrequency.csv")

## ----- Frequency of Usage Plots -----

#Shows how many papers used the term 'hybrid swarm' each year
(freq_raw <- ggplot(df_relevant, aes(Year)) +
  geom_bar(fill = "#0073C2FF") +
  #theme_pubclean() +
  #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Year") + ylab("Number of Publications")+ 
   ylim(0,8))

(freq <- ggplot(df_hybridswarm, aes(Year)) +
    geom_bar(fill = "#0073C2FF") +
    #theme_pubclean() +
    #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
    xlab("Year") + ylab("Number of Publications")+ 
    ylim(0,8))


pdf("freq_of_term_usage.pdf", height = 5, width = 10)
  freq_raw + freq + 
  plot_annotation(tag_levels = 'A') 
dev.off()


## ----- Summary Stats Plots -----

#Shows how many papers used the term 'hybrid swarm' to describe each type of organism
(organism <- ggplot(data = subset(df_hybridswarm, !is.na(OrganismType)), aes(fct_infreq(OrganismType))) +
  geom_bar(fill = "#0073C2FF") +
  #theme_pubclean() +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Organism Type") + ylab("Number of Publications")+ 
    scale_y_continuous(breaks = breaks_pretty()))

#Shows how many papers used the term 'hybrid swarm' to describe each genus
(genus <- ggplot(data = subset(df_hybridswarm, !is.na(Genus)), aes(fct_infreq(Genus))) +
    geom_bar(fill = "#0073C2FF") +
    #theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90)) +
    #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)+
    xlab("Genus") + ylab("Number of Publications")+ 
    scale_y_continuous(breaks = breaks_pretty()))



# number of species used
(species <- ggplot(data = df_hybridswarm, aes(x=Nspecies)) + 
    geom_bar(fill = "#0073C2FF") +
    scale_x_continuous(breaks = round(seq(min(df_hybridswarm$Nspecies), max(df_hybridswarm$Nspecies), by = 2),1)) +
    xlab("Number of Species") + ylab("Number of Publications"))


# Number of inds used, one is NA
(inds <- ggplot(data = df_hybridswarm, aes(x=Nind)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,15) +
    xlab("Number of Individuals") + ylab("Number of Publications"))

# look only at entries w fewer than 2000 inds
subset_hybridswarm2 <- subset(df_hybridswarm, df_hybridswarm$Nind < 2000) # 45 of the original 48 inds
(inds2 <- ggplot(data = subset_hybridswarm2, aes(x=Nind)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,15) +
    xlab("Number of Individuals (only fewer than 2000)") + ylab("Number of Publications"))


# Number of sampling sites used, 4 are NA
(sites <- ggplot(data = df_hybridswarm, aes(x=Locations)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,15) +
    xlab("Number of Sampling Sites") + ylab("Number of Publications"))

# look only at entries w fewer than sites
subset_hybridswarm2 <- subset(df_hybridswarm, df_hybridswarm$Locations < 100) # 42 of the original 48 inds
(sites2 <- ggplot(data = subset_hybridswarm2, aes(x=Locations)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,15) +
    xlab("Number of Sampling Sites (only fewer than 100)") + ylab("Number of Publications"))



# distribution of loci, only 40 used loci
df_hybridswarm$Nloci <- as.numeric(df_hybridswarm$Nloci)
(loci <- ggplot(data = df_hybridswarm, aes(x=Nloci)) + 
    geom_histogram(fill = "#0073C2FF") +
    xlab("Number of loci") + ylab("Number of Publications"))

#get position of the outlier and remove it
a <- max(which(df_hybridswarm$Nloci > 100000))
subset_hybridswarm <- df_hybridswarm[-a,]

# distrib of loci w outlier removed
(loci2 <- ggplot(data = subset_hybridswarm, aes(x=Nloci)) + 
    geom_histogram(fill = "#0073C2FF") +
    xlab("Number of loci w outlier removed") + ylab("Number of Publications"))

# look only at entries w fewer than 500 loci
subset_hybridswarm2 <- subset(df_hybridswarm, df_hybridswarm$Nloci < 500) # 28 of the original 40 inds, so only 12 have more than 5000
(loci3 <- ggplot(data = subset_hybridswarm2, aes(x=Nloci)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,30) +
    xlab("Number of loci (only fewer than 500)") + ylab("Number of Publications"))


# #Shows the relationship between Nloci and Nind
# (locivsinds <- ggplot(data = subset(df_hybridswarm, !is.na(Nind), !is.na(Nloci)), aes(Nind,Nloci)) +
#   geom_point()+
#   xlab("Number of loci") + ylab("Number of inds"))
# cor.test(df_hybridswarm$Nloci,df_hybridswarm$Nind)

# Plot of SNP usage over time
# proportion of studies
(SNP_prop <- ggplot(data = subset(df_hybridswarm, !is.na(UsedSNPs)), aes(fill=UsedSNPs,x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(SNP_count <- ggplot(data = subset(df_hybridswarm, !is.na(UsedSNPs)), aes(fill=UsedSNPs,x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Used \nSNPs?", labels = c("No", "Yes")))


# Plots of microsat usage over time
# proportion of studies
(micro_prop <- ggplot(data = subset(df_hybridswarm, !is.na(UsedMicrosatellites)), aes(fill=UsedMicrosatellites,x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(micro_count <- ggplot(data = subset(df_hybridswarm, !is.na(UsedMicrosatellites)), aes(fill=UsedMicrosatellites,x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Used \nMicrostats?", labels = c("No", "Yes")))


# Plots of phenotypic data usage over time
# proportion of studies
(pheno_prop <- ggplot(data = subset(df_hybridswarm, !is.na(PhenotypicEvidence)), aes(fill=PhenotypicEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(pheno_count <- ggplot(data = subset(df_hybridswarm, !is.na(PhenotypicEvidence)), aes(fill=PhenotypicEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Used \nPhenptypic \nEvidence?", labels = c("No", "Yes")))


# Plots of genetic data usage over time
# proportion of studies
(geno_prop <- ggplot(data = subset(df_hybridswarm, !is.na(GeneticEvidence)), aes(fill=GeneticEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(geno_count <- ggplot(data = subset(df_hybridswarm, !is.na(GeneticEvidence)), aes(fill=GeneticEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Used \nGenetic \nEvidence?", labels = c("No", "Yes")))


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
  xlab("Year") + ylab("Proportion of Publications")+
  scale_fill_discrete(name = "Data \nAvailable?", labels = c("No", "Yes")))

# number of studies
(data_count <- ggplot(data = subset(df_hybridswarm, !is.na(DataAvailable)), aes(fill=DataAvailable, x=Year)) + 
  geom_bar(position="stack") +
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Data \nAvailable?", labels = c("No", "Yes")))
  # geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)


# Disturbance per taxa
df_disturbance <- df_hybridswarm[!is.na(df_hybridswarm$Disturbance),]
(disturbance_taxa <- ggplot(data=subset(df_disturbance, !is.na(OrganismType)), aes(x=OrganismType, fill=Disturbance)) + 
  geom_bar(position="dodge") +
  labs(y = "Number of Publications", x = "Organism Type")+
  scale_fill_discrete(name = "Disturbance \nPresent?", labels = c("No", "Yes")))
  #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25))

# disturbance over years
(disturbance_years <- ggplot(data=subset(df_disturbance, !is.na(OrganismType)), aes(x=Year, fill=Disturbance)) + 
  geom_bar(position="stack") +
  labs(y = "Number of Publications", x = "Year")+
  scale_fill_discrete(name = "Disturbance \nPresent?", labels = c("No", "Yes")))


## Author frequency -----

# #More simplistic model
# wordcloud(words = df_authors$Author, freq = df_authors$Frequency, min.freq = 1,
#           max.words=500, random.order=FALSE, rot.per=0.35,
#           colors=brewer.pal(8, "Dark2"))
# 
# #Newer package with more options
# wordcloud2(data = df_authors, size=0.5, color='random-dark')


## ----- Output as a pdf -----

pdf("summary_plots_all.pdf", height = 48, width = 14)
  freq + organism + 
  genus + species +
  inds + inds2 + 
  sites + sites2 +
  loci + loci2 + loci3 +
  SNP_prop + SNP_count + 
  micro_prop + micro_count +
  geno_prop + geno_count +
  pheno_prop + pheno_count +
  both_prop + both_count +
  data_prop + data_count +
  disturbance_taxa + disturbance_years + 
  plot_layout(ncol = 2)+ 
  plot_annotation(tag_levels = 'A')
dev.off()

pdf("summary_plots_numeric.pdf", height = 26, width = 14)
  organism + species +
  inds + inds2 + 
  sites + sites2 +
  loci + loci3 + 
  plot_layout(ncol = 2)+ 
  plot_annotation(tag_levels = 'A')
dev.off()
  
pdf("summary_plots_YorN.pdf", height = 22, width = 14)  
  SNP_count + micro_count +
  geno_count + pheno_count +
  data_count + disturbance_taxa + 
  plot_layout(ncol = 2)+ 
  plot_annotation(tag_levels = 'A')
dev.off()


## ----- Hybrid outcomes flow chart -----
# install.packages("devtools")
# devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# example 
df <- mtcars
df <- mtcars %>%
  make_long(cyl, vs, am, gear, carb)

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey() +
  scale_fill_discrete(drop=FALSE)

# now with our data
df_relevant_filled <- df_relevant %>% replace_na(list(F1Present = "N", F2orLaterPresent = "N", BackcrossPresent = "N"))
df_long <- df_relevant_filled %>% make_long(ActiveHybSwarm, GenoAndPhenoEvidence, F1Present, F2orLaterPresent, BackcrossPresent)

## Testing out example plots
# ggplot(df_long, aes(x = x, 
#                next_x = next_x, 
#                node = node, 
#                next_node = next_node,
#                fill = factor(node))) +
#   geom_sankey() +
#   scale_fill_discrete(drop=FALSE)
# 
# ggplot(df_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
#   geom_sankey(flow.alpha = .6,
#               node.color = "gray30") +
#   geom_sankey_label(size = 3, color = "white", fill = "gray40") +
#   scale_fill_viridis_d(drop = FALSE) +
#   theme_sankey(base_size = 18) +
#   labs(x = NULL) +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust = .5)) +
#   ggtitle("Hybrid swarm outcomes")


## actual plot
(sankey <- ggplot(df_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6) +
  geom_alluvial_text(size = 3, color = "white") +
  scale_fill_brewer(palette = "Dark2", drop = FALSE) +
  theme_alluvial(base_size = 18) +
  labs(x = NULL, y = "Number of Publications") +
  theme(legend.position = "none", axis.text.y= element_text(size = 10), axis.text.x= element_text(size = 8, angle = 30), axis.title.y =  element_text(size = 10))) 

pdf("sankey_plot.pdf")  
sankey
dev.off()

