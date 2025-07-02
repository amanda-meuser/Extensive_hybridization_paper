# Logistic regression script
# Amanda Meuser -- Oct. 2023


library(stringr)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(corrr)
library(patchwork)
library(RColorBrewer)


#import site data and metadata
site_data <- read.csv("Leuciscid_OFAT_May2023.csv")
colnames(site_data)[1:2] <- c("Waterbody", "Waterbody_Code")
metadata <- read.delim("Leuciscid_Metadata_May2023.csv", sep = ";")

#edit site_data and metadata
site_data <- site_data[1:30,] #remove blank rows
site_data <- site_data[-15,] #remove Costello upstream (only one fish there and sites are <1km apart)
site_data$Waterbody[site_data$Waterbody_Code == "COS"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "OP2"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "OP7"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "Weir DS"] <- "Costello_Creek"
metadata$Waterbody_Code[metadata$Waterbody == "Costello_Creek"] <- "COS"
metadata <- metadata[!(metadata$Waterbody_Code == "CC****"),]

# import data on hybrids
all_fish <- read.csv("AMP22_genoID_multispecies_k12_Jun2023.csv")

# simplify hybrid type and plot
all_fish$Hybrid_Type_Simple = NA

for (i in 1:nrow(all_fish)){
  if(str_detect(all_fish[i,15],'x') == TRUE ){
    all_fish$Hybrid_Type_Simple[i] <- "Hybrid" 
  }
  else if(str_detect(all_fish[i,15],'x') == FALSE){
    all_fish$Hybrid_Type_Simple[i] <- "Parental"
  }
}

# changing some of the data to factor class
all_fish$Pheno_Correct <-as.factor(all_fish$Pheno_Correct)
all_fish$Hybrid_Status <-as.factor(all_fish$Hybrid_Status)
all_fish$Multi_Status <-as.factor(all_fish$Multi_Status)
all_fish$Hybrid_Type_Simple <-as.factor(all_fish$Hybrid_Type_Simple)

# create counts of hybrid types
all_fish_details <- merge(all_fish, metadata, by = c("Mandeville_ID", "Common_Name"))
fish_count <- all_fish_details %>% group_by(Waterbody_Code) %>% count(Hybrid_Type_Simple)

# create frequency column
sums <- fish_count %>% 
  group_by(Waterbody_Code) %>%
  summarise(n_sum = sum(n),
            .groups = 'drop')
fish_count <- merge(fish_count, sums, by='Waterbody_Code')
getproportion <- function(row) {
  return (row$n/row$n_sum)
}
fish_count <- fish_count %>% mutate(proportion = n / n_sum)


# retain only the count of hybrids and total fish per site
hybrid_count <- fish_count %>% filter(Hybrid_Type_Simple == "Hybrid")

# drop Hybrid_Type_Simple column and rename columns
hybrid_count$Hybrid_Type_Simple = NULL
colnames(hybrid_count) <- c("Waterbody_Code", "n_Hybrids", "n_Total_Fish", "Proportion_Hybrids")

# merge w site data
site_fish_data <- merge(hybrid_count, site_data, by = "Waterbody_Code")

#remove variables that I don't need (waterbody name, lab name, and notes, variables in km2)
colnames(site_fish_data)
site_fish_logreg <- site_fish_data[-c(4:6,10,14,16:18,24:39,61)]
colnames(site_fish_logreg)
#write.csv(colnames(site_fish_logreg), "colnames_logreg.csv", quote = F, row.names = F)


# scatter plots for all 33 variables 
par(mfrow = c(5,7))
par(mar = c(4,4,1,1))

for (col in names(site_fish_logreg)){
  if (col != "n_Hybrids" & col != "n_Total_Fish" & col != "Waterbody_Code"){
    plot(site_fish_logreg[[col]], site_fish_logreg$n_Hybrids,
         xlab = col,
         ylab = "Number of hybrids")
  }
}


#reset to default
par(mfrow = c(1,1))
par(mar = c(5.1, 4.1, 4.1, 2.1))

# create a correlogram and remove predictor variables that are too closely correlated
cor <- cor((site_fish_logreg[-c(1:3)]), method = c("pearson"))
#plot
(corrplot <- corrplot(cor, method="color", type = "lower", tl.cex = 0.75))

# pdf("correlogram_logreg_33_variables.pdf", width = 8, height = 8)
# corrplot(cor, method="color", type = "lower", tl.cex = 0.75)
# dev.off()

#print 25 most correlated variables
cor %>%
  correlate() %>% 
  stretch() %>% 
  arrange(r) %>%
  print(n=50)  # Annual_mean_temp_C seems correlated to soo many things, so is Slope_main_channel_percent

# remove some of the correlated variables
site_fish_logreg$Annual_mean_temp_C = NULL
site_fish_logreg$Slope_main_channel_percent = NULL
site_fish_logreg$Swamp_percent = NULL
site_fish_logreg$Total_agriculture_percent = NULL
site_fish_logreg$Total_urban_and_development_percent = NULL
site_fish_logreg$Open_water_percent = NULL
site_fish_logreg$Total_treed_percent = NULL
site_fish_logreg$Marsh_percent = NULL
site_fish_logreg$Bog_percent = NULL
site_fish_logreg$Fen_percent = NULL


# create a histogram of each variable used
colours <- brewer.pal(8, "Set2")

# pdf("histograms_logreg_23_variables.pdf", height = 12, width = 9)
# par(mfrow = c(5,5))
# par(mar = c(4,4,1,1))
# 
# for (col in names(site_fish_logreg)){
#   if (col != "n_Hybrids" & col != "n_Total_Fish" & col != "Waterbody_Code"){
#     hist(site_fish_logreg[[col]],
#          xlab = col,
#          ylab = "Frequency",
#          cex.lab = 0.6,
#          main = "",
#          col = colours)
#   }
# }
# dev.off()

#reset to default
par(mfrow = c(1,1))
par(mar = c(5.1, 4.1, 4.1, 2.1))

# run the model
glm<-glm(n_Hybrids/n_Total_Fish ~ . -Waterbody_Code,family = binomial(link=logit), weights = n_Total_Fish, data = site_fish_logreg)
summary(glm)

# look at plots of residuals etc
plot(glm)

# pdf("Logistic_regression_residuals.pdf")
# layout(matrix(1:4, ncol = 2))
# plot(glm)
# layout(1)
# mtext("A", side=3, cex=1.5, outer=T, line=-3, adj = 0.03)
# mtext("B", side=3, cex=1.5, outer=T, line=-3, adj = 0.52)
# mtext("C", side=3, cex=1.5, outer=T, line=-21, adj = 0.03)
# mtext("D", side=3, cex=1.5, outer=T, line=-21, adj = 0.52)
# dev.off()

# plot the predictor variable that has the strongest relationship with proportion of hybrids

(clear_open_water <- ggplot(site_fish_logreg, aes(x=Clear_Open_Water_percent, y=n_Hybrids/n_Total_Fish)) +
    geom_point() +
    stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    labs(y = "Proportion of Hybrids", x = "Clear Open Water Percent") +
    ylim(0,1) +
    theme_bw())

(sparse_treed <-ggplot(site_fish_logreg, aes(x=Sparse_Treed_percent, y=n_Hybrids/n_Total_Fish)) +
    geom_point() +
    stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    labs(y = "Proportion of Hybrids", x = "Sparse Treed Percent") +
    ylim(0,1) +
    theme_bw())

(deciduous_treed <-ggplot(site_fish_logreg, aes(x=Deciduous_Treed_percent, y=n_Hybrids/n_Total_Fish)) +
    geom_point() +
    stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    labs(y = "Proportion of Hybrids", x = "Deciduous Treed Percent") +
    ylim(0,1) +
    theme_bw())

(mixed_treed <-ggplot(site_fish_logreg, aes(x=Mixed_Treed_percent, y=n_Hybrids/n_Total_Fish)) +
    geom_point() +
    stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    labs(y = "Proportion of Hybrids", x = "Mixed Treed Percent") +
    ylim(0,1) +
    theme_bw())

(coniferous_treed <-ggplot(site_fish_logreg, aes(x=Coniferous_Treed_percent, y=n_Hybrids/n_Total_Fish)) +
    geom_point() +
    stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    labs(y = "Proportion of Hybrids", x = "Coniferous Treed Percent") +
    ylim(0,1) +
    theme_bw())

(sand_gravel_mine <-ggplot(site_fish_logreg, aes(x=Sand_Gravel_Mine_Tailings_Extraction_percent, y=n_Hybrids/n_Total_Fish)) +
    geom_point() +
    stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    labs(y = "Proportion of Hybrids", x = "Gravel/Sand Mine Percent") +
    ylim(0,1) +
    theme_bw())

(community_infrastructure <-ggplot(site_fish_logreg, aes(x=Community_Infrastructure_percent, y=n_Hybrids/n_Total_Fish)) +
    geom_point() +
    stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    labs(y = "Proportion of Hybrids", x = "Community Infrastructure Percent") +
    ylim(0,1) +
    theme_bw())

(total_wetland <-ggplot(site_fish_logreg, aes(x=Total_wetland_percent, y=n_Hybrids/n_Total_Fish)) +
    geom_point() +
    stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    labs(y = "Proportion of Hybrids", x = "Wetland Percent") +
    ylim(0,1) +
    theme_bw())


# save as a pdf
# pdf("Logistic_regression_plots.pdf", height = 11, width = 8)
# clear_open_water + sparse_treed + deciduous_treed + mixed_treed + coniferous_treed + sand_gravel_mine + community_infrastructure + total_wetland +
#   plot_layout(ncol = 2) +
#   plot_annotation(tag_levels = 'A')
# dev.off()


# add column to bin sites by type (agri, urban, low-disturb)
site_fish_logreg$Disturbance_category <- "Agriculturalized" #agriculturalized
site_fish_logreg$Disturbance_category[site_fish_logreg$Waterbody_Code == "COS" | site_fish_logreg$Waterbody_Code == "SIC"] <- "Low-Disturbance" # low disturbance
site_fish_logreg$Disturbance_category[site_fish_logreg$Waterbody_Code == "LAC" | site_fish_logreg$Waterbody_Code == "LAP" | site_fish_logreg$Waterbody_Code == "CLC"] <- "Urbanized" # urbanized
site_fish_logreg$Disturbance_category <- as.factor(site_fish_logreg$Disturbance_category)

# make it use low disturbance as reference
site_fish_logreg <- within(site_fish_logreg, Disturbance_category <- relevel(Disturbance_category, ref = 2))

# run the model
glm_cat<-glm(n_Hybrids/n_Total_Fish ~ Disturbance_category,family = binomial(link=logit), weights = n_Total_Fish, data = site_fish_logreg)
summary(glm_cat)


# pdf("Logistic_regression_categories_residuals.pdf")
# layout(matrix(1:4, ncol = 2))
# plot(glm_cat)
# layout(1)
# mtext("A", side=3, cex=1.5, outer=T, line=-3, adj = 0.03)
# mtext("B", side=3, cex=1.5, outer=T, line=-3, adj = 0.52)
# mtext("C", side=3, cex=1.5, outer=T, line=-21, adj = 0.03)
# mtext("D", side=3, cex=1.5, outer=T, line=-21, adj = 0.52)
# dev.off()

(disturb_cat <-ggplot(site_fish_logreg, aes(x=Disturbance_category, y=n_Hybrids/n_Total_Fish, fill = Disturbance_category)) +
    #stat_smooth(method="glm",colour="black", se=TRUE, method.args = list(family=binomial)) + 
    geom_violin(trim = T) +
    geom_point() +
    labs(y = "Proportion of Hybrids", x = "Disturbance Category") +
    ylim(0,1) +
    scale_fill_brewer(palette="Set2") +
    theme(legend.position="none"))

# pdf("Logistic_regression_categories.pdf", height = 8, width = 8)
# disturb_cat
# dev.off()


#save as a pdf with the 8 other plots
pdf("Logistic_regression_plots_all.pdf", height = 10, width = 16)
(clear_open_water | sparse_treed) / (deciduous_treed | mixed_treed) / (coniferous_treed | sand_gravel_mine) | (community_infrastructure | total_wetland) / disturb_cat +
  plot_layout(widths = c(2, 1), heights = unit(c(7, 1), c('cm', 'null'))) 
  #plot_annotation(tag_levels = 'A') # this doesn't work w the crazy layout that i'm doing
dev.off()


# perform an anova on glm object
anova <- anova(glm_cat, test="Chisq")
print(anova)



#-----------------------------------------------------------------------------------------------
# Eryn's simulation code
#-----------------------------------------------------------------------------------------------

n_hybrids<-sample(0:15, 25, replace=T)
n_animals<-sample(15:30, 25, replace=T)

environment_1<-rnorm(25)
environment_2<-n_hybrids*0.3+rnorm(25)

data<-data.frame(cbind(n_hybrids, n_animals, environment_1, environment_2))

glm_sim<-glm(n_hybrids/n_animals ~ environment_1+environment_2 ,family = binomial(link=logit), weights = n_animals)
summary(glm_sim)

plot(glm_sim)

### plot the logistic regression
# Plot Predicted data and original data points
ggplot(data, aes(x=environment_2, y=n_hybrids/n_animals)) + geom_point() +
  stat_smooth(method="glm",colour="black", se=TRUE,
              method.args = list(family=binomial))+theme_bw()

