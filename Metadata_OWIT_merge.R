# Script for creating merged metadata and OWIT data CSV file

library(dplyr)

setwd("C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/")

# read in both csv files
metadata <- read.csv("Leuciscid_Metadata_May2023.csv")
OWIT <- read.csv("Leuciscid_OFAT_May2023.csv")

#keep only the target species - remove the 23 extra inds
metadata <- filter(metadata, Common_Name == "Central_Stoneroller" | Common_Name == "Common_Shiner" | Common_Name == "Creek_Chub" | Common_Name == "Hornyhead_Chub" | Common_Name == "Longnose_Dace" | Common_Name == "River_Chub" | Common_Name == "Rosyface_Shiner" | Common_Name == "Striped_Shiner" | Common_Name == "Western_Blacknose_Dace")

# make a table to check how many inds are at each location
(metadata_table <- table(metadata$Waterbody))

# clean up the data frames' column names
names(OWIT)[names(OWIT) == "ï..Site"] <- "Site"
names(metadata)[names(metadata) == "ï..Date"] <- "Date"
names(metadata)[names(metadata) == "BarcodeÂ."] <- "Barcode"
names(metadata)[names(metadata) == "SexÂ."] <- "Sex"

# remove blank rows
OWIT <- OWIT[(1:30),]

# remove Costello Creek Upstream and rename Downstream
OWIT <- OWIT[-15,]
OWIT$Site[OWIT$Site == "Costello_Creek_Downstream"] <- "Costello_Creek"
OWIT$Site

# Modifying Charlotte Ward's fish to all say Costello Creek as location
metadata$Waterbody[metadata$Waterbody_Code == "OP2"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "OP7"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "Weir DS"] <- "Costello_Creek"

(metadata_table <- table(metadata$Waterbody))

# bind the two data frames together
metadata_OWIT <- merge(metadata, OWIT, by.x = "Waterbody", by.y = "Site", all.x = T)

# check the waterbody codes are the same
all(metadata_OWIT$Waterbody_Code == metadata_OWIT$Code)
# create new column that checks where they're not the same
df_temp <- metadata_OWIT %>%
  select('Waterbody_Code', 'Code') %>%
  rowwise %>%
  mutate(match = n_distinct(unlist(cur_data())) == 1) %>%
  ungroup()

# export metadata_OWIT as CSV file
write.csv(metadata_OWIT, "C:/Users/ameus/Documents/Mandeville_lab_grad/Binf_work/Leuciscid_Metadata_OFAT_May2023.csv", row.names = F)
