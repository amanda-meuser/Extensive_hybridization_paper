# Script for creating the 2 barcode key files needed for demultiplexing 2 libraries with sabre
# AM - April 21, 2023

# read in metadata file
data <- read.csv("Leuciscid_Metadata_Apr2023.csv")

# retain only the columns with the barcodes and the individual IDs 
data_edit <- data[,c(10,4)]

# add the .fq suffix to all individual IDs
data_edit$Mandeville_ID = paste0(data_edit$Mandeville_ID, '.fq')

# remover headers
names(data_edit)<- NULL

# truncate the file into two at the last individual on plate 6 (library 1 is plates 1-6, library 2 is plates 7-13)
lib1 <- data_edit[c(1:576),]
lib2 <- data_edit[c(577:1236),]

# save the two library dataframes as text files
write.table(lib1, "AMP22_library1_sabre_barcodes_21apr23.txt", sep = "\t", row.names = F, quote = F)
write.table(lib2, "AMP22_library2_sabre_barcodes_21apr23.txt", sep = "\t", row.names = F, quote = F)
