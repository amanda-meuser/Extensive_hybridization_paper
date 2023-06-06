library(tidyverse)
library(tools)

DIC <- read.delim("AMP22_target_DIC_trimmed.txt", header=T, sep = "\t")

# take the mean for each k value across the 3 reps
DIC_avg <- DIC %>% group_by(k) %>% summarize_all(mean)

# which model is best fit
DIC_avg$k[which.min(DIC_avg$Model_DIC)]

# order by best fit (change both the data frame order and the 'levels')
DIC_arrange <- DIC_avg %>% arrange(Model_DIC)
DIC_arrange$k <- factor(DIC_arrange$k, levels = DIC_arrange$k[order(DIC_arrange$Model_DIC)])

# plot it
(plot <- ggplot(DIC_arrange, aes(k, Model_DIC)) + geom_point())

# save as PDF 
pdf("AMP22_target_DIC.pdf")
plot
dev.off()




#testing random stuff
DIC_avg <- DIC_avg[,-2]
file_path_sans_ext("EGM19_cc_target_DIC_trimmed.txt")
