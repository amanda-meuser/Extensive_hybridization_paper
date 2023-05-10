library(tidyverse)
library(tools)

DIC <- read.delim("EGM19_cc_target_DIC_trimmed.txt", header=T, sep = "\t")

DIC_avg <- DIC %>% group_by(k) %>% summarize_all(mean)

ggplot(DIC_avg, aes(k, Model_DIC)) + geom_point()



#testing random stuff
DIC_avg$k[which.min(DIC_avg$Model_DIC)]
DIC_avg <- DIC_avg[,-2]
file_path_sans_ext("EGM19_cc_target_DIC_trimmed.txt")
