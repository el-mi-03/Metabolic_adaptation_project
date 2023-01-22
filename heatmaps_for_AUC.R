
setwd("H:/Liza/A.baylyi_other_data")
library(tidyverse)
library(dplyr)
library(tidyr)
data_name <- "AUC_22_12_22_merged_table"
data <- as_tibble(read.csv(data_name))

data <- data %>%
  separate(conditions, into = c('conditions', 'replica'), sep = -2, convert = TRUE) %>%
  separate(conditions, into = c('Carbon_source', 'TM_conc'), sep = -5, convert = TRUE)

data$replica<-gsub("_","",as.character(data$replica))
data$TM_conc<-gsub("_","",as.character(data$TM_conc))

data <- data %>% pivot_wider(names_from = replica, values_from = AUC)

data$mean_AUC <- (data$`1`+data$`2`)/2
data_for_heatmap <- data[c(1,2,5)]
data_for_heatmap <- data_for_heatmap %>% pivot_wider(names_from = TM_conc, values_from = mean_AUC)
#data_for_heatmap[,2:3] <- sapply(data_for_heatmap[,2:3],fun = as.numeric(sub(",", "\\.", x)))

data_for_heatmap <- data.frame(data_for_heatmap, row.names = 1)
data_for_heatmap <- data.matrix(data_for_heatmap)
heatmap(data_for_heatmap)

        