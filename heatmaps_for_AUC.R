
setwd("H:/Liza/A.baylyi_other_data")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
data_name <- "AUC_25_12_22_merged_table_big_reader"
data <- as_tibble(read.csv(data_name))
a <- c('..')
b <- c('z')
data$conditions <- sapply(data$conditions,  stringi::stri_replace_all_fixed, a, b, vectorize_all = FALSE, USE.NAMES = FALSE)

data$conditions<-gsub(pattern = "z.*","",as.character(data$conditions))
data <- data %>%
  separate(conditions, into = c('conditions', 'replica'), sep = -2, convert = TRUE) %>%
  separate(conditions, into = c('Carbon_source', 'TM_conc'), sep = -5, convert = TRUE)

data$replica<-gsub("_","",as.character(data$replica))
data$TM_conc<-gsub("_","",as.character(data$TM_conc))

data <- data %>% pivot_wider(names_from = replica, values_from = AUC)
data <-na.omit(data)
data$mean_AUC <- (data$`1`+data$`2`)/2
data_for_heatmap <- data[c(1,2,5)]
#data_for_heatmap <- data_for_heatmap %>% pivot_wider(names_from = TM_conc, values_from = mean_AUC)
#data_for_heatmap[,2:3] <- sapply(data_for_heatmap[,2:3],fun = as.numeric(sub(",", "\\.", x)))

#data_for_heatmap <- data.frame(data_for_heatmap, row.names = 1)
#data_for_heatmap <- data.matrix(data_for_heatmap)
#heatmap(data_for_heatmap)
ggplot(data_for_heatmap, mapping = aes(x = TM_conc, y = Carbon_source, fill = mean_AUC))+
  geom_tile()+scale_fill_gradient(high = "orange", low = "blue")

ggsave(paste0("Heatmap_for_", data_name,".png"),
       path = "H:/Liza/A.baylyi_other_data/", width = 5, 
       height = 5, device='png', dpi=700)        
