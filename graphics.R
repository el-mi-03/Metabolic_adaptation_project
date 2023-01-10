int <- function(name){
    t <- data_old[,c("Time [s]",name)]
    colnames(t) <- c("time","values")
  x <- integrate(approxfun(t$time, t$values), 1, 12)
  return(x)
}

setwd("H:/Liza/A.baylyi_other_data")
library(tidyverse)
library(readxl)
library(dplyr)
library(here)

data <- as_tibble(read_excel("22_12_22_merged_table.xlsx"))
data <- na.omit(data)
data$`Time [s]` <- round(data$`Time [s]`/3600)
data_old <- data
c <- colnames(data)
c <- c[!c == "Cycle Nr."]
c <- c[!c == "Time [s]"]
data <- data %>% 
  pivot_longer(c, names_to = "Carbon_source", values_to = "OD")

#a <- data[1:10, ]

data <- data %>%
  separate(
    Carbon_source,
    c("carbon_soucre","concentration,mM",NA,"TM concentration","replica"),
    "_"
  )
#making table nicer
data <- data[ ,2:7]
colnames(data) <- c("Time", "carbon_source", "cs_concentration_mM", "TM_concentration","replica","OD")
data$cs_and_conc <- paste(data$carbon_source, "_", data$cs_concentration_mM)


#plotting the data
p <- ggplot(data = data, aes(x = Time, y = OD, group = interaction(carbon_source, cs_concentration_mM))) +
  geom_line(aes(color = cs_and_conc), linewidth = 1, stat = "summary", fun = mean) +
  geom_errorbar(stat = "summary", fun.data = function(x) {
    data.frame(ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
  }, width = 0.1) + facet_grid(carbon_source~TM_concentration)
print(p)

table_fin <- data.frame(matrix(nrow = length(c), ncol = 2))
colnames(table_fin) <- c("conditions","AUC")
table_fin$conditions <- c
table_fin$AUC <- lapply(table_fin$conditions, FUN = int)
