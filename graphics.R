setwd("H:/Liza/A.baylyi_other_data")
library(tidyverse)
library(readxl)
library(dplyr)
library(here)
data <- as_tibble(read_excel("22_12_22_merged_table.xlsx"))
data <- na.omit(data)
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
data <- data[ ,2:7]
colnames(data) <- c("Time", "carbon_source", "cs_concentration_mM", "TM_concentration","replica","OD")

p <- ggplot(data = data, aes(x = Time, y = OD, group = interaction(carbon_source, cs_concentration_mM))) +
  geom_line(aes(linetype = cs_concentration_mM), linewidth = 1) + facet_grid(carbon_source~TM_concentration)
print(p)
