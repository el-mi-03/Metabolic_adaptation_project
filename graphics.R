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
