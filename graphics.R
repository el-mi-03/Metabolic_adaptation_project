library(tidyverse)
library(readxl)
library(dplyr)
library(here)
data <- as_tibble(read_excel("22_12_22_merged_table.xlsx"))
c <- colnames(data)
c <- c[!c == "Cycle Nr."]
c <- c[!c == "Time [s]"]
data %>% 
  pivot_longer(c, names_to = "Carbon_source", values_to = "OD")
