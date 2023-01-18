int <- function(name){
    t <- data_old[,c("Time [s]",name)]
    colnames(t) <- c("time","values")
  x <- integrate(approxfun(t$time, t$values), 1, 12)$val
  return(x)
}
#function which create a pivot table
pivot_my_table <- function(data){
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
  #data <- data[ ,2:7]
  colnames(data) <- c("Time", "carbon_source", "cs_concentration_mM", "TM_concentration","replica","OD")
  data$cs_and_conc <- paste(data$carbon_source,"_",data$cs_concentration_mM)
  return(data)
  
}
#function which creates plot and save it into the home directory
create_my_plot <- function(data, data_name){
  p <- ggplot(data = data, aes(x = Time, y = OD, group = interaction(carbon_source, cs_concentration_mM))) +
    geom_line(aes(color = cs_and_conc), linewidth = 1, stat = "summary", fun = mean) +
    geom_errorbar(stat = "summary", fun.data = function(x) {
      data.frame(ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
    }, width = 0.1) + facet_grid(carbon_source~TM_concentration)
  
  ggsave(paste0("Growth curves_", data_name,".png"),path = "H:/Liza/A.baylyi_other_data/", width = 5, height = 5, device='png', dpi=700)
  print(p)
}

setwd("H:/Liza/A.baylyi_other_data")
library(tidyverse)
library(readxl)
library(dplyr)
library(here)
data_name <- "3_11_23_merged_table"
data <- as_tibble(read_excel(paste0(data_name, ".xlsx")))

data <- na.omit(data)
#data$`Time [s]` <- round(data$`Time [s]`/3600)
for (col in 2:length(colnames(data))){
  data[col] <-  data[col]*2
}
c <- colnames(data)
c <- c[!c == "Cycle Nr."]
c <- c[!c == "Time [s]"]
data_old <- data
#data_contr <- cbind(data[ ,"Time [s]"], data[ ,grep(("contr"), colnames(data))])
#data <- cbind(data[ ,"Time [s]"], data[ ,grep(("TM"), colnames(data))])
#data_contr <- pivot_my_table(data_contr)
data <- pivot_my_table(data)
#name2 <- paste0("control", data_name)
create_my_plot(data, data_name)
#create_my_plot(data_contr, name2)

#counting AUC's for all my curves
table_fin <- data.frame(matrix(nrow = length(c), ncol = 2))
colnames(table_fin) <- c("conditions","AUC")
table_fin$conditions <- c
table_fin$AUC <- lapply(table_fin$conditions, FUN = int)

#exporting the final table
table_fin$AUC <- unlist(table_fin$AUC)
write.table(table_fin, paste0("AUC_", data_name), sep = ",", quote = FALSE, row.names = F)
