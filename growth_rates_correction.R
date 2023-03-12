library(tidyverse)
library(readxl)
library(dplyr)
library(cowplot)
library(data.table)
library(openxlsx)
dir_name <- "H:/Liza/Chi.Bio_data/15_02_23_A.baylyi_transformation"
setwd(dir_name)
data_name <- "2023-02-15 17_01_41_M4_data"
data <- read.csv(paste0(data_name,".csv"))
#data_name <- "2023-01-30_M9_Succinate_0.4_M4_no_error_bars"
data_old <- data
data <- data[,c(1,2)]
a <- 0
b <- 0
t_median <- 0
t_min <- 0
t_max <- 0
x_min <- 0
x_max <- 0
y_min <- 0
y_max <- 0
growth_rate <- data.frame(matrix(nrow=0, ncol=2))
for (i in 30:length(data$exp_time)){
  if (data[i,2] > data[i-1,2]){
    a <- a+1
    x_max <- data[i,2]
    t_max <- data[i,1]
    if (a == 3){
      x_min <- data[i-3,2]
      t_min <- data[i-3,1]
      b <- 0
    }
  }
  if (data[i-1,2] > data[i,2]){
    b <- b+1
    if (b == 3){
      a <-0
      delta_t <- (t_max - t_min)/3600
      G <- delta_t*log(2)/(log(x_max/x_min))
      g_rate <- 1/G
      t_median <- (t_min + t_max)/7200
      growth_rate <- rbind(growth_rate,c(t_median,g_rate))
    }
  }
}
colnames(growth_rate)<- c("time","G_rate")

growth_rate <-  lapply(growth_rate, function(x) replace(x, is.infinite(x), NA))
growth_rate <- as_data_frame(growth_rate)
growth_rate <- growth_rate %>% drop_na()
#growth_rate <- growth_rate[2:93,]
ggplot(growth_rate,  aes(x = time, y = G_rate))+
  geom_line()
a <- 0
smooth_grate <- data.frame(matrix(nrow=0, ncol=4))

for (i in 1:(length(growth_rate$time)-9)){
  v_c <- as.vector(growth_rate[i:(i+9),2])
  v_c <- v_c[,1]
  mean_G <- mean(v_c$G_rate, trim = 0, na.rm = TRUE)
  dev <- sd(v_c$G_rate)
  v_t <- as.vector(growth_rate[i:(i+9),1])
  v_t <- v_t[,1]
  time <- mean(v_t$time, trim = 0, na.rm = TRUE)
  smooth_grate <- rbind(smooth_grate, c(i,time,mean_G,dev))
}

smooth_grate_1h <- data.frame(matrix(nrow=0, ncol=4))
for (i in 1:(length(growth_rate$time))){
  dt1 <- growth_rate[growth_rate$time %>% between(growth_rate$time[[i]],(growth_rate$time[[i]]+1)),]
  mean_G <- mean(dt1$G_rate, trim = 0, na.rm = TRUE)
  dev <- sd(dt1$G_rate)
  time <- mean(dt1$time, trim = 0, na.rm = TRUE)
  smooth_grate_1h <- rbind(smooth_grate_1h, c(i,time,mean_G,dev))
}

write.xlsx(growth_rate, paste0("growth_rate_", data_name), ".xlsx")
colnames(smooth_grate) <- c("cycle","time","mean_G","standart_deviation")
colnames(smooth_grate_1h) <- c("cycle","time","mean_G","standart_deviation")
write.xlsx(smooth_grate, paste0("smooth_grate_", data_name,".xlsx"))
write.xlsx(smooth_grate_1h, paste0("smooth_grate_1h_", data_name,".xlsx"))
y_max <- max((growth_rate$G_rate))
y_min <- min((growth_rate$G_rate))

#data_old <- data_old[2:600, ]
#growth_rate <- growth_rate[2:60, ]
#smooth_grate <- smooth_grate[2:60, ]
#smooth_grate_1h <- smooth_grate_1h[2:60,]
plot1 <- ggplot(data_old, aes(x = exp_time/3600, y = growth_rate ))+
  geom_line()+
  ylim(y_min, y_max)
 
plot1 <- plot1+labs(x ="Time", y = "Growth_rate")

plot2 <- ggplot(growth_rate,  aes(x = time, y = G_rate))+
  geom_line()+
  ylim(y_min, y_max)
plot2 <- plot2+labs(x ="Time", y = "Growth_rate")

plot3 <- ggplot(smooth_grate,  aes(x = time, y = mean_G))+
  geom_line()+
  ylim(y_min, y_max)+
  geom_errorbar(aes(ymin=mean_G-standart_deviation, ymax=mean_G+standart_deviation), width=.2,
                position=position_dodge(0.05))
plot3 <- plot3+labs(x ="Time", y = "Growth_rate")

plot4 <- ggplot(smooth_grate_1h,  aes(x = time, y = mean_G))+
  geom_line()+
  ylim(y_min, y_max)+
  geom_errorbar(aes(ymin=mean_G-standart_deviation, ymax=mean_G+standart_deviation), width=.2,
                position=position_dodge(0.05))
plot4 <- plot4+labs(x ="Time", y = "Growth_rate")

plot_grid(plot1, plot2, plot3,plot4, 
          labels = c("Chi.Bio data","raw calculations","smoothed growth rate","smoothed GR - 1h sliding window"), align = "h", scale = 1 )+
ggsave(paste0("Growth rate_", data_name,".png"),
       path = dir_name, width = 10, height = 5, device='png', dpi=700)
