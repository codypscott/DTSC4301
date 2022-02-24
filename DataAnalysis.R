# install.packages("tidyverse", dependencies = TRUE)
install.packages("car", dependencies=TRUE)
install.packages("olsrr", dependencies=TRUE)

library(tidyverse)
library(ggplot2)  ## for the graphics


full_data <- read.csv("C:\\Users\\ricke\\OneDrive\\Desktop\\DTSC4301A\\DTSC4301\\Dataprep\\Full_Data.csv")


head(full_data, 5)

names(full_data)

model_data <- data.frame(full_data)

model_data$X <- model_data$agency_code <- model_data$year <- model_data$school <- model_data$zip_code <- model_data$X2019_med_hh_inc <- model_data$category_code <- NULL

model_data

cor(na.omit(model_data))
