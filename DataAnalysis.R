# install.packages("tidyverse", dependencies = TRUE)
# install.packages("car", dependencies=TRUE)
# install.packages("olsrr", dependencies=TRUE)

library(tidyverse)
library(car)

## full data 

## need to get relative path for this from repository
full_data <- read.csv("C:\\Users\\ricke\\OneDrive\\Desktop\\DTSC4301A\\DTSC4301\\Dataprep\\Full_Data.csv")


head(full_data, 5)

names(full_data)



## hypothesis 1 data
H1_data <- full_data[c('int_commcoll', 'int_pubsr', 'AP_part_pct', 'AP_pass_pct', 'enroll_Disadvantaged')]
cor(na.omit(H1_data))



## hypothesis 2 data
H2_data <-full_data[c('CTE_enroll_pct', 'CTE_cred_pct', 'int_trdbusnrs' )]
cor(na.omit(H2_data))

