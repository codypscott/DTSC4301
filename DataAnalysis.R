# install.packages("tidyverse", dependencies = TRUE)
#install.packages("mctest", dependencies=TRUE)

library(tidyverse)
library(mctest)

## full data 

## need to get relative path for this from repository
full_data <- read.csv("C:\\Users\\ricke\\OneDrive\\Desktop\\DTSC4301A\\DTSC4301\\Dataprep\\Full_Data.csv")


head(full_data, 5)

str(full_data)

names(full_data)

pre_2017_data <- subset(full_data, full_data$year < 2017)


post_2017_data <- subset(full_data, full_data$year > 2016)


######### hypothesis 1 data #########

## pre 2016-2017 year
pre_H1_data <- pre_2017_data[c('int_commcoll', 'int_pubsr', 'AP_part_pct', 'AP_pass_pct', 'enroll_Disadvantaged')]

## correlation matrix for pre 2016-17 data
cor(pre_H1_data, use="pairwise.complete.obs")

## linear model for pre 2016-17 data
pre_H1_model <- lm(enroll_Disadvantaged ~ int_commcoll + int_pubsr + AP_part_pct + AP_pass_pct,
                   data = pre_H1_data)

## VIF of pre 2016-17 model
imcdiag(pre_H1_model, method = "VIF", vif=5)


## post 2016-2017 year
post_H1_data <- post_2017_data[c('int_commcoll', 'int_pubsr', 'AP_part_pct', 'AP_pass_pct', 'enroll_Disadvantaged')]

## correlation matrix for post 2016-17 data
cor(post_H1_data,use="pairwise.complete.obs")

## linear model for post 2016-17 data
post_H1_model = lm(enroll_Disadvantaged ~ int_commcoll + int_pubsr + AP_part_pct + AP_pass_pct,
                   data = post_H1_data)
  
imcdiag(post_H1_model, method = "VIF", vif=5)




######### hypothesis 2 data #########

## pre 2016-2017 year
pre_H2_data <-pre_2017_data[c('CTE_enroll_pct', 'CTE_cred_pct', 'int_trdbusnrs' )]

## correlation matrix for pre 2016-17 data
cor(pre_H2_data,use="pairwise.complete.obs") ## getting NA because no data exists pre 2017-2018 school year

#######################################################################
########   CURRENTLY GETTING AN ERROR DUE TO COMPLETELY MISSING DATA  #
## linear model for pre 2016-17 data                                  #
pre_H2_model = lm(int_trdbusnrs ~ CTE_enroll_pct + CTE_cred_pct,      #
                   data = pre_H2_data)                                #
## VIF for pre 2016-17 data                                           #
imcdiag(pre_H2_model, method = "VIF", vif=5)                          #
#######################################################################

## post 2016-2017 year
post_H2_data <-post_2017_data[c('CTE_enroll_pct', 'CTE_cred_pct', 'int_trdbusnrs' )]

## correlation matrix for post 2016-17 data
cor(post_H2_data, use="pairwise.complete.obs")

## linear model for post 2016-17 data
post_H2_model = lm(int_trdbusnrs ~ CTE_enroll_pct + CTE_cred_pct,
                  data = post_H2_data)

## VIF for post 2016-17 data
imcdiag(post_H2_model, method = "VIF", vif=5)
