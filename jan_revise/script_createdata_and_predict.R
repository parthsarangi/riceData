setwd("/Users/admin1/Documents/projects/riceData/jan_revise")
df <- read.csv("ricedata_jan.csv")

dim(df)

max(df$year)

str(df)



library(dplyr)
df_sub1 <- df %>% dplyr::filter(year == 2556 | year == 2557 | year == 2558 | year == 2559) 


dim(df_sub1)

n_sub <- names(df_sub1)
n_sub

str(df_sub1)
summarise(df_sub1)
summary(df_sub1)

# need to use random function and 1st quart and 3rd quart of each feature and the generate random data
# then use the predict neural network from script_final_glm.R script

