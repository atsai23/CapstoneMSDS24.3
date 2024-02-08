library(tidyverse)

data <- read.csv('Data/daily-avg-tmp-23.csv')

sitedemo <- data %>% filter(Site == 'MS39' | Site == 'MS36')

sitedemo <- sitedemo %>% pivot_wider(names_from = 'Site', values_from = 'Temp')

save(sitedemo, file='sitedemo.rda')

load('sitedemo.rda')s