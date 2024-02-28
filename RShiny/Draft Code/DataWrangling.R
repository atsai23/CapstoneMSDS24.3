library(tidyverse)

data <- read.csv('Data/mergetemp_test.csv')

mergedemo <- data %>% pivot_wider(names_from = 'ward5', values_from = 'Temp')

save(mergedemo, file='mergedemo_wide.rda')

load('mergedemo.rda')
