library(tidyverse)
require(matrixStats)
require(readxl)
theme_set(theme_MI_clear())

#this is an IPUMS extract, containing Ancestry and HH income, as well as household weights, for the 2021 ACS
#acs <- read.csv("Ancestry and HH Income.csv")
ancestry_codes <- readxl::read_excel("Ancestry Codes.xlsx")

acs %>%
  filter(HHINCOME < 9999999,
         ANCESTR1 < 999) %>%
  group_by(ANCESTR1) %>%
  summarise(HHINCOME = matrixStats::weightedMedian(HHINCOME, HHWT)) %>%
  left_join(ancestry_codes, by = c('ANCESTR1' = 'Code')) %>%
  write.csv("Ancestry and HH Income_Summary.csv", row.names = F)