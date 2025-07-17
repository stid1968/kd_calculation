library(tidyverse)
library(readxl)
library(BioRssay)
library(DT)

data = read_excel("/Users/dia/Etudiants/Ndeye Marie Sene/kd_estimates/data_pyrethroids_all.xlsx",
                  sheet = "Grand Yoff")
b1 = data %>% 
  select(site, family, insecticide, dose, batch1, n1) %>% 
  pivot_longer(cols = batch1) %>% 
  rename(total = "n1")

b2 = data %>% 
  select(site, family, insecticide, dose, batch2, n2) %>% 
  pivot_longer(cols = batch2) %>% 
  rename(total = "n2")

b3 = data %>% 
  select(site, family, insecticide, dose, batch3, n3) %>% 
  pivot_longer(cols = batch3) %>% 
  rename(total = "n3")

b4 = data %>% 
  select(site, family, insecticide, dose, batch4, n4) %>% 
  pivot_longer(cols = batch4) %>% 
  rename(total = "n4")

data = rbind(b1, b2, b3, b4)



data %<>% 
  rename(replicate = "name",
         dead = "value") %>% 
  mutate(replicate = str_replace(replicate, "batch", "")) %>% 
  select(insecticide, dose, total, replicate, dead) %>% 
  rename(strain = "insecticide")

transd <- probit.trans(data)
data = resist.ratio(data, conf.level = 0.95, KD.value = c(50, 95))
data = as.data.frame(data)

data %>%
  select(LD50, LD50min, LD50max, LD95, LD95min, LD95max) %>%
  rename(
    KD50 = "LD50",
    KD50min = "LD50min",
    KD50max = "LD50max",
    KD95 = "LD95",
    KD95min = "LD95min",
    KD95max = "LD95max"
  ) %>%
  
  mutate(
    KD50_IC95 = paste(KD50min, KD50max, sep = "-"),
    KD95_IC95 = paste(KD95min, KD95max, sep = "-")
  ) %>%
  select(KD50, KD95, KD50_IC95, KD95_IC95) %>% 
  datatable(data, extensions = 'Buttons',
            options = list(dom='Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'print', 'pdf')))