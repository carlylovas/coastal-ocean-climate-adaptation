# results for all species 

# load libraries
library(tidyverse)
library(gmRi)
library(here)
library(patchwork)
library(gt)

# read in prepped results
results_table <- readRDS(here("data", "results_table.rds")) %>%
  mutate(us_customary = availability*5.71) 

results_table %>% 
  ungroup() %>% 
  select(region, Fahrenheit_Lab, us_customary, COMNAME, VULNERABILITY) %>%
  filter(!Fahrenheit_Lab == "0.0°F") %>%
  pivot_wider(names_from = Fahrenheit_Lab, values_from = us_customary) %>% 
  group_by(region) %>%
  nest() -> results_table

# map over region
results_table %>% 
  mutate(table = map(data, function(x){
    gt <- x %>%  
      gt() %>%
      cols_move_to_start(COMNAME) %>%
      cols_move_to_end(VULNERABILITY) %>%
      tab_header(title  = md("**Changes in Species Availability**")) %>%
      cols_label(COMNAME = md("**Species**"),
                 VULNERABILITY = md("**Climate Vulnerability** *")) %>%
      tab_spanner(label = md("**Temperature Thresholds**"), columns = !c(COMNAME, VULNERABILITY)) %>%
      tab_footnote(footnote = md("*Values given in pounds per square mile*")) %>%
      tab_footnote(footnote = md("**Hare et al. 2016*")) %>% 
      fmt_number(columns = everything(), n_sigfig = 2, decimal = 1)
  })) -> results_table 

# save out individual ports 
results_table %>% 
  mutate(file = map2(table, region, function(x,y){
      gt <- x
      filename=paste(region, "table", sep="_")
      gtsave(gt, filename = paste0(filename, ".png"), path = here("outputs", "species change"))
  })) -> results_table

