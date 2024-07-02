# results for all species 

# load libraries
library(tidyverse)
library(gmRi)
library(here)
library(patchwork)
library(gt)

# read in prepped results
results_table <- readRDS(here("data", "results_table.rds"))

### June 4 revisions
# percent change in results table
results_table %>%
  select(region, species, availability, Threshold) %>%
  filter(Threshold == 0) %>%
  pivot_wider(names_from = Threshold, values_from = availability) %>%
  rename("baseline" = "0") %>%
  mutate(baseline = baseline*5.71) %>% # us units
  left_join(results_table %>% filter(!Threshold == 0) %>% mutate(availability = availability*5.71)) %>% # us units
  mutate(percent_change = ((availability-baseline)/baseline)) -> results_table # fmt_percent function will divide by 100

results_table %>% 
  ungroup() %>% 
  # select(region, Fahrenheit_Lab, us_customary, COMNAME, VULNERABILITY) %>%
  # filter(!Fahrenheit_Lab == "0.0°F") %>%
  select(region, Fahrenheit_Lab, baseline, percent_change, COMNAME, VULNERABILITY) %>%
  filter(Fahrenheit_Lab %in% c("0.9°F","1.8°F", "3.6°F", "5.4°F")) %>%
  pivot_wider(names_from = Fahrenheit_Lab, values_from = percent_change) %>% 
  separate(region, c("region", "state"), sep = ",") %>%
  mutate(baseline = as.numeric(baseline),
         region = str_to_title(region),
         title = paste("Projected Changes in Species Availability in", region)) %>%
  arrange(region,COMNAME) %>%
  group_by(region, state, title) %>%
  nest() -> results_table

# map over region
results_table %>% 
  mutate(table = map2(data, title, function(x,y){
    gt <- x %>%  
      gt() %>%
      cols_move_to_start(COMNAME) %>%
      tab_style(
        style = cell_text(size = "large"),
        locations = cells_title(groups = "subtitle")) %>%
      cols_move_to_end(VULNERABILITY) %>%
      tab_header(title  = title, subtitle = md("Values represent percent change in modeled species availability at potential<br>
      levels of warming relative to *2010-2019* baseline conditions.<br>
                                               Species in gray had low availability *(<5 lbs/sq. mile)* during the baseline period.")) %>%
      cols_label(COMNAME = md("**Species**")) %>% 
      cols_width(COMNAME ~px(150)) %>% 
      tab_spanner(label = md("**Increase in Sea Surface Temperature**"), columns = !c(COMNAME, baseline, VULNERABILITY)) %>%
      fmt_number(columns = everything(), n_sigfig = 2, decimal = 1)  %>% 
      fmt_percent(columns = c("0.9°F","1.8°F", "3.6°F", "5.4°F"), decimal = 1) %>% 
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_title(groups = "title")
      ) %>% 
      tab_style(
        style = cell_text(color = "#848884") ,
        locations = cells_body(
          columns = c(COMNAME, "0.9°F","1.8°F", "3.6°F", "5.4°F"),
          rows = baseline < 5)) %>% 
      cols_hide(c(baseline, VULNERABILITY))
  })) -> results_table 

gtsave(results_table$table[[1]], "example_0606.png")

# save out individual ports 
results_table %>% 
  mutate(file = pmap(list(table, region, state), function(x,y,z){
      gt <- x
      filename=paste(region, state, "table", sep="_")
      gtsave(gt, filename = paste0(filename, ".png"), path = here("outputs", "species change"))
  })) -> results_table

