# Prep script to combine VAST output results, community footprints, temperature thresholds, common names, baselines,
# species of interest and emerging opportunities for plotting script

# Load libraries
library(tidyverse)
library(gmRi)
library(here)

# Read in model outputs which have been previously cropped to non-confidential VTR fishing "footprints"
project_box_path              <- "/Users/clovas/Library/CloudStorage/Box-Box/Mills Lab/Projects/COCA19_Projections/"
projection_res_path           <- paste0(project_box_path, "community_projections/")
community_footprints_annual   <- paste0(projection_res_path, "annual_community_footprint_projections.csv")
community_footprints_baseline <- paste0(projection_res_path, "baseline_community_footprint_projections.csv")
stat_areas_annual             <- paste0(projection_res_path, "annual_small_footprint_szone_overlap_projections.csv")
stat_areas_baseline           <- paste0(projection_res_path, "baseline_small_footprint_szone_overlap_projections.csv")

community_results   <- read_csv(community_footprints_annual) %>% filter(!region %in% c("PORTSMOUTH, NH", "SCITUATE, MA", "STONINGTON, ME"))
community_baselines <- read_csv(community_footprints_baseline) %>% filter(!region %in% c("PORTSMOUTH, NH", "SCITUATE, MA", "STONINGTON, ME"))
stat_area_results   <- read_csv(stat_areas_annual)
stat_baselines      <- read_csv(stat_areas_baseline)

# Read in common names
common_names <- read_csv(here("data", "common_names.csv")) %>% 
  mutate(species = tolower(Common_Name)) # to match Adam's outputs

# Filter scenario (SSP5) of interest and group by port, species and year. Average across seasons. 
community_results %>% 
  full_join(stat_area_results) %>%
  filter(scenario == "CMIP6_SSP5_85") %>%
  group_by(region, species, Year) %>%
  summarise(availability = mean(mean_Prob_0.5)) -> community_results

# Read in sea surface temperature crossing points from A. Kemberling
crossing_points <- read_csv(here("data", "crossing_points_ssp5.csv")) 

crossing_points %>%
  mutate(Decade = 10*Year %/% 10) %>%
  rename("Threshold_Year" = "Year") %>%
  mutate(Celsius_Labels = factor(Threshold, label = c(bquote('0.5\u00B0C'),
                                                        bquote('1.0\u00B0C'),
                                                        bquote('1.5\u00B0C'),
                                                        bquote('2.0\u00B0C'),
                                                        bquote('3.0\u00B0C'),
                                                        bquote('4.0\u00B0C')))) %>% 
  mutate(Fahrenheit     = (Threshold*(9/5)),
         Fahrenheit_Lab = factor(Fahrenheit, label =c(bquote('0.9\u00B0F'),
                                                      bquote('1.8\u00B0F'),
                                                      bquote('2.7\u00B0F'),
                                                      bquote('3.6\u00B0F'),
                                                      bquote('5.4\u00B0F'),
                                                      bquote('7.2\u00B0F')))) -> crossing_points

## Crossing points table ####
library(gt)
# crossing_points %>% 
#   # filter(Scenario == "CMIP6-SSP5_85" & Variable == "SST") %>%
#   select(Threshold_Year, Celsius_Labels, Fahrenheit_Lab) %>%
#   relocate(Threshold_Year, .after = Fahrenheit_Lab) %>%
#   gt() %>%
#   tab_header(title = md("**Temperature Crossing Points**"), 
#              subtitle = md("*Based on SSP5-8.5 Climate Projections*")) %>%
#   cols_label(
#     Celsius_Labels = md("**Celsius**"),
#     Fahrenheit_Lab   = md("**Fahrenheit**"),
#     Threshold_Year   = md("**As soon as**")) %>% 
#   tab_style(
#     style = cell_text(size = "large"),
#     locations = cells_body() 
#   ) %>%
#   tab_style(
#     style = cell_text(size = "x-large"),
#     locations = cells_title(groups = c("title", "subtitle"))) %>%
#   tab_style(
#     style = cell_text(size = "large"),
#     locations = cells_column_labels(columns = everything())) %>%
#   cols_width(everything()~px(110)) -> equiv_table
# 
# gtsave(equiv_table, "crossing_points_table.png")

# wide crossing points table
# crossing_points_wide <- read_csv(here("data", "crossing_points_wide.csv"))

# crossing_points_wide %>% 
#   gt() %>% 
#   tab_header(title = md("**Temperature Crossing Points**"), 
#              subtitle = md("*Based on SSP5-8.5 Climate Projections*")) 
  

# Baselines
community_baselines %>% 
  full_join(stat_baselines) %>%
  filter(scenario == "CMIP6_SSP5_85") %>%
  group_by(region, species, Year) %>%
  summarise(availability = mean(mean_Prob_0.5)) %>%
  mutate(Year = 2019,
         Threshold = 0.0,
         Threshold_Labels = factor(Threshold, label = c(bquote('0.0\u00B0C'))),
         Fahrenheit_Lab = factor(Threshold, label = c(bquote('0.0\u00B0F')))) -> community_baselines 

# Filter out "threshold years" and attach baseline data 
community_results %>% 
  filter(Year >= 2030) %>% 
  mutate(Decade = 10*Year %/% 10) %>% 
  full_join(crossing_points) %>%
  filter(Year == Threshold_Year) %>%
  full_join(community_baselines) %>%
  mutate(region = str_remove(region, " - statistical zone overlap")) %>%
  arrange(region, Threshold) -> all_results

all_results %>% 
  left_join(common_names) -> results_table

# Save out for change table 
write_rds(results_table, file = here("data", "results_table.rds"))

# Increasing species (emerging opportunities)
all_results %>%
  select(region, species, Threshold, availability) %>%
  filter(Threshold %in% c("0", "2")) %>%
  distinct() %>%
  pivot_wider(names_from = Threshold, values_from = availability) %>%
  rename("baseline" = "0", "two" = "2") %>%
  mutate(Perc_Change = ((two-baseline)/baseline * 100)) %>% 
  filter(Perc_Change >= 100) %>%
  arrange(desc(Perc_Change)) -> emerging_species # may wanna check on this one...

emerging_species %>%
  select(region, species) %>%
  left_join(common_names) %>%
  select(region, species, COMNAME) -> emerging_species

# Read in selected species by port and attach emerging species ## 3/15 Note: Drop surf clam & ocean quahog
select_species <- read_csv(here("data", "ports_filtered.csv")) %>%
  rename("region" = "PORT") %>%
  left_join(common_names) %>%
  filter(!COMNAME %in% c("Surf clam", "Ocean quahog clam", "Hagfish", "Atlantic menhaden"))

select_species %>%
  full_join(emerging_species) %>% 
  filter(!COMNAME %in% c("Surf clam", "Ocean quahog clam", "Hagfish", "Atlantic menhaden")) %>%
  select(region, species, COMNAME) -> select_species

# US Density (Availability * 5.71)
select_species %>%
  left_join(all_results) %>%
  mutate(us_density = (availability*5.71),
         Fahrenheit_Lab = as.character(Fahrenheit_Lab)) %>%
  arrange(Threshold) -> all_communities # ugh

# save out as rds to plot
write_rds(all_communities, file = here("data", "all_communities.rds"))

