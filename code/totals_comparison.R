# By species
landings %>%
  group_by(PORT, SPPNAME) %>%
  summarise(AVG.LANDED.LBS = mean(LANDED.LBS),
            AVG.VALUE      = mean(VALUE)) %>%
  summarise(TOTAL.LBS = (sum(AVG.LANDED.LBS))/1000000,
            TOTAL.VAL = (sum(AVG.VALUE))/1000000) -> by_species

# By port 
landings %>%
  group_by(PORT, YEAR) %>%
  summarise(LANDED.LBS = sum(LANDED.LBS, na.rm = T),
            VALUE      = sum(VALUE, na.rm = T)) %>% 
  group_by(PORT) %>%
  summarise(value_by_port  = (mean(VALUE))/1000000,
            volume_by_port = (mean(LANDED.LBS))/1000000) -> by_port


# Combine 
by_species %>% 
  rename("volume_by_species" = "TOTAL.LBS",
         "value_by_species"  = "TOTAL.VAL") %>% 
  left_join(by_port) %>% 
  rename("volume_by_port" = "TOTAL.LBS",
         "value_by_port"  = "TOTAL.VAL") -> totals

# Save out 
write_csv(totals, "totals_comparison.csv")


## Okay so averages at the species level vary in the number of samples; some years, a species is present in all years, others just a few.

landings %>% 
  select(PORT,YEAR,SPPNAME) %>% 
  distinct() %>%
  group_by(PORT, YEAR) %>% 
  nest() %>% 
  mutate(NUM_SPP = map_dbl(data, function(x){nrow(x)})) %>% 
  arrange(PORT) -> num_spp

## If we force NAs on the years of no data for species, would that count as 10 observations?

landings %>%
  select(SPPNAME) %>%
  distinct() %>% 
  # nest(all_species = SPPNAME) %>% 
  mutate(LANDED.LBS = NA,
         VALUE = NA) -> all_species

landings %>% 
  group_by(PORT) %>%
  full_join(all_species)

species_ranks %>% 
  select(PORT, SPPNAME) %>% 
  left_join(landings) %>% 
  group_by(PORT, SPPNAME) %>% 
  nest() %>% 
  mutate(n_years = map_dbl(data, function(x){nrow(x)})) %>% 
  drop_na() %>%
  select(!data) -> species_count

write_csv(species_count, "selected_species_availability.csv")

### sum species, divide by 10 
landings %>% 
  group_by(PORT, SPPNAME) %>% 
  summarise(total_lbs = sum(LANDED.LBS),
            total_value = sum(VALUE),
            avg_volume = (total_lbs/10), # forced average (sorta; assumes zero for years of no data)
            avg_value = (total_value/10)) %>% 
  group_by(PORT) %>%
  summarise(volume_by_species = sum(avg_volume)/1000000,
            value_by_species = sum(avg_value)/1000000) -> species_by_10

species_by_10 %>% 
  full_join(by_port) %>%
  relocate("value_by_port", .after = "volume_by_port") -> combined_totals

write_csv(combined_totals, "combined_totals.csv")
