# Code to create 'total landings' figures 
# Load libraries ####
library(tidyverse)
library(gmRi)
library(ggpubr)
library(gt)
library(scales)
library(webshot2)
library(patchwork)

# Read in required data ####
ports        <- read.csv("data/ports_filtered.csv")
common_names <- read.csv("data/common_names.csv")
landings     <- read.csv("data/landings.csv")

me <- landings %>% filter(STATE == "ME") %>% arrange(PORT.NAME)
unique(me$PORT.NAME)

# Tidy GARFO Data ####
landings <- landings %>%
  dplyr::select(YEAR, PORT.NAME, STATE, SPPNAME, LANDED.LBS, LIVE.LBS, VALUE) %>%
  filter(YEAR >= 1985) %>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ") %>%
  filter(PORT %in% ports$PORT) %>% # | PORT %in% c("KITTERY, ME", "MT. DESERT, ME", "LUBEC, ME", "TREMONT, ME")) %>%
  mutate(LANDED.LBS = parse_number(LANDED.LBS),
         VALUE = parse_number(VALUE)) %>%
  drop_na() %>% # river herring in NJ messing everything up
  full_join(common_names)

unique(landings$PORT)

# Totals ####
total_annual <- landings %>% 
  group_by(PORT, YEAR) %>% 
  filter(!PORT %in% c("NEW LONDON, CT", "STONINGTON, CT")) %>% # will add back in with shortened time series
  summarise(LANDED.LBS = sum(LANDED.LBS),
            VALUE      = sum(VALUE)) %>% 
  mutate(across(c(LANDED.LBS, VALUE))/1000000) %>%
  nest(DATA = YEAR:VALUE) %>% 
  drop_na()

landings %>%
  filter(PORT %in% c("NEW LONDON, CT", "STONINGTON, CT") & YEAR >= 2003) %>%
  group_by(PORT, YEAR) %>% 
  summarise(LANDED.LBS = sum(LANDED.LBS),
            VALUE      = sum(VALUE)) %>% 
  mutate(across(c(LANDED.LBS, VALUE))/1000000) %>%
  nest(DATA = YEAR:VALUE) %>% 
  full_join(total_annual) -> total_annual

# Grabbing total averages for report text
total_averages <- landings %>%
  filter(YEAR %in% seq(2012,2021)) %>%
  group_by(PORT, YEAR) %>%
  summarise(TOTAL.LBS   = sum(LANDED.LBS),
            TOTAL.VALUE = sum(VALUE)) %>% 
  summarise(MEAN.LBS    = mean(TOTAL.LBS),
            MEAN.VALUE  = mean(TOTAL.VALUE)) %>%
  mutate(across(c(MEAN.LBS, MEAN.VALUE))/1000000)
  
yearly_averages <- total_annual %>%
  unnest(DATA) %>%
  arrange(PORT)

write_csv(total_averages, here("outputs", "annual_totals.csv"))
write_csv(yearly_averages, here("outputs", "yearly_averages.csv"))


# Volume and Value Plots ####
volume_plot <- function(df){
  plot <- ggplot(data = df) +
    geom_area(aes(x=YEAR, y=LANDED.LBS, fill = ""), alpha = 0.75)+
    #xlim(c(1985, 2021)) +
    ggtitle("All Species - Million lbs") +
    scale_fill_manual(values = "#00736D") +
    theme_gmri(legend.position = "none",
               axis.text = element_text(size = 20),
               axis.title = element_blank(),
               plot.title = element_text(size=25))
  
  return(plot)
}

value_plot <- function(df){
  plot <- ggplot(data = df) +
    geom_line(aes(x=YEAR, y=VALUE), color = "#EA4F12", linewidth = 2) +
    #xlim(c(1985, 2021)) +
    ggtitle("All Species - $US Million") +
    theme_gmri(legend.position = "none",
               axis.text = element_text(size = 20),
               axis.title = element_blank(),
               plot.title = element_text(size=25))
  
  return(plot)
}

total_annual %>% 
  mutate(VOLUME = map(DATA, volume_plot),
         VALUE  = map(DATA, value_plot)) -> total_annual

# Stitch plots together ####

totals_plot <- function(df, PORT){
  volume <- df$VOLUME
  value  <- df$VALUE
  list   <- c(volume, value)
  wrap   <- wrap_plots(list, ncol = 2)
  print(wrap)
  
  filename=paste(PORT, "landings" , sep="_")
  ggsave(wrap, file = paste("outputs/landings/totals/", filename, ".png", sep=""), width = 20, height = 8.5, units = "in")
}

total_annual <- total_annual %>%
  select(!DATA) %>%
  group_by(PORT) %>%
  nest() %>%
  mutate(PLOT = map2(data, PORT, totals_plot))
