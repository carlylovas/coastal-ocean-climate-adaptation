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

# me <- landings %>% filter(STATE == "ME") %>% arrange(PORT.NAME)
# unique(me$PORT.NAME)

landings <- landings %>%
  select(YEAR, PORT.NAME, STATE, SPPNAME, LANDED.LBS, VALUE) %>%
  filter(YEAR >= 1985) %>%
  # filter(YEAR %in% seq(2012,2021)) %>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ") %>%
  filter(PORT %in% ports$PORT) %>%
  mutate(LANDED.LBS = parse_number(LANDED.LBS),
         VALUE = parse_number(VALUE)) %>%
  filter(!SPPNAME == "HERRING, RIVER") %>% # only shows up in Cape May in 2015, has no value 
  left_join(common_names)

# Totals ####
total_annual <- landings %>% 
  filter(!PORT %in% c("NEW LONDON, CT", "STONINGTON, CT")) %>% # will add back in with shortened time series
  group_by(PORT, YEAR) %>% 
  summarise(LANDED.LBS = sum(LANDED.LBS, na.rm = T)/1000000,
            VALUE      = sum(VALUE, na.rm = T)/1000000) %>% 
  nest(DATA = YEAR:VALUE) 

landings %>%
  filter(PORT %in% c("NEW LONDON, CT", "STONINGTON, CT") & YEAR >= 2003) %>%
  group_by(PORT, YEAR) %>% 
  summarise(LANDED.LBS = sum(LANDED.LBS, na.rm = T)/1000000,
            VALUE      = sum(VALUE, na.rm = T)/1000000) %>% 
  nest(DATA = YEAR:VALUE) %>% 
  full_join(total_annual) -> total_annual
  

# Volume and Value Plots ####
volume_plot <- function(df){
  plot <- ggplot(data = df) +
    geom_area(aes(x=YEAR, y=LANDED.LBS, fill = ""), alpha = 0.75)+
    #xlim(c(1985, 2021)) +
    ggtitle("All Species - Million lbs") +
    scale_fill_manual(values = "#00736D") +
    theme_gmri(legend.position = "none",
               axis.text = element_text(size = 17),
               axis.title = element_blank(),
               plot.title = element_text(size=23))
  
  return(plot)
}

value_plot <- function(df){
  plot <- ggplot(data = df) +
    geom_line(aes(x=YEAR, y=VALUE), color = "#EA4F12", linewidth = 2) +
    #xlim(c(1985, 2021)) +
    ggtitle("All Species - $US Million") +
    theme_gmri(legend.position = "none",
               axis.text = element_text(size = 17),
               axis.title = element_blank(),
               plot.title = element_text(size=23))
  
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
  
  filename=paste(PORT, "totals" , sep="_")
  ggsave(wrap, file = paste("outputs/landings/totals/", filename, ".png", sep=""), width = 18, height = 4.5, units = "in")
}

total_annual <- total_annual %>%
  select(!DATA) %>%
  group_by(PORT) %>%
  nest() %>%
  mutate(PLOT = map2(data, PORT, totals_plot))
