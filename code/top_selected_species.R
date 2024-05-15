# Top selected species landed at each port 
## "species change over time" figure

# Load libraries ####
library(tidyverse)
library(patchwork)
library(gmRi)
library(here)

# Read in required data ####
select_species  <- read.csv("data/ports_filtered.csv")
common_names    <- read.csv("data/common_names.csv")
garfo_landings  <- read.csv("data/landings.csv")

# Tidy GARFO landings data ####
landings <- garfo_landings %>%
  dplyr::select(YEAR, PORT.NAME, STATE, SPPNAME, LANDED.LBS, LIVE.LBS, VALUE) %>%
  filter(YEAR >= 1995) %>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ") %>%
  filter(PORT %in% select_species$PORT) %>%
  filter(!PORT %in% c("NEW LONDON, CT", "STONINGTON, CT")) %>% # will add back in 
  mutate(LANDED.LBS = parse_number(LANDED.LBS),
         VALUE = parse_number(VALUE)) %>%
  drop_na()%>%
  full_join(common_names)

# Filter landings data to select species ####
garfo_landings %>%
  dplyr::select(YEAR, PORT.NAME, STATE, SPPNAME, LANDED.LBS, LIVE.LBS, VALUE) %>%
  filter(YEAR >= 2003) %>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ") %>%
  filter(PORT %in% c("NEW LONDON, CT", "STONINGTON, CT")) %>% 
  mutate(LANDED.LBS = parse_number(LANDED.LBS),
         VALUE = parse_number(VALUE)) %>%
  drop_na()%>%
  full_join(common_names) %>%
  full_join(landings) %>%
  select(PORT, YEAR, LANDED.LBS, COMNAME) %>%
  right_join(select_species) %>%
  group_by(PORT, COMNAME) %>%
  mutate(LANDED.LBS = (LANDED.LBS/1000000)) %>%
  nest() %>% 
  arrange(COMNAME) %>% 
  filter(!COMNAME == "Atlantic menhaden") -> select_species

# Plot ####
plot <- function(x,y){
   out <- ggplot(data = x) +
    geom_col(aes(x=YEAR, y=LANDED.LBS), fill = "#00736D", linewidth = 2, alpha = 0.75)+  
    ggtitle(y) +
    scale_fill_manual(values = "#00736D") +
    theme_gmri(legend.position = "none",
               axis.text = element_text(size = 20),
               axis.title = element_blank(), 
               plot.title = element_text(size=25))
  
  return(out)
}

patchwork <-function(x, PORT){
  list <- x$plot
  wrap <- wrap_plots(list, ncol = 3)
  print(wrap)
  
  filename = paste(PORT, "landings" , sep="_")
  ggsave(wrap, file = paste("outputs/landings/selected species/", filename, ".png", sep=""), width = 20, height = 8.5, units = "in")
}

select_species %>% 
  mutate(plot = map2(data, COMNAME, plot)) %>%
  group_by(PORT) %>%
  nest() %>%
  arrange(PORT) %>%
  mutate(patchwork = map2(data, PORT, patchwork)) 
