# Code to create 'what is landed here' figures
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


# Tidy GARFO landings data ####
landings <- landings %>%
  dplyr::select(YEAR, PORT.NAME, STATE, SPPNAME, LANDED.LBS, LIVE.LBS, VALUE) %>%
  filter(YEAR >= 2012) %>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ") %>%
  filter(PORT %in% ports$PORT) %>%
  mutate(LANDED.LBS = parse_number(LANDED.LBS),
         VALUE = parse_number(VALUE)) %>%
  drop_na()%>%
  full_join(common_names)

# Average lbs and $$ per species per port across all years (values in millions)
species_landings <- landings %>%
  group_by(PORT, SPPNAME, COMNAME) %>%
  nest() %>%
  mutate(AVG.LANDED.LBS = map_dbl(data, function(df){mean(df$LANDED.LBS, na.rm=TRUE)}),
         AVG.VALUE      = map_dbl(data, function(df){mean(df$VALUE, na.rm=TRUE)}))

# Ranks ####
weight_ranks <- species_landings %>%
  select(PORT, SPPNAME, COMNAME, data, AVG.LANDED.LBS) %>%
  arrange(desc(AVG.LANDED.LBS))%>%
  group_by(PORT)%>%
  nest() %>%
  mutate(rowid = map(data, rowid_to_column)) %>%
  select(!data)%>%
  unnest(rowid) %>%
  arrange(PORT)%>%
  rename("WEIGHT.RANK" = "rowid") 

value_ranks <- species_landings %>%
  select(PORT, SPPNAME, COMNAME, data, AVG.VALUE) %>%
  arrange(desc(AVG.VALUE))%>%
  group_by(PORT)%>%
  nest() %>%
  mutate(rowid = map(data, rowid_to_column)) %>%
  select(!data)%>%
  unnest(rowid) %>%
  arrange(PORT)%>%
  rename("VALUE.RANK" = "rowid") 

species_ranks <- weight_ranks %>%
  full_join(value_ranks) %>%
  relocate("data", .after = "PORT") %>%
  relocate("VALUE.RANK", .after = "WEIGHT.RANK") %>%
  rename("DATA" = "data") 

species_ranks %>%
  filter(WEIGHT.RANK %in% seq(1,10) & (VALUE.RANK %in% seq(1,10))) %>%
  ungroup(PORT) %>%
  select(COMNAME) %>% distinct()

others <- species_ranks %>%
  filter(WEIGHT.RANK > 10 & VALUE.RANK > 10) %>%
  unnest(DATA) %>% 
  group_by(PORT) %>%
  summarise(AVG.LANDED.LBS = sum(AVG.LANDED.LBS),
            AVG.VALUE = sum(AVG.VALUE))%>%
  mutate(across(where(is.numeric), round, 1)) %>%
  mutate(COMNAME = "Other") %>%
  dplyr::filter(!PORT == "NA") 

# Plots ####
## donuts ####

gmri <- c("darkred", "orangered", "gold", "olivedrab", "purple", "forestgreen", 
          "darkcyan", "steelblue", "midnightblue", "pink", "darkgray")

plots <- species_ranks %>%
  filter(WEIGHT.RANK %in% seq(1,10) & (VALUE.RANK %in% seq(1,10))) %>%
  dplyr::select(PORT, COMNAME, AVG.VALUE, AVG.LANDED.LBS) %>%
  full_join(others) %>%
  mutate(RANK = (COMNAME %in% c("Confidential", "Other"))) %>%
  mutate(OTHER = (COMNAME == "Other")) %>%
  arrange(desc(AVG.VALUE)) %>%
  arrange(RANK) %>%
  arrange(OTHER) %>%
  group_by(PORT) %>%
  nest() %>% 
  filter(PORT %in% c("STONINGTON, ME",
                     "PORTLAND, ME",
                     "PORTSMOUTH, NH",
                     "GLOUCESTER, MA",
                     "BOSTON, MA",
                     "SCITUATE, MA",
                     "CHATHAM, MA",
                     "NEW BEDFORD, MA",
                     "POINT JUDITH, RI",
                     "NEWPORT, RI",
                     "STONINGTON, CT",
                     "NEW LONDON, CT",
                     "MONTAUK, NY",
                     "POINT PLEASANT, NJ",
                     "CAPE MAY, NJ",
                     "NEWPORT NEWS, VA")) 

# plotting function {ggpubr} ####
donut_func <- function(x){
  
  val_labs <- paste("Value", "($US million)", sep = "\n")
  weight_labs <- paste("Weight", "(million lbs)", sep = "\n")
  
  value_donut <- 
    ggdonutchart(x, "AVG.VALUE", fill = "COMNAME", group = rowid, color = "#FFFFFF", lab.font = c(0.25, "plain", "transparent")) +
    scale_fill_manual(values = gmri) +
    theme(legend.position="none",
          legend.title = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(size =30),
          plot.margin = margin(-1,-1,-1,-1, "cm")) +
    annotate("text", label = val_labs, x = .5, y= 0, size = 8)
  
  weight_donut <- 
    ggdonutchart(x, "AVG.LANDED.LBS", fill = "COMNAME", color = "#FFFFFF", lab.font = c(0.25, "plain", "transparent")) +
    scale_fill_manual(values = gmri) +
    theme(legend.position="none",
          legend.title = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(size =30),
          plot.margin = margin(-1,-1,-1,-1, "cm")) +
    annotate("text", label = weight_labs, x = .5, y= 0, size = 8)
  
  donut <- ggarrange(value_donut, weight_donut, ncol = 2, common.legend = F)
  return(donut)
}

## table ####
gt_table <-function(x){
  gt <- x %>%
    mutate(color ="") %>%
    select(!RANK) %>%
    select(!OTHER) %>%
    gt() %>%
    tab_style(
      style = cell_text(size = "x-large"),
      locations = cells_body(columns = everything(), rows = everything())) %>%
    tab_style(
      style = cell_text(size = "xx-large"),
      locations = cells_column_labels(columns = everything()))%>%
    tab_style(
      style = cell_text(size = "x-large"),
      locations = cells_footnotes()) %>%
    fmt_currency(columns = AVG.VALUE, decimals = 0) %>%
    fmt_number(columns = AVG.LANDED.LBS, decimals = 0, pattern = "{x} lbs") %>%
    cols_width(color ~px(25)) %>%     
    data_color(
      columns = COMNAME,
      target_columns = color,
      method = "factor",
      palette = gmri) %>%
    cols_label(
      COMNAME = md("**Species**"),
      AVG.VALUE = md("**Annual Total Value**"),
      AVG.LANDED.LBS = md("**Annual Total Volume**"),
      color = ""
    ) %>%
    tab_footnote("Above are the annual total averages for value and landed volume from 2012 to 2021 for the top species landed at this port")
} 

plots %>%
  mutate(DONUT = map(data, donut_func),
         TABLE = map(data, gt_table)) -> plots


# Save gt ####
# gt renders html, so we need to save the tables as .png in order to stitch it to the table

save_gt <- function(TABLE, PORT){
  gt <- TABLE
  filename=paste(PORT, "table", sep="_")
  gtsave(gt, file = paste0(filename, ".png"), path = "outputs/landings/tables")
}

plots <- plots %>%
  mutate(IMAGE = map2(TABLE, PORT, save_gt))

# Read back in as png, wrap and attach to donuts
path <- "outputs/landings/tables"

read_png <- function(file_name) {
  out <- png::readPNG(file_name, native = TRUE) 
  return(out)
}

file_path <- tibble(FILE_PATH = list.files(path, pattern = "_table.png", full.names = TRUE)) %>%
  mutate(PNG = map(FILE_PATH, read_png))

plots <- plots %>% 
  arrange(PORT) %>% 
  cbind(file_path) %>%
  group_by(PORT) %>%
  nest()

# Final function to stitch png and donut together and save out 
donut_table <- function(df, PORT){
  chart <- df$DONUT[[1]]
  table <- df$PNG[[1]]
  table <- patchwork::wrap_elements(table)
  plot  <- wrap_plots(table, chart)
  filename <- paste0(PORT, ".png")
  ggsave(plot, file = paste0("outputs/landings/final/", filename), height = 8.7, width = 16.9, units="in", bg = "transparent")
}

plots <- plots %>%
  mutate(PLOT = map2(data, PORT, donut_table))
