# Code to create 'what is landed here' figures
# Load libraries ####
library(tidyverse)
library(gmRi)
library(ggpubr)
library(gt)
library(scales)
library(webshot2)
library(patchwork)
library(here)

# Read in required data ####
ports        <- read.csv("data/ports_filtered.csv")
common_names <- read.csv("data/common_names.csv")
landings     <- read.csv("data/landings.csv")

# Tidy GARFO landings data ####
landings <- landings %>%
  select(YEAR, PORT.NAME, STATE, SPPNAME, LANDED.LBS, VALUE) %>%
  # filter(YEAR %in% seq(2012,2021)) %>%
  unite(PORT.NAME, STATE, col="PORT", sep=", ") %>%
  # filter(PORT %in% ports$PORT) %>%
  mutate(LANDED.LBS = parse_number(LANDED.LBS),
         VALUE = parse_number(VALUE)) %>%
  filter(!SPPNAME == "HERRING, RIVER") %>% # only shows up in Cape May in 2015, has no value 
  left_join(common_names)
  
landings %>% 
  filter(PORT %in% c("KITTERY, ME", "TREMONT, ME", "LUBEC, ME")) %>% 
  select(YEAR, PORT, SPPNAME, COMNAME, LANDED.LBS, VALUE) %>% 
  arrange(PORT) -> steph

write.csv(steph, "kittery_tremont_lubec_landings.csv")

# Average lbs and $$ per species per port across all years 
# Not all species have 10 years worth of data...
species_landings <- landings %>%
  group_by(PORT, SPPNAME, COMNAME) %>%
  summarise(AVG.LANDED.LBS = mean(LANDED.LBS),
            AVG.VALUE      = mean(VALUE))

# Ranks ####
weight_ranks <- species_landings %>%
  select(PORT, SPPNAME, COMNAME, AVG.LANDED.LBS) %>%
  arrange(desc(AVG.LANDED.LBS))%>%
  group_by(PORT)%>%
  nest() %>%
  mutate(rowid = map(data, rowid_to_column)) %>%
  select(!data)%>%
  unnest(rowid) %>%
  arrange(PORT)%>%
  rename("WEIGHT.RANK" = "rowid") 

value_ranks <- species_landings %>%
  select(PORT, SPPNAME, COMNAME, AVG.VALUE) %>%
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
  relocate("VALUE.RANK", .after = "WEIGHT.RANK")  %>%
  filter(WEIGHT.RANK %in% seq(1,10) & (VALUE.RANK %in% seq(1,10)))

species_ranks %>% 
  select(PORT, SPPNAME) %>% 
  left_join(landings) %>% 
  group_by(PORT, SPPNAME) %>% 
  nest() %>% 
  mutate(n_years = map_dbl(data, function(x){nrow(x)})) %>% 
  select(!data) %>% 
  filter(n_years == 10) -> species_count

species_count %>% 
  left_join(species_ranks) %>%
  filter(!SPPNAME == "CONFIDENTIAL") -> species_ranks

others <- landings %>% 
  anti_join(species_ranks) %>%
  group_by(PORT) %>% 
  summarise(AVG.LANDED.LBS = sum(LANDED.LBS)/10, 
            AVG.VALUE = sum(VALUE)/10,
            COMNAME = "Other")

# Plots ####
## donuts ####

gmri <- c("orangered", "gold", "olivedrab","darkred", "purple", "forestgreen", 
          "darkcyan", "steelblue", "midnightblue", "pink", "darkgray")

plots <- species_ranks %>%
  dplyr::select(PORT, COMNAME, AVG.VALUE, AVG.LANDED.LBS) %>%
  full_join(others) %>%
  mutate(OTHER = (COMNAME == "Other")) %>%
  arrange(desc(AVG.VALUE)) %>%
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

## totals for reports
# plots %>%
  # group_by(PORT) %>%
  # summarise(TOTAL_LBS = map_dbl(data, function(x){(sum(x$AVG.LANDED.LBS))/1000000}),
         # TOTAL_VAL = map_dbl(data, function(x){sum(x$AVG.VALUE)})/1000000) -> totals

# write.csv(totals, here("outputs", "totals_from_landings.csv"))

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
    annotate("text", label = val_labs, x = .5, y= 0, size = 7.5)
  
  weight_donut <- 
    ggdonutchart(x, "AVG.LANDED.LBS", fill = "COMNAME", color = "#FFFFFF", lab.font = c(0.25, "plain", "transparent")) +
    scale_fill_manual(values = gmri) +
    theme(legend.position="none",
          legend.title = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(size =30),
          plot.margin = margin(-1,-1,-1,-1, "cm")) +
    annotate("text", label = weight_labs, x = .5, y= 0, size = 7.5)
  
  donut <- ggarrange(value_donut, weight_donut, ncol = 2, common.legend = F)
  return(donut)
}

## table ####
gt_table <-function(x){
  gt <- x %>%
    mutate(color ="") %>%
    # select(!RANK) %>%
    select(!OTHER) %>%
    gt() %>%
    tab_style(
      style = cell_text(size = "xx-large"),
      locations = cells_body(columns = everything(), rows = everything())) %>%
    tab_style(
      style = cell_text(size = "xx-large"),
      locations = cells_column_labels(columns = everything()))%>%
    tab_style(
      style = cell_text(size = "xx-large"),
      locations = cells_footnotes()) %>%
    fmt_currency(columns = AVG.VALUE, decimals = 0) %>%
    fmt_number(columns = AVG.LANDED.LBS, decimals = 0, pattern = "{x} lbs") %>%
    cols_hide(SPPNAME) %>%
    cols_width(color ~px(25)) %>%     
    cols_width(COMNAME ~px(300)) %>%    
    data_color(
      columns = COMNAME,
      target_columns = color,
      method = "factor",
      palette = gmri) %>%
    cols_label(
      COMNAME = md("**Species**"),
      AVG.VALUE = md("**Annual Average Value**"),
      AVG.LANDED.LBS = md("**Annual Average Volume**"),
      color = ""
    ) %>%
    tab_footnote("Above are the annual average value and volume for the top species landed at this port in each year from 2012-2021.")
} 

plots %>%
  mutate(DONUT = map(data, donut_func),
         TABLE = map(data, gt_table)) -> plots
plots$DONUT[[2]]

# Save gt ####
# gt renders html, so we need to save the tables as .png in order to stitch it to the table

save_gt <- function(TABLE, PORT){
  gt <- TABLE
  filename=paste(PORT, "table", sep="_")
  gtsave(gt, filename = paste0(filename, ".png"), path = here("outputs", "landings", "tables/"))
}

plots <- plots %>%
  mutate(IMAGE = map2(TABLE, PORT, save_gt))

# Read back in as png, wrap and attach to donuts
path <- here("outputs", "landings", "tables/")

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
  ggsave(plot, file = paste0("outputs/landings/donut/", filename), height = 8.7, width = 16.9, units="in", bg = "transparent")
}

plots <- plots %>%
  mutate(PLOT = map2(data, PORT, donut_table))


### looking at gloucester 
# gloucester <- landings %>% 
#   filter(PORT == "GLOUCESTER, MA")
# 
# gloucester %>%
#   filter(YEAR >= 2019) %>%
#   ggplot() +
#   geom_col(aes(x = COMNAME, y = VALUE, group = COMNAME, fill = COMNAME)) +
#   facet_wrap(~YEAR) +
#   theme_gmri()
# 
# gloucester %>% 
#   filter(COMNAME == "American lobster") %>% 
#   ggplot() +
#   geom_col(aes(x = YEAR, y = LANDED.LBS)) +
#   geom_line(aes(x = YEAR, y = VALUE)) +
#   theme_gmri()
# 
# # lobster is the culprit 
# landings %>% 
#   filter(COMNAME == "American lobster") %>% 
#   group_by(COMNAME, YEAR, PORT) %>%
#   summarise(TOTAL_VOLUME = sum(LANDED.LBS),
#             TOTAL_VALUE  = sum(VALUE)) %>%
#   ggplot() +
#   geom_col(aes(x=YEAR, y=TOTAL_VOLUME)) +
#   geom_line(aes(x=YEAR, y = TOTAL_VALUE)) +
#   facet_wrap(~PORT, scales = "free_y") +
#   theme_gmri()
