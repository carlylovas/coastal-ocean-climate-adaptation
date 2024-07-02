# plotting community results 

# load libraries
library(tidyverse)
library(gmRi)
library(here)
library(patchwork)

# read in prepped community results
all_communities <- readRDS(here("data", "all_communities.rds"))

all_communities$Fahrenheit[all_communities$Fahrenheit_Lab == "0.0°F"] = "0.0"

# plotting function 
# will need to plot for each individual species and then group by port
plot_func <- function(x,y){
    temp <-  ggplot(data = x) +
      geom_col(aes(x = Fahrenheit, y = us_density, fill = Threshold > 0), position = "dodge", alpha = 0.75, width = 0.5)+
      #geom_smooth(aes(x = Fahrenheit_Lab, y = us_density, group = Common_Name), method = "glm", color = "#EA4F12", se=FALSE) +
      ylab("Availability (pounds per square mile)") + 
      xlab("Increase in Sea Surface Temperature") +
      ggtitle(y) +
      scale_fill_manual(values = c("#00736D", "#00608A")) +
      theme_gmri(legend.position = "none",
                 plot.title = element_text(size = 25),
                 axis.text.y = element_text(size = 18), #17
                 axis.text.x = element_text(size = 18), #16
                 axis.title = element_blank())}

# Arrange by port
arrange_func <- function(x,y){
  list <- x$plot
  wrap <- wrap_plots(list, nrow = 2)
  print(wrap)
  
  filename = paste(y, "change" , sep="_")
  ggsave(wrap, file = paste("outputs/species change/", filename, ".png", sep=""), width = 20, height = 8.5, units = "in")
} # this file path will likely change 

all_communities %>% 
  group_by(region, COMNAME) %>% 
  nest() %>% 
  mutate(plot = map2(data, COMNAME, plot_func)) -> all_communities

all_communities %>% 
  arrange(region, COMNAME) %>%
  group_by(region) %>%
  nest() %>%
  mutate(port_plot = map2(data, region, arrange_func)) -> all_communities
