# plotting community results 

# load libraries
library(tidyverse)
library(gmRi)
library(here)
library(patchwork)

# read in prepped community results

# plotting function 
# will need to plot for each individual species and then group by port
plot_func <- function(x,y){
    temp <-  ggplot(data = x) +
      geom_col(aes(x = Fahrenheit_Lab, y = us_density, fill = Threshold > 0), position = "dodge", alpha = 0.75, width = 0.5)+
      #geom_smooth(aes(x = Fahrenheit_Lab, y = us_density, group = Common_Name), method = "glm", color = "#EA4F12", se=FALSE) +
      ylab("Availability (pounds per square mile)") + 
      xlab("Increase in Sea Surface Temperature") +
      ggtitle(Clean_Name) +
      scale_fill_manual(values = c("#00736D", "#00608A")) +
      theme_gmri(legend.position = "none",
                 axis.text.y = element_text(size = 20),
                 axis.text.x = element_text(size = 18),
                 axis.title = element_blank())}

# Arrange by port
arrange_func <- function(x,y){
  list <- x$Plot
  wrap <- wrap_plots(list, nrow = 2)
  print(wrap)
  
  filename = paste(y, "landings" , sep="_")
  ggsave(wrap, file = paste("Temp_Results/Species Change/US_Customary/", filename, ".png", sep=""), width = 22, height = 8.5, units = "in")
} # this file path will likely change 