# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 31/01/2023

### Calculating freshwater biodiversity impact scores for transport biofuels ---

# Working directory
setwd("C:\\Users\\sifd\\6.FishCF\\Fish_CF_code")

# Libraries
library(readxl); library(dplyr); library(ggplot2); 

# Read files
Results <- read_excel("data\\Biofuels\\Biofuels_characterization_results.xlsx")

# Create a list of impact scores -----------------------------------------------
climate_change_new <- Results[1:4,2]
climate_change <- Results[1:4,3]
eutrophication <- Results[1:4,4]
water_stress <- Results[1:4,5]
list <- data.frame(climate_change_new, climate_change,eutrophication,water_stress)
impact_scores <- unlist(list) 
scores <- as.numeric(impact_scores)

# Lists of impact categories and fuel types
categories <- c(rep("Climate change new",4),
                rep("Climate change",4),
                rep("Eutrophication",4),
                rep("Water stress",4))
fuels <- rep(c("Diesel", "Biodiesel", "Petrol", "Biopetrol"),4)

# Combine in dataframe
biofuels <- data.frame(scores,categories,fuels) 

################################################################################
# Plot Fig 4
Fig_4 <- ggplot(data = biofuels, aes(x = fuels, y = scores, fill = categories)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(trans=scales::pseudo_log_trans(1e-20,10),
                     limits = c(0, 1e-13),
                     breaks = c(0e+00, 1e-19, 1e-18, 1e-17, 1e-16, 1e-15, 1e-14, 1e-13)) +
  ylab(expression(paste("Impact scores [PDF??yr??",km^-1,"]"))) +
  scale_fill_viridis_d(option = "B", begin = 0.2, end = 0.9) +
  theme(panel.spacing.x = unit(0,"line")) +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 50, hjust=1),
        axis.text=element_text(size=8),
        axis.title.x = element_blank(),
        legend.position = "right",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 9),
        strip.placement = 'outside',
        strip.background.x = element_blank())
Fig_4  

# Save figure ------------------------------------------------------------------
ggsave(path = "visualisations/",
       filename = "Fig_4.png",
       Fig_4,
       dpi = 600,
       width = 110,
       height = 70,
       unit = 'mm')
