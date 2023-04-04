# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 30/03/2023

### Calculating freshwater biodiversity impact scores for transport biofuels ---

# Working directory
setwd("~/Desktop/Fish_CF_code")

# Libraries
library(readxl); library(dplyr); library(ggplot2); 

# Read files
Results <- read_excel("data_revised/Biofuels/Biofuels_characterization_results.xlsx")

# Increase in impact scores compared to LC-IMPACT climate change ---------------
Percentual_increase <- (Results$CC_FW_new - Results$`CC_FW_LC-IMPACT`) /
  Results$`CC_FW_LC-IMPACT` * 100

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
# Plot Fig 6
Fig_6 <- ggplot(data = biofuels, aes(x = fuels, y = scores, fill = categories)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(trans=scales::pseudo_log_trans(1e-20,10),
                     limits = c(0, 1e-13),
                     breaks = c(0e+00, 1e-19, 1e-18, 1e-17, 1e-16, 1e-15, 1e-14, 1e-13)) +
  ylab(expression(paste("Impact scores [PDF\u00B7yr\u00B7",km^-1,"]"))) +
  scale_fill_viridis_d(option = "B", begin = 0.2, end = 0.9) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 50, hjust=1),
        axis.text=element_text(size=8),
        axis.title.x = element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom", # move legend to bottom
        legend.box = "vertical", # change legend box orientation to vertical
        legend.box.just = "top", # align legend box to the top of the legend area
        legend.margin=margin(t=0, b=-20, l=0), # adjust legend margin to move it below the plot
        plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5))
Fig_6

# Save figure ------------------------------------------------------------------
ggsave(path = "visualisations_revised/",
       filename = "Fig_6.tiff",
       Fig_6,
       dpi = 500,
       width = 90/25.4,
       height = 90/25.4,
       unit = 'in')
