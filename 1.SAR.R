# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 30/03/2023

### Species Area Relationship specifically for riverine fish species -----------

# Set working directory
setwd("~/Desktop/Fish_CF_code")

# Libraries
library(readr); library(dplyr); library(sars); library(ggplot2); library(viridis)

# Read files -------------------------------------------------------------------

# Provides area of each basin
Drainage_Basins_Table <- read_delim("data_revised/Barbarossa/Drainage_Basins_Table.csv",
                                    ";",
                                    escape_double = FALSE,
                                    trim_ws = TRUE,
                                    col_types = cols_only("1.Basin.Name" = col_character(),
                                                          "9.Surface.Area" = col_double()))

# Provides basin name each time a native fish species occurs there
Occurrence_Table <- read_delim("data_revised/Barbarossa/Occurrence_Table.csv",
                               ";",
                               escape_double = FALSE,
                               trim_ws = TRUE, 
                               col_types = cols_only("1.Basin.Name" = col_character(),
                                                     "3.Native.Exotic.Status" = col_character())) %>%
  subset(., `3.Native.Exotic.Status` != "exotic") %>%
  dplyr::select(.,-2)

# Count species richness (sum of native fish species) per basin
SAR_table <- aggregate(Occurrence_Table,
                       by = list(Occurrence_Table$`1.Basin.Name`),
                       FUN = length) %>%
  rename('Basin.Name' = 'Group.1', "Species.Richness" = "1.Basin.Name") %>%
  inner_join(x = ., y = Drainage_Basins_Table, by = c("Basin.Name" = "1.Basin.Name")) %>%
  rename(., "Surface.Area"="9.Surface.Area")

### Regression -----------------------------------------------------------------
sa1 <- lm(log(Species.Richness)~log(Surface.Area), data= SAR_table)
summary(sa1)

# Fig. S2 SAR plot -------------------------------------------------------------
Fig_S2 <- ggplot(SAR_table, aes(x=Surface.Area, y=Species.Richness)) + 
  geom_point(col = 'black', size=0.5) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  labs(x = expression(paste("Surface area [",km^2,"]")),y ='Species richness [-]') +
  annotation_logticks() +
  geom_smooth(method = 'lm', col = '#F6D645') +
  theme_minimal() +
  theme(text = element_text(size = 10),
        axis.text=element_text(size=8), 
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 10),
        plot.margin=unit(c(t = 0, r = 0.4, b = 0, l = 0), "cm"))
Fig_S2

### Save files -----------------------------------------------------------------
ggsave(path = "visualisations_revised", filename = "Fig_S2.tiff", Fig_S2,
       width = 3.54, height = 3.54, unit = 'in', dpi = 500)
