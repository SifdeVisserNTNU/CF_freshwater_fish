# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 31/03/2023

### Plots for the grid cell contribution to global extinction risk -------------


# Working directory
setwd("~/Desktop/Fish_CF_code")

# Libraries 
library(sf); library(terra); library(spData); library(ggplot2); library(cowplot)

yes# Read files
Contribution <- readRDS("Data/Contribution/Contribution.rds")

### Determine which grid cells belongs to which continent ----------------------

world_polygons <- world["continent"] # Both dataframes are CRS WGS 84

# Spatial join
joined <- st_join(Contribution, left = TRUE, world_polygons["continent"])

# Summing up EP per continent
Continent_contributions <- aggregate(cbind(Ep_1.5_grid,
                                           Ep_2.0_grid,
                                           Ep_3.2_grid,
                                           Ep_4.5_grid)
                                     ~ continent,
                                     data = joined,
                                     FUN=sum,
                                     na.rm = TRUE)

sum(Continent_contributions$Ep_1.5_grid)
sum(Continent_contributions$Ep_4.5_grid)

# ~ 97% of the contribution of grid cells to global extinction risk is assigned
# to a continent

# Shorten continent names
Continent_contributions[4,1] <- "N. America"
Continent_contributions[6,1] <- "S. America"

# In main paper - Fig_4 --------------------------------------------------------

base_fig4 <- ggplot() +
  geom_sf(data = Contribution, aes(geometry = geometry, colour = Ep_3.2_grid), col = NA) +
  scale_colour_viridis_c(option = "B",
                         begin = 0.2, end = 1,
                         na.value = "grey",
                         trans = "log",
                         limits = c(1.0e-10, 1.0e-01),
                         breaks = c(1.0e-10,1.0e-09,1.0e-08,1.0e-07,1.0e-06,1.0e-05,1.0e-04,1.0e-03,1.0e-02,1.0e-01),
                         guide = guide_colourbar(title = "Ep 3.2 [%]",
                                                 title.position = "top",
                                                 title.hjust = 0.5)) + 
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.key.width = unit(1.5, 'cm'),
        legend.text= element_text(size=8),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_text(size=8),
        plot.margin=unit(c(0,0,0,0),units='cm'))

inset_fig4 <- ggplot(data=Continent_contributions, aes(x=reorder(continent, -Ep_3.2_grid), y=Ep_3.2_grid, fill= '#65156e')) +
  geom_bar(stat="identity") +
  scale_fill_viridis_d(option = "B", begin = 0.2, end = 0.9) +
  scale_y_continuous(limits = c(0, 40),
                     breaks = c(0, 10, 20, 30, 40)) +
  labs(title = "Ep 3.2 [%]") +
  theme_minimal() +
  theme(text = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text=element_text(size=8, colour="black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 8),
        strip.placement = 'outside',
        plot.margin=unit(c(0,0,0,0),units='cm')) +
  coord_flip()

Fig_4 <-
  ggdraw() +
  draw_plot(base_fig4) +
  draw_plot(inset_fig4, x = .05, y = .27, hjust = 0, vjust = 0, width = .22, height = .3)

# Save files --------------------------------------------------------------------
ggsave(path = "visualisations/", filename = "Fig_4.tiff", Fig_4, dpi = 300, width = 190/25.4, height = 90/25.4, unit = 'in')

