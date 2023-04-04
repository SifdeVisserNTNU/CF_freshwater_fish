# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 31/03/2023

### Calculating characterization factors based on fate and effect factors ------

# Set working directory
setwd("~/Desktop/Fish_CF_code")

# Libraries
library(readxl); library(dplyr); library(ggplot2)

# Parameters
IAGTP100 <- 4.756299931086110000000E-14
IAGTP1000 <- 4.235945271201920000000E-13


# Read files
GWP <- read_excel("data_revised/Fate_factors/Fate_factors_207.xlsx")
EFs <- read.csv('data_revised/Effect_factors/EFs.csv')

################################################################################
## Calculate fate factors

FF <- data.frame(Element = GWP$Element,
                 FF_100 = IAGTP100 * GWP$GWP100,
                 FF_1000 = IAGTP1000 * GWP$GWP1000)

################################################################################
## Calculate characterization factors

# EFs for average warming levels and dispersal, for all extinction metrics -----
EF <- EFs %>% 
  filter(Warming_level == "Av.") %>%
  select("Metric", "EFs")

### Sets of CF
# number = time horizon of fate factors
CF100 <- FF %>% transmute(Element = Element,
                          CF100_E1 = FF$FF_100 * EF[3,2],
                          CF100_E2 = FF$FF_100 * EF[2,2],
                          CF100_EB = FF$FF_100 * EF[1,2])
CF100[,2:4] <- signif(CF100[,2:4], digits = 3)  

CF1000 <- FF %>% transmute(Element = Element,
                          CF1000_E1 = FF$FF_1000 * EF[3,2],
                          CF1000_E2 = FF$FF_1000 * EF[2,2],
                          CF1000_EB = FF$FF_1000 * EF[1,2])
CF1000[,2:4] <- signif(CF1000[,2:4], digits = 3)  

### Standard deviation
SD <- EFs %>% 
  filter(Warming_level != "Av." & Dispersal != "Average" & Metric == "EB") %>%
  select("Warming_level", "Dispersal", "EFs")

CF100_SD <- FF %>% transmute(Element = Element,
                          CF100_4.5N = FF$FF_100 * SD[1,3],
                          CF100_3.2N = FF$FF_100 * SD[2,3],
                          CF100_2.0N = FF$FF_100 * SD[3,3],
                          CF100_4.5Y = FF$FF_100 * SD[4,3],
                          CF100_1.5N = FF$FF_100 * SD[5,3],
                          CF100_3.2Y = FF$FF_100 * SD[6,3],
                          CF100_2.0Y = FF$FF_100 * SD[7,3],
                          CF100_1.5Y = FF$FF_100 * SD[8,3])

CF100apply_sd <- apply(CF100_SD[,2:9],1,sd)
sd100 <- cbind(CF100, CF100apply_sd)

CF1000_SD <- FF %>% transmute(Element = Element,
                              CF1000_4.5N = FF$FF_1000 * SD[1,3],
                              CF1000_3.2N = FF$FF_1000 * SD[2,3],
                              CF1000_2.0N = FF$FF_1000 * SD[3,3],
                              CF1000_4.5Y = FF$FF_1000 * SD[4,3],
                              CF1000_1.5N = FF$FF_1000 * SD[5,3],
                              CF1000_3.2Y = FF$FF_1000 * SD[6,3],
                              CF1000_2.0Y = FF$FF_1000 * SD[7,3],
                              CF1000_1.5Y = FF$FF_1000 * SD[8,3])

CF1000apply_sd <- apply(CF1000_SD[,2:9],1,sd)
sd1000 <- cbind(CF1000, CF1000apply_sd)

################################################################################
### Plot CFs for few GHG

# Selection of GHGs
CF_100 <- CF100 %>%
  filter(Element == "Carbon dioxide" |
           Element == "Methane" |
           Element == "Nitrous oxide" |
           Element == "HFC-125" |
           Element == "Sulphur hexafluoride")

CF_1000 <- CF1000 %>%
  filter(Element == "Carbon dioxide" |
           Element == "Methane" |
           Element == "Nitrous oxide" |
           Element == "HFC-125" |
           Element == "Sulphur hexafluoride")

CF_fewGHG <- list(c(CF_100$CF100_EB, CF_1000$CF1000_EB))

# Restructure dataframe
few_GHG <- data.frame(Index = 1:10,
                      Time_horizon = c(rep("100",5),rep("1000",5)),
                      GHG = rep(c("CO2","CH4","N20","HFC-125","SF6"),2),
                      CF = unlist(CF_fewGHG))

# Plot Fig_5 -------------------------------------------------------------------
Fig_5 <- ggplot(data = few_GHG, aes(x=reorder(GHG,CF), y=CF, fill=Time_horizon)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_discrete(labels=c(expression("CO"[2]),
                            expression("CH"[4]),
                            expression("N"[2]*"O"),
                            "HFC-125",
                            expression("SF"[6]))) +
  scale_y_continuous(trans=scales::pseudo_log_trans(1e-15,10),
                     limits = c(0,1e-9),
                     breaks = c(1e-15,1e-14,1e-13,1e-12,1e-11,1e-10, 1e-09)) +
  scale_fill_viridis_d(option = "B", begin = 0.2, end = 0.9) +
  labs(y = expression(paste("Characterization factor [PDF\u00B7yr\u00B7",kg^-1,"]")),
       x = NULL,
       fill = "Time horizon [yr]") +
  theme(legend.position = "bottom") +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(angle = 0, vjust = -1, size = 6))
Fig_5

# Save files -------------------------------------------------------------------
write.csv(sd100,'data_revised/Characterization_factors/CF100.csv')
write.csv(sd1000,'data_revised/Characterization_factors/CF1000.csv')
ggsave(path = "visualisations_revised/", filename = "Fig_5.tiff", Fig_5, width = 90/25.4, height = 90/25.4, unit = 'in', dpi = 500)

### Results section 3.2 --------------------------------------------------------
# stats in results section
cat("The global characterization factors range from:\n")
range(CF100$CF100_EB) # 100-year time horizon
range(CF1000$CF1000_EB) # 1000-year time horizon

