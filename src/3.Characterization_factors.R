# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 31/01/2023

### Calculating characterization factors based on fate and effect factors ------

# Set working directory
setwd("C:\\Users\\sifd\\6.FishCF\\Fish_CF_code")

# Libraries
library(readxl); library(dplyr); library(ggplot2)

# Parameters
IAGTP100 <- 4.756299931086110000000E-14
IAGTP1000 <- 4.235945271201920000000E-13


# Read files
GWP <- read_excel("data\\Fate_factors\\Fate_factors_207.xlsx")
EFs <- read.csv('data\\Effect_factors\\EFs.csv')

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

# Plot Fig_3 -------------------------------------------------------------------
Fig_3 <- ggplot(data = few_GHG, aes(x=reorder(GHG,CF), y=CF, fill=Time_horizon)) +
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
  labs(y = expression(paste("Characterization factor [PDF??yr??",kg^-1,"]")),
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
Fig_3

# Save files -------------------------------------------------------------------
write.csv(CF100,'data\\Characterization_factors\\CF100.csv')
write.csv(CF1000,'data\\Characterization_factors\\CF1000.csv')
ggsave(path = "visualisations\\", filename = "Fig_3.png", Fig_3, width = 90, height = 80, unit = 'mm', dpi = 600)

### Results section 3.2 --------------------------------------------------------
# stats in results section
cat("The global characterization factors range from:\n")
range(CF100$CF000_EB) # 100-year time horizon
range(CF1000$CF1000_EB) # 1000-year time horizon

