# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 31/01/2023

### Calculating effect factors -------------------------------------------------


# Set working directory
setwd("C:\\Users\\sifd\\6.FishCF\\Fish_CF_code")

# Libraries
library(readr); library(dplyr); library(ggplot2); library(ggpattern)

## Parameters ------------------------------------------------------------------

# Number of fish species
n = 11425
# Parameter in extinction risk formula by Thomas et al. (2004)
# Developed by SAR for freshwater fish
# Main scenario in paper
z = 0.21

## Read files ------------------------------------------------------------------
# This dataset contains the threatened ranges of each species for various
# scenarios (2 dispersal scenarios, 4 warming levels)
RangesThreatened <- read_delim("data\\Barbarossa\\RT_species.csv", 
                 ",",
                 escape_double = FALSE,
                 trim_ws = TRUE,
                 col_types = cols_only("binomial" = col_character(),
                                       "1.5C_no dispersal" = col_double(),
                                       "1.5C_maximal dispersal" = col_double(),
                                       "2.0C_no dispersal" = col_double(),
                                       "2.0C_maximal dispersal" = col_double(),
                                       "3.2C_no dispersal" = col_double(),
                                       "3.2C_maximal dispersal" = col_double(),
                                       "4.5C_no dispersal" = col_double(),
                                       "4.5C_maximal dispersal" = col_double()))

# This datasets contains the original area of each species
Original_area <- read_delim("data\\Barbarossa\\species_traits.csv",
                   ",",
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_types = cols_only("id_no" = col_double(),
                                        "binomial" = col_character(),
                                         "area" = col_double()))


## Calculate new area from threatened ranges -----------------------------------
Areas <- as.data.frame(inner_join(x= RangesThreatened, y = Original_area, by = "binomial")) %>%
  # rename because otherwise column name is not recognized when starting with a number
  rename(RT_1.5_ND = '1.5C_no dispersal', RT_2.0_ND = '2.0C_no dispersal', 
         RT_3.2_ND = '3.2C_no dispersal', RT_4.5_ND = '4.5C_no dispersal', 
         RT_1.5_MD = '1.5C_maximal dispersal', RT_2.0_MD = '2.0C_maximal dispersal',
         RT_3.2_MD = '3.2C_maximal dispersal', RT_4.5_MD = '4.5C_maximal dispersal',
         Aoriginal = area) %>%
  mutate(Anew_1.5_ND = ((100 - RT_1.5_ND)/100) * Aoriginal,
         Anew_2.0_ND = ((100 - RT_2.0_ND)/100) * Aoriginal,
         Anew_3.2_ND = ((100 - RT_3.2_ND)/100) * Aoriginal,
         Anew_4.5_ND = ((100 - RT_4.5_ND)/100) * Aoriginal,
         Anew_1.5_MD = ((100 - RT_1.5_MD)/100) * Aoriginal,
         Anew_2.0_MD = ((100 - RT_2.0_MD)/100) * Aoriginal,
         Anew_3.2_MD = ((100 - RT_3.2_MD)/100) * Aoriginal,
         Anew_4.5_MD = ((100 - RT_4.5_MD)/100) * Aoriginal) %>%
  select(-c(2:9))


################################################################################
### EXTINCTION RISK


# Since RT is maximally 100%, the assumption that Anew cannot be larger than Aoriginal is guaranteed. 
# Naming: Extinction Metrics_Temperature_Dispersal_Z


## E1 --------------------------------------------------------------------------
# E1 = 1- (sum_Anew / sum_Aoriginal)^z

# Calculate sum_Aoriginal
sum_Aoriginal <- sum(Areas$Aoriginal)

E1_1.5_ND <- 1 - (sum(Areas$Anew_1.5_ND) / sum_Aoriginal)^z
E1_2.0_ND <- 1 - (sum(Areas$Anew_2.0_ND) / sum_Aoriginal)^z
E1_3.2_ND <- 1 - (sum(Areas$Anew_3.2_ND) / sum_Aoriginal)^z
E1_4.5_ND <- 1 - (sum(Areas$Anew_4.5_ND) / sum_Aoriginal)^z
E1_1.5_MD <- 1 - (sum(Areas$Anew_1.5_MD) / sum_Aoriginal)^z
E1_2.0_MD <- 1 - (sum(Areas$Anew_2.0_MD) / sum_Aoriginal)^z
E1_3.2_MD <- 1 - (sum(Areas$Anew_3.2_MD) / sum_Aoriginal)^z
E1_4.5_MD <- 1 - (sum(Areas$Anew_4.5_MD) / sum_Aoriginal)^z

## E2 --------------------------------------------------------------------------
# E2 = 1 - {(1/n) [sum (Anew/Aoriginal) ]}^z
E2 <- Areas %>% transmute(E2_fraction_1.5_ND = Anew_1.5_ND / Aoriginal,
                       E2_fraction_2.0_ND = Anew_2.0_ND / Aoriginal,
                       E2_fraction_3.2_ND = Anew_3.2_ND / Aoriginal,
                       E2_fraction_4.5_ND = Anew_4.5_ND / Aoriginal,
                       E2_fraction_1.5_MD = Anew_1.5_MD / Aoriginal,
                       E2_fraction_2.0_MD = Anew_2.0_MD / Aoriginal,
                       E2_fraction_3.2_MD = Anew_3.2_MD / Aoriginal,
                       E2_fraction_4.5_MD = Anew_4.5_MD / Aoriginal)

E2_1.5_ND <- 1 - ((1/n)*sum(E2$E2_fraction_1.5_ND))^z
E2_2.0_ND <- 1 - ((1/n)*sum(E2$E2_fraction_2.0_ND))^z
E2_3.2_ND <- 1 - ((1/n)*sum(E2$E2_fraction_3.2_ND))^z
E2_4.5_ND <- 1 - ((1/n)*sum(E2$E2_fraction_4.5_ND))^z
E2_1.5_MD <- 1 - ((1/n)*sum(E2$E2_fraction_1.5_MD))^z
E2_2.0_MD <- 1 - ((1/n)*sum(E2$E2_fraction_2.0_MD))^z
E2_3.2_MD <- 1 - ((1/n)*sum(E2$E2_fraction_3.2_MD))^z
E2_4.5_MD <- 1 - ((1/n)*sum(E2$E2_fraction_4.5_MD))^z

## EB --------------------------------------------------------------------------
# EB = (1/n) sum[1 - (Anew/Aoriginal)^z]
EB <- Areas %>% transmute(EB_fraction_1.5_ND = 1 - (Anew_1.5_ND / Aoriginal)^z, 
                          EB_fraction_2.0_ND = 1 - (Anew_2.0_ND / Aoriginal)^z,
                          EB_fraction_3.2_ND = 1 - (Anew_3.2_ND / Aoriginal)^z,
                          EB_fraction_4.5_ND = 1 - (Anew_4.5_ND / Aoriginal)^z,
                          EB_fraction_1.5_MD = 1 - (Anew_1.5_MD / Aoriginal)^z, 
                          EB_fraction_2.0_MD = 1 - (Anew_2.0_MD / Aoriginal)^z,
                          EB_fraction_3.2_MD = 1 - (Anew_3.2_MD / Aoriginal)^z,
                          EB_fraction_4.5_MD = 1 - (Anew_4.5_MD / Aoriginal)^z)

EB_1.5_ND <- (1/n) * sum(EB$EB_fraction_1.5_ND)
EB_2.0_ND <- (1/n) * sum(EB$EB_fraction_2.0_ND)
EB_3.2_ND <- (1/n) * sum(EB$EB_fraction_3.2_ND)
EB_4.5_ND <- (1/n) * sum(EB$EB_fraction_4.5_ND)
EB_1.5_MD <- (1/n) * sum(EB$EB_fraction_1.5_MD)
EB_2.0_MD <- (1/n) * sum(EB$EB_fraction_2.0_MD)
EB_3.2_MD <- (1/n) * sum(EB$EB_fraction_3.2_MD)
EB_4.5_MD <- (1/n) * sum(EB$EB_fraction_4.5_MD, na.rm = TRUE)

################################################################################
### CURRENT SITUATION

# Calculate temperature increase -----------------------------------------------
GMST <- data.frame(
  year = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
  GMST = c(0.81, 0.85, 0.89, 0.95, 1.10, 1.23, 1.13, 1.05, 1.19, 1.21)
)

# Get current temperature increase
CT <- mean(GMST$GMST) 


# Get extinction risk at current temperature for various scenarios (different for
# dispersal  scenario, type of extinction metrics and z value)
E1_CT_ND <- E1_1.5_ND / 1.5 * CT
E1_CT_MD <- E1_1.5_MD / 1.5 * CT
E2_CT_ND <- E2_1.5_ND / 1.5 * CT
E2_CT_MD <- E2_1.5_MD / 1.5 * CT
EB_CT_ND <- EB_1.5_ND / 1.5 * CT
EB_CT_MD <- EB_1.5_MD / 1.5 * CT

################################################################################
### EFFECT FACTORS

# Calculate EFs according to average approach ----------------------------------
# EF_average = (E(X) - E(CT)) / (X - CT)

calculate_EFs <- function(CT) {
  EF <- c(E1_1.5_ND - E1_CT_ND,
          E1_2.0_ND - E1_CT_ND,
          E1_3.2_ND - E1_CT_ND,
          E1_4.5_ND - E1_CT_ND,
          E1_1.5_MD - E1_CT_MD,
          E1_2.0_MD - E1_CT_MD,
          E1_3.2_MD - E1_CT_MD,
          E1_4.5_MD - E1_CT_MD,
          E2_1.5_ND - E2_CT_ND,
          E2_2.0_ND - E2_CT_ND,
          E2_3.2_ND - E2_CT_ND,
          E2_4.5_ND - E2_CT_ND,
          E2_1.5_MD - E2_CT_MD,
          E2_2.0_MD - E2_CT_MD, 
          E2_3.2_MD - E2_CT_MD, 
          E2_4.5_MD - E2_CT_MD,
          EB_1.5_ND - EB_CT_ND, 
          EB_2.0_ND - EB_CT_ND, 
          EB_3.2_ND - EB_CT_ND, 
          EB_4.5_ND - EB_CT_ND,
          EB_1.5_MD - EB_CT_MD, 
          EB_2.0_MD - EB_CT_MD, 
          EB_3.2_MD - EB_CT_MD, 
          EB_4.5_MD - EB_CT_MD) /
    (c(1.5, 2.0, 3.2, 4.5) - CT)
  return(EF)
}

EFs <- calculate_EFs(CT)

# Dataframe with calculated effect factors
Warming_level <- rep(c("1.5", "2.0", "3.2", "4.5"), 6)
Dispersal <- rep(c(rep("No", 4), rep("Yes", 4)), 3)
Metric <- c(rep("E1", 8), rep("E2", 8), rep("EB", 8))
All_EFs <- data.frame(Metric, Warming_level, Dispersal, EFs)

# Range of effect factors (section 3.1)
range(All_EFs$EFs)

# Average across warming levels first and then dispersal scenarios
## E1
E1_nodispersal <- mean(All_EFs[1:4,4])
E1_maxdispersal <- mean(All_EFs[5:8,4])
E1_mean <- (E1_nodispersal + E1_maxdispersal) / 2
## E2
E2_nodispersal <- mean(All_EFs[9:12,4])
E2_maxdispersal <- mean(All_EFs[13:16,4])
E2_mean <- (E2_nodispersal + E2_maxdispersal) / 2
## E3
EB_nodispersal <- mean(All_EFs[17:20,4])
EB_maxdispersal <- mean(All_EFs[21:24,4])
EB_mean <- (EB_nodispersal + EB_maxdispersal) / 2

# Add averages to effect factors dataframe
Add <- data.frame(Metric = c("E1", "E2", "EB"),
                  Warming_level = rep("Av.", 3), 
                  Dispersal = rep("Average", 3),
                  EFs = c(E1_mean, E2_mean, EB_mean))

EF_complete <- bind_rows(All_EFs, Add) %>% arrange(-EFs)
EF_complete$Index <- 1:27
labels <- EF_complete$Warming_level

################################################################################
## Figure 1 - All effect factors

Fig_1 <- ggplot(EF_complete, aes(x = reorder(Index, -EFs), y = EFs)) +
  scale_y_continuous(breaks=seq(0,0.07,0.01), limits = c(0, 0.07)) +
  scale_x_discrete(labels= labels) +
  scale_fill_viridis_d(option = "B", begin = 0.2, end = 0.9, direction = -1) +
  geom_bar(aes(fill = Metric),
           position= "dodge",
           stat='identity') +
  geom_bar_pattern(aes(pattern = Dispersal),
                   position= "dodge",
                   stat='identity',
                   fill = "transparent",
                   col = "grey",
                   pattern_color = "grey",
                   pattern_fill = "grey") +
  scale_pattern_manual(values = c("none", "stripe", "circle")) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.text=element_text(size=8),
        legend.key.size = unit(0.6, 'cm'),
        legend.title = element_text(size=8),
        axis.text.x = element_text(angle = 0, hjust=1, size=8),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.placement = 'outside',
        strip.background.x = element_blank()) +
  labs(x = expression("Warming level [\u00B0C]"), y= expression(paste("Effect factor [PDF??\u00B0",C^-1,"]")), fill="Extinction metrics") +
  coord_flip()
Fig_1

################################################################################
 # Figure S2 - plot of extinction risk vs temperature, marginal vs average type
# of effect factor

# Isolate values for benchmark extinction metrics for each warming level
# No dispersal scenario taken for example
EB_1.5 <- All_EFs %>%
  filter(Metric == "EB" & Warming_level == "1.5" & Dispersal == "No") %>%
  select("EFs")
EB_2.0 <- All_EFs %>%
  filter(Metric == "EB" & Warming_level == "2.0" & Dispersal == "No") %>%
  select("EFs")
EB_3.2 <- All_EFs %>%
  filter(Metric == "EB" & Warming_level == "3.2" & Dispersal == "No") %>%
  select("EFs")
EB_4.5 <- All_EFs %>%
  filter(Metric == "EB" & Warming_level == "4.5" & Dispersal == "No") %>%
  select("EFs")
  
EB_extinction <- data.frame(dT = c(0,1.5,2.0,3.2,4.5),
                            EB_all = unlist(c(0, EB_1.5, EB_2.0, EB_3.2, EB_4.5)))

Fig_S1 <- ggplot(data = EB_extinction, aes(x = dT, y = EB_all)) +
  geom_segment(aes(x = 0, xend = 1.5, y = 0, yend = EB_1.5_ND, col = "Marginal"), linetype = "dashed") +
  geom_segment(aes(x = CT, xend = 1.5, y = EB_CT_ND, yend = EB_1.5_ND, col = "Average"), linetype = "dashed") +
  geom_segment(aes(x = CT, xend = 2.0, y = EB_CT_ND, yend = EB_2.0_ND, col = "Average"), linetype = "dashed") +
  geom_segment(aes(x = CT, xend = 3.2, y = EB_CT_ND, yend = EB_3.2_ND, col = "Average"), linetype = "dashed") +
  geom_segment(aes(x = CT, xend = 4.5, y = EB_CT_ND, yend = EB_4.5_ND, col = "Average"), linetype = "dashed") +
  scale_color_manual(values=c("#420A68","#CC4248")) +
  geom_point(aes(x=CT, y=EB_CT_ND), col = "Red", shape = 8) +
  geom_point(aes(x=0, y=0), col = "#CC4248", size = 2) +
  geom_point(aes(x=1.5, y=EB_1.5_ND), col = "#420A68", size = 2) +
  geom_point(aes(x=1.5, y=EB_1.5_ND), col = "#CC4248", size = 1) +
  geom_point(aes(x=2.0, y=EB_2.0_ND), col = "#420A68", size = 2) +
  geom_point(aes(x=3.2, y=EB_3.2_ND), col = "#420A68", size = 2) +
  geom_point(aes(x=4.5, y=EB_4.5_ND), col = "#420A68", size = 2) +
  labs(color = 'Type', x = 'Temperature increase [\u00B0C]', y = 'Extinction risk [PDF]') +
  scale_x_continuous(limits=c(0,5), breaks=seq(0,5,0.5)) +
  scale_y_continuous(limits=c(0,0.3), breaks=seq(0,0.3,0.05)) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.text=element_text(size=10),
        strip.text = element_text(angle = 0, vjust = -1, size = 10),
        legend.position = "bottom",
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        )
Fig_S1

################################################################################
### SAVE FILES
write.csv(EF_complete, file = "C:\\Users\\sifd\\6.FishCF\\Fish_CF_code\\data\\Effect_factors\\EFs.csv")
ggsave(path = "visualisations/", filename = "Fig_1.png", Fig_1, width = 160, height = 100, unit = 'mm', dpi = 600)
ggsave(path = "visualisations/", filename = "Fig_S1.png", Fig_S1, dpi = 600, width = 80, height = 80, unit = 'mm')
