# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 30/03/2022

### Perform ANOVA --------------------------------------------------------------

# Set working directory
setwd("~/Desktop/Fish_CF_code")

# Libraries
library(dplyr); library(AICcmodavg); library(writexl)

# Read files -------------------------------------------------------------------
Ano <- read.csv(file = 'data_revised/Effect_factors/EFs.csv') %>%
  select("Metric", "Warming_level", "Dispersal", "EFs") %>%
  filter(Dispersal == "Yes" | Dispersal == "No") # kicks out averages

# Two-way additive model
two_way <- aov(EFs ~ Metric + Warming_level + Dispersal, data = Ano)
two_way_sum <- summary(two_way)

# Two way interaction model
interaction <- aov(EFs ~ Metric + Warming_level + Dispersal + 
                     Metric * Warming_level + Metric * Dispersal +
                     Warming_level * Dispersal, 
                   data = Ano)
interaction_sum <- summary(interaction) 

# Test best model fit (lowest AIC score) ---------------------------------------
model.set <- list(two_way, interaction)
model.names <- c("two_way", "interaction")
aictab(model.set, modnames = model.names)

# Conclusion: two-way (without interaction) best fit

### Check for homoscedasticity -------------------------------------------------
jpeg("visualisations_revised/Fig_S1.jpg")
par(mfrow=c(2,2))
test <- plot(two_way)
dev.off()

### % variance explained by factor ---------------------------------------------
sum_sq <- two_way_sum[[1]]
s_sq <- sum_sq[1:5,2]
total <- sum(s_sq, na.rm = TRUE)
sum_of_squares <- data.frame(Extinction_metrics = sum_sq[1,2] / total * 100,
                             Temperature = sum_sq[2,2] / total * 100,
                             Dispersal = sum_sq[3,2] / total * 100,
                             Residuals = sum_sq[4,2] / total * 100,
                             Total = 100)

# Save files
Table_4 <- write_xlsx(sum_of_squares, "data_revised/ANOVA/Table_4.xlsx")
  