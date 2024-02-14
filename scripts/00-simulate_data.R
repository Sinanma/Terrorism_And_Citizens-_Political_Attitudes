#### Preamble ####
# Purpose: To simulate "year of collection/publication", terrorism meansures and types
# Author: Sinan Ma, Yuean Wang, Yang Zhou
# Date: 15 Feb 2024
# Contact: cocoyang.zhou@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
set.seed(1)


#### Simulate data ####
n <- 50

years_studies <- sample(1985:2020, n, replace = TRUE)
years_pub <- sample(1985:2020, n, replace = TRUE)


measures <- sample(c("Acts of violence", "Threat", "Other", "Reported exposure", "Fear", "Anger"), n, replace = TRUE)
types <- sample(c("Islamist", "No ideology", "Other", "State terror", "Extreme right"), n, replace = TRUE)

sim <- data.frame(
	years_studies,
	years_pub,
	measures,
	types)

write_csv(sim, "outputs/data/sim.csv")