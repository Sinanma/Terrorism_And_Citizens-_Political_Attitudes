#### Preamble ####
# Purpose: Simple testing on data
# Author: Sinan Ma, Yuean Wang, Yang Zhou
# Date: 15 Feb 2024
# Contact: cocoyang.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have run 00-simulation.R and outputs/data/sim.csv available

#### Workspace setup ####
library(tidyverse)
data <- read_csv("outputs/data/sim.csv", show_col_types = FALSE)

# check if the list contain all integers
all(sapply(data$years_pub, function(x) is.numeric(x) && x == floor(x)))
all(sapply(data$years_studies, function(x) is.numeric(x) && x == floor(x)))

# Some details: the function return true if the numeric is an integer, 
# sapply() will apply such function to all elements in the array
# and all() will check if the boolean values are all True in the array


# check the range of years
min(data$years_pub)>=1985
min(data$years_pub)<=2020

min(data$years_studies)>=1985
min(data$years_studies)<=2020

# compare if the elements of two arrays are identical, regardless of ordering
compare <- function(list1, list2){
  identical(sort(list1), sort(list2))
}

# check if the unique elements are exactly the categories we have
compare(unique(data$measures), c("Acts of violence", "Threat", "Other", "Reported exposure", "Fear", "Anger"))
compare(unique(data$types), c("Islamist", "No ideology", "Other", "State terror", "Extreme right"))
