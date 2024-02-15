###############################################################
# Citation link:
# https://dataverse.harvard.edu/file.xhtml?fileId=5370285&version=1.2
# Codes are obtained from the above link, in 02-meta-analysis.R, with adjusment
###############################################################

#### Preamble ####
# Purpose: Scripts for generating figure 1
# Author: Sinan Ma, Yuean Wang, Yang Zhou
# Date: 15 Feb 2024
# Contact: sinan.ma@mail.utoronto.ca, yuean.wang@mail.utoronto.ca, cocoyang.zhou@mail.utoronto.ca
# License: MIT


library(tidyverse)
library(readxl) # reading excel files
library(ggplot2)
library(ggpubr) # for gg rearrange
library(dplyr)
options(warn=-1)

library(broom)
library(dotwhisker)
library(cowplot)
library(metafor)

dat <- read_csv("outputs/data/dat.csv")
dat_reports <- read_csv("outputs/data/dat_reports.csv")
dat_studies <- read_csv("outputs/data/dat_studies.csv")

### custome theme
mytheme <- theme(plot.title = element_text(face = "bold", size = (22), colour = "black"), 
                 axis.text = element_text(size = (18), colour = "black"),
                 axis.ticks.length = unit(0.5, "cm"),
                 axis.title.y = element_text(size = (22), colour = "black"),
                 axis.title.x = element_text(size = (22), colour = "white"))




#visualization: Figure 2A
tmdat <- data.frame(prop.table(table(dat$TerrorMeasure)))
TerrorMeasures <- ggplot(tmdat, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.7, color="#000000", fill="#939598") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,.6), breaks = c(0,.1,.2,.3,.4,.5,.6)) +
  labs(title = "Terrorism Measures", x="", y = "") +
  theme_classic() +
  mytheme


#visualization: Figure 2B
ttdat <- data.frame(prop.table(table(dat$TerrorType)))
TerrorType <- ggplot(ttdat, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.7, color="#000000", fill="#939598") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = scales::percent, breaks = c(0,.1,.2,.3,.4,.5,.6)) +
  labs(title = "Type of Terrorism", x = "", y = "") +
  theme_classic() +
  mytheme


jpeg("outputs/figures/Figure2.jpeg", width = 15, height = 8, units = 'in', res = 300)
ggarrange(TerrorMeasures, TerrorType,
          labels = c("(A)", "(B)"),
          font.label = list(size = 22, color = "black"),
          ncol = 2, nrow = 1) #arrange all plots
dev.off() #safe picture in high resolution.
