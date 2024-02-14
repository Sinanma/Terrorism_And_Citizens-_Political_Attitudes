###############################################################
# Citation link:
# https://dataverse.harvard.edu/file.xhtml?fileId=5370285&version=1.2
# Codes are obtained from the above link, in 02-meta-analysis.R, with adjusment
###############################################################

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


# Figure 2A
StudyYear <- ggplot(dat_studies, aes(x=StudyYear)) + 
  geom_histogram(binwidth=1, color="#000000", fill="#939598") +
  labs(x="to fill some space", y = "", title = "Year of Data Collection") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010, 2015,2020)) +
  theme_classic() + #histogram year of study 
  geom_vline(xintercept=2001, linetype="dashed", size = 1) + #add 9/11 line
  geom_vline(xintercept=2004, linetype="dashed", size = 1) + #add line
  geom_vline(xintercept=2015, linetype="dashed", size = 1) + #add ISIS line
  geom_label(data = dat_studies, aes(x=2001, y=27, label = "9/11"), size=8) + #add 9/11 label
  geom_label(data = dat_studies, aes(x=2007, y=35, label = "Madrid & \n Israel-Palestine"), size=6) + #add label
  geom_label(data = dat_studies, aes(x=2014, y=44, label = "IS Attacks"), size=8) + #add ISIS label
  mytheme

## Figure 2B
PubYear <- ggplot(dat_reports, aes(x=Year)) + 
  geom_histogram(binwidth=1, color="#000000", fill="#939598") +
  labs(title ="Year of Publication", x = "to fill some space", y = "") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010, 2015,2020)) +
  theme_classic() + #histogram year of study 
  geom_vline(xintercept=2001, linetype="dashed", size = 1) + #add 9/11 line
  geom_vline(xintercept=2015, linetype="dashed", size = 1) + #add ISIS France line
  geom_label(data = dat_reports, aes(x=2001, y=27, label = "9/11"), size=8) + #add 9/11 label
  geom_label(data = dat_reports, aes(x=2014, y=44, label = "IS Attacks"), size=8) + #add ISIS label
  mytheme


#visualization: Figure 2C
tmdat <- data.frame(prop.table(table(dat$TerrorMeasure)))
TerrorMeasures <- ggplot(tmdat, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.7, color="#000000", fill="#939598") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,.6), breaks = c(0,.1,.2,.3,.4,.5,.6)) +
  labs(title = "Terrorism Measures", x="", y = "") +
  theme_classic() +
  mytheme


#visualization: Figure 2D
ttdat <- data.frame(prop.table(table(dat$TerrorType)))
TerrorType <- ggplot(ttdat, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.7, color="#000000", fill="#939598") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = scales::percent, breaks = c(0,.1,.2,.3,.4,.5,.6)) +
  labs(title = "Type of Terrorism", x = "", y = "") +
  theme_classic() +
  mytheme


jpeg("outputs/figures/Figure1.jpeg", width = 15, height = 16, units = 'in', res = 300)
ggarrange(StudyYear, PubYear, TerrorMeasures, TerrorType,
          labels = c("(A)", "(B)", "(C)", "(D)"),
          font.label = list(size = 22, color = "black"),
          ncol = 2, nrow = 2) #arrange all plots
dev.off() #safe picture in high resolution.
