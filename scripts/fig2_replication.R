###############################################################
# Citation link:
# https://dataverse.harvard.edu/file.xhtml?fileId=5370285&version=1.2
# Codes are obtained from the above link, in 02-meta-analysis.R, with adjusment
###############################################################

#### Preamble ####
# Purpose: Scripts for generating figure 2
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


##### Fig3A.1 Overall ##### 
library(broom)
library(dotwhisker)
library(cowplot)
library(metafor)

dat_outgroup <- read_csv("outputs/data/dat_outgroup.csv")
dat_conservatism <- read_csv("outputs/data/dat_conservatism.csv")
dat_rally <- read_csv("outputs/data/dat_rally.csv")
################################################
set.seed(1234)
og_overall <- rma.mv(y=Fisher, V=Variance_F,
                     random = ~ 1 | ID_R/ID_ES_Unique, data=dat_outgroup,
                     method = "ML")
og_overall_df <- tidy(og_overall) |> # create data.frame of regression results
  relabel_predictors(overall = "Overall Fisher's Z \n Correlation Coef." )           
og_overall_plot <- dwplot(og_overall_df, #plot
                          dot_args = list(size = 3),
                          whisker_args = list(size = 1),
                          vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 1))+
  ggtitle("Outgroup Hostility") +
  scale_color_grey() +
  xlim(-0.1, .25) +
  theme(axis.title.x=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.text.y = element_text(face = "bold",size = 30),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        panel.background = element_rect(fill = "grey90",
                                        colour = "grey90"),
        panel.grid = element_line(colour = "grey60"))


##### Fig3A.2 Terror type ##### 
#fit model and plot results
set.seed(1234)
og_terror <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(Islam, Other_Ideology, No_Ideology),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_outgroup,
                    method = "ML")

og_terror_df <- tidy(og_terror) |> # create data.frame of regression results
  relabel_predictors(c(Islam = "Islamist",
                       Other_Ideology  = "Other ideology",
                       No_Ideology  = "No ideology"))
og_terror_plot <- dwplot(og_terror_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.text.y = element_text(size = 25)) +
  scale_color_grey() +
  xlim(-0.1, .25) 

##### Fig3A.3 Research Design ##### 
#fit model and plot results
set.seed(1234)
og_design <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(Exp, QuasiLong, Corr),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_outgroup,
                    method = "ML")

og_design_df <- tidy(og_design) |> # create data.frame of regression results
  relabel_predictors(c(Exp = "Experiment",
                       Corr  = "Cross-sectional",
                       QuasiLong  = "Other designs"))
og_design_plot <- dwplot(og_design_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.text.y = element_text(size = 25)) +
  scale_color_grey() +
  xlim(-0.1, .25) 

##### Fig3A.4 Sample  ##### 
#fit model and plot results
set.seed(1234)
og_sample <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(General, Student, Convenience),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_outgroup,
                    method = "ML")

og_sample_df <- tidy(og_sample) # create data.frame of regression results

og_sample_df <- og_sample_df |>  #re-label variable
  relabel_predictors(c(General = "General population",
                       Student  = "Student sample",
                       Convenience  = "Convenience sample"))
og_sample_plot <- dwplot(og_sample_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.text.y = element_text(size = 25)) +
  scale_color_grey() +
  xlim(-0.1, .25) 

##### Fig3A.5 Geography ##### 
#fit model and plot results
set.seed(1234)
og_location <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(US, Israel, Other_c),
                      intercept=F, 
                      random = ~ 1 | ID_R/ID_ES_Unique, data=dat_outgroup,
                      method = "ML")
og_location_df <- tidy(og_location) # create data.frame of regression results

og_location_df <- og_location_df |>  #re-label variable
  relabel_predictors(c(US = "United States",
                       Israel  = "Israel",
                       Other_c  = "Other countries"))
og_location_plot <- dwplot(og_location_df, #plot
                           dot_args = list(size = 2),
                           whisker_args = list(size = 0.5),
                           vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 25)) +
  scale_color_grey() +
  xlim(-0.1, .25) 

##### Fig3A.6 Guilt-by-Association ##### 
#fit model and plot results
set.seed(1234)
og_association <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(Lot, Bit, No),
                         intercept=F, 
                         random = ~ 1 | ID_R/ID_ES_Unique, data=dat_outgroup,
                         method = "ML")

og_association_df <- tidy(og_association) # create data.frame of regression results

og_association_df <- og_association_df |>  #re-label variable
  relabel_predictors(c(Lot = "Strong association",
                       Bit  = "Moderate association",
                       No  = "No association"))
og_association_plot <- dwplot(og_association_df, #plot
                              dot_args = list(size = 2),
                              whisker_args = list(size = 0.5),
                              vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  ggtitle("Guilt-By-Association") +
  scale_y_discrete(position = "left") +
  scale_color_grey() +
  theme(axis.title=element_text(face = "bold"),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.text.y = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        plot.title = element_text(size = 20, face = "bold"))+
  xlim(-0.1, .25) 

##### Fig3A Outgroup Hostility Effect Sizes Plot ##### 
og_plots <- plot_grid(og_overall_plot, og_terror_plot, og_design_plot, og_sample_plot, og_location_plot,
                      og_association_plot,
                      ncol=1, nrow=6, 
                      rel_heights=c(0.8,1,1,1,1,1), align = "v")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
##### Fig3B. Conservative Shift Hypothesis ##### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
##### Fig3B.1 Overall ##### 
set.seed(1234)
cs_overall <- rma.mv(y=Fisher, V=Variance_F,
                     random = ~ 1 | ID_R/ID_ES_Unique, data=dat_conservatism,
                     method = "ML")
cs_overall_df <- tidy(cs_overall) # create data.frame of regression results

cs_overall_df <- cs_overall_df |> 
  relabel_predictors(c(overall = "Overall Effect Size")) #re-label variable
cs_overall_plot <- dwplot(cs_overall_df, #plot
                          dot_args = list(size = 3),
                          whisker_args = list(size = 1),
                          vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 1))+
  ggtitle("Conservative Shift") +
  scale_color_grey() +
  xlim(-0.1, .25) +
  theme(axis.title.x=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        panel.background = element_rect(fill = "grey90",
                                        colour = "grey90"),
        panel.grid = element_line(colour = "grey60"))

##### Fig3B.2 Terror type ##### 
#fit model and plot results
set.seed(1234)
cs_terror <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(Islam, Other_Ideology, No_Ideology),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_conservatism,
                    method = "ML")

cs_terror_df <- tidy(cs_terror) # create data.frame of regression results

cs_terror_df <- cs_terror_df |>  #re-label variable
  relabel_predictors(c(Islam = "Islamist",
                       Other_Ideology  = "Other Ideology",
                       No_Ideology  = "No Ideology"))
cs_terror_plot <- dwplot(cs_terror_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(plot.title = element_text(size = 30), axis.text=element_blank(), axis.ticks=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  scale_color_grey() +
  xlim(-0.1, .25) 

##### Fig3B.3 Research Design ##### 
#fit model and plot results
set.seed(1234)
cs_design <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(Exp, QuasiLong, Corr),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_conservatism,
                    method = "ML")
cs_design_df <- tidy(cs_design) # create data.frame of regression results

cs_design_df <- cs_design_df |>  #re-label variable
  relabel_predictors(c(Exp = "Experiments",
                       Corr  = "Cross-Sectional",
                       QuasiLong  = "Other Designs"))
cs_design_plot <- dwplot(cs_design_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(plot.title = element_text(size = 30), axis.text=element_blank(), 
        axis.ticks=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  scale_color_grey() +
  xlim(-0.1, .25) 

##### Fig3B.4 Sample  ##### 
#fit model and plot results
set.seed(1234)
cs_sample <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(General, Student, Convenience),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_conservatism,
                    method = "ML")

cs_sample_df <- tidy(cs_sample) # create data.frame of regression results

cs_sample_df <- cs_sample_df |>  #re-label variable
  relabel_predictors(c(General = "General Population",
                       Student  = "Student Samples",
                       Convenience  = "Convenience Samples"))
cs_sample_plot <- dwplot(cs_sample_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(plot.title = element_text(size = 30), 
        axis.text=element_blank(), axis.ticks=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  scale_color_grey() +
  xlim(-0.1, .25) 

##### Fig3B.5 Geography ##### 
#fit model and plot results
set.seed(1234)
cs_location <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(US, Israel, Other_c),
                      intercept=F, 
                      random = ~ 1 | ID_R/ID_ES_Unique, data=dat_conservatism,
                      method = "ML")
cs_location_df <- tidy(cs_location) # create data.frame of regression results

cs_location_df <- cs_location_df |>  #re-label variable
  relabel_predictors(c(US = "United States",
                       Israel  = "Israel",
                       Other_c  = "Other Countries"))
cs_location_plot <- dwplot(cs_location_df, #plot
                           dot_args = list(size = 2),
                           whisker_args = list(size = 0.5),
                           vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(plot.title = element_text(size = 30), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.text.x = element_text(size = 20)) +
  scale_color_grey() +
  xlim(-0.1, .25) 

##### Fig3B Conservative Shift Effect Sizes Plot ##### 
cs_plots <- plot_grid(cs_overall_plot, cs_terror_plot, cs_design_plot, cs_sample_plot, cs_location_plot, 
                      ncol=1, nrow=6, 
                      rel_heights=c(0.8,1,1,1,1,1), align = "v")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
##### Fig3C. Rally Effects Hypothesis ##### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

##### Fig3C.1 Overall ##### 

set.seed(1234)
rf_overall <- rma.mv(y=Fisher, V=Variance_F,
                     random = ~ 1 | ID_R/ID_ES_Unique, data=dat_rally,
                     method = "ML")
rf_overall_df <- tidy(rf_overall) # create data.frame of regression results

rf_overall_df <- rf_overall_df |> 
  relabel_predictors(c(overall = "Overall Effect Size")) #re-label variable
rf_overall_plot <- dwplot(rf_overall_df, #plot
                          dot_args = list(size = 3),
                          whisker_args = list(size = 1),
                          vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 1))+
  ggtitle("Rally Effects") +
  scale_color_grey() +
  xlim(-0.1, .25) +
  theme(axis.title.x=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        panel.background = element_rect(fill = "grey90",
                                        colour = "grey90"),
        panel.grid = element_line(colour = "grey60"))


##### Fig3C.2 Terror type ##### 
#fit model and plot results
set.seed(1234)
rf_terror <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(Islam, Other_Ideology, No_Ideology),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_rally,
                    method = "ML")
rf_terror_df <- tidy(rf_terror) # create data.frame of regression results

rf_terror_df <- rf_terror_df |>  #re-label variable
  relabel_predictors(c(Islam = "Islamist",
                       Other_Ideology  = "Other Ideology",
                       No_Ideology  = "No Ideology"))
rf_terror_plot <- dwplot(rf_terror_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  ylab("Ideology") +
  scale_y_discrete(position = "right") +
  scale_color_grey() +
  theme(axis.text=element_blank(), axis.ticks=element_blank(), 
        axis.title.y=element_text(face = "bold", size = 30),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  xlim(-0.1, .25) 


##### Fig3C.3 Research Design ##### 
#fit model and plot results
set.seed(1234)
rf_design <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(Exp, QuasiLong, Corr),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_rally,
                    method = "ML")
rf_design_df <- tidy(rf_design) # create data.frame of regression results

rf_design_df <- rf_design_df |>  #re-label variable
  relabel_predictors(c(Exp = "Experiments",
                       Corr  = "Cross-Sectional",
                       QuasiLong  = "Other Designs"))
rf_design_plot <- dwplot(rf_design_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(axis.text=element_blank(), axis.ticks=element_blank())+
  ylab("Design") +
  scale_y_discrete(position = "right") +
  scale_color_grey() +
  theme(axis.text=element_blank(), axis.ticks=element_blank(), 
        axis.title.y=element_text(face = "bold", size = 30),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  xlim(-0.1, .25) 

##### Fig3C.4 Sample  ##### 
#fit model and plot results
set.seed(1234)
rf_sample <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(General, Student, Convenience),
                    intercept=F, 
                    random = ~ 1 | ID_R/ID_ES_Unique, data=dat_rally,
                    method = "ML")
rf_sample_df <- tidy(rf_sample) # create data.frame of regression results

rf_sample_df <- rf_sample_df |>  #re-label variable
  relabel_predictors(c(General = "General Population",
                       Student  = "Student Samples",
                       Convenience  = "Convenience Samples"))
rf_sample_plot <- dwplot(rf_sample_df, #plot
                         dot_args = list(size = 2),
                         whisker_args = list(size = 0.5),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  theme(axis.text=element_blank(), axis.ticks=element_blank())+
  ylab("Sample") +
  scale_y_discrete(position = "right") +
  scale_color_grey() +
  theme(axis.text=element_blank(), axis.ticks=element_blank(), 
        axis.title.y=element_text(face = "bold", size = 30),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))+
  xlim(-0.1, .25) 


##### Fig3C.5 Geography ##### 
#fit model and plot results
set.seed(1234)
rf_location <- rma.mv(y=Fisher, V=Variance_F, mods = cbind(US, Israel, Other_c),
                      intercept=F, 
                      random = ~ 1 | ID_R/ID_ES_Unique, data=dat_rally,
                      method = "ML")
rf_location_df <- tidy(rf_location) # create data.frame of regression results

rf_location_df <- rf_location_df |>  #re-label variable
  relabel_predictors(c(US = "United States",
                       Israel  = "Israel",
                       Other_c  = "Other Countries"))
rf_location_plot <- dwplot(rf_location_df, #plot
                           dot_args = list(size = 2),
                           whisker_args = list(size = 0.5),
                           vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.7)) +
  ylab("Country") +
  scale_y_discrete(position = "right") +
  scale_color_grey() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.title.y=element_text(face = "bold", size = 30),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.text.x = element_text(size = 20))+
  xlim(-0.1, .25) 

##### Fig3C Rally Effects Effect Sizes Plot ##### 
rf_plots <- plot_grid(rf_overall_plot, rf_terror_plot, rf_design_plot, rf_sample_plot, rf_location_plot,
                      ncol=1, nrow=6, 
                      rel_heights=c(0.8,1,1,1,1,1), align = "v")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
################################# FIGURE 3 ###############################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
jpeg("outputs/figures/Figure2.jpeg", width = 15, height = 16, units = 'in', res = 300)
ggarrange(og_plots, cs_plots, rf_plots,
          align = "hv",
          ncol = 3, nrow = 1,
          widths=c(2.2,1.3,1.4)) #arrange all plots
dev.off() 
