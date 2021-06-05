library(tidyverse) 
library(plotly) 
library(glue) 
library(scales) 
library(ggpubr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(caret)

placement <- read.csv("Placement_Data_Full_Class (1).csv")

p_status <- placement %>% select(-c(sl_no,salary)) %>%
  mutate(gender = as.factor(gender),
         ssc_b = as.factor(ssc_b),
         hsc_b = as.factor(hsc_b),
         hsc_s = as.factor(hsc_s),
         degree_t = as.factor(degree_t),
         workex = as.factor(workex),
         specialisation = as.factor(specialisation),
         status = as.factor(status)) %>%
  drop_na()

theme_algoritma <- theme(legend.key = element_rect(fill="black"),
                         legend.background = element_rect(color="white", fill="#263238"),
                         plot.subtitle = element_text(size=6, color="white"),
                         panel.background = element_rect(fill="#dddddd"),
                         panel.border = element_rect(fill=NA),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(color="darkgrey", linetype=2),
                         panel.grid.minor.y = element_blank(),
                         plot.background = element_rect(fill="#263238"),
                         text = element_text(color="white"),
                         axis.text = element_text(color="white")
                         
)

model_forest <- readRDS("model_forest.RDS")