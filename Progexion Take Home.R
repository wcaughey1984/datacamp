#
# Title:    Progrexion Data 
# Purpose:  (Progexion Senior Data Analyst) 
# Author:   Billy Caughey
# Date:     2020.11.24 - Initial Build 
# 

##### Libraries #####

library(tidyverse)
library(Amelia)

##### Bring in the data #####

prog_data <- read.csv("/Users/wcaughey/Downloads/Case Study/data.csv")

##### Missmap #####

missmap(prog_data)

