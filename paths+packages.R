## paths+packages.R
# a place to keep variables, functions, etc. relevant across numerous scripts

# load packages
library(tidyverse)
library(lubridate)
library(zoo)
library(imputeTS)
library(ggpubr)
library(stringr)
library(xts)
library(dygraphs)
library(factoextra)
library(dataRetrieval)
library(cowplot)

# Setting up paths for data and scripts

# GitHub repository with scripts 
dir_DataAnalysis <- file.path("..", "wolv_DOC")

# Directory for data 
dir_data <- file.path("~/BSU_drive/Projects/AK_post-doc/DOC/wolv_DOC/Data")

## Color Palettes 
# Sampling/sonde sites 
col.forest <- "#E2725B"
col.nellie <- "#EA9DFF"
col.shrub <- "#FFAA00"
col.tundra <- "#A80084"
col.gage <- "#73DFFF"
col.term <- "#059E41"
col.glacier <- "#0084A8"
col.lake_in <-  "#6600CC" 

# Other data tyes in plots 
col.Q <- "black"
col.Precip <- "darkslateblue"
col.cum_flux <- '#006633'

## GGplot theme
theme_cust <- function(base_size = 16, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text = element_text(color = "black")
    )
}

  