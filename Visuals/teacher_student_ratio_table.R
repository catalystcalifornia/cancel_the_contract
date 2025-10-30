# Create static table of counselor student ration for CTC report
# Author: CR

# Set up workspace----------------------------------

list.of.packages <- c("RPostgres", "tidyverse", "gt", "showtext", "scales", "forcats") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

#### Loading Libraries ####
library(RPostgres)
library(tidyverse)
library(gt)
library(showtext)
library(scales)
library(forcats)

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("cancel_the_contract")

# create student teacher ratio table
label <- c("Desert Winds Continuation", "Antelope Valley High","Eastside High","Highland High","Knight High","Lancaster High","Littlerock High","Palmdale High","Quartz Hill High")
acs_english <- c(9,18,19,21,17,18,18,17,18)
acs_math <- c(9,18,23,21,21,22,17,22,20)
acs_science <- c(7,19,25,23,23,21,13,20,22)
acs_history <- c(8,20,25,24,21,22,19,19,18)
teachers_needed <- c(NA,NA,5.2,4.1,2.7,2.2,NA,1.2,1.5)

# create data frame
teacher_df <- data.frame(label, acs_english, acs_math, acs_science, acs_history, teachers_needed)

# source visual script
source("./Visuals/visual_fx.R")


# COUNSELORS STATIC TABLE--------------------------

indicator="Student-to-Teacher Ratio"
title_text="Seventeen Teachers are Needed for Recommended Student-to-Teacher Ratios"

# Apply function

static_table(df=teacher_df,
             indicator=indicator,
             group_col="School", # specify the way you want the category column to be labeled as on the visual
             title_text=title_text)

