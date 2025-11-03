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
english_average_class_size <- c(9,18,19,21,17,18,18,17,18)
math_average_class_size <- c(9,18,23,21,21,22,17,22,20)
science_average_class_size <- c(7,19,25,23,23,21,13,20,22)
history_average_class_size <- c(8,20,25,24,21,22,19,19,18)
number_of_teachers_needed <- c(NA,NA,5.2,4.1,2.7,2.2,NA,1.2,1.5)

# create data frame
teacher_df <- data.frame(label, english_average_class_size, math_average_class_size,
                         science_average_class_size, history_average_class_size, number_of_teachers_needed)

# source visual script
source("./Visuals/visual_fx.R")


# COUNSELORS STATIC TABLE--------------------------

indicator="Student-to-Teacher Ratio"
title_text="Hiring 17 Teachers Reduces Class Size to 20–23 Students Across AVUHSD"

# Apply function

static_table(df=teacher_df,
             indicator=indicator,
             group_col="School", # specify the way you want the category column to be labeled as on the visual
             title_text=title_text)

