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

# create vectors for each column
label <- c("Desert Winds Continuation",
           "Antelope Valley High",
           "Eastside High",
           "Highland High",
           "Knight High",
           "Lancaster High","Littlerock High",
           "Palmdale High","Quartz Hill High")
student_counselor_ratio <- c("621:1","252:1","471:1","397:1","280:1","337:1","305:1","393:1","453:1")

counselors <- c(1.0,6.0,5.5,7.0,9.0,8.0,5.0,6.0,7.0)
students <- c(621,1512,2592,2778,2520,2697,1526,2358,3170)
counselors_needed_to_match <- c(1.1,0,3.2,2.3,0,1.1,0.1,1.9,3.6)

# create data frame
counselor_df <- data.frame(label, total_students,  total_counselors, student_counselor_ratio, counselors_needed_to_match)

# Move NA values to bottom and order numerically 
counselor_df<-counselor_df%>%
  arrange(desc(number_of_counselors_needed))

# source visual script
source("./Visuals/visual_fx.R")


# COUNSELORS STATIC TABLE--------------------------

indicator="Counselor Ratio"
title_text="13 Counselors are Needed in AVUHSD to Match Ideal Student-to-Counselor Ratio"

# Apply function

static_table(df=counselor_df,
             indicator=indicator,
             group_col="School", # specify the way you want the category column to be labeled as on the visual
             title_text=title_text)

