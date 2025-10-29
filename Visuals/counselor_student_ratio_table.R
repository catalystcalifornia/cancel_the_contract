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
school <- c("Desert Winds Continuation", "Antelope Valley High","Eastside High","Highland High","Knight High","Lancaster High","Littlerock High","Palmdale High","Quartz Hill High")
student_counselor_ratio <- c("621:1","252:1","471:1","397:1","280:1","337:1","305:1","393:1","453:1")
counselors <- c(1.0,6.0,5.5,7.0,9.0,8.0,5.0,6.0,7.0)
students <- c(621,1512,2592,2778,2520,2697,1526,2358,3170)
counselors_needed_to_match <- c(1.1,NA,3.2,2.3,NA,1.1,0.1,1.9,3.6)

# create data frame
counselor_df <- data.frame(school, student_counselor_ratio, counselors, students, counselors_needed_to_match)

# create student teacher ratio table
acs_english <- c(9,18,19,21,17,18,18,17,18)
acs_math <- c(9,18,23,21,21,22,17,22,20)
acs_science <- c(7,19,25,23,23,21,13,20,22)
acs_history <- c(8,20,25,24,21,22,19,19,18)
teachers_needed <- c(NA,NA,5.2,4.1,2.7,2.2,NA,1.2,1.5)

# create data frame
teacher_df <- data.frame(school, acs_english, acs_math, acs_science, acs_history, teachers_needed)

