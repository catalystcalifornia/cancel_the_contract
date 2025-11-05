# Create demographic chart for CTC report 
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
con_ctc <- connect_to_db("cancel_the_contract")

# connect to function script
source("./Visuals/visual_fx.R")


# AV DEMOGRAPHICS (RACE) BAR GRAPH ------------------------------------

df<-dbGetQuery(con_ctc, "SELECT * FROM data.av_population_race")

# first apply the race_recode function if you are visualizing something disaggregated by race
# for it to work you need to rename your column that needs to be recoded to 'label'

df<-df%>%
  rename("label"="race")%>% # This is the column that needs to get renamed
  race_recode() # apply race recoding

# NOTE: The indicator field needs to match the way it is in the data dictionary indicator_short column
## i.e.) for suspensions by race, I need to set indicator== "Suspensions by race"

indicator<-"Race"
title_text<-"The Majority of the Antelope Valley Population is Latinx, White or Black"

# Apply function

single_bar(df=df,
           indicator=indicator,
           title_text=title_text
               )

# Go check results in ./Visuals
