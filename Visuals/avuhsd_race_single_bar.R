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
con_shared <- connect_to_db("rda_shared_data")
con_ctc <- connect_to_db("cancel_the_contract")

# connect to function script
source("./Visuals/visual_fx.R")

# AVUHSD DEMOGRAPHICS (RACE) BAR GRAPH ------------------------------------

df<-dbGetQuery(con_ctc, "SELECT * FROM data.avuhsd_population_race")

# first apply the race_recode function if you are visualizing something disaggregated by race
# for it to work you need to rename your column that needs to be recoded to 'label'

df<-df%>%
  rename("label"="race")%>% # This is the column that needs to get renamed
  race_recode() # apply race recoding

# NOTE: The indicator field needs to match the way it is in the data dictionary indicator_short column
## i.e.) for suspensions by race, I need to set indicator== "Suspensions by race"

indicator<-"Student Race"
title_text<-"Latinx, Black, and white students comprise the majority of students in AVUHSD"

# Apply function

single_bar(df=df,
           indicator=indicator,
           title_text=title_text
)

# Go check results in ./Visuals


# PUT AVUHSD DEMOGRAPHICS (RACE) TABLE IN POSTGRES -----------------------------

# THIS IS CODE FROM W:/Project/RJS/CTC/Github/CR/cancel_the_contract/RDA Sense Making/RDA_findings_report.Rmd
# I figured it might be better to run it here and export table to Postgres after a little formatting

# enrollment <- dbGetQuery(con_shared, "SELECT * FROM education.cde_multigeo_enrollment_census_day_2024_25")
# 
# av_enrollment <- enrollment %>% filter(districtname == "Antelope Valley Union High" & charter == "ALL")%>%
#    filter(grepl("RE_|TA", reportingcategory))%>%
#    mutate(total = total_enr[reportingcategory == "TA"])%>%
#    filter(!grepl("TA", reportingcategory))
# 
# av_enrollment<-av_enrollment%>%
#    mutate(reportingcategory_re=ifelse(reportingcategory %in% "RE_B", "nh_black",
#                                       ifelse(reportingcategory %in% "RE_I", "nh_aian",
#                                              ifelse(reportingcategory %in% "RE_A", "nh_asian",
#                                                     ifelse(reportingcategory %in% "RE_F", "nh_filipino",
#                                                            ifelse(reportingcategory %in% "RE_H", "latinx",
#                                                                   ifelse(reportingcategory %in% "RE_D", "missing_race",
#                                                                          ifelse(reportingcategory %in% "RE_P", "nh_nhpi",
#                                                                                 ifelse(reportingcategory %in% "RE_T", "nh_twoormor",
#                                                                                        ifelse(reportingcategory %in% "RE_W", "nh_white",
#                                                                                               reportingcategory))))))))))%>%
#    mutate(race_label=ifelse(reportingcategory %in% "RE_B", "Black",
#                             ifelse(reportingcategory %in% "RE_I", "AIAN",
#                                    ifelse(reportingcategory %in% "RE_A", "Asian",
#                                           ifelse(reportingcategory %in% "RE_F", "Filipinx",
#                                                  ifelse(reportingcategory %in% "RE_H", "Latinx",
#                                                         ifelse(reportingcategory %in% "RE_D", "Missing race",
#                                                                ifelse(reportingcategory %in% "RE_P", "NHPI",
#                                                                       ifelse(reportingcategory %in% "RE_T", "Multiracial",
#                                                                              ifelse(reportingcategory %in% "RE_W", "White",
#                                                                                     reportingcategory))))))))))%>%
# 
#    select(districtname, race_label, total_enr, total)%>%
#    mutate(rate=total_enr/total*100)%>%
#    arrange(-rate)
# 
# # # format
#  df <- av_enrollment %>%
#    rename(geography = districtname,
#           race = race_label,
#           count = total_enr,
#           total = total) %>%
#    mutate(race = case_when(race == "Missing race" ~ "Other",
#                     TRUE ~ race))
# 
# ############### PUSH TABLE TO POSTGRES #####################
# 
# # set column types
#  charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df
#  charvect <- replace(charvect, c(3:5), c("numeric"))
# 
#  # add df colnames to the character vector
#  names(charvect) <- colnames(df)
# 
#  table_name <- "avuhsd_population_race"
#  schema <- "data"
#  indicator <- "AVUHSD Population by Race, 2024-25"
#  source <- "R script: W:/Project/RJS/CTC/Github/CR/cancel_the_contract/Visuals/avuhsd_race_single_bar.R"
#  qa_filepath <- "W:/Project/RJS/CTC/Documentation/QA_cr_viz.docx"
#  table_comment <- paste0(indicator, source)
# 
#  # push to postgres
#  dbWriteTable(con_ctc, table_name, df,
#               overwrite = TRUE, row.names = FALSE,
#               field.types = charvect)
# 
#  # add meta data
# 
#  column_names <- colnames(df) # Get column names
#  column_comments <- c(
#    "School District",
#    "Student Subgroup",
#    "Student Subgroup Enrollment",
#    "Total Student Enrollment",
#    "Percent of Student Subgroup Enrollment out of Total Student Enrollment"
#  )
# 
# add_table_comments(con_ctc, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


#dbDisconnect(con_shared)
dbDisconnect(con_ctc)

