# Antelope Valley Union High School District Stops by Stop Result


############### SET UP ########################

#install packages if not already installed
list.of.packages <- c("tidyverse","janitor", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(janitor)
library(data.table)

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con_shared <- connect_to_db("rda_shared_data")
con<- connect_to_db("cancel_the_contract")


############### GET STOPS DATA ########################

# incidents
lasd_incidents <- dbGetQuery(con_shared, "SELECT * FROM crime_and_justice.lasd_stops_incident_2018_2023
WHERE school_name
IN (
'Antelope Valley High',
'Antelope Valley Union High',
'Eastside High',
'Highland High',
'William J. (Pete) Knight High',
'Lancaster High',
'Littlerock High',
'Palmdale High',
'Quartz Hill High',
'R. Rex Parris High',
'Desert Winds Continuation High',
'Phoenix High Community Day',
'Phoenix Continuation'
)
ORDER BY school_name
")

# persons
lasd_persons <- dbGetQuery(con_shared, "SELECT * FROM crime_and_justice.lasd_stops_person_2018_2023")


############### CALCULATE STOPS BY STOP REASON ########################

av_stops <- lasd_incidents %>% left_join(lasd_persons, by = "contact_id")

# take out stop reason columns and recode true/yes/false/no values to 1/0

av_stops_result<-av_stops%>%
  select(contact_id, person_id, starts_with("result"))


# convert to long form

av_stops_result_long<-av_stops_result%>%
  mutate(across(3:19, ~ case_when(
    . %in% c("Yes", "true") ~ 1,
    . %in% c("No", "false") ~ 0,
    TRUE ~ NA_real_)))%>%
  pivot_longer(
    cols = 3:19,
    names_to = "stop_reason",
    values_to = "value"
  )%>%
  group_by(contact_id, person_id)%>%
  filter(value!=0) # only keep where at least one of the racial columns == 1 

# Explore if the same person had 2 different stop results

twoormor<-av_stops_result%>%
  group_by(person_id, contact_id) %>%
  filter(n() > 1) %>%
  ungroup()

# explore stop reasons

stop_reasons<-as.data.frame(table(av_stops_result_long$stop_reason)) # the top result is result_of_contact_in_field_cite_and_release = 620

##### Export stops where a person was stopped for an in field cite and release and the accompanying citation code for CS to review #######

infield<-av_stops%>%
  select(person_id, contact_id, result_of_contact_in_field_cite_and_release, 
         result_of_contact_in_field_cite_and_release_offense_codes)%>%
  filter(result_of_contact_in_field_cite_and_release=="Yes" | result_of_contact_in_field_cite_and_release == "true")

# export as excel file



############### SEND TO POSTGRES ########################


# set column types

charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df

# add df colnames to the character vector

names(charvect) <- colnames(df)

table_name <- "analysis_stops_disability_avuhsd"
schema <- "data"
indicator <- "Analysis table of AVUSHD LASD stops by disability and CDE enrollment by disability."
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_disability.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_Sheet_Disability.R" 
table_comment <- paste0(indicator, source)
table_name <- "analysis_stops_disability_avuhsd"

# push to postgres
dbWriteTable(con,  table_name, df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data


# Comment on table and columns

column_names <- colnames(df) # Get column names
column_comments <- c(
  "geography level",
  "CDE reporting category recoded",
  "LASD stops in AVUHSD",
  "LASD stops with a disability in AVUHSD",
  "Percent of LASD stops with a disability in AVUHSD",
  "AVUHSD enrollment",
  "AVUHSD enrollment with a disability",
  "Percent of AVUHSD enrollment with a disability"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
dbDisconnect(con_shared)