# Antelope Valley Stops by Perceived Disability 2018-2023


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


############### CALCULATE STOPS BY DISABILITY ########################

av_stops <- lasd_incidents %>% left_join(lasd_persons, by = "contact_id")

# check how many of each category of no_disabilities
table(av_stops$no_disabilities)

# QA: Explore the other separate disability columns to make sure the no_disabilities flag accounts for those

disab<-lasd_persons%>%
  select(person_id, contact_id, deaf_or_difficulty_hearing, speech_impaired_or_limited_use_of_language,
         blind_or_limited_vision, mental_health_condition, intellectually_or_developmental_disability, other_disabilities,
         hyperactive_or_impulsive_behavior, no_disabilities)

disab_no <- disab %>%
  filter(no_disabilities %in% c("false", "No")) 

# I manually checked each of the disability columns and they all contain some `'true' or 'Yes' values 

# Now check if any of the columns have disabilities == Yes/true when filtering for the opposite

disab_yes <- disab %>%
  filter(no_disabilities %in% c("true", "Yes")) ## there are NO Yes/true values in the other disability columns so we're OK

# create data frame of just stops with disabilities

av_stops_disabled <- av_stops %>% filter(no_disabilities %in% c("false", "No"))

# double check all have a disability
table(av_stops_disabled$no_disabilities)

# check that all disabilities have at least one true - all except blind or limited vision
table(av_stops_disabled$deaf_or_difficulty_hearing) 
table(av_stops_disabled$speech_impaired_or_limited_use_of_language)
table(av_stops_disabled$blind_or_limited_vision)
table(av_stops_disabled$mental_health_condition)
table(av_stops_disabled$intellectually_or_developmental_disability)
table(av_stops_disabled$other_disabilities)
table(av_stops_disabled$hyperactive_or_impulsive_behavior)

# calculate number of stops with a disability
stops_w_disability <- nrow(av_stops_disabled)

# calculate total stops
stops <- nrow(av_stops)

# calculate stops with a disability as a percent of total stops
pct_stops_w_disability <- stops_w_disability / stops * 100


############### GET ENROLLMENT DATA and CALUCLATE ENROLLMENT BY DISABILITY ########################

enrollment <- dbGetQuery(con_shared, "SELECT * FROM education.cde_multigeo_enrollment_census_day_2024_25")

# get Antelope Valley Union High School District enrollment
av_enrollment <- enrollment %>% filter(districtname == "Antelope Valley Union High" & charter == "ALL" & 
                                         reportingcategory == "TA") %>% 
  pull(total_enr)

# get AVUHSD disabled enrollment
av_enrollment_disabled <- enrollment %>% filter(districtname == "Antelope Valley Union High" & 
                                                     charter == "ALL" & 
                                                     reportingcategory == "SG_DS") %>% 
  pull(total_enr)

# calculate enrollment with a disability as a percent of total enrollment
pct_enrollment_w_disability <- av_enrollment_disabled / av_enrollment * 100


############### PUT IT TOGETHER ########################

df <- data.frame(stops, 
                 stops_w_disability, 
                 pct_stops_w_disability, 
                 av_enrollment, 
                 av_enrollment_disabled, 
                 pct_enrollment_w_disability)

#### Clean up final df for postgres #####

df<-df%>%
  mutate(geography="Antelope Valley Union High School District",
         reportingcategory_re="Students with Disabilities")%>%
  rename("stops_total"="stops",
         "stops_disability_count"="stops_w_disability",
         "stops_disability_rate"="pct_stops_w_disability",
         "enrollment_total"="av_enrollment",
         "enrollment_disabled_count"="av_enrollment_disabled",
         "enrollment_disability_rate"="pct_enrollment_w_disability")%>%
  select(geography, reportingcategory_re, everything())


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






