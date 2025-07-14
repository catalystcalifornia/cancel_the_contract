library(RPostgreSQL)
library(dplyr)
library(stringr)
library(tidytext)

# Connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con_rda <- connect_to_db("rda_shared_data")
con_ctc <- connect_to_db("cancel_the_contract")

# Get LASD stop-level data for Antelope Valley high schools
lasd_incidents <- dbGetQuery(con_rda, "SELECT * FROM crime_and_justice.lasd_stops_incident_2018_2023
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
'Phoenix Continuation')
ORDER BY school_name") 

# Get LASD person-level data
lasd_persons <- dbGetQuery(con_rda, "SELECT * FROM crime_and_justice.lasd_stops_person_2018_2023")

# join stops and person level table together so each row is a person

av_stops <- lasd_persons %>% right_join(lasd_incidents, by = "contact_id")


############### CLEAN UP AND RECODE COLUMNS ########################

# select columns I want and across the reasonable suspicion columns recode them to be 1/0 columns

av_stops_re<-av_stops%>%
  select(1,2,14,40, 41, 44:53)%>% 
  mutate(across(6:14, ~ case_when( 
    . %in% c("Yes", "true") ~ 1,
    . %in% c("No", "false") ~ 0,
    TRUE ~ NA_real_)))
  

# EXPLORE/RECODE:  People who are given more than 1 stop reason----------------------------

# pivot data longer                          

av_stops_long<-av_stops_re%>%
  pivot_longer(
    cols = 6:14, # adjusted from race_avuhsd
    names_to = "reportingcategory",
    values_to = "value"
  )%>%
  group_by(contact_id, person_id)%>%
  filter(value!=0) # only keep where at least one of the racial columns == 1 

reason_age<-av_stops_long%>%
  group_by(contact_id, person_id)%>%
  mutate(total=n()) # we can see there are people who have more than 1 stop reason

# filter out everyone who shows up more than once 

reason_age_multi<-reason_age%>%
  filter(total>1)

length(unique(reason_age_multi$person_id)) # 75 distinct people have stops for more than 1 reason out of 904 total people. I feel OK just recoding these to be "two or more"

# Recode my wide AND long table

av_stops_re<-av_stops_re%>%
  mutate(reason_for_contact=ifelse(person_id %in% reason_age_multi$person_id, "Two or more reasons", reason_for_contact))

av_stops_long<-av_stops_long%>%
  mutate(reason_for_contact=ifelse(person_id %in% reason_age_multi$person_id, "Two or more reasons", reason_for_contact))%>%
  group_by(person_id, contact_id)%>%
  slice(1) # now we can remove duplicate rows because the stop reason has been recoded

length(unique(av_stops_long$person_id)) # now only 811 unique people but there should be 904?

# see who is dropped

join<-av_stops_re%>%
  left_join(av_stops_long)%>%
  filter(is.na(reportingcategory)) # these are people where NONE of the 1/0 reasonable suspicion columns are indicated. These need to stay in the data

# redo the av_long tablea code but make sure to keep the 93 people who have 0 dummy variables marked

keep<-join$person_id

av_stops_long_fix<-av_stops_re%>%
  pivot_longer(
    cols = 6:14, # adjusted from race_avuhsd
    names_to = "reportingcategory",
    values_to = "value"
  )%>%
  group_by(contact_id, person_id)%>%
  filter(any(value != 0) | person_id %in% keep)

check<-av_stops_long_fix%>%
  filter(person_id %in% keep )


# Recode age categories-----------------------------

av_stops_re <- av_stops_re %>% mutate(age_re=case_when(age <= 17 ~ "0 to 17",
                                                              age >= 18 & age <= 24 ~ "18 to 24",
                                                              age >= 25 & age <= 34 ~ "25 to 34",
                                                              age >= 35 & age <= 44 ~ "35 to 44",
                                                              age >= 45 & age <= 54 ~ "45 to 54",
                                                              age >= 55 & age <= 64 ~ "55 to 64",
                                                              age >= 65 ~ "65 plus"))







         