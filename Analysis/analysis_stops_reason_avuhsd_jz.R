library(RPostgres)
library(dplyr)
library(stringr)
library(tidytext)

# Connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con_rda <- connect_to_db("rda_shared_data")
con_ctc <- connect_to_db("cancel_the_contract")

# Get LASD stop-level data for Antelope Valley high schools
lasd_stops_incident_raw <- dbGetQuery(con_rda, "SELECT street_number, full_street, contact_id, k_12_school, school_name, 
civilians_contacted, call_for_service FROM crime_and_justice.lasd_stops_incident_2018_2023
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
lasd_stops_person_raw <- dbGetQuery(con_rda, "SELECT * FROM crime_and_justice.lasd_stops_person_2018_2023")


# Join stop-level data with person-level data, such that each row represents one person.
person_lasd <- lasd_stops_person_raw %>% 
  right_join(lasd_stops_incident_raw, by="contact_id")

# Grab only person reason columns

person_reason<-person_lasd%>%
  select(contact_id, person_id, reason_for_contact, reason_for_contact_narrative, starts_with("reasonable_suspicion_"))

# tabulate reasons

reason_table<-as.data.frame(table(person_reason$reason_for_contact))

# Exploration----------------

# First I am curious if any stops can be listed for NOT 'reasonable suspicion' but still have any of the subsequent reasonable suspicion columns marked as true

not_rs<-person_reason%>%
  filter(
    !grepl("Reasonable suspicion", reason_for_contact),
    apply(select(., starts_with("reasonable_suspicion_")), 1, function(row) any(row %in% c("true", "Yes")))) # turns up 0 so we know those columns are clean



# now lets focus in on reasonable suspicion

rs<-person_reason%>%
  filter(grepl("Reasonable suspicion", reason_for_contact))

# lets get a sense of rates for each reasonable suspicion column: start by recoding yes/true values to be 1 and all other values to be 0

rs_re<-rs%>%
  mutate(across(
    starts_with("reasonable_suspicion_"),
    ~ ifelse(. %in% c("true", "Yes"), 1, 0)
  ))%>%
  select(-reason_for_contact_narrative)%>%
  pivot_longer(4:11, names_to = "reasonable_suspicion_reason", values_to = "value")%>%
  filter(value==1)%>%
  group_by(reasonable_suspicion_reason)%>%
  mutate(count=sum(value))%>%
  slice(1)%>%
  select(reasonable_suspicion_reason, count)%>%
  arrange(-count)

# Explore raw meta data here: https://data.lacounty.gov/datasets/lacounty::sheriff-officer-contacts-person-details-/about 
