
# Explore raw meta data here: https://data.lacounty.gov/datasets/lacounty::sheriff-officer-contacts-person-details-/about 

library(RPostgres)
library(dplyr)
library(stringr)
library(tidytext)

# Connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con_rda <- connect_to_db("rda_shared_data")
con_fresno <- connect_to_db("eci_fresno_ripa")
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

# Grab offense codes from the Fresno RIPA db

codes<-dbGetQuery(con_fresno, "SELECT * FROM cadoj_ripa_offense_codes_2023")

# Join stop-level data with person-level data, such that each row represents one person.
person_lasd <- lasd_stops_person_raw %>% 
  right_join(lasd_stops_incident_raw, by="contact_id")

# tabulate reasons

reason_table<-as.data.frame(table(person_reason$reason_for_contact))

# Grab only reasonable suspicion columns because it is the overwhelming majority of stop reasons

person_reason<-person_lasd%>%
  select(contact_id, person_id, reason_for_contact, reason_for_contact_narrative, starts_with("reasonable_suspicion_"), offense_code_of_the_reasonable_suspicion)


# Exploration----------------

# First I am curious if any stops can be listed for NOT 'reasonable suspicion' but still have any of the subsequent reasonable suspicion columns marked as true

not_rs<-person_reason%>%
  filter(
    !grepl("Reasonable suspicion", reason_for_contact),
    apply(select(., starts_with("reasonable_suspicion_")), 1, function(row) any(row %in% c("true", "Yes")))) # turns up 0 so we know those columns are clean

# Now I want to test if any stops are listed 'reasonable suspicion' but NONE of the 1/0 reasonable suspicion columns are selected

rs_no_dummy<-person_reason %>%
  filter(reason_for_contact == "reasonable suspicion") %>%
  group_by(person_id, contact_id)%>%
  filter(
    if_all(starts_with("reasonable_suspicion"), 
           ~ !(. %in% c("Yes", "true", TRUE)))
  )

# this turns up zero cases

# now lets focus in on reasonable suspicion sub-types

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
  select(-value)

# Left join the offense codes tables so we can get descriptions

rs_re_codes<-rs_re%>%
  left_join(codes, by=c("offense_code_of_the_reasonable_suspicion"="offense_statute"))

# see what did not join

nonjoin<-rs_re_codes%>%
  filter(is.na(statute_literal_25))  # none of these 27 people had a statute to go with their reasonable suspicion stop

table(nonjoin$reasonable_suspicion_reason) # see what reasonable suspicion column was selected for these


# Continue exploring the stops that did join to the offense code description---------

# note there is a many to many relationship to the join because some offense statutes have different actual offense codes and some are M/I/F

# look at the different reasonable suspicion subtypes
table(rs_re$reasonable_suspicion_reason) # majority are reasonable_suspicion_person_witness_or_victim_ofsuspect: 662

# lets look at person witness or victim of suspect since thats the majority of the reasonable suspicions


rs_witness_victim<-rs_re_codes%>%
  filter(reasonable_suspicion_reason=="reasonable_suspicion_person_witness_or_victim_ofsuspect")

# Rate: Offense codes / all citations WITHIN reasonable suspicion

df1<-rs_re_codes%>%
  group_by(reasonable_suspicion_reason)%>%
  mutate(total=n())%>%
  group_by(reasonable_suspicion_reason, statute_literal_25, offense_type_of_charge)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  ungroup()%>%
  select(reason_for_contact, reasonable_suspicion_reason, statute_literal_25, offense_type_of_charge, total, count, rate)%>%
  arrange(reasonable_suspicion_reason, -rate)


