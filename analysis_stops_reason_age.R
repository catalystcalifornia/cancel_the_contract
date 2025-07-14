library(RPostgreSQL)
library(dplyr)
library(stringr)
library(tidytext)

# Connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con_rda <- connect_to_db("rda_shared_data")
con_ctc <- connect_to_db("cancel_the_contract")

# Get LASD stop-level data for Antelope Valley high schools
lasd_stops_incident_raw <- dbGetQuery(con_rda, "SELECT street_number, full_street, contact_id, k_12_school, school_name, civilians_contacted, call_for_service FROM crime_and_justice.lasd_stops_incident_2018_2023
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
lasd_stops_person_raw <- dbGetQuery(con_rda, "SELECT full_address, contact_id, person_id, k12_student, reason_for_contact, reason_for_contact_narrative FROM crime_and_justice.lasd_stops_person_2018_2023")

# Get xwalk
xwalk <- dbGetQuery(con_ctc, "SELECT cde_school, lasd_school FROM data.avuhsd_lasd_xwalk") 