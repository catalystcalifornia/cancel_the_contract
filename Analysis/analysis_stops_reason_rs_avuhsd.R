
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

reason_table<-as.data.frame(table(person_lasd$reason_for_contact))

# Grab the reasonable suspicion columns because it is the overwhelming majority of stop reasons

person_reason<-person_lasd%>%
  select(contact_id, person_id, reason_for_contact, reason_for_contact_narrative, starts_with("reasonable_suspicion_"), offense_code_of_the_reasonable_suspicion)


# Data checks----------------------

# First I am curious if any stops can be listed for NOT 'reasonable suspicion' but still have any of the subsequent reasonable suspicion columns marked as true

not_rs<-person_reason%>%
  filter(
    !grepl("Reasonable suspicion", reason_for_contact),
    apply(select(., starts_with("reasonable_suspicion_")), 1, function(row) any(row %in% c("true", "Yes")))) # turns up 0 so we know those columns are clean

rm(not_rs)

# Now I want to test if any stops are listed 'reasonable suspicion' but NONE of the 1/0 reasonable suspicion columns are selected

rs_no_dummy<-person_reason %>%
  filter(reason_for_contact == "reasonable suspicion") %>%
  group_by(person_id, contact_id)%>%
  filter(
    if_all(starts_with("reasonable_suspicion"), 
           ~ !(. %in% c("Yes", "true", TRUE)))
  )

# this turns up zero cases

rm(rs_no_dummy)

# Now lets focus in on reasonable suspicion stop reasons and the sub-types---------------------------

# grab only stop where the reason was reasonable suspicion

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

# look at tabulation of the subtype of reasonable suspicions

rs_table<-as.data.frame(table(rs_re$reasonable_suspicion_reason))

# Left join the Fresno RIPA offense codes tables so we can get descriptions for the reasonable suspicion statutes

rs_re_codes<-rs_re%>%
  left_join(codes, by=c("offense_code_of_the_reasonable_suspicion"="offense_statute"))

# see what did not join

nonjoin<-rs_re_codes%>%
  filter(is.na(statute_literal_25))  # none of these 27 people had a statute to go with their reasonable suspicion stop

table(nonjoin$reasonable_suspicion_reason) # see what reasonable suspicion column was selected for these

# bring back the reason_narrative column to see if any of those contain the offense codes

nonjoin<-nonjoin%>%left_join(person_reason%>%select(contact_id, person_id, reason_for_contact_narrative), by=c("contact_id", "person_id"))%>%
  mutate(offense_code_of_the_reasonable_suspicion=reason_for_contact_narrative)

# we can see now more offense codes get filled in. Lets clean up the offense_code column a bit

nonjoin<-nonjoin%>%
  select(1:5)

# going to manually recode the ones that seem obvious to me but leave the rest. I am not 100% sure what the best way to go about these are.

# the only ones I feel comfortable recoding are the ones with a standalone number that is also in the fresno codes table:

manually_add<-c("602", "32210", "211", "242", "415(2)", "25608", "243.4e1pc", "211")

add<-nonjoin%>%
  filter(grepl(paste(manually_add, collapse = "|"), offense_code_of_the_reasonable_suspicion))

rs_re<-rs_re%>%
  mutate(offense_code_of_the_reasonable_suspicion = ifelse(grepl("602 PC", offense_code_of_the_reasonable_suspicion), "602",
                              ifelse(grepl("602 pc", offense_code_of_the_reasonable_suspicion), "602",
                                 ifelse(grepl("242pc", offense_code_of_the_reasonable_suspicion), "242",
                                               ifelse(grepl("32210 M", offense_code_of_the_reasonable_suspicion), "32210", 
                                                      ifelse(grepl("25608abp", offense_code_of_the_reasonable_suspicion), "25608", 
                                                             ifelse(grepl("243.4e1pc", offense_code_of_the_reasonable_suspicion), "243.4(E)(1)", 
                                                                    ifelse(grepl("211PC", offense_code_of_the_reasonable_suspicion), "211", 
                                                                           offense_code_of_the_reasonable_suspicion))))))))
           

# Continue exploring the reasonable suspicion subtypes---------

# look at the different reasonable suspicion subtypes
table(rs_re$reasonable_suspicion_reason) # majority are reasonable_suspicion_person_witness_or_victim_ofsuspect: 662

# ANALYSIS 1: Calculate Counts and Rates of the Reasonable Suspicion offense codes WITHIN each Reasonable Suspicion subtype---------

df<-rs_re%>%
  group_by(reasonable_suspicion_reason)%>%
  mutate(total=n())%>%
  left_join(codes, by=c("offense_code_of_the_reasonable_suspicion"="offense_statute"))%>%
  group_by(reasonable_suspicion_reason, offense_code_of_the_reasonable_suspicion, statute_literal_25, offense_type_of_charge)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  ungroup()%>%
  select(reason_for_contact, reasonable_suspicion_reason, offense_code_of_the_reasonable_suspicion, 
         statute_literal_25, offense_type_of_charge, total, count, rate)%>%
  arrange(reasonable_suspicion_reason, -rate)

# clean up columns

df<-df%>%
  mutate(reasonable_suspicion_reason = str_replace_all(reasonable_suspicion_reason, "_", " "),
         reasonable_suspicion_reason = str_remove(reasonable_suspicion_reason, "reasonable suspicion "))

# Push table to postgres--------------------------------------

# Write table with metadata
table_name <- "analysis_stops_reason_rs_avuhsd"
schema <- "data"
indicator<- "This is a long form table looking specifically at peopel stopped in AVUHSD where the reason for the stop was
reasonable suspicion. It then calculates the counts and rates of each reasonable suspicion offense code WITHIN each specific reasonable suspicion sub-type.
For exampple, for reasonable suspicion: action indicative violentcrime, we calculate the different reasonable suspicion offense codes (numerator) / total reasonable suspicion indicative of a violent crime stops (denominator).
We left join the CADOJ offense code table to get specific statute descriptions that correspond with the reasonable suspicion offense codes. 
There is a many to many relationship between the reasonable suspicion offense codes and the CADOJ offense code table because the same reasonable suspicion offense codes 
can either be a Misdemeanor/Infraction/Felony and can also have slightly different statute descriptions. The total/count/rate values in the table do not duplicate based off this many to many join
but in order to examine the different statute descriptions rows are duplicated to reflect this many to many relationship."
source <- "Script: W:/Project/RJS/CTC/Github/AV/cancel_the_contract/Analysis/analysis_stops_reason_rs_avuhsd.R"
qa_filepath <- "See QA doc for details: W:/Project/RJS/CTC/Documentation/QA_analysis_stops_reason_rs_avuhsd.docx"
table_comment <- paste0(indicator, source)
dbWriteTable(con_ctc, Id(schema, table_name), df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(df) # Get column names
column_comments <- c(
  "Stop reason (all will be reasonable suspicion)",
  "Sub-reasonable suspicion reason type",
  "Offense code for reasonable suspicion stop",
  "Description of reasonable suspicion offense code",
  "Type of offense charge ( I = Infraction, M = Misdemeanor, F = Felony)",
  "Denominator: The total number of students stopped in the school or district for the reasonable suspicion sub-reason type",
  "Numerator: The number of students stopped for the specific combination of reasonable suspicion sub-reason type and reasonable suspicion  offense code",
  "The rate represents the percentage of reasonable suspicion offense codes within the specific reasonable suspicion reason type"
)

 add_table_comments(con_ctc, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)