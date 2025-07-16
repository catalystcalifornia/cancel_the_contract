
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
lasd_stops_incident_raw <- dbGetQuery(con_rda, "SELECT date_reformatted, street_number, full_street, contact_id, k_12_school, school_name, 
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

# qa checks added
length(unique(lasd_stops_incident_raw$contact_id)) # 725

lasd_stops_incident_raw %>%
  distinct(contact_id,date_reformatted)%>%
  nrow() #725 unique contact id by date

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
  filter(reason_for_contact == "Reasonable suspicion that the person was engaged in criminal activity") %>%
  group_by(person_id, contact_id)%>%
  filter(
    if_all(starts_with("reasonable_suspicion"), 
           ~ !(. %in% c("Yes", "true", TRUE)))
  )

# this turns up 59 cases

check <- person_reason %>%
  filter(reason_for_contact == "Reasonable suspicion that the person was engaged in criminal activity") %>%
  mutate(across(starts_with("reasonable_suspicion"), ~if_else(.x %in% c("Yes","true"), 1 , 0))) %>%
  rowwise() %>%
  mutate(sum = sum(across(starts_with("reasonable_suspicion")), na.rm = T))

table(check$sum) # 59 cases where zero 1/0 columns selected, 70 cases where 2 columns selected


# Now lets focus in on reasonable suspicion stop reasons and the sub-types---------------------------

# grab only stop where the reason was reasonable suspicion

rs<-person_reason%>%
  filter(grepl("Reasonable suspicion", reason_for_contact))

# lets get a sense of rates for each reasonable suspicion column: start by recoding yes/true values to be 1 and all other values to be 0

# code to delete that has duplicates
# rs_re<-rs%>%
#   mutate(across(
#     starts_with("reasonable_suspicion_"),
#     ~ ifelse(. %in% c("true", "Yes"), 1, 0)
#   ))%>%
#   select(-reason_for_contact_narrative)%>%
#   pivot_longer(4:11, names_to = "reasonable_suspicion_reason", values_to = "value")%>%
#   filter(value==1)%>%
#   select(-value)
# 
# qa checks added
# nrow(rs_re) # 826
# 
# rs_re %>%
#   distinct(contact_id,person_id)%>%
#   nrow() #752 # duplicate rows
# alternate code that also includes folks that didn't have a 1/0 selected
rs_re_df<-rs%>%
  mutate(across(
    starts_with("reasonable_suspicion_"),
    ~ ifelse(. %in% c("true", "Yes"), 1, 0)
  ))%>%
  select(-reason_for_contact_narrative)%>%
  rowwise() %>%
  mutate(sum_rs = sum(across(starts_with("reasonable_suspicion")), na.rm = T)) %>% # count number of columns selected
  ungroup()

nrow(rs_re_df) # 811 matches reason_table

# look at tabulation of the subtype of reasonable suspicions

rs_table_final<-rs_re_df %>%
  summarise(across(starts_with("reasonable_suspicion_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(names_to = "reasonable_suspicion_reason", values_to = "value", cols=everything())

# Left join the Fresno RIPA offense codes tables so we can get descriptions for the reasonable suspicion statutes

# first create a unique crosswalk of codes

length(unique(codes$offense_statute)) # 5523
nrow(codes) # 6999

codes_unique <- codes %>%
  group_by(offense_statute,offense_type_of_statute_cd, statute_literal_25, offense_type_of_charge) %>%
  summarise(count=n())
nrow(codes_unique) # 6992 # still duplication

# select codes in the rs df first
rs_re_codes_list<-rs_re_df %>% select(offense_code_of_the_reasonable_suspicion) %>% filter(!is.na(offense_code_of_the_reasonable_suspicion)) # codes in the data

# first join active codes
codes_unique_active <- codes %>%
  filter(offense_statute %in% (rs_re_codes_list$offense_code_of_the_reasonable_suspicion)) %>%
  filter(is.na(offense_repealed)) %>% # remove repealed offenses
  filter(!changes %in% c("REMOVED","REPEALED"))

length(unique(codes_unique_active$offense_statute)) # still some duplicates 73 compared to 121 rows

# deduplicate further and paste different types of charges or statutes in a list
codes_unique_active <- codes_unique_active %>%
  group_by(offense_statute) %>%
  summarise(offense_type_of_charge=paste(offense_type_of_charge, collapse="; "),
            statute_literal_25=paste(statute_literal_25,collapse="; "))%>%
  mutate(deactive="No")

rs_re_codes_part1<-rs_re_df%>%
  left_join(codes_unique_active, by=c("offense_code_of_the_reasonable_suspicion"="offense_statute"))

# see what did not join

nonjoin<-rs_re_codes_part1%>%
  filter(is.na(statute_literal_25))  # 80 people, some seem to have deactived codes

# get deactivated codes that officers still used - may be older stops
codes_unique_deactive <- codes %>%
  filter(offense_statute %in% (rs_re_codes_list$offense_code_of_the_reasonable_suspicion)) %>% # in our df
  filter(!offense_statute %in% (codes_unique_active$offense_statute)) # but not in the active list

codes_unique_deactive <- codes_unique_deactive %>%
  group_by(offense_statute) %>%
  summarise(offense_type_of_charge=paste(offense_type_of_charge, collapse="; "),
            statute_literal_25=paste(statute_literal_25,collapse="; ")) %>%
  mutate(deactive="Yes")

rs_re_codes_part2<-nonjoin%>%
  select(-c(offense_type_of_charge,statute_literal_25,deactive))%>%
  left_join(codes_unique_deactive, by=c("offense_code_of_the_reasonable_suspicion"="offense_statute"))

# see what did not join
nonjoin<-rs_re_codes_part2%>%
  filter(is.na(statute_literal_25))  # 29 people do not have offense code list

# bring back the reason_narrative column to see if any of those contain the offense codes

nonjoin<-nonjoin%>%left_join(person_reason%>%select(contact_id, person_id, reason_for_contact_narrative), by=c("contact_id", "person_id"))%>%
  mutate(offense_code_of_the_reasonable_suspicion=reason_for_contact_narrative)

# look at tabulation of the subtype of reasonable suspicions

rs_table<-as.data.frame(table(rs_re$reasonable_suspicion_reason))

# Left join the Fresno RIPA offense codes tables so we can get descriptions for the reasonable suspicion statutes

rs_re_codes<-rs_re%>%
  left_join(codes, by=c("offense_code_of_the_reasonable_suspicion"="offense_statute"))

# see what did not join

nonjoin<-rs_re_codes%>%
  filter(is.na(statute_literal_25))  # none of these 27 people had a statute to go with their reasonable suspicion stop

# bring back the reason_narrative column to see if any of those contain the offense codes

nonjoin<-nonjoin%>%left_join(person_reason%>%select(contact_id, person_id, reason_for_contact_narrative), by=c("contact_id", "person_id"))%>%
  mutate(offense_code_of_the_reasonable_suspicion=reason_for_contact_narrative)

# we can see now more offense codes get filled in. Lets clean up the offense_code column a bit

nonjoin<-nonjoin%>%
  select(1:3,offense_code_of_the_reasonable_suspicion,reason_for_contact_narrative)


# going to manually recode the ones that seem obvious to me but leave the rest. I am not 100% sure what the best way to go about these are.

# the only ones I feel comfortable recoding are the ones with a standalone number that is also in the fresno codes table:

manually_add<-c("602", "32210", "211", "242", "415(2)", "25608", "243.4e1pc", "211","11359","1357d h&S")

add<-nonjoin%>%
  filter(grepl(paste(manually_add, collapse = "|"), offense_code_of_the_reasonable_suspicion))

rs_re_codes_part3<-rs_re_df %>% select (-offense_code_of_the_reasonable_suspicion) %>%
  right_join(add %>% select(contact_id,person_id,offense_code_of_the_reasonable_suspicion)) %>% # selecting only those being added manually
  mutate(offense_code_of_the_reasonable_suspicion = ifelse(grepl("602 PC", offense_code_of_the_reasonable_suspicion), "602",
                              ifelse(grepl("602 pc", offense_code_of_the_reasonable_suspicion), "602",
                                 ifelse(grepl("242pc", offense_code_of_the_reasonable_suspicion), "242",
                                               ifelse(grepl("32210 M", offense_code_of_the_reasonable_suspicion), "32210", 
                                                      ifelse(grepl("32210pc", offense_code_of_the_reasonable_suspicion), "32210", # assuming this is a typo
                                                      ifelse(grepl("25608abp", offense_code_of_the_reasonable_suspicion), "25608", 
                                                             ifelse(grepl("243.4e1pc", offense_code_of_the_reasonable_suspicion), "243.4(E)(1)", 
                                                                    ifelse(grepl("211PC", offense_code_of_the_reasonable_suspicion), "211", 
                                                                           ifelse(grepl("11359", offense_code_of_the_reasonable_suspicion), "11359", 
                                                                                  ifelse(grepl("1357d h&S", offense_code_of_the_reasonable_suspicion), "11357(D)", 
                                                                           offense_code_of_the_reasonable_suspicion)))))))))))




# join 
rs_re_codes_part3<-rs_re_codes_part3%>%
  left_join(codes_unique_active, by=c("offense_code_of_the_reasonable_suspicion"="offense_statute"))

# 11359 missing info still
# fields statute_literal_25 - POSS MARIJUANA FOR SALE, offense_type_of_charge - F, deactive - Yes

rs_re_codes_part3<-rs_re_codes_part3%>%
  mutate(statute_literal_25=ifelse( offense_code_of_the_reasonable_suspicion=='11359','POSS MARIJUANA FOR SALE',statute_literal_25),
         offense_type_of_charge=ifelse( offense_code_of_the_reasonable_suspicion=='11359','F',offense_type_of_charge),
         deactive=ifelse( offense_code_of_the_reasonable_suspicion=='11359','Yes',deactive))


# join all 3 parts together
rs_re_codes_part1<-anti_join(rs_re_codes_part1,rs_re_codes_part3, by=c("contact_id", "person_id"))
rs_re_codes_part1<-anti_join(rs_re_codes_part1,rs_re_codes_part2, by=c("contact_id", "person_id"))
rs_re_codes_part2<-anti_join(rs_re_codes_part2,rs_re_codes_part3, by=c("contact_id", "person_id"))

rs_re_codes<-rbind(rs_re_codes_part1,rs_re_codes_part2,rs_re_codes_part3)

# JZ QA: I just want to make sure the final set of person_id and contact_ids are unique and also that all of the anti-joins worked

sum(as.numeric(rs_re_codes$contact_id)) # 231293208
sum(as.numeric(rs_re_codes$person_id)) # 254999653

check<-person_lasd%>%filter(reason_for_contact=="Reasonable suspicion that the person was engaged in criminal activity")%>%
  mutate(contact_total=sum(as.numeric(contact_id)),
         person_total=sum(as.numeric(person_id)))%>%
  select(contact_total, person_total)

# Continue exploring the reasonable suspicion subtypes---------

# ANALYSIS 1: Calculate Counts and Rates of the Reasonable Suspicion subtypes ---------
rs_table_final # majority are reasonable_suspicion_person_witness_or_victim_ofsuspect: 662 - push this table

rs_total<-nrow(rs_re_codes)

rs_table_final<- rs_table_final %>%
  rename(count=value) %>%
  mutate(reason_for_contact='Reasonable suspicion that the person was engaged in criminal activity',
       total=rs_total, # 811 total reasonable suspicion stops
       rate=count/total) %>% # rates will not add up to 100% given that one stop can be in multiple columns of subtypes
  select(reason_for_contact,reasonable_suspicion_reason,total,count,rate)%>%
 
  # add some code to clean up the suspicion reason column
   mutate(
    reasonable_suspicion_reason = str_replace_all(
      str_remove(reasonable_suspicion_reason, "reasonable_suspicion_"),  # remove the prefix
      "_", " "                                            # replace _ with space
    )
  )
  
# ANALYSIS 2: Calculate Counts and Rates of the Reasonable Suspicion offense codes ---------

df<-rs_re_codes%>%
  mutate(total=n())%>%
  group_by(offense_code_of_the_reasonable_suspicion, statute_literal_25, offense_type_of_charge, deactive)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  ungroup()%>%
  select(reason_for_contact, offense_code_of_the_reasonable_suspicion, deactive,
         statute_literal_25, offense_type_of_charge, total, count, rate)%>%
  arrange(-rate)

sum(df$count) # 811

# push df and rs_table_final to postgres

# # Push reason_table to postgres--------------------------------------

# # Write table with metadata
table_name <- "analysis_stops_reason_rs_avuhsd"
schema <- "data"
indicator<- "This table looks at all of the stops for a reasonable suspicion reason broken down by the specific sub-type of
reasonable suspicion reason. Universe or denominator is all people stopped for a reasonable suspicion reason."
source <- "Script: W:/Project/RJS/CTC/Github/AV/cancel_the_contract/Analysis/analysis_stops_reason_rs_avuhsd.R"
qa_filepath <- "See QA doc for details: W:/Project/RJS/CTC/Documentation/QA_analysis_stops_reason_rs_avuhsd.docx"
table_comment <- paste0(indicator, source)
dbWriteTable(con_ctc, Id(schema, table_name), rs_table_final, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(rs_table_final) # Get column names
column_comments <- c(
  "Highest level of reason for contact (all reasonable suspicion)",
  "Reasonable suspicion sub-reason type",
  "Denominator: The total number of students stopped in the school or district for  reasonable suspicion",
  "Numerator: The number of students stopped for the specificreasonable suspicion sub-reason",
  "The rate represents the percentage of reasonable suspicion sub types out of all people stopped for reasonable suspicion"
)

 add_table_comments(con_ctc, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
 
 # # Push reason codes table to postgres--------------------------------------
 
 # # Write table with metadata
 table_name <- "analysis_stops_reason_rs_codes_avuhsd"
 schema <- "data"
 indicator<- "This table looks at all of the stops for a reasonable suspicion reason broken down by the specific offense code for
the reasonable suspicion reason. Universe or denominator is all people stopped for a reasonable suspicion reason."
 source <- "Script: W:/Project/RJS/CTC/Github/AV/cancel_the_contract/Analysis/analysis_stops_reason_rs_avuhsd.R"
 qa_filepath <- "See QA doc for details: W:/Project/RJS/CTC/Documentation/QA_analysis_stops_reason_rs_avuhsd.docx"
 table_comment <- paste0(indicator, source)
 dbWriteTable(con_ctc, Id(schema, table_name), df, overwrite = TRUE, row.names = FALSE)
 
 # Comment on table and columns
 column_names <- colnames(df) # Get column names
 column_comments <- c(
   "Highest level of reason for contact (all reasonable suspicion)",
   "Reasonable suspicion offense code",
   "Whether or not the offense code is deactivated (Yes/No)",
   "Text description of reasonable suspicion offense code",
   "Offense type (I= Infraction, F = Felony, M = Misdemeanor",
   "Denominator: The total number of students stopped in the school or district for  reasonable suspicion",
   "Numerator: The number of students stopped for the specific reasonable suspicion offense code",
   "The rate represents the percentage of reasonable suspicion offense codes out of all people stopped for reasonable suspicion"
 )
 
 add_table_comments(con_ctc, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
 
