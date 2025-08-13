# Antelope Valley Union High School District Stops where person was handcuffed


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

# race
race <- dbGetQuery(con, "SELECT * FROM rel_school_race_recode")

############### CLEAN UP AND RECODE COLUMNS ########################

av_stops <- lasd_persons %>% right_join(lasd_incidents, by = "contact_id")%>%
  mutate(contact_id = as.character(contact_id),
         person_id = as.character(person_id))

av_stops<-av_stops%>%
  left_join(race) # add relational race table

# select columns of interest and clean up columns 

av_stops_re<-av_stops%>%
  select(contact_id, person_id, reportingcategory_re, aian_flag, nhpi_flag, sswana_flag, latinx,
         person_handcuffed_or_flex_cuffed)%>%
  mutate(person_handcuffed_or_flex_cuffed = case_when(
    person_handcuffed_or_flex_cuffed %in% c("Yes", "true")  ~ 1,
    person_handcuffed_or_flex_cuffed %in% c("No", "false")  ~ 0,
    TRUE                                    ~ NA_real_
  ))

####################### Calculate rates for handcuffed ######################

# not grouped by race just total searches / all people stopped

tot<-av_stops_re%>%
  mutate(total=n())%>%
  group_by(person_handcuffed_or_flex_cuffed)%>%
  mutate(reportingcategory_re="total",
         count=n(),
         rate=count/total*100)%>%
  mutate(universe="All stops",
         geography="Antelope Valley Union High School District")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  ungroup()%>%
  filter(person_handcuffed_or_flex_cuffed==1)%>%
  select(universe, geography,  reportingcategory_re, total, count, rate)

# by NH race

nh<-av_stops_re%>%
  filter(person_handcuffed_or_flex_cuffed==1)%>%
  mutate(total=n())%>%
  group_by(person_handcuffed_or_flex_cuffed, reportingcategory_re)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(universe="All persons handcuffed",
         geography="Antelope Valley Union High School District")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  ungroup()%>%
  select(universe, geography,  reportingcategory_re, total, count, rate)%>%
  arrange(-rate)

# for AIAN/NHPI/SSWANA

tot_flags <- av_stops_re %>%
  ungroup() %>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1 & person_handcuffed_or_flex_cuffed==1) %>%
  mutate(total=tot$count)%>%
  group_by(flag_type) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    universe="All persons handcuffed",
    geography = "Antelope Valley Union High School District",
    .groups = "drop"
  ) %>%
  mutate(
    reportingcategory_re = case_when(
      flag_type == "aian_flag" ~ "AIAN AOIC",
      flag_type == "nhpi_flag" ~ "NHPI AOIC",
      flag_type == "sswana_flag" ~ "SSWANA AOIC"
    )
  ) %>%
  select( universe, geography, reportingcategory_re, total, count, rate)

# Combine

df<-rbind(tot, nh, tot_flags)%>%
  arrange(universe, -rate)

############### PUSH TABLE TO POSTGRES #####################

# set column types

charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df
charvect <- replace(charvect, c(4,5,6), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

table_name <- "analysis_stops_handcuff_avuhsd"
schema <- "data"
indicator <- "Analysis table of AVUSHD LASD stops where a person was handcuffed, as well as people handcuffed disaggregated by 
perceived race"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_handcuff_avuhsd.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_analysis_stops_handcuff_avuhsd.R" 
table_comment <- paste0(indicator, source)

# push to postgres
dbWriteTable(con,  table_name, df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

column_names <- colnames(df) # Get column names
column_comments <- c(
  "Universe: All stops -meaning out of all people who were stopped (n=904) our out of ALL people handcuffed (n=178)",
  "geography level",
  "CDE reporting category recoded",
  "Total (see universe for specific total value definition)",
  "Count of LASD stops where someone was handcuffed by LASD in AVUHSD",
  "Percent of LASD stops where someone was handcuffed by perceived race in AVUHSD"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

####################### Calculate rates for HIT RATES ######################

# not grouped by race just total hit rate

df_contra<-av_stops_re%>%
  filter(search_of_person_conducted==1)%>%
  mutate(total=n())%>%
  group_by(search_of_person_conducted, contraband_evidence_discovered_none)%>%
  mutate(reportingcategory_re="total",
         count=n(),
         rate=count/total*100)%>%
  mutate(universe="All searches",
         geography="Antelope Valley Union High School District")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  ungroup()%>%
  select(universe, geography, contraband_evidence_discovered_none, reportingcategory_re, total, count, rate)


############### PUSH  HIT RATE TABLE TO POSTGRES #####################

# set column types

charvect = rep("varchar", ncol(df_contra)) #create vector that is "varchar" for the number of columns in df
charvect <- replace(charvect, c(5,6,7), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(df_contra)

table_name <- "analysis_stops_hitrate_avuhsd"
schema <- "data"
indicator <- "Analysis table of AVUSHD LASD stops that containded a search where contraband was either found or not (hit rate)
out of all people searched"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_search_avuhsd.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_analysis_stops_search_avuhsd.R" 
table_comment <- paste0(indicator, source)

# push to postgres
dbWriteTable(con,  table_name, df_contra,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

column_names <- colnames(df_contra) # Get column names
column_comments <- c(
  "Universe: All searches -meaning out of all people who were searched (n=177)",
  "geography level",
  "Flag for if contraband was found or not during the search (1/0)",
  "Total number of people searched",
  "Count of people searched by whether or not contraband was found or not",
  "Rate of searches where contraband was found or not (hit rate)"
)

dbDisconnect(con)
dbDisconnect(con_shared)
