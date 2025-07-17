library(RPostgreSQL)
library(dplyr)
library(stringr)
library(tidytext)

# Connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con_rda <- connect_to_db("rda_shared_data")
con_ctc <- connect_to_db("cancel_the_contract")

# Get LASD stop-level data for Antelope Valley high schools
lasd_incidents <- dbGetQuery(con_ctc, "SELECT * FROM lasd_stops_spa1_2023") 

# Get LASD person-level data
lasd_persons <- dbGetQuery(con_rda, "SELECT * FROM crime_and_justice.lasd_stops_person_2018_2023")

# race

race<-dbGetQuery(con_ctc, "SELECT * FROM rel_race_recode") 

# age population data

age<-dbGetQuery(con_ctc, "SELECT * FROM av_population_age") 

# join stops and person level table together so each row is a person

av_stops <- lasd_persons %>% 
  right_join(lasd_incidents, by = "contact_id")

 ############### CLEAN UP AND RECODE COLUMNS ########################

# select columns I want and across the reasonable suspicion columns recode them to be 1/0 columns
# then recode age column to be in age brackets

av_stops_re<-av_stops%>%
  select(1,2,14,40, 41, 44:53)%>% 
  mutate(across(6:14, ~ case_when( 
    . %in% c("Yes", "true") ~ 1,
    . %in% c("No", "false") ~ 0,
    TRUE ~ NA_real_)))%>% mutate(age_re=case_when(age <= 17 ~ "17 and under",
                                                       age >= 18 & age <= 24 ~ "18-24",
                                                       age >= 25 & age <= 34 ~ "25-34",
                                                       age >= 35 & age <= 44 ~ "35-44",
                                                       age >= 45 & age <= 54 ~ "45-54",
                                                       age >= 55 & age <= 64 ~ "55-64",
                                                       age >= 65 ~ "65 and older"))%>%
  left_join(age%>%select(age_re, count)%>%rename(age_total=count))


######### ANALYSIS 1a: Just look at reason_for_contact by age group ######
## DENOM == out of all people stopped in SPA 1

reason<-av_stops_re%>%
  mutate(denom="Total people stopped",
         total=n())%>%
  group_by(age_re, reason_for_contact)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley")%>%
  slice(1)%>%
  select(geography, denom, age_re, reason_for_contact, total, count, rate)


######### ANALYSIS 1b: Just look at reason_for_contact by age group WITHIN each age group ######
## DENOM == out of all people stopped WITHIN each age group

reason_age<-av_stops_re%>%
  group_by(age_re)%>%
  mutate(denom="Total people stopped in age group",
         total=n())%>%
  group_by(age_re, reason_for_contact)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley")%>%
  slice(1)%>%
  select(geography, denom, age_re, reason_for_contact, total, count, rate)

######### ANALYSIS 1c: Just look at reason_for_contact by age group WITHIN each age group ######
## DENOM == out of TOTAL POP within each age group


reason_age_pop<-av_stops_re%>%
  mutate(denom="Total age population")%>%
  group_by(age_re, reason_for_contact)%>%
  mutate(count=n(),
         rate=count/age_total*1000,
         geography="Antelope Valley")%>%
  slice(1)%>%
  rename(total=age_total)%>%
  select(geography, denom, age_re, reason_for_contact, total, count, rate)


### Combine Analysis 1a and 1b into one table ######

df<-rbind(reason, reason_age, reason_age_pop)

# Push table to postgres--------------------------------

table_name <- "analysis_stops_reason_age"
schema <- "data"
indicator <- "Rate of reason for contact at the person level within each age group out of all people stopped in SPA 1 AND out of all people stopped within each age group AND out of total population within each age group.
Denominator is specified in the denom column"
source <- "R Script: W:\\Project\\RJS\\CTC\\Github\\JZ\\cancel_the_contract\\Analysis\\analysis_stops_reason_age.R"
qa_filepath <- " W:\\Project\\RJS\\CTC\\Documentation\\QA_analysis_stops_reason_ages.docx" 
table_comment <- paste0(indicator, source)

dbWriteTable(con_ctc, Id(schema, table_name), df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(df) # Get column names
column_comments <- c(
  "geography level",
  "denominator used in rate calculation. Total people stopped == everyone stopped in SPA 1. Total people stopped in age group == everyone stopped within each age bracket",
  "Age group recoded",
  "Stop reason",
  "Total (see denominator column for specific definition of total)",
  "Count of people stopped within each age bracket and stop reason",
  "Rate of people stopped within age bracket for each stop reason"
)

add_table_comments(con_ctc, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


######### ANALYSIS 2a: Explore stop reason by age AND race ######

# I want to add race and see if it is worth exporting --start again where denom == out of all people stopped in SPA 1

av_stops_re<-av_stops_re%>%
  mutate(contact_id=as.character(contact_id),
         person_id=as.character(person_id))%>%
  left_join(race)%>%
  filter(!is.na(reportingcategory_re)) # need to filter out our NULL Race groups or where # of races was >= 6

reason_race<-av_stops_re%>%
  mutate(denom="Total people stopped",
         total=n())%>%
  group_by(reason_for_contact, age_re, reportingcategory_re)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley")%>%
  slice(1)%>%
  select(geography, denom, reason_for_contact, reportingcategory_re, age_re, total, count, rate)%>%
  arrange( reportingcategory_re, age_re, -rate)%>%
  filter(!is.na(reportingcategory_re)) # need to filter out our NULL Race groups or where # of races was >= 6
  

reason_race_flag<-av_stops_re %>%
  ungroup() %>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  mutate(total=reason_race$total[1])%>%
  group_by(flag_type,age_re, reason_for_contact) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    geography = "Antelope Valley",
    .groups = "drop"
  ) %>%
  mutate(
    denom="Total people stopped",
    reportingcategory_re = case_when(
      flag_type == "aian_flag" ~ "AIAN AOIC",
      flag_type == "nhpi_flag" ~ "NHPI AOIC",
      flag_type == "sswana_flag" ~ "SSWANA AOIC"
    )
  ) %>%
  select( geography, denom, reason_for_contact, reportingcategory_re, age_re, total, count, rate)

######### ANALYSIS 2b: Explore stop reason by age AND race: denom == total stops within each age group ######

reason_race1<-av_stops_re%>%
  group_by(age_re)%>%
  mutate(denom="Total people stopped in age group",
         total=n())%>%
  group_by(reason_for_contact, age_re, reportingcategory_re)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley")%>%
  slice(1)%>%
  select(geography, denom, reason_for_contact, reportingcategory_re, age_re, total, count, rate)%>%
  arrange( reportingcategory_re, age_re, -rate)

reason_race_flag1<-av_stops_re %>%
  ungroup() %>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  group_by(flag_type)%>%
  mutate(total=n())%>%
  group_by(flag_type,age_re, reason_for_contact) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    geography = "Antelope Valley",
    .groups = "drop"
  ) %>%
  mutate(
    denom="Total people stopped in age group",
    reportingcategory_re = case_when(
      flag_type == "aian_flag" ~ "AIAN AOIC",
      flag_type == "nhpi_flag" ~ "NHPI AOIC",
      flag_type == "sswana_flag" ~ "SSWANA AOIC"
    )
  ) %>%
  select( geography, denom, reason_for_contact, reportingcategory_re, age_re, total, count, rate)


### Combine Analysis 2a and 2b into one table ######

df<-rbind(reason_race,reason_race_flag, reason_race1,reason_race_flag1)


# Push table to postgres--------------------------------

table_name <- "analysis_stops_reason_age_race"
schema <- "data"
indicator <- "Rate of reason for contact at the person level within each perceived race and age group out of all people stopped in SPA 1 AND out of all people stopped within each age group.
Denominator is specified in the denom column. NOTE 19 people have NULL racial group because LASD indicated them as having 6+ races so they are filtered out of the analyses."
source <- "R Script: W:\\Project\\RJS\\CTC\\Github\\JZ\\cancel_the_contract\\Analysis\\analysis_stops_reason_age.R"
qa_filepath <- " W:\\Project\\RJS\\CTC\\Documentation\\QA_analysis_stops_reason_ages.docx" 
table_comment <- paste0(indicator, source)

dbWriteTable(con_ctc, Id(schema, table_name), df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(df) # Get column names
column_comments <- c(
  "geography level",
  "denominator used in rate calculation. Total people stopped == everyone stopped in SPA 1. Total people stopped in age group == everyone stopped within each age bracket",
  "reason for stop",
  "Perceived racial group",
  "Age group recoded",
  "Total (see denominator column for specific definition of total)",
  "Count of people stopped within each age bracket and stop reason",
  "Rate of people stopped within age bracket for each stop reason"
)

add_table_comments(con_ctc, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

