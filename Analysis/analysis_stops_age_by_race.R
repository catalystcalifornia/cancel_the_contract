# Calculate stops in the Antelope Valley by age and race


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
con <- connect_to_db("cancel_the_contract")


############### GET STOPS DATA ########################


# SPA 1 incidents
lasd_incidents <- dbGetQuery(con, "SELECT * FROM data.lasd_stops_spa1_2023")

# persons
lasd_persons <- dbGetQuery(con_shared, "SELECT * FROM crime_and_justice.lasd_stops_person_2018_2023")

# race
race <- dbGetQuery(con, "SELECT * FROM rel_race_recode")


############### CLEAN UP AND RECODE COLUMNS ########################


av_stops <- lasd_persons %>% right_join(lasd_incidents, by = "contact_id")%>%
  mutate(contact_id = as.character(contact_id),
         person_id = as.character(person_id))

av_stops<-av_stops%>%
  left_join(race) # add relational race table


# select columns I want

av_stops_re<-av_stops%>%
  select(contact_id, person_id, age, call_for_service, reportingcategory_re, aian_flag, nhpi_flag, sswana_flag, latinx)%>%
  mutate(call_for_service = case_when(
    call_for_service %in% c("Yes", "true")  ~ 1,
    call_for_service %in% c("No", "false")  ~ 0,
    TRUE                                    ~ NA_real_
  )) %>% mutate(age_re=case_when(age <= 17 ~ "17 and under",
                    age >= 18 & age <= 24 ~ "18-24",
                    age >= 25 & age <= 34 ~ "25-34",
                    age >= 35 & age <= 44 ~ "35-44",
                    age >= 45 & age <= 54 ~ "45-54",
                    age >= 55 & age <= 64 ~ "55-64",
                    age >= 65 ~ "65 and older"))

# QA: Check  CFS recoding worked

 qa<-av_stops%>%
     filter(call_for_service %in% c("true", "Yes"))%>%
     summarise(n)  # n = 6983
sum(av_stops_re$call_for_service) # 6983 

# QA age recoding

qa17<-av_stops%>%
  filter(age<=17)%>%
  mutate(count=n()) # this produces 982 obs

check<-av_stops_re%>%
  filter(age_re=="17 and under") # also 982

qa55<-av_stops%>%
  filter(age>=55 & age <= 64)%>%
  summarise(count=n()) # 2395

check<-av_stops_re%>%
  filter(age_re=="55-64")%>%
  summarise(count=n()) # also 2359

####################### Calculate rates for CFS+Not CFS ######################

# for all nh races

tot<-av_stops_re%>%
  mutate(total=n())%>%
  group_by(reportingcategory_re, age_re)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(universe="All stops",
         geography="Antelope Valley")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  select(universe, geography, reportingcategory_re, age_re, total, count, rate)

# for AIAN/NHPI/SSWANA

tot_flags <- av_stops_re %>%
  ungroup() %>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  mutate(total=nrow(av_stops_re))%>%
  group_by(flag_type, age_re) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    universe = "All stops",
    geography = "Antelope Valley",
    .groups = "drop"
  ) %>%
  mutate(
    reportingcategory_re = case_when(
      flag_type == "aian_flag" ~ "AIAN AOIC",
      flag_type == "nhpi_flag" ~ "NHPI AOIC",
      flag_type == "sswana_flag" ~ "SSWANA AOIC"
    )
  ) %>%
  select(universe, geography, reportingcategory_re, age_re, total, count, rate)

# Combine

tot<-tot%>%
  rbind(tot_flags)%>%
  arrange(-rate)

####################### Calculate rates for CFS ######################

# for all nh races

tot_cfs<-av_stops_re%>%
  filter(call_for_service==1)%>%
  ungroup()%>%
  mutate(total=n())%>%
  group_by(reportingcategory_re, age_re)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(universe="Stops with call for service",
         geography="Antelope Valley")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  select(universe, geography, reportingcategory_re, age_re, total, count, rate)

# for AIAN/NHPI/SSWANA

tot_flags_cfs <- av_stops_re %>%
  ungroup() %>%
  filter(call_for_service==1)%>%
  mutate(total=n())%>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  group_by(flag_type, age_re) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    universe = "Stops with call for service",
    geography = "Antelope Valley",
    .groups = "drop"
  ) %>%
  mutate(
    reportingcategory_re = case_when(
      flag_type == "aian_flag" ~ "AIAN AOIC",
      flag_type == "nhpi_flag" ~ "NHPI AOIC",
      flag_type == "sswana_flag" ~ "SSWANA AOIC"
    )
  ) %>%
  select(universe, geography, reportingcategory_re, age_re, total, count, rate)

# Combine

tot_cfs<-tot_cfs%>%
  rbind(tot_flags_cfs)%>%
  arrange(-rate)


####################### Calculate rates for NON CFS ######################

# for all nh races

tot_nocfs<-av_stops_re%>%
  filter(call_for_service==0)%>%
  ungroup()%>%
  mutate(total=n())%>%
  group_by(reportingcategory_re, age_re)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(universe="Stops with no call for service",
         geography="Antelope Valley")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  select(universe, geography, reportingcategory_re, age_re, total, count, rate)

# for AIAN/NHPI/SSWANA

tot_flags_nocfs <- av_stops_re %>%
  ungroup() %>%
  filter(call_for_service==0)%>%
  mutate(total=n())%>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  group_by(flag_type, age_re) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    universe = "Stops with no call for service",
    geography = "Antelope Valley",
    .groups = "drop"
  ) %>%
  mutate(
    reportingcategory_re = case_when(
      flag_type == "aian_flag" ~ "AIAN AOIC",
      flag_type == "nhpi_flag" ~ "NHPI AOIC",
      flag_type == "sswana_flag" ~ "SSWANA AOIC"
    )
  ) %>%
  select(universe, geography, reportingcategory_re, age_re, total, count, rate)

# Combine

tot_nocfs<-tot_nocfs%>%
  rbind(tot_flags_nocfs)%>%
  arrange(-rate)

############### FINAL COMBINE TOT/CFS/NO CFS TABLES ########################

df<-rbind(tot, tot_cfs, tot_nocfs)


############### PUSH TABLE TO POSTGRES #####################

# set column types

charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df
charvect <- replace(charvect, c(5,6,7), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

table_name <- "analysis_stops_age_by_race"
schema <- "data"
indicator <- "Analysis table of AVUSHD LASD stops by age by perceived race"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_age_by_race.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_analysis_stops_age_by_race.docx"
table_comment <- paste0(indicator, source)

# push to postgres
dbWriteTable(con, table_name, df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

column_names <- colnames(df) # Get column names
column_comments <- c(
  "Universe: call for service, NOT call for service, or COMBINED CFS+Not CFs",
  "geography level",
  "LASD reporting category recoded",
  "Age category_recoded",
  "Total LASD stops in Antelope Valley",
  "Count of LASD stops by age by perceived race",
  "Percent of LASD stops by age by perceived race"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


############## Just Age ######################

just_age <- df %>% group_by(universe, age_re) %>%
  summarize(geography = first(geography),
            total = first(total),
            count = sum(count),
            rate = sum(count)/sum(total)*100)


############### PUSH TABLE TO POSTGRES #####################

# set column types

charvect = rep("varchar", ncol(just_age)) #create vector that is "varchar" for the number of columns in df
charvect <- replace(charvect, c(4,5,6), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(just_age)

table_name <- "analysis_stops_age"
schema <- "data"
indicator <- "Analysis table of AVUSHD LASD stops by age"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_age_by_race.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_analysis_stops_age_by_race.docx"
table_comment <- paste0(indicator, source)

# push to postgres
dbWriteTable(con, table_name, just_age,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

column_names <- colnames(just_age) # Get column names
column_comments <- c(
  "Universe: call for service, NOT call for service, or COMBINED CFS+Not CFs",
  "geography level",
  "Age category_recoded",
  "Total LASD stops in Antelope Valley",
  "Count of LASD stops by age",
  "Percent of LASD stops by age"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


############## Just Race ######################


just_race <- df %>% group_by(universe, reportingcategory_re) %>%
  summarize(geography = first(geography),
            total = first(total),
            count = sum(count),
            rate = sum(count)/sum(total)*100)


############### PUSH TABLE TO POSTGRES #####################

# set column types

charvect = rep("varchar", ncol(just_race)) #create vector that is "varchar" for the number of columns in df
charvect <- replace(charvect, c(4,5,6), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(just_race)

table_name <- "analysis_stops_race"
schema <- "data"
indicator <- "Analysis table of AVUSHD LASD stops by race"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_age_by_race.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_analysis_stops_age_by_race.docx"
table_comment <- paste0(indicator, source)

# push to postgres
dbWriteTable(con, table_name, just_race,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

column_names <- colnames(just_race) # Get column names
column_comments <- c(
  "Universe: call for service, NOT call for service, or COMBINED CFS+Not CFs",
  "geography level",
  "Race category_recoded",
  "Total LASD stops in Antelope Valley",
  "Count of LASD stops by race",
  "Percent of LASD stops by race"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
dbDisconnect(con_shared)
