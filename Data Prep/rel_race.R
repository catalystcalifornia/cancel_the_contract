# Create relational table for people stopped by race in SPA 1  people stopped in AVUHSD schools by race

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

# SPA 1 incidents
lasd_incidents <- dbGetQuery(con, "SELECT * FROM data.lasd_stops_spa1_2023")

# persons
lasd_persons <- dbGetQuery(con_shared, "SELECT * FROM crime_and_justice.lasd_stops_person_2018_2023")

############### CLEAN UP AND RECODE COLUMNS ########################

av_stops <- lasd_persons %>% right_join(lasd_incidents, by = "contact_id")


# select columns of interest and clean up columns 

av_stops_re<-av_stops%>%
  select(1,2,25:31)%>%
  mutate(across(3:9, ~ case_when(
    . %in% c("Yes", "true") ~ 1,
    . %in% c("No", "false") ~ 0,
    TRUE ~ NA_real_)))%>%
  mutate(aian_flag=ifelse(native_american==1, 1,0))%>%
  mutate(nhpi_flag=ifelse(pacific_islander==1, 1,0))%>%
  mutate(sswana_flag=ifelse(middle_eastern_south_asian==1, 1,0))%>%
  mutate(latinx=ifelse(hispanic_latino_latina==1, 1,
                       ifelse(hispanic_latino_latina==1 & (white == 1 | black_african_american == 1 | 
                                                             asian == 1 | native_american == 1 | 
                                                             pacific_islander == 1 | middle_eastern_south_asian==1), 1, 0)))

# pivot data longer                          

av_stops_long<-av_stops_re%>%
  pivot_longer(
    cols = 3:9,
    names_to = "reportingcategory",
    values_to = "value"
  )%>%
  group_by(contact_id, person_id)%>%
  filter(value!=0) # only keep where at least one of the racial columns == 1 

# Explore duplicates (these should be people where more than 1 race column == 1 

dup<-av_stops_long %>%
  group_by(contact_id, person_id) %>%
  filter(n() > 1)%>%
  tally()%>%
  arrange(-n)%>%
  group_by(n)%>%
  mutate(n_tot=n())

# From this we see there are are some people where 7 race categories are indicated. We need to recode.

# I think we should recode where anyone with 7 races marked as recoded as NULL
# Everyone else with  7< and >=2 races marked will be recoded as nh_twoormor as long as one of them is not Latinx
# If someone has 2+ races marked but one is latinx then they will be coded as latinx


av_stops_long_re<-av_stops_long%>%
   group_by(contact_id, person_id) %>%
  mutate(reportingcategory = ifelse(n() >= 2 & n() <=4 & latinx != 1, "nh_twoormor", 
                              ifelse(n() >= 2 & n() <=4 & latinx == 1, "latinx", 
                              ifelse(n() >= 7, "NULL", reportingcategory))))%>%
  mutate(reportingcategory = na_if(reportingcategory, "NULL"))

# test view the people/stops with more than 1 race indicated and spot check some of these people against what they are recoded as in av_stops_long
multi<-av_stops_re%>%filter(contact_id %in% dup$contact_id)


# Now that all the races are recoded I can clean up the groups so every stop/person is one row

av_stops_long_re<-av_stops_long_re%>%
  group_by(contact_id, person_id)%>%
  slice(1)%>%
  select(-value)

# now recode the racial category column

av_stops_long_re<-av_stops_long_re%>%
  mutate(reportingcategory_re=ifelse(reportingcategory %in% "black_african_american" & latinx!=1, "nh_black",
                                ifelse(reportingcategory %in% "middle_eastern_south_asian" & latinx!=1, "sswana_aoic",
                                     ifelse(reportingcategory %in% "native_american" & latinx!=1, "nh_aian", 
                                            ifelse(reportingcategory %in% "asian" & latinx!=1, "nh_asian", 
                                                   ifelse(reportingcategory %in% "hispanic_latino_latina", "latinx", 
                                                          ifelse(reportingcategory %in% "pacific_islander" & latinx!=1, "nh_nhpi",  
                                                                 ifelse(reportingcategory %in% "white" & latinx!=1, "nh_white", 
                                                                        "latinx"))))))))%>%
  mutate(contact_id=as.character(contact_id),
         person_id=as.character(person_id)) 


table(av_stops_long_re$reportingcategory_re)


# Push table to postgres--------------------------

table_name <- "rel_race_recode"
schema <- "data"
indicator <- "Relational table of every stop/person in SPA 1 and their race with recoding for multiracial people as well as flags for AIAN/NHPI/SSWANA AOIC. 19 people had 7 races indicated and 
were recoded to have their race be NULL. Everyone else that had 2 or more races indicated were recoded as multiracial UNLESS latinx was one of the indicated races then their race is latinx."
source <- "R Script: W:\\Project\\RJS\\CTC\\Github\\JZ\\cancel_the_contract\\Data Prep\\rel_race.R"
qa_filepath <- " W:\\Project\\RJS\\CTC\\Documentation\\QA_rel_race.docx" 
table_comment <- paste0(indicator, source)

dbWriteTable(con, Id(schema, table_name), av_stops_long_re, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(av_stops_long_re) # Get column names
column_comments <- c(
  "Stop ID",
  "Person ID",
  "AIAN AOIC flag 1/0",
  "NHPI AOIC flag 1/0",
  "SSWANA AOIC flag 1/0",
  "Latinx AOIC flag 1/0",
  "race category",
  "race category recoded"

)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
dbDisconnect(con_shared)



