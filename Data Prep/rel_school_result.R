# Create relational table for stop results among people in AVUHSD

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


  ############### EXPLORE STOP REASONS ########################

av_stops <- lasd_incidents %>% left_join(lasd_persons, by = "contact_id") 
n_distinct(av_stops$person_id) # 904 unique people

# take out stop reason columns and recode true/yes/false/no values to 1/0

av_stops_result<-av_stops%>%
  select(contact_id, person_id, starts_with("result"))

# convert to long form

# see the unique values across columns 3:19
lapply(av_stops_result[ , 3:19], unique)

av_stops_result_long<-av_stops_result%>%
  mutate(across(3:19, ~ case_when(
    . %in% c("Yes", "true") ~ 1,
    . %in% c("No", "false") ~ 0,
    TRUE ~ NA_real_)))%>%
  pivot_longer(
    cols = 3:19,
    names_to = "stop_result",
    values_to = "value"
  )%>%
  group_by(contact_id, person_id)%>%
  filter(value!=0) # only keep where at least one of the result columns == 1 

# verified that value only contains 1
unique(av_stops_result_long$value)

# Explore if the same person had 2 different stop results vs only 1 stop result-----------------------

twoormor<-av_stops_result_long%>%
  group_by(person_id, contact_id) %>%
  mutate(count=n())%>%
  filter(count > 1) %>%
  ungroup() # some people are stopped with 4 results at most but majority have 2 results 

table(twoormor$count)

# count how many distinct people had 2 or more stop results
n_distinct(twoormor$person_id)  # 392 --- this is about 43% of total people stopped so I don't want to just recode these as 'two or more results'

# look at stop reasons for people with 2 or more results
stop_result_twoormor<-as.data.frame(table(twoormor$stop_result))

# separate out and look at people with only 1 stop result

onlyone<-av_stops_result_long%>%
  group_by(person_id, contact_id) %>%
  mutate(count=n())%>%
  filter(count ==1) %>%
  ungroup()

# look at stop reasons for people with 1 result
stop_result_onlyone<-as.data.frame(table(onlyone$stop_result))

# explore stop reasons across all people stopped

stop_result<-as.data.frame(table(av_stops_result_long$stop_result)) 

# the top result is result_of_contact_in_field_cite_and_release = 620 across all people, and it is the top reason whether a person had multiple stop results or just 1 stop result

##### Export stops where a person was stopped for an in field cite and release and the accompanying citation code for CS to review #######

infield<-av_stops%>%
  select(person_id, contact_id, result_of_contact_in_field_cite_and_release, 
         result_of_contact_in_field_cite_and_release_offense_codes)%>%
  filter(result_of_contact_in_field_cite_and_release=="Yes" | result_of_contact_in_field_cite_and_release == "true")

# export as excel file
library(openxlsx)
# write.xlsx(infield, 'W://Project//RJS//CTC//Data//school_stop_result_cite_release_codes.xlsx')

#Create relational table for stop results rates--------------

# JZ: My initial thinking is that 1) we should do it at the person level and 2) just take all the stop results and calculate rates straight and NOT recode if a person had two or more results
# Because the purpose of this is to get a gauge of what in general is the result of these police interactions in AV schools
# BUT doing this will not allow us to calculate rates by race because the 'numerator' of stop results would not be person-unique for those with more than 1 result

# could add a flag for now for if a person has more than 1 result and export as excel

df<-av_stops_result_long%>%
  group_by(contact_id, person_id)%>%
  mutate(result_twoormor_flag=ifelse(n()>1, 1, 0))%>%
  select(-value)

# check the flag worked
qa<-df%>%slice(1)%>%filter(result_twoormor_flag==1) # we are left with 392 unique people with two or more stop results which is correct

# continue cleaning up

df<-df%>%
  mutate(stop_result = str_remove(stop_result, "^result_of_contact_")%>% # clean up stop result column
           str_replace_all("_", " "), # clean up stop result column
         stop_result_re=ifelse(result_twoormor_flag==1, "Two or more results", stop_result))%>%
  select(contact_id, person_id, stop_result, stop_result_re, result_twoormor_flag)

n_distinct(df$person_id) #904 unique persons

############### SEND TO POSTGRES ########################

# set column types

charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df

# add df colnames to the character vector

names(charvect) <- colnames(df)

table_name <- "rel_school_result"
schema <- "data"
indicator <- "Relational table of people stopped by LASD in AVUSHD and the stop result. Table is long form. People who were stopped and had multiple results are listed in multiple rows.
Table includes a flag to show if a person was stopped and had two or more stop results versus only one stop result. Has a stop reason recoded so the table contains both the original stop result, meaning that if a person was stopped
with more than 1 stop result then it is listed out each time and that person shows up in more than one row, AND a recoded stop_result column which recodes people with more than 1 stop result as having  Two or more results as the stop result"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//rel_school_result.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_rel_school_result.R" 
table_comment <- paste0(indicator, source)
table_name <- "rel_school_result"

# push to postgres
dbWriteTable(con,  table_name, df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

# Comment on table and columns
column_names <- colnames(df) # Get column names
column_comments <- c(
  "Stop ID",
  "Person ID",
  "Stop result original",
  "Stop result recoded",
  "Flag for if a person had two or more stop results in their stop"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
