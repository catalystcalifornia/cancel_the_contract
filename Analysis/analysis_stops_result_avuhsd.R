# AVUHSD stops by stop result


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


############### GET DATA ########################

result <- dbGetQuery(con, "SELECT * FROM rel_school_result")
race<-dbGetQuery(con, "SELECT * FROM rel_school_race_recode")%>% # QAed
  filter(reportingcategory_re != "NULL") # this is our person who was recoded as race == NULL because LASD indicated 6+ races for this person
enrollment <- dbGetQuery(con_shared, "SELECT * FROM education.cde_multigeo_enrollment_census_day_2024_25")

# Join result table with persons table to get race

df<-result%>%
  left_join(race)

############### ANALYSIS 1: Counts/Rates of Stops WITHOUT race ########################

# This takes the rate of results that are alone or in combination with other results.
# SO the numerator is all results that were given alone or in combination with others but encompasses all results given
# But the denominator is total unique people stopped.

result_df<-df%>%
  mutate(total=length(unique(result$person_id)))%>%
  group_by(stop_result)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley Union High School District")%>%
  slice(1)%>%
  select(geography, stop_result, total, count, rate)%>%
  arrange(-rate)

#### Push to postgres 

# set column types

charvect = rep("varchar", ncol(result_df)) #create vector that is "varchar" for the number of columns in df

# add df colnames to the character vector

names(charvect) <- colnames(result_df)

table_name <- "analysis_stops_result_avuhsd"
schema <- "data"
indicator <- "Total and rate of stop results alone or in combination with other results for people in AVUHSD schools. 
This table counts each individual stop result even if it is the same person to get a sense of stop results over-all.
The denominator for the rate calcs (the total) is all unique pepole stopped"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_result_avuhsd.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_stops_result_avuhsd.docx" 
table_comment <- paste0(indicator, source)

# push to postgres
dbWriteTable(con,  table_name, result_df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

# Comment on table and columns
column_names <- colnames(result) # Get column names
column_comments <- c(
  "geography",
  "Stop result",
  "Total counts of stop results, there are double counts if a person was stopped for more than one reason",
  "Count of each specific stop result, there are double counts if a person was stopped for more than one reason each reason is counted",
  "Rate of each stop result"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

############### ANALYSIS 2a: Focus JUST on non-serious results BY RACE ########################

# DENOM == Total unique people stopped

# Want to focus on results that are not-serious to illustrate that policing in schools is not necessary and we can see
# based on the type of results these stops are getting

# look at the stop results to pull out the ones that are not serious results indicative of real "crime" 
table(df$stop_result)

target<-list("citation for infraction", "referral school staff","contacted legal guardian",
             "no action","warning", "referral to school administrator", "in field cite and release")


# create version of df where the stop_result is collapsed into one value so people with two or more stop results
# don't show up in more than 1 row

combined_df <- df %>%
  group_by(contact_id, person_id) %>%
  summarise(
    stop_result_list = paste0('c("', paste(sort(unique(stop_result)), collapse = '", "'), '")'),
    .groups = "drop"
  )%>%
  mutate(
    unique_stop_result_count = lengths(strsplit(stop_result_list, ",\\s*"))
  ) # create count of how many unique results are in the stop_result column

# stop_result_list is not an actual list - need to convert and then can create  
# column that checks if elements in that list are not in our target list 

df_target <- combined_df %>%
  mutate(stop_result_list2 = map(stop_result_list, ~eval(parse(text=.x)))) %>%
  # actual check: returns TRUE if any of the elements in stop_result_list2 are NOT in our target list
  mutate(target_check = map_lgl(stop_result_list2, ~any(!(. %in% target)))) %>%
  # create flag for if result is in our target list
  mutate(target_result=ifelse(target_check == FALSE, 1, 0)) %>%
  select(-c(stop_result_list2, target_check))


# confirm worked
table(df_target$stop_result_list)

# Now that the combined df is ready I can calculate rates of these non serious stop results by race

# Note EMG has a point that a by race cut might not be the most strategic since we wouldn't want policing of any children
# regardless of race. So just running this so I can get a sense of the rates myself

# first calculate NOT by race just total target stop results / total stop results

df_total<-df_target%>%
  mutate(denom="Total people stopped",
         total=n())%>% # this will give 904 for unique people stopped
  group_by(target_result)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley Union High School District",
         reportingcategory_re="Total")%>%
  slice(1)%>%
  select(geography, denom, target_result, reportingcategory_re, total, count, rate)%>%
  arrange(-rate)

# now calculate by race for our NH race groups

df_race<-df_target%>%
  left_join(race)%>%
  mutate(denom="Total people stopped",
         total=n())%>%
  group_by(reportingcategory_re, target_result)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley Union High School District")%>%
  slice(1)%>%
  filter(!is.na(reportingcategory_re))%>% # our person with NULL for race because they had too many races indicated by LASD
  select(geography, denom, target_result, reportingcategory_re, total, count, rate)%>%
  arrange(reportingcategory_re, -rate)

# now calculate for AIAN/NHPI/SSWANA

# for AIAN/NHPI/SSWANA

df_aoic <- df_target %>%
  left_join(race)%>%
  mutate(
         total=n())%>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  group_by(flag_type, target_result) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    geography = "Antelope Valley Union High School District",
    .groups = "drop"
  ) %>%
  mutate(denom="Total people stopped",
    reportingcategory_re = case_when(
      flag_type == "aian_flag" ~ "AIAN AOIC",
      flag_type == "nhpi_flag" ~ "NHPI AOIC",
      flag_type == "sswana_flag" ~ "SSWANA AOIC"
    )
  ) %>%
  select(geography,denom, target_result, reportingcategory_re, total, count, rate)

# combine everything

df_final<-rbind(df_total, df_race, df_aoic)

############### ANALYSIS 2b: Focus JUST on non-serious results BY RACE ########################

# DENOM == ENROLLMENT IN HS DISTRICT BY RACE for each racial group to get more at 'population'

# get Antelope Valley Union High School District enrollment
av_enrollment <- enrollment %>% filter(districtname == "Antelope Valley Union High" & charter == "ALL")%>%
  filter(grepl("RE_|TA", reportingcategory))

# check I got all the racial reporting groups

table(av_enrollment$reportingcategory)

# recode racial categories

av_enrollment<-av_enrollment%>%
  mutate(reportingcategory_re=ifelse(reportingcategory %in% "TA", "total",
                                ifelse(reportingcategory %in% "RE_B", "nh_black",
                                     ifelse(reportingcategory %in% "RE_I", "nh_aian", 
                                            ifelse(reportingcategory %in% "RE_A", "nh_asian", 
                                                   ifelse(reportingcategory %in% "RE_F", "nh_filipino",  
                                                          ifelse(reportingcategory %in% "RE_H", "latinx", 
                                                                 ifelse(reportingcategory %in% "RE_D", "missing_race",
                                                                        ifelse(reportingcategory %in% "RE_P", "nh_nhpi",  
                                                                               ifelse(reportingcategory %in% "RE_T", "nh_twoormor",  
                                                                                      ifelse(reportingcategory %in% "RE_W", "nh_white", 
                                                                                             reportingcategory)))))))))))%>%
  select(cdscode, districtname, reportingcategory_re, total_enr)

# pull the total enrollment value
total_enrollment<-av_enrollment%>%filter(reportingcategory_re=="total")%>%select(total_enr)%>%pull(total_enr)

# Join this to the df_target table

df_target<-df_target%>%
  left_join(race)%>%
  left_join(av_enrollment)

# first calculate NOT by race just total target stop results / total stop results

df_total_enr<-df_target%>%
  mutate(denom="Total race enrollment",
         total=total_enrollment)%>% 
  group_by(target_result)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley Union High School District",
         reportingcategory_re="Total")%>%
  slice(1)%>%
  select(geography, denom, target_result, reportingcategory_re, total, count, rate)%>%
  arrange(-rate)

# now calculate by race for our NH race groups

df_race_enr<-df_target%>%
  group_by(reportingcategory_re)%>%
  mutate(denom="Total race enrollment",
         total=total_enr)%>%
  group_by(reportingcategory_re, target_result)%>%
  mutate(count=n(),
         rate=count/total*100,
         geography="Antelope Valley Union High School District")%>%
  slice(1)%>%
  filter(!is.na(reportingcategory_re))%>% # our person with NULL for race because they had too many races indicated by LASD
  select(geography, denom, target_result, reportingcategory_re, total, count, rate)%>%
  arrange(reportingcategory_re, -rate)

# NOTE there is no enrollment data for AIAN/NHPI/SSWANA AOIC and so we won't have these rate calcs with an enrollment denominator


# combine everything

df_final<-rbind(df_final, df_total_enr, df_race_enr)


#### Push to postgres 

# set column types

charvect = rep("varchar", ncol(df_final)) #create vector that is "varchar" for the number of columns in df
charvect <- replace(charvect, c(5,6,7), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(df_final)

table_name <- "analysis_stops_result_target_avuhsd"
schema <- "data"
indicator <- "Total and rate of target stop results by race out of total people stopped, which we subset as non-serious stop results which we define ourselves,
for people in AVUHSD schools. The target stop results currently are:
citation for infraction, referral school staff,contacted legal guardian,
no action,warning, referral to school administrator, in field cite and release.
The universe is all people stopped in AVUHSD OR total enrollment at AV school district for each racial group (which denominator is used is specific in the denom column),
and the numerator is the count people stopped with one or more target stop results
within each perceived racial group. Note this is person-level. NOTE for the enrollment denominator CDE does not have enrollment data for NHPI/SSWANA/AIAN AOIC so there are no 
rate calcs for these groups for the enrollment denominator, ONLY for the total people stopped denominator."
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_result_avuhsd.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_stops_result_avuhsd.docx" 
table_comment <- paste0(indicator, source)

# push to postgres
dbWriteTable(con,  table_name, df_final,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

# Comment on table and columns
column_names <- colnames(df_final) # Get column names
column_comments <- c(
  "geography",
  "type of denominator for the rate calc. It is either out of all unique people stopped in AVUHSD OR out of total enrollment of each racial group",
  "flag for if result is one of our target results (1/0)",
  "Perceived race",
  "Total number of people stopped",
  "Count of people stopped for either a target resutl or non-target result within each racial group",
  "Rate of target stop results for each racial group"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

