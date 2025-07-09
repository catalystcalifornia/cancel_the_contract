# Antelope Valley Union High School District Stops by Perceived Race


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


############### CLEAN UP AND RECODE COLUMNS ########################

av_stops <- lasd_persons %>% right_join(lasd_incidents, by = "contact_id")

# explore call for service break down

table(av_stops$call_for_service) ## 65% not CFS, 35% CFS 

# I am going to do the analysis for CFS, Not CFS and Combined

# select columns of interest and clean up columns 

av_stops_re<-av_stops%>%
  select(1,2,159, 25:31)%>%
  mutate(across(3:10, ~ case_when(
    . %in% c("Yes", "true") ~ 1,
    . %in% c("No", "false") ~ 0,
    TRUE ~ NA_real_)))%>%
  mutate(aian_flag=ifelse(native_american==1, 1,0))%>%
  mutate(nhpi_flag=ifelse(pacific_islander==1, 1,0))%>%
  mutate(sswana_flag=ifelse(middle_eastern_south_asian==1, 1,0))%>%
  mutate(latinx=ifelse(hispanic_latino_latina==1, 1,
                ifelse(hispanic_latino_latina==1 & (white == 1 | black_african_american == 1 | 
                                     asian == 1 | native_american == 1 | 
                                     pacific_islander == 1 | middle_eastern_south_asian==1), 1, 0)))%>%
  mutate(total=n())

# pivot data longer                          
                          
av_stops_long<-av_stops_re%>%
  pivot_longer(
    cols = 4:10,
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
  arrange(-n)

# From this we see there is only one stop/person that had 7 races indicated, and one stop/person that had 4 races indicated. 
# All others are 2 races --these can be recoded as nh_twoormor as long as one of the races is NOT latinx (if one race is latinx the coding is latinx) 
# The person with 4 races indicated--recode as nh_twoormor as long as one of the races is NOT latinx
# Person with 7 races indicated --recode as NULL 

test<-av_stops_long%>%
  group_by(contact_id, person_id) %>%
  mutate(reportingcategory = ifelse(n() >= 2 & n() <= 6 & latinx != 1, 
                                    "nh_twoormor", 
                                    reportingcategory))%>%
  filter(reportingcategory=="nh_twoormor")

check<-av_stops%>%filter(person_id == '132767')

# Doing this made me realize the ONLY person with 2+ races where one is NOT latinx is someone who is Black/Pacific Islander.
# Instead of coding them as nh_twoormor I want to code them as NHPI since that group is commonly multiracial and is underrepresented. 

av_stops_long_re<-av_stops_long%>%
  group_by(contact_id, person_id) %>%
  mutate(reportingcategory = ifelse(n() >= 2 & n() <= 6 & latinx != 1, 
                                    "nh_nhpi", 
                               ifelse(n() >= 2 & n() <= 6 & latinx == 1, "latinx", # add this so if someone has more than 1 race indicated and one of them is latinx then that person is coded as latinx       
                                      ifelse(n() >=6, "NULL",    
                                      reportingcategory))))

# Check why there are no sswana rows in the reportingcategory column left

sswana_check<-av_stops%>%filter(middle_eastern_south_asian=="true" | middle_eastern_south_asian== 
                                  "Yes") # looks like everyone who was perceived as SSWANA also was perceived as Latinx so they were recoded as Latinx but they will still have a sswana_flag


# Now that all the races are recoded I can clean up the groups so every stop/person is one row

av_stops_long_re<-av_stops_long_re%>%
  group_by(contact_id, person_id)%>%
  slice(1)

# now recode the racial category column

av_stops_long_re<-av_stops_long_re%>%
  mutate(reportingcategory_re=ifelse(reportingcategory %in% "black_african_american", "nh_black",
                                     ifelse(reportingcategory %in% "native_american", "nh_aian", 
                                            ifelse(reportingcategory %in% "asian", "nh_asian", 
                                                          ifelse(reportingcategory %in% "hispanic_latino_latina", "latinx", 
                                                                        ifelse(reportingcategory %in% "pacific_islander", "nh_nhpi",  
                                                                                      ifelse(reportingcategory %in% "white", "nh_white", 
                                                                                             reportingcategory)))))))
         
  
table(av_stops_long_re$reportingcategory_re)

####################### Calculate rates for CFS+Not CFS ######################

# for all nh races

tot<-av_stops_long_re%>%
  group_by(reportingcategory_re)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(universe="All stops",
         geography="Antelope Valley Union High School District")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  select(universe, geography, reportingcategory_re, total, count, rate)

# for AIAN/NHPI/SSWANA

tot_flags <- av_stops_long_re %>%
  ungroup() %>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  group_by(flag_type) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    universe = "All stops",
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
  select(universe, geography, reportingcategory_re, total, count, rate)

# Combine

tot<-tot%>%
  rbind(tot_flags)%>%
  arrange(-rate)

####################### Calculate rates for CFS ######################

# for all nh races

tot_cfs<-av_stops_long_re%>%
  filter(call_for_service==1)%>%
  ungroup()%>%
  mutate(total=n())%>%
  group_by(reportingcategory_re)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(universe="Stops with call for service",
         geography="Antelope Valley Union High School District")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  select(universe, geography, reportingcategory_re, total, count, rate)

# for AIAN/NHPI/SSWANA

tot_flags_cfs <- av_stops_long_re %>%
  ungroup() %>%
  filter(call_for_service==1)%>%
  mutate(total=n())%>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  group_by(flag_type) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    universe = "Stops with call for service",
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
  select(universe, geography, reportingcategory_re, total, count, rate)

# Combine

tot_cfs<-tot_cfs%>%
  rbind(tot_flags_cfs)%>%
  arrange(-rate)

####################### Calculate rates for NON CFS ######################

# for all nh races

tot_nocfs<-av_stops_long_re%>%
  filter(call_for_service==0)%>%
  ungroup()%>%
  mutate(total=n())%>%
  group_by(reportingcategory_re)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(universe="Stops with no call for service",
         geography="Antelope Valley Union High School District")%>%
  filter(reportingcategory_re!="NULL")%>%
  slice(1)%>%
  select(universe, geography, reportingcategory_re, total, count, rate)

# for AIAN/NHPI/SSWANA

tot_flags_nocfs <- av_stops_long_re %>%
  ungroup() %>%
  filter(call_for_service==0)%>%
  mutate(total=n())%>%
  filter(reportingcategory_re != 'NULL') %>%
  pivot_longer(cols = c(aian_flag, nhpi_flag, sswana_flag), 
               names_to = "flag_type", 
               values_to = "flag_value") %>%
  filter(flag_value == 1) %>%
  group_by(flag_type) %>%
  summarise(
    total = first(total),
    count = sum(flag_value),
    rate = count / total * 100,
    universe = "Stops with no call for service",
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
  select(universe, geography, reportingcategory_re, total, count, rate)

# Combine

tot_nocfs<-tot_nocfs%>%
  rbind(tot_flags_nocfs)%>%
  arrange(-rate)

############### FINAL COMBINE TOT/CFS/NO CFS TABLES ########################

df<-rbind(tot, tot_cfs, tot_nocfs)

############### GET ENROLLMENT DATA and CALUCLATE ENROLLMENT BY RACE ########################

enrollment <- dbGetQuery(con_shared, "SELECT * FROM education.cde_multigeo_enrollment_census_day_2024_25")

# get Antelope Valley Union High School District enrollment
av_enrollment <- enrollment %>% filter(districtname == "Antelope Valley Union High" & charter == "ALL")%>%
  filter(grepl("RE_", reportingcategory))

# check I got all the racial reporting groups

table(av_enrollment$reportingcategory)

# recode racial categories

av_enrollment<-av_enrollment%>%
  mutate(reportingcategory_re=ifelse(reportingcategory %in% "RE_B", "nh_black",
                                     ifelse(reportingcategory %in% "RE_I", "nh_aian", 
                                            ifelse(reportingcategory %in% "RE_A", "nh_asian", 
                                                   ifelse(reportingcategory %in% "RE_F", "nh_filipino",  
                                                          ifelse(reportingcategory %in% "RE_H", "latinx", 
                                                                 ifelse(reportingcategory %in% "RE_D", "missing_race",
                                                                        ifelse(reportingcategory %in% "RE_P", "nh_nhpi",  
                                                                               ifelse(reportingcategory %in% "RE_T", "nh_twoormor",  
                                                                                      ifelse(reportingcategory %in% "RE_W", "nh_white", 
                                                                                             reportingcategory))))))))))%>%
  select(cdscode, districtname, reportingcategory_re, total_enr)

############### JOIN AVUSD ENROLLMENT DATA TO STOP RATES  ########################

df<-df%>%
  left_join(av_enrollment, by=c("reportingcategory_re"="reportingcategory_re"))%>%
  select(-c("cdscode", "districtname"))%>%
  rename("enrollment_race_count"="total_enr")%>%
  mutate(rate_enr_1k=count/enrollment_race_count*1000,
        total_enrollment=21958,
        enrollment_rate=enrollment_race_count/total_enrollment*100
       )


############### PUSH TABLE TO POSTGRES #####################

# set column types

charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df
charvect <- replace(charvect, c(4,5,6,7,8,9,10), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

table_name <- "analysis_stops_race_avuhsd"
schema <- "data"
indicator <- "Analysis table of AVUSHD LASD stops by perceived race, stops per 1000 total CDE enrollment by race, and CDE enrollment/rates by race"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_race_avuhsd.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_analysis_stops_race_avuhsd.R" 
table_comment <- paste0(indicator, source)
table_name <- "analysis_stops_race_avuhsd"

# push to postgres
dbWriteTable(con,  table_name, df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

column_names <- colnames(df) # Get column names
column_comments <- c(
  "Universe: call for service, NOT call for service, or COMBINED CFS+Not CFs",
   "geography level",
  "CDE reporting category recoded",
  "TOtal LASD stops in AVUHSD",
  "Count of LASD stops by perceived race in AVUHSD",
  "Percent of LASD stops by perceived race in AVUHSD",
  "AVUHSD enrollment by race",
  "Rate of stops of in AVUHSD by perceived race per 1,000 total students enrolled in AVUHSD by race",
  "AVUHSD total enrollment",
  "AVUHSD enrollment percentages by race"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
dbDisconnect(con_shared)
