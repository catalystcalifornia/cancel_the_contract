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

# first create a cleaner cfs flag

av_stops<-av_stops%>%
  mutate(cfs=ifelse(call_for_service %in% "false", 0,
                    ifelse(call_for_service %in% "No", 0,
                           ifelse(call_for_service %in% "true", 1,
                                  1
                    ))))

# double check it

table(av_stops$cfs)

# select columns of interest and clean up columns 

test<-av_stops%>%
  select(1,2,159, 25:31)%>%
  mutate(across(3:10, ~ case_when(
    . %in% c("Yes", "true") ~ 1,
    . %in% c("No", "false") ~ 0,
    TRUE ~ NA_real_)))%>%
  mutate(aian_flag=ifelse(native_american==1, 1,0))%>%
  mutate(nhpi_flag=ifelse(pacific_islander==1, 1,0))%>%
  mutate(nhpi_flag=ifelse(middle_eastern_south_asian==1, 1,0))%>%
  mutate(latinx=ifelse(hispanic_latino_latina==1, 1,
                ifelse(white==1 & (white == 1 | black_african_american == 1 | 
                                     asian == 1 | native_american == 1 | 
                                     pacific_islander == 1 | middle_eastern_south_asian==1), 1, 0)))%>%
  mutate(total=n())

# pivot data longer                          
                          
av_stops_long<-test%>%
  pivot_longer(
    cols = 4:10,
    names_to = "reportingcategory",
    values_to = "value"
  )%>%
  group_by(contact_id, person_id)%>%
  filter(value!=0) # only keep where at least one of the racial columns == 1 

# Explore duplicates (these should be people where more than 1 race column == 1

dup<-av_stops_long %>%
  group_by(person_id) %>%
  filter(n() > 1)

####################### Calculate rates for CFS+Not CFS ######################

tot<-av_stops_long%>%
  group_by(reportingcategory)%>%
  mutate(count=n(),
         rate=count/total*100)
  

############### GET ENROLLMENT DATA and CALUCLATE ENROLLMENT BY RACE ########################

enrollment <- dbGetQuery(con_shared, "SELECT * FROM education.cde_multigeo_enrollment_census_day_2024_25")

# get Antelope Valley Union High School District enrollment
av_enrollment <- enrollment %>% filter(districtname == "Antelope Valley Union High" & charter == "ALL" & 
                                         reportingcategory == "TA")


############### PUT IT TOGETHER ########################

df <- data.frame(stops, 
                 stops_w_disability, 
                 pct_stops_w_disability, 
                 av_enrollment, 
                 av_enrollment_disabled, 
                 pct_enrollment_w_disability)

#### Clean up final df for postgres #####

df<-df%>%
  mutate(geography="Antelope Valley Union High School District",
         reportingcategory_re="Students with Disabilities")%>%
  rename("stops_total"="stops",
         "stops_disability_count"="stops_w_disability",
         "stops_disability_rate"="pct_stops_w_disability",
         "enrollment_total"="av_enrollment",
         "enrollment_disabled_count"="av_enrollment_disabled",
         "enrollment_disability_rate"="pct_enrollment_w_disability")%>%
  select(geography, reportingcategory_re, everything())


############### SEND TO POSTGRES ########################


# set column types

charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df

# add df colnames to the character vector

names(charvect) <- colnames(df)

table_name <- "analysis_stops_disability_avuhsd"
schema <- "data"
indicator <- "Analysis table of AVUSHD LASD stops by disability and CDE enrollment by disability."
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_stops_disability.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_Sheet_Disability.R" 
table_comment <- paste0(indicator, source)
table_name <- "analysis_stops_disability_avuhsd"

# push to postgres
dbWriteTable(con,  table_name, df,
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data


# Comment on table and columns

column_names <- colnames(df) # Get column names
column_comments <- c(
  "geography level",
  "CDE reporting category recoded",
  "LASD stops in AVUHSD",
  "LASD stops with a disability in AVUHSD",
  "Percent of LASD stops with a disability in AVUHSD",
  "AVUHSD enrollment",
  "AVUHSD enrollment with a disability",
  "Percent of AVUHSD enrollment with a disability"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
dbDisconnect(con_shared)
