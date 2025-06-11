# Antelope Valley Stops by Perceived Disability 2018-2023


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


############### CALCULATE STOPS BY DISABILITY ########################

av_stops <- lasd_incidents %>% left_join(lasd_persons, by = "contact_id")

# check how many of each category of no_disabilities
table(av_stops$no_disabilities)

# create data frame of just stops with disabilities
av_stops_disabled <- av_stops %>% filter(no_disabilities %in% c("false", "No"))

# double check all have a disability
table(av_stops_disabled$no_disabilities)

# check that all disabilities have at least one true - all except blind or limited vision
table(av_stops_disabled$deaf_or_difficulty_hearing) 
table(av_stops_disabled$speech_impaired_or_limited_use_of_language)
table(av_stops_disabled$blind_or_limited_vision)
table(av_stops_disabled$mental_health_condition)
table(av_stops_disabled$intellectually_or_developmental_disability)
table(av_stops_disabled$other_disabilities)
table(av_stops_disabled$hyperactive_or_impulsive_behavior)

# calculate number of stops with a disability
stops_w_disability <- nrow(av_stops_disabled)

# calculate total stops
stops <- nrow(av_stops)

# calculate stops with a disability as a percent of total stops
pct_stops_w_disability <- stops_w_disability / stops * 100


############### GET ENROLLMENT DATA and CALUCLATE ENROLLMENT BY DISABILITY ########################

enrollment <- dbGetQuery(con_shared, "SELECT * FROM education.cde_multigeo_enrollment_census_day_2024_25")

# get Antelope Valley Union High School District enrollment
av_enrollment <- enrollment %>% filter(districtname == "Antelope Valley Union High" & charter == "ALL" & 
                                         reportingcategory == "TA") %>% 
  pull(total_enr)

# get AVUHSD disabled enrollment
av_enrollment_disabled <- enrollment %>% filter(districtname == "Antelope Valley Union High" & 
                                                     charter == "ALL" & 
                                                     reportingcategory == "SG_DS") %>% 
  pull(total_enr)

# calculate enrollment with a disability as a percent of total enrollment
pct_enrollment_w_disability <- av_enrollment_disabled / av_enrollment * 100


############### PUT IT TOGETHER ########################

df <- data.frame(stops, 
                 stops_w_disability, 
                 pct_stops_w_disability, 
                 av_enrollment, 
                 av_enrollment_disabled, 
                 pct_enrollment_w_disability)


############### SEND TO POSTGRES ########################


# set column types

charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df

# add df colnames to the character vector

names(charvect) <- colnames(df)

table_name <- "analysis_stops_disability"

# push to postgres
dbWriteTable(con,  table_name, df,
             overwrite = FALSE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE stops_by_disability IS 'Analysis table of AVUSHD LASD stops by disability and CDE enrollment by disability.
R script: W:/Project/RJS/CTC/Github/CR/cancel_the_contract/Analysis/analysis_stops_disability.R
QA document: W:\\Project\\RJS\\CTC\\Documentation\\QA_Sheet_Disability.R;

 COMMENT ON COLUMN stops_by_disability.stops IS LASD stops in AVUHSD;
 COMMENT ON COLUMN stops_by_disability.stops_w_disability IS LASD stops with a disability in AVUHSD;
 COMMENT ON COLUMN stops_by_disability.pct_stops_w_disability IS Percent of LASD stops with a disability in AVUHSD;
 COMMENT ON COLUMN stops_by_disability.av_enrollment IS AVUHSD enrollment;
 COMMENT ON COLUMN stops_by_disability.av_enrollment_disabled IS AVUHSD enrollment with a disability;
 COMMENT ON COLUMN stops_by_disability.pct_enrollment_w_disability IS Percent of AVUHSD enrollment with a disability;'

")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)


dbDisconnect(con)
dbDisconnect(con_shared)






