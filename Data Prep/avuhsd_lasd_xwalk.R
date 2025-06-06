### Antelope Valley Union High School District - LA Sheriff's RIPA Crosswalk ### 
# list of schools from: https://www.avdistrict.org/schools

#install packages if not already installed
list.of.packages <- c("RPostgres", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(RPostgres)
library(tidyverse)

# create connection for rda database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")
con_ctc <- connect_to_db("cancel_the_contract")

# select Antelope Valley Union High School District schools in school data

avuhsd_schools<-dbGetQuery(con, "
SELECT cdscode, ncesdist, ncesschool, district, school  FROM education.cde_public_schools_2023_24
WHERE statustype = 'Active' AND district = 'Antelope Valley Union High' AND school 
IN (
'Antelope Valley High', 
'No Data', -- Antelope Valley Union High 
'Desert Winds Continuation High',
'Eastside High', 
'Highland High', 
'Lancaster High', 
'Littlerock High',
'Palmdale High', 
'Phoenix High Community Day',
'Quartz Hill High',
'R. Rex Parris High',
'William J. (Pete) Knight High'
)
ORDER BY school;
")

# Show district (Office) name for no data
# avuhsd_schools$school[avuhsd_schools$school=="No Data"] <- "Antelope Valley Union High"
# avuhsd_schools$ncesschool[avuhsd_schools$ncesschool=="No Data"] <- "00000"

# Make crosswalk
xwalk <- avuhsd_schools %>%
  
  # clarify school name from CDE
  rename(cde_school = school) %>%

  # create school name from LASD
  mutate(lasd_school = ifelse(cde_school == "No Data", "Antelope Valley Union High", cde_school)) %>%
  
  # add duplicate Phoenix High Community Day row but capturing errant LASD Phoenix Continuation
  add_row(cdscode="19642461995968",
           ncesdist="0602820",
           ncesschool="05827",
           district="Antelope Valley Union High",
           cde_school ="Phoenix High Community Day",
           lasd_school ="Phoenix Continutation") %>%
  
  # update "No Data" to Antelope Valley Union High
  filter(cdscode != "19642460000000") %>% add_row(cdscode="19642460000000",
          ncesdist="0602820",
          ncesschool="",
          district="Antelope Valley Union High",
          cde_school ="Antelope Valley Union High",
          lasd_school ="Antelope Valley Union High")


# Test it with LASD RIPA data

lasd_stops <- dbGetQuery(con, "SELECT * FROM crime_and_justice.lasd_stops_incident_2018_2023
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

# Join
join <- lasd_stops %>% left_join(xwalk, join_by("school_name" == "lasd_school")) 


# send Xwalk to Postgres
df <- xwalk

# set column types

charvect = rep("varchar", ncol(df)) #create vector that is "varchar" for the number of columns in df

# add df colnames to the character vector

names(charvect) <- colnames(df)

table_name <- "avuhsd_lasd_xwalk"

# push to postgres
dbWriteTable(con_ctc,  table_name, df,
             overwrite = FALSE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE avuhsd_lasd_xwalk IS 'Crosswalk of California Department of Education school names to LA County Sheriff school names for RIPA Analysis by School District.
R script: W:\\Project\\RJS\\CTC\\Github\\CR\\cancel_the_contract\\Data Prep\\avuhsd_lasd_xwalk.R
QA document: W:\\Project\\RJS\\CTC\\Documentation\\QA_Sheet_AVUHSD_LASD_xwalk.R;

 COMMENT ON COLUMN avuhsd_lasd_xwalk.cdscode IS CDS Code;
 COMMENT ON COLUMN avuhsd_lasd_xwalk.ncesdist IS NCES District Code;
 COMMENT ON COLUMN avuhsd_lasd_xwalk.ncesschool IS NCES School Code;
 COMMENT ON COLUMN avuhsd_lasd_xwalk.district IS NCES School District;
 COMMENT ON COLUMN avuhsd_lasd_xwalk.cde_school IS CDE School Name;
 COMMENT ON COLUMN avuhsd_lasd_xwalk.lasd_school IS LASD School Name;'

")

# send table comment + column metadata
dbSendQuery(conn = con_ctc, table_comment)


dbDisconnect(con)
dbDisconnect(con_ctc)
