# Make data dictionary for Cancel the Contract Report from research plan 
# https://catalystcalifornia.sharepoint.com/:w:/s/Portal/EZbzpfdC3JVGppzpk3qbtD0BE8wH2y_CbDr4Mb_HFIxU9w?e=f7IGHo
# Save it to Postgres for use in other files

##### Environment set up #####

library(tidyverse)
library(RPostgres) 

source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("cancel_the_contract")

# Preset the racenote
racenote<-"Race Note: AIAN=American Indian/Alaskan Native Alone, NHPI=Native Hawaiian/Pacific Islander, SSWANA=South/Southwest Asian and North African."
racenote_cde<-"Race Note: Latinx is alone or in combination with other racial groups."

##### Data frame set up #####

# declaring an empty data frame
df = data.frame(
  indicator_short = character(),
  indicator = character(),
  source = character(),
  source_url = character(),
  year = character(),
  geographies = character(),
  method_note = character(),
  limitations = character(),
  race_note=character(),
  stringsAsFactors = FALSE)

print (df)

## Demographic indicators:

df[1, ] <- list("Population", "Total population", "American Community Survey 5-Year Estimates Table DP05", "https://data.census.gov/", "2019-2023", "Service Planning Area, LA County", "Antelope Valley population estimated by aggregating Census Tracts in Service Planning Area 1.", "Estimates are based on samples with margins of error.", "")

df[2, ] <- list("Race", "Antelope Valley population by race", "American Community Survey 5-Year Estimates Table DP05", "https://data.census.gov/", "2019-2023", "Service Planning Area, LA County", "Antelope Valley population by race estimated by aggregating Census Tracts in Service Planning Area 1.", "Estimates are based on samples with margins of error.", racenote)

df[3, ] <- list("Student Population", "Student population", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "")

df[4, ] <- list("Student Race", "AVUHSD student population by race", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "Race Note: AIAN=American Indian/Alaskan Native Alone, NHPI=Native Hawaiian/Pacific Islander.")

## Education indicators

df[5, ] <- list("Special Education Enrollment", "Student population enrolled in special education", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "")

df[6, ] <- list("Student graduation by race", "AVUHSD student graduation rate by race", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "2023-2024", "", "", "")


df[7, ] <- list("Suspensions", "AUHSD student suspension rate", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "")

df[8, ] <- list("Suspensions by race", "AVUHSD student suspension rate by race", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "")

df[9, ] <- list("Suspensions by special education enrollment", "Student population suspended by enrollment in special education", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "")

df[10, ] <- list("Expulsions", "Student population expelled", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "")

df[11, ] <- list("Expulsions by race", "Student population expelled by race", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "")

df[12, ] <- list("Expulsions by special education enrollment", "Student population expelled by enrollment in special education", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "", "")


### RIPA indicators: AVUHSD



df[13, ] <- list("Police Stops by Race AVUHSD", "Students law enforcement stopped in a AVUHSD school by perceived race", "California Department of Justice", "", "2018-2023", "", "", "", "", "")

df[14, ] <- list("Police Stops by Reason of Stop AVUHSD", "Students law enforcement stopped in a AVUHSD school by reason of stop", "California Department of Justice", "", "2018-2023", "", "", "", "")
df[15, ] <- list("Police Stops by Result of Stop AVUHSD", "Stops of AVUHSD students by stop result", "California Department of Justice", "", "2018-2023", "", "", "", "Note: 904 students were stopped by police in AVUHSD between 2018-2023. Rates do not add to 100% because a police officer can report more than one result for a stop of a single student. Stop results analyzed here are alone or in combination with other stop results.")

df[16, ] <- list("Police Stops Search AVUHSD", "Searches by race", "California Department of Justice", "", "2018-2023", "", "", "", "")
df[17, ] <- list("Police Hit Rate AVUHSD", "Students law enforcement stopped and searched with contraband found in a AVUHSD school", "California Department of Justice", "", "2018-2023", "", "", "", "")


### RIPA indicators: AV

df[18, ] <- list("Police Stops by Race", "People law enforcement stopped in the AV by perceived race", "California Department of Justice", "", "2023", "", "", "", "", "")



### TCE indicators

df[19, ] <- list("Green space", "", "", "", "", "", "", "", racenote)
df[20, ] <- list("Infant Mortality", "", "LA County Department of Public Health", "", "", "", "", "", racenote)
df[21, ] <- list("Youth Mental Health", "", "", "", "", "", "", "", "", "")
df[22, ] <- list("Manager employment", "AV manager rates by race", "American Community Survey 5-Year Estimates Public Use Microdata Sample", "", "2019-2023", "", "", "", "Race Note: AIAN=American Indian/Alaskan Native Alone, NHPI=Native Hawaiian/Pacific Islander, SSWANA=South/Southwest Asian and North African.")
df[23, ] <- list("Healthy food", "", "", "", "", "", "", "", "", "")
df[24, ] <- list("Home ownership", "AV homeownership rates by race", "American Community Survey 5-Year Estimates Table B25003","", "2019-2023", "", "", "", "Race Note: AIAN=American Indian/Alaskan Native Alone, NHPI=Native Hawaiian/Pacific Islander")
df[25, ] <- list("Rent burden", "", "", "", "", "", "", "", "", racenote)


### ECI Budget indicators

df[26, ] <- list("Teacher Ratio", "Student to teacher ratios in AVUHSD", "School Accountability Report Cards, Antelope Valley Union High School District", "https://www.cta.org/our-advocacy/class-size-matters", "2023-2024", "School District", "", "",  "")
df[27, ] <- list("Counselor Ratio", "Student to counselor ratios in AVUHSD", "School Accountability Report Cards Antelope Valley Union High School District; California Department of Education", "https://www.avdistrict.org/schools/school-accountability-report-cards;  https://dq.cde.ca.gov/dataquest/page2.asp?level=District&subject=Enrollment&submit1=Submit", "2023-2024", "School District", "", "", "")

print (df)

##### Push data frame to Postgres #####

# # write table

table_name <- 'data_dictionary'
schema<- 'data'
indicator <- "Data dictionary" 
source <- "Catalyst California, 2025"
qa_filepath <-  "W:\\Project\\RJS\\CTC\\Documentation\\QA_data_dictionary.docx
Script: W:\\Project\\RJS\\CTC\\github\\CR\\cancel_the_contract\\Data Prep\\data_dictionary.R"

table_comment <- paste0(indicator, source)

dbWriteTable(con,
             Id(schema = schema, table = table_name),
             df, overwrite = TRUE, row.names = FALSE)


#Add comment on table and columns

column_names <- colnames(df) # get column names

column_comments <- c(
  "indicator short name",
  "indicator long name",
  "data source",
  "data source website",
  "year(s) of data",
  "geographies of data used",
  "brief methodology",
  "limitations of indicator or methodology"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


dbDisconnect(con)