# Make data dictionary for Cancel the Contract Report from research plan 
# https://catalystcalifornia.sharepoint.com/:w:/s/Portal/EZbzpfdC3JVGppzpk3qbtD0BE8wH2y_CbDr4Mb_HFIxU9w?e=f7IGHo
# Save it to Postgres for use in other files

##### Environment set up #####

library(tidyverse)
library(RPostgres) 

source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("cancel_the_contract")


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
  stringsAsFactors = FALSE)

print (df)

df[1, ] <- list("Population", "Total population", "American Community Survey 5-Year Estimates Table DP05", "https://data.census.gov/", "2019-2023", "Service Planning Area, LA County", "Antelope Valley population estimated by aggregating Census Tracts in Service Planning Area 1.", "Estimates are based on samples with margins of error.")
df[2, ] <- list("Race", "Population by race", "American Community Survey 5-Year Estimates Table DP05", "https://data.census.gov/", "2019-2023", "Service Planning Area, LA County", "Antelope Valley population by race estimated by aggregating Census Tracts in Service Planning Area 1.", "Estimates are based on samples with margins of error.")

df[3, ] <- list("Student Population", "Student population", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")
df[4, ] <- list("Student Race", "Student population by race", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")
df[5, ] <- list("Special Education Enrollment", "Student population enrolled in special education", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")
df[6, ] <- list("Suspensions", "Student population suspended", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")
df[7, ] <- list("Suspensions by race", "Student population suspended by race", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")
df[8, ] <- list("Suspensions by special education enrollment", "Student population suspended by enrollment in special education", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")
df[9, ] <- list("Expulsions", "Student population expelled", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")
df[10, ] <- list("Expulsions by race", "Student population expelled by race", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")
df[11, ] <- list("Expulsions by special education enrollment", "Student population expelled by enrollment in special education", "California Department of Education", "https://dq.cde.ca.gov/dataquest/", "2023-2024", "School District, LA County", "", "")

df[12, ] <- list("Police Stops", "People stopped by law enforcement", "California Department of Justice", "", "2023", "", "", "")
df[13, ] <- list("Police Stops by Race", "People stopped by law enforcement by race", "California Department of Justice", "", "2023", "", "", "")
df[14, ] <- list("Police Stops by Reason of Stop", "People stopped by law enforcement by reason of stop", "California Department of Justice", "", "2023", "", "", "")
df[15, ] <- list("Police Stops by Reason of Stop", "People stopped by law enforcement by reason of stop", "California Department of Justice", "", "2023", "", "", "")

df[16, ] <- list("Pollution Burden", "", "OEHHA CalEnviroScreen 4.0", "", "2024", "", "", "")
df[17, ] <- list("Infant Mortality", "", "LA County Department of Public Health", "", "", "", "", "")
df[18, ] <- list("Youth Mental Health Hospitalizations", "", "", "", "", "", "", "")

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
             df, overwrite = FALSE, row.names = FALSE)


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