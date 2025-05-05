# Make data dictionary for Cancel the Contract Report
# Save it to Postgres for use in other files

##### Environment set up #####

library(tidyverse)
library(RPostgres) 

source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("cancel_the_contract")


##### Data frame set up #####

# declaring an empty data frame
data_frame = data.frame(
  indicator_short = character(),
  indicator = character(),
  source = character(),
  source_url = character(),
  method_note = character(),
  limitations = character(),
  stringsAsFactors = FALSE)

print (data_frame)

data_frame[1, ] <- list("test", "test", "test", "test", "test", "test")

print (data_frame)

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
             df, overwrite = FALSE, row.names = FALSE, field.types = charvect)


#Add comment on table and columns

column_names <- colnames(df) # get column names

column_comments <- c(
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  ""
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)



dbDisconnect(con)