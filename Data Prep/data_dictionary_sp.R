# Make SP data dictionary for Cancel the Contract Report from research plan 
# Translations saved here: "W:\Project\RJS\CTC\Data\ctc_chart_translation_sp.xlsx" 

##### Environment set up #####

library(tidyverse)
library(RPostgres) 
library(readxl)

source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("cancel_the_contract")

# Preset the racenote
racenote<-"Race Note: AIAN=American Indian/Alaskan Native Alone, NHPI=Native Hawaiian/Pacific Islander, SSWANA=South/Southwest Asian and North African."
racenote_cde<-"Race Note: Latinx is alone or in combination with other racial groups."

# read in translation csv

sp<-read.csv("W:\\Project\\RJS\\CTC\\Data\\ctc_chart_translation_sp.csv", fileEncoding = "UTF-8")

# clean up column names

sp<-sp%>%
  rename("filename"="Filename..no.need.to.translate.",
         "indicator_short"="indicator_short..no.need.to.translate.",
         "subtitle"="indicator",
         "total_label"="Total.Label"
         )%>%
  mutate(across(where(is.character),
                ~ gsub("Indio americano y nativo de Alaska",
                       "indígena de las Américas o nativo de Alaska",
                       .))) # replace the AIAN Spanish translation to match how it is worded in Census per AB's recommendation
# remove empty rows
sp<-sp%>%slice(1:13)

# some titles are missing a period. add those for consistency:

sp$title <- ifelse(grepl("\\.$", sp$title),
                   sp$title,
                   paste0(sp$title, "."))

##### Push data frame to Postgres #####

# # write table

table_name <- 'data_dictionary_sp'
schema<- 'data'
indicator <- "Data dictionary" 
source <- "Catalyst California, 2025"
qa_filepath <-  "Script: W:\\Project\\RJS\\CTC\\github\\CR\\cancel_the_contract\\Data Prep\\data_dictionary_sp.R"

table_comment <- paste0(indicator, source)

dbWriteTable(con,
             Id(schema = schema, table = table_name),
             sp, overwrite = TRUE, row.names = FALSE)


#Add comment on table and columns

column_names <- colnames(sp) # get column names

column_comments <- c(
  "file name",
  "indicator short name",
  "title of visual",
  "subtitle of visuasl",
  "Label1",
  "Label2",
  "Label3",
  "Label4",
  "Label5",
  "Label6",
  "Label7",
  "Label8",
"Label9",
"Label10",
"Label11",
"Label12",
"Label13",
"Label14",
"Label15",
"total label",
"source",
"race note")


add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
dbDisconnect(con)
