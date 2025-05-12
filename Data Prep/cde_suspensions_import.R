### Suspensions Data Import ### 

#install packages if not already installed
list.of.packages <- c("readr","tidyr","dplyr","DBI","RPostgreSQL","tidycensus", "rvest", "tidyverse", "stringr", "usethis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#library(readr)
library(dplyr)
library(tidyr)
library(DBI)
library(RPostgres)
library(tidycensus)
library(sf)
library(tidyverse) # to scrape metadata table from cde website
#library(rvest) # to scrape metadata table from cde website
library(stringr) # cleaning up data
library(usethis) # connect to github


# create connection for rda database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")

############### PREP RDA_SHARED_DATA TABLE ########################

# ## Get Expulsions
filepath = "https://www3.cde.ca.gov/demo-downloads/discipline/suspension24.txt"   # will need to update each year
fieldtype = 1:11 # specify which cols should be varchar, the rest will be assigned numeric in table export

# ## Manually define postgres schema, table name, table comment, data source for rda_shared_data table
table_schema <- "education"
table_name <- "cde_multigeo_suspension_2023_24"
table_comment_source <- "Suspension data downloaded from https://www.cde.ca.gov/ds/ad/filessd.asp"
table_source <- "Wide data format, multigeo table with state, county, district, and school"


# Manually use CDE data function-------------------------------------

#### Use this fx to get most CDE data ####
get_cde_data <- function(filepath, fieldtype, table_schema, table_name, table_comment_source, table_source) {
  df <- read_delim(file = filepath, delim = "\t", na = c("*", ""))#, #name_repair=make.names ),

  #format column names
  names(df) <- str_replace_all(names(df), "[^[:alnum:]]", "") # remove non-alphanumeric characters
  names(df) <- gsub("([a-z])([A-Z])", "\\1_\\2", names(df))
  names(df) <- tolower(names(df))  # make col names lowercase
  
  df<-df%>%
    rename("districtcode"="district_code",
           "academicyear"="academic_year",
           'aggregatelevel'="aggregate_level",
           "schoolcode"="school_code",
           "schoolname"="school_name",
           "countycode"="county_code",
           "countyname"="county_name",
           "districtname"="district_name",
           "districtcode"="district_code",
           "reportingcategory"="reporting_category",
           "charteryn"="charter_yn") # rename these to match other cde data tables
  
  Encoding(df$schoolname) <- "ISO 8859-1"  # added this piece in 2023 script bc Spanish accents weren't appearing properly bc CDE native encoding is not UTF-8
  Encoding(df$districtname) <- "ISO 8859-1"  # added this piece in 2023 script bc Spanish accents weren't appearing properly bc CDE native encoding is not UTF-8
  df$districtcode<-as.character(df$districtcode)
  
  #create cdscode field
  df$cdscode <- ifelse(df$aggregatelevel == "D", paste0(df$countycode,df$districtcode,"0000000"),
                       ifelse(df$aggregatelevel == "S", paste0(df$countycode,df$districtcode,df$schoolcode), paste0(df$countycode,"000000000000")))
  df <- df %>% relocate(cdscode) # make cds code the first col
    
  #  WRITE TABLE TO POSTGRES DB
  
  # make character vector for field types in postgres table
  charvect = rep('numeric', dim(df)[2]) 
  charvect[fieldtype] <- "varchar" # specify which cols are varchar, the rest will be numeric
  
  # add names to the character vector
  names(charvect) <- colnames(df)
  
  dbWriteTable(con, Id(table_schema, table_name), df, 
               overwrite = TRUE, row.names = FALSE,
               field.types = charvect)
  
  # write comment to table, and the first three fields that won't change.
  table_comment <- paste0("COMMENT ON TABLE ", table_schema, ".", table_name, " IS '", table_comment_source, ". ", table_source, ".';")
  
  # send table comment to database
  dbSendQuery(conn = con, table_comment)      			
  
  return(df)
}


df <- get_cde_data(filepath, fieldtype, table_schema, table_name, table_comment_source, table_source) # function to create and export rda_shared_table to postgres db

# get column names
colnames<-as.data.frame(colnames(df))

