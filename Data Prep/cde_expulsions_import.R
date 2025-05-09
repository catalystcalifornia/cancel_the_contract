### Expulsions Data Import ### 

#install packages if not already installed
list.of.packages <- c("readr","tidyr","dplyr","DBI","RPostgreSQL","tidycensus", "rvest", "tidyverse", "stringr", "usethis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#library(readr)
library(dplyr)
library(tidyr)
library(DBI)
library(RPostgreSQL)
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
   filepath = "https://www3.cde.ca.gov/demo-downloads/discipline/expulsion24.txt"   # will need to update each year
   fieldtype = 1:11 # specify which cols should be varchar, the rest will be assigned numeric in table export
 
# ## Manually define postgres schema, table name, table comment, data source for rda_shared_data table
     table_schema <- "education"
     table_name <- "cde_multigeo_expulsion_2023_24"
     table_comment_source <- "Expulsion data downloaded from https://www.cde.ca.gov/ds/ad/filesed.asp"
     table_source <- "Wide data format, multigeo table with state, county, district, and school"

     
# Manually use CDE data function-------------------------------------
     
     #### Use this fx to get most CDE data ####
     get_cde_data <- function(filepath, fieldtype, table_schema, table_name, table_comment_source, table_source) {
       df <- read_delim(file = filepath, delim = "\t", na = c("*", ""))#, #name_repair=make.names ),
       #col_types = cols('District Code' = col_character()))
       
       #format column names
       names(df) <- str_replace_all(names(df), "[^[:alnum:]]", "") # remove non-alphanumeric characters
       names(df) <- gsub(" ", "", names(df)) # remove spaces
       names(df) <- tolower(names(df))  # make col names lowercase
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
     
     
     
# ## Run function to prep and export rda_shared_data table
  source("https://raw.githubusercontent.com/catalystcalifornia/RaceCounts/main/Functions/rdashared_functions.R")
  df <- get_cde_data(filepath, fieldtype, table_schema, table_name, table_comment_source, table_source) # function to create and export rda_shared_table to postgres db
  View(df)
# 
# ## Run function to add rda_shared_data column comments
# # See for more on scraping tables from websites: https://stackoverflow.com/questions/55092329/extract-table-from-webpage-using-r and https://cran.r-project.org/web/packages/rvest/rvest.pdf
url <-  "https://www.cde.ca.gov/ds/ad/fsed.asp"   # define webpage with metadata
html_nodes <- "table"
colcomments <- get_cde_metadata(url, html_nodes, table_schema, table_name)
View(colcomments)

df <- st_read(con, query = "SELECT * FROM education.cde_multigeo_expulsion_2023_24") # comment out code to pull data and use this once rda_shared_data table is created

#### Continue prep for RC ####

#filter for county and state rows, all types of schools, and racial categories
df_subset <- df %>% filter(aggregatelevel %in% c("C", "T", "D") & charteryn == "All" &
                             reportingcategory %in% c("TA", "RB", "RI", "RA", "RF", "RH", "RP", "RT", "RW")) %>%
  
  #select just fields we need
  select(cdscode, countyname, districtname, aggregatelevel, reportingcategory, cumulativeenrollment, totalexpulsions, unduplicatedcountofstudentsexpelledtotal,
         unduplicatedcountofstudentsexpelleddefianceonly, expulsionratetotal, expulsioncountviolentincidentinjury, expulsioncountviolentincidentnoinjury,
         expulsioncountweaponspossession, expulsioncountillicitdrugrelated, expulsioncountdefianceonly, expulsioncountotherreasons
         )%>%
  rename("cumulative_enrollment"="cumulativeenrollment",
         "total_expulsions"="totalexpulsions", 
         "unduplicated_count_students_expelled_total"="unduplicatedcountofstudentsexpelledtotal",
         "unduplicated_count_students_expelled_defiance_only"="unduplicatedcountofstudentsexpelleddefianceonly", 
         "expulsion_rate_total"="expulsionratetotal", 
         "expulsion_count_violent_incident_injury"="expulsioncountviolentincidentinjury", 
         "expulsion_count_violent_incident_noinjury"="expulsioncountviolentincidentnoinjury",
         "expulsion_count_weapons_possession"="expulsioncountweaponspossession", 
         "expulsion_count_illicit_drugrelated"="expulsioncountillicitdrugrelated", 
         "expulsion_count_defiance_only"="expulsioncountdefianceonly",
         "expulsion_count_other_reasons"="expulsioncountotherreasons"
    
  )

#format for column headers
df_subset <- rename(df_subset, 
                    raw = "total_expulsions",
                    pop = "cumulative_enrollment",
                    rate = "expulsion_rate_total")

df_subset$reportingcategory <- gsub("TA", "total", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RB", "nh_black", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RI", "nh_aian", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RA", "nh_asian", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RF", "nh_filipino", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RH", "latino", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RP", "nh_pacisl", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RT", "nh_twoormor", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RW", "nh_white", df_subset$reportingcategory)

#pop screen on number of chronically absent students (raw)
threshold <- 20
df_subset <- df_subset %>% mutate(raw = ifelse(raw < threshold, NA, raw)) %>%
  mutate(rate = ifelse(raw < threshold, NA, rate))
# View(df_subset)

#pivot
df_wide <- df_subset %>% pivot_wider(names_from = reportingcategory, names_glue = "{reportingcategory}_{.value}", 
                                     values_from = c(raw, pop, rate)) 


####### GET COUNTY & SCHOOL DISTRICT GEOIDS ##### ---------------------------------------------------------------------
census_api_key(census_key1, overwrite=TRUE) # In practice, may need to include install=TRUE if switching between census api keys
Sys.getenv("CENSUS_API_KEY") # confirms value saved to .renviron

# county geoids
counties <- get_acs(geography = "county",
                    variables = c("B01001_001"), 
                    state = "CA", 
                    year = 2023)

counties <- counties[,1:2]
counties$NAME <- gsub(" County, California", "", counties$NAME) 
names(counties) <- c("geoid", "geoname")
county_match <- filter(df_wide,aggregatelevel=="C") %>% right_join(counties,by=c('countyname'='geoname'))

# get school district geoids - pull in active district records w/ geoids from CDE schools' list (NCES District ID)]i 




####################################################################################################################################################
############## CALC RACE COUNTS STATS ##############i8[9[d]]
#set source for RC Functions script
source("https://raw.githubusercontent.com/catalystcalifornia/RaceCounts/main/Functions/RC_Functions.R")

d$asbest = 'min'    #YOU MUST UPDATE THIS FIELD AS APPROPRIATE: assign 'min' or 'max'

d <- count_values(d) #calculate number of "_rate" values
d <- calc_best(d) #calculate best rates -- be sure to update d$asbest accordingly before running this function.
d <- calc_diff(d) #calculate difference from best
d <- calc_avg_diff(d) #calculate (row wise) mean difference from best
d <- calc_p_var(d) #calculate (row wise) population or sample variance. be sure to use calc_s_var for sample data or calc_p_var for population data.
d <- calc_id(d) #calculate index of disparity
View(d)

#split STATE into separate table and format id, name columns
state_table <- d[d$geoname == 'California', ]

#calculate STATE z-scores
state_table <- calc_state_z(state_table)
state_table <- state_table %>% dplyr::rename("state_id" = "geoid", "state_name" = "geoname") %>% select(-c(districtname, cdscode, aggregatelevel))
View(state_table)

#remove state from county table
county_table <- d[d$aggregatelevel == 'C', ]

#calculate COUNTY z-scores
county_table <- calc_z(county_table)
county_table <- calc_ranks(county_table)
county_table <- county_table %>% dplyr::rename("county_id" = "geoid", "county_name" = "geoname") %>% select(-c(districtname, cdscode, aggregatelevel))
View(county_table)

#remove county/state from place table
city_table <- d[d$aggregatelevel == 'D', ] %>% select(-c(aggregatelevel)) 

#calculate DISTRICT z-scores
city_table <- calc_z(city_table)
city_table <- calc_ranks(city_table)
city_table <- city_table %>% dplyr::rename("dist_id" = "geoid", "district_name" = "districtname", "county_name" = "geoname") %>% relocate(county_name, .after = district_name)
View(city_table)


###update info for postgres tables###
county_table_name <- "arei_educ_chronic_absenteeism_county_2024"
state_table_name <- "arei_educ_chronic_absenteeism_state_2024"
city_table_name <- "arei_educ_chronic_absenteeism_district_2024"
rc_schema <- "v6"

indicator <- paste0("Created on ", Sys.Date(), ". Chronic Absenteeism Eligible Cumulative Enrollment, Chronic Absenteeism Count, and Chronic Absenteeism Rate. This data is")

source <- "CDE 2022-23 https://www.cde.ca.gov/ds/ad/filesabd.asp"

#send tables to postgres
#to_postgres(county_table,state_table)
#city_to_postgres()