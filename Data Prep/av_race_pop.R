#### Calculate population estimates by race for Antelope Valley #### 

#### Set up workspace ####
library(RPostgres)
library(knitr)
library(dplyr)
library(sf)
library(data.table)
library(tidyr)
library(tidyverse)
library(tidycensus)
library(Hmisc)
library(glue)
library(labelled)
library(readxl)
library(srvyr)

# Connect to postgres 

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("cancel_the_contract")
con_shared<-connect_to_db("rda_shared_data")

# Read in tables from postgres ----

# race table
df<- dbGetQuery(con_shared, "SELECT * FROM demographics.acs_5yr_multigeo_2023_race_long")

# AV-tract xwalk from the AV State of the Child project

av_ct<-dbGetQuery(con, "SELECT * FROM av_ct")

# South asian estimates ----
# calculate south asian pop to subtract from nh_asian pop 
# getting data and vars from census api
v23 <- load_variables(2023, "acs5", cache = TRUE)%>%
  filter(grepl("B02015",name))

asian <- get_acs(geography ='tract', 
                 table = c("B02015"), #subject table code
                 state="CA",
                 year = 2023,
                 survey = 'acs5')

# create a list of South Asian vars
# reference from race recoding script - soasian_list<-list("Asian Indian", "Bangladeshi", "Bhutanese","Maldivian","Nepalese","Pakistani","Sikh","Sindhi","Sri Lankan","Other South Asian")
soasian_variables<-v23%>%filter(grepl("South Asian",label))

# Convert to a list
soasian_columns<-as.list(soasian_variables$name)

# Narrow down variables from acs table based on the list of South Asian variables
soasian<-asian[grep(paste(soasian_columns,collapse="|"), asian$variable,ignore.case=TRUE),]

colnames(soasian)<-tolower(names(soasian))

## Calculate South Asian Count in AV
soasian_av<-soasian%>%
  filter(geoid %in% av_ct$tract_geoid)%>%
  group_by(geoid)%>%
  summarise(soasian_count=sum(estimate),
            soasian_moe=moe_sum(moe,estimate))


# Calculate race estimates for AV ----

##### standard race groups  ------
av_race<-df%>%
  filter(geoid %in% av_ct$tract_geoid)

##### asian minus south asian  ------
# reference for calcs using tidycensus https://psrc.github.io/psrccensus/articles/calculate-reliability-moe-transformed-acs.html
# get nh asian counts from dp05 table and join to soasian counts
asian_wo_soasian<-av_race%>%filter(variable=='nh_asian')%>%left_join(soasian_av, by=c("geoid"="geoid"))

# calculate new count for nh asian without south asian
asian_wo_soasian<-asian_wo_soasian%>%
  mutate(new_count=(count-soasian_count))

# calculate new moe for nh asian without south asian
asian_wo_soasian<-asian_wo_soasian%>%
  rowwise()%>%
  mutate(new_moe=moe_sum(estimate=c(count,soasian_count), 
                         moe=c(moe,soasian_moe)))

# clean up
asian_wo_soasian<-asian_wo_soasian%>%
  mutate(variable="nh_asian_wo_sa")%>%
  select(geolevel,name,geoid,variable,new_count,new_moe)%>%
  rename(count=new_count,
         moe=new_moe)

##### add in percentages out of total and cvs -----
df_final<-rbind(av_race,asian_wo_soasian)%>%
  left_join(av_race%>%
              select('geoid','name','variable','count')%>%
              rename('total'='variable')%>%rename('population'='count')%>%
              filter(total=='total'),
            by=c('geoid','name'))%>%
  mutate(rate=count/population,
         cv=moe/1.645/count*100)%>%
  select('geoid','name','variable','count','moe','rate','cv')

# Remove SWANA since we don't need it 
df_final<-df_final%>%
  rename('race'='variable')%>%
  filter(race!='swana') # don't need swana for ripa estimates just sswana

# Aggregate tracts up to Antelope Valley level --------------------------------

df_final <- df_final %>%
  group_by(race) %>%
  summarise(
    count = sum(count),
    moe = sqrt(sum(moe^2)),        # Aggregate MOE using square root of sum of squares
    .groups = "drop"
  ) %>%
  mutate(
    total = count[race == "total"],
    rate = count / total,
    cv = moe / (1.645 * count),   # Recalculate CV from new MOE and count
    geography = "Antelope Valley"
  ) %>%
  select(geography, race, total, count, rate, moe, cv)%>%
  filter(race!="total")

# Push table to postgres----------------------------

# set field types for postgresql db
charvect = rep('varchar', ncol(df_final)) #create vector that is "numeric" for the number of columns in df

charvect <- replace(charvect, c(3,4,5,6,7), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(df_final)

table_name <- "av_population_race"
schema <- 'data'
indicator <- "2023 ACS Race estimates and rates for Antelope Valley"
source <- "AV to census tract xwalk from the Region 5 State of the Child report. Race data from ACS
imported to rda_shared_data."

dbWriteTable(con, Id(schema, table_name), df_final,
             overwrite = FALSE, row.names = FALSE)

qa_filepath <- "W:\\Project\\RJS\\CTC\\Documentation\\QA_av_race_pop.docx" 

# comment on table and columns
column_names <- colnames(df_final)
column_comments <- c('geo level',
                     'race',
                     'total AV population',
                     'count based on race group',
                     'rate of race groups',
                     'MOE',
                     'CV')

# add comment on table and columns using add_table_comments() (accessed via credentials script) 
add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments) 

