# SPA 1 households paying 30% or more of their income on rent
# Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2018-2022.pdf

library(tidyverse)
library(data.table)
library(readxl)
library(tidycensus)
library(srvyr)
library(stringr)

#SOURCE from the script that has: styling, packages, dbconnection, colors
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("f5la_v2")
con_ctc <- connect_to_db("cancel_the_contract")

# Function to output cost burden data for year I enter
cost_burden_race_function <- function(year, xwalk) {
  
  
  
  #### Step 1 load the data ####
  
  # PUMS Data
  root <- "W:/Data/Demographics/PUMS/"
  indicator_name <- "rent_burden"
  data_type <- "race" #change the applicable one: race, spa, county
  
  # Load the people PUMS data
  people <- fread(paste0(root, "CA_", year, "/psam_p06.csv"), header = TRUE, data.table = FALSE,
                  colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH")))
  
  
  # Load the housing PUMS data
  housing <- fread(paste0(root, "CA_", year, "/psam_h06.csv"), header = TRUE, data.table = FALSE,
                   colClasses = list(character = c("PUMA")))
  
  
  #### Step 2 filter for LA & Recode racial-ethnic groups ####
  
  people <- people %>% filter(grepl('037', PUMA)) 
  housing <- housing %>% filter(grepl('037', PUMA))
  
  #SOURCE recode function
  source("W:\\Project\\RDA Team\\First5LA\\Ad-hoc Research\\Laura ask housing update\\ask_functions.R")
  people_recoded <- race_recode(people)
  
  
  ####  Step 3: filter LA County households eligible for rent-burden calculation  #### 
  
  ## Select LA County renter households (universe) by
  
  #joining people to households
  eligible_hhs <- people_recoded %>% left_join(housing %>% 
                                                 
                                                 #filtering for renter and LA county                               
                                                 filter(TEN == "3" & !is.na(GRPIP)), 
                                               
                                               by = c("SERIALNO", "PUMA")) %>%
    
    #remove records with no weights
    filter(!is.na(WGTP)) #%>%
    
    #filter for age 0-5 and select distinct households
    #filter(AGEP < 5) %>% distinct(SERIALNO, .keep_all = TRUE)
  
  
  #### Step 4: get PUMA SPA xwalk and join ####
  
  #get puma-spa xwalk
  conn <- connect_to_db("f5la_v2")
  puma_spa_xwalk <- st_read(conn, query = xwalk)
  dbDisconnect(conn)
  
  # join
  hh <- eligible_hhs %>% left_join(puma_spa_xwalk, by = "PUMA", relationship = "many-to-many") %>%
    
    # and filter for SPA 1
    filter(SPA == '1')
  
  
  #### Step 5: Set up surveys and calculate percentages by race/ethnicity
  
  # set threshold
  threshold = 30
  
  # survey design code
  
  # Define weight variable and population base which will be used in the survey design set up
  ## You must use WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected under5)
  weight <- 'WGTP' # using WGTP b/c calculating percentage of rent-burdened households
  
  repwlist = rep(paste0("WGTP", 1:80))
  
  # prep data and add in burdened indicator
  hh$geoid <- "037"
  
  hh<-hh%>%
    mutate(indicator=(ifelse(hh$GRPIP >= threshold, "burdened", "not burdened")))
  
  # create survey design
  
  hh_county <- hh %>%               
    as_survey_rep(
      variables = c(geoid, indicator, race, latino, aian, nhpi, swana),   # dplyr::select grouping variables
      weights = weight,                       #  weight
      repweights = repwlist,                  # list of replicate weights
      combined_weights = TRUE,                # tells the function that replicate weights are included in the data
      mse = TRUE,                             # tells the function to calc mse
      type="other",                           # statistical method
      scale=4/80,                             # scaling set by ACS
      rscale=rep(1,80)                        # setting specific to ACS-scaling
    )
  
  
  ###### Latino ######
  lat <- hh_county  %>%
    group_by(geoid,latino,indicator) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(hh_county %>%                                        # left join in the denominators
                group_by(geoid,latino) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count
  
  
  ###### NHPI ######
  nhpi <- hh_county  %>%
    group_by(geoid,nhpi,indicator) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(hh_county %>%                                        # left join in the denominators
                group_by(geoid,nhpi) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count
  
  
  ###### AIAN ######
  aian <- hh_county  %>%
    group_by(geoid,aian,indicator) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(hh_county %>%                                        # left join in the denominators
                group_by(geoid,aian) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count
  
  
  ###### SWANA ######
  swana <- hh_county  %>%
    group_by(geoid,swana,indicator) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(hh_county %>%                                        # left join in the denominators
                group_by(geoid,swana) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count
  
  
  ###### RACE ######
  race <- hh_county  %>%
    group_by(geoid,race,indicator) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(hh_county %>%                                        # left join in the denominators
                group_by(geoid,race) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count
  
  
  ###### TOTAL ######
  total <- hh_county  %>%
    group_by(geoid,indicator) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(hh_county %>%                                        # left join in the denominators
                group_by(geoid) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count
  
  
  ####  Step 5: format  ####
  
  # rename race name columns as subgroup
  total$subgroup = "Total"
  total <- total %>% select(geoid, subgroup, everything())
  
  aian <- aian %>% rename(subgroup = aian)
  lat <- lat %>% rename(subgroup = latino)
  nhpi <- nhpi %>% rename(subgroup = nhpi)
  race <- race %>% rename(subgroup = race)
  swana <- swana %>% rename(subgroup = swana)
  
  # merge tables except for bipoc - need total
  d_long <- rbind(total, aian, lat, race, nhpi, swana) %>%
    filter(indicator == "burdened" & 
             subgroup != "Not AIAN" &
             subgroup != "Not Latinx" &
             subgroup != "Not NHPI" &
             subgroup != "Not SWANA" &
             subgroup != "AIAN placeholder" &
             subgroup != "NHPI placeholder" &
             subgroup != "Latinx placeholder")
  
  d_long <- as.data.frame(d_long)
  d_long$geoid <- "SPA 1"
  d_long$year <- year
  d_long$universe <- "Renter households"
  d_long <- d_long %>% select(year, universe, geoid, everything())
  
  # write to one bigger csv with all data years
  # write.csv(d_long, file = paste0("./cost_burden.csv"))
  
  # for copy/pasting in template
  # final_wide <- data.frame()
  # final_wide <- d_long %>% select(subgroup, rate) %>% pivot_wider(names_from = subgroup, values_from = rate) %>%
  #  select('AIAN Alone or in Combination', 'Asian NL', 'Black NL', 'Latinx', 'NHPI Alone or in Combination', 'White NL', 'SWANA', 'Other NL', 'Two or More NL')
  
  return(d_long)
}


# run for 2014-2018 5-year estimates
# df_2014_2018 <- cost_burden_race_function("2014_2018", "SELECT * FROM puma_spa_xwalk_2019")

# run for 2019-2023 5-year estimates
df_2019_2023 <- cost_burden_race_function("2019_2023", "SELECT * FROM puma_spa_xwalk_2022")


############### PUSH TABLE TO POSTGRES #####################

# set column types

charvect = rep("varchar", ncol(df_2019_2023)) #create vector that is "varchar" for the number of columns in df
charvect <- replace(charvect, c(6:15), c("numeric"))

# add df colnames to the character vector
 
names(charvect) <- colnames(df_2019_2023)

table_name <- "analysis_rent_burden"
schema <- "data"
indicator <- "Rent Burden by Race, SPA 1 2019-2023"
source <- "R script: W://Project//RJS//CTC//Github//CR//cancel_the_contract//Analysis//analysis_rent_burden.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_analysis_rent_burden.docx"
table_comment <- paste0(indicator, source)
 
# push to postgres
dbWriteTable(con_ctc, table_name, df_2019_2023,
              overwrite = TRUE, row.names = FALSE,
              field.types = charvect)
 
# add meta data
 
column_names <- colnames(df_2019_2023) # Get column names
 column_comments <- c(
   "ACS PUMS vintage",
   "Universe",
   "Geography",
   "Total or Racial-ethnic group",
   "indicator",
   "Number (numerator)",
   "Number standard error",
   "Rate",
   "Rate standard error",
   "Population (denominator)",
   "Population standard error",
   "Rate margin of error",
   "Rate coefficient of variation",
   "Number margin of error",
   "Number coefficient of variation"
)
 
add_table_comments(con_ctc, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)


dbDisconnect(con_ctc)
dbDisconnect(con)

