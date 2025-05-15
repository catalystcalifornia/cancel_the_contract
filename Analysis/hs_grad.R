# High School Graduation Rates 2023-24 School Year

#install packages if not already installed
list.of.packages <- c("readr","tidyr","dplyr","janitor")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(readr)
library(tidyr)
library(dplyr)
library(janitor)

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")

############### PREP DATA ########################

#Get HS Grad, handle nas, ensure DistrictCode reads in right
# Data Dictionary: https://www.cde.ca.gov/ds/ad/filesacgr.asp

df<-dbGetQuery(con, "SELECT * FROM education.cde_multigeo_calpads_graduation_2023_24")

dbDisconnect(con)

#dplyr::filter for county and state rows, all types of schools, and racial categories
df_subset <- df %>% dplyr::filter(aggregatelevel %in% c("D") & countycode == "19" & charterschool == "All" & dass == "All" & 
                                    reportingcategory %in% c("TA", "RB", "RI", "RA", "RF", "RH", "RP", "RT", "RW","GM", # no data for non-binary "GX"
                                                             "GF","SE","SD","SS","SM","SF","SH" )) %>%
  # filter for Antelope Valley Union High
  dplyr::filter(districtcode=="64246") %>%
  
  #select just the fields we need
  dplyr::select(districtname, reportingcategory, cohortstudents, regularhsdiplomagraduatescount, regularhsdiplomagraduatesrate)
# View(df_subset)

df_subset$reportingcategory <- gsub("TA", "total", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RB", "nh_black", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RI", "nh_aian", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RA", "nh_asian", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RF", "nh_filipino", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RH", "latino", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RP", "nh_pacisl", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RT", "nh_twoormor", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RW", "nh_white", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("SE", "ell", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("SD", "disabilities", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("SS", "socioeconomically_disadvantaged", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("SM", "migrant", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("SF", "foster", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("SH", "homeless", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("GM", "male", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("GF", "female", df_subset$reportingcategory)

df_subset <- rename(df_subset,c("geoname" = "districtname")) %>% 
  dplyr::filter(!is.na(df_subset$regularhsdiplomagraduatescount) & !is.na(df_subset$cohortstudents) & !is.na(df_subset$regularhsdiplomagraduatesrate) )

#format for column headers
df_subset <- rename(df_subset,
                    raw = regularhsdiplomagraduatescount,
                    pop = cohortstudents,
                    rate = regularhsdiplomagraduatesrate)
# #View(df_subset)


#pop screen. suppress raw/rate for groups with fewer than 0 graduating students.
threshold = 0
df_subset <- df_subset %>%
  mutate(rate = ifelse(raw < threshold, NA, rate), raw = ifelse(raw < threshold, NA, raw))

#pivot
df_wide <- df_subset %>% pivot_wider(names_from = reportingcategory, names_glue = "{reportingcategory}_{.value}", 
                                     values_from = c(raw, pop, rate))

# change the data type to numeric if it is not numeric
df_wide <- df_wide %>% mutate(across(ends_with("_rate") | ends_with("_raw") | ends_with("_pop"), as.numeric))


av_table1 <- df_wide%>% dplyr::select(geoname, ends_with("_rate"))  %>% 
  mutate_at(vars(-geoname), funs(round(., 1)))%>% janitor::remove_empty()%>% dplyr::rename("Total"="total_rate", "Asian"="nh_asian_rate",
                                                                                           "Black"="nh_black_rate","Filipino"="nh_filipino_rate","Latinx"="latino_rate",
                                                                                           "Two or More Races"="nh_twoormor_rate","White"="nh_white_rate","AIAN"="nh_aian_rate", 
                                                                                           "Disability"="disabilities_rate", "Socioeconomically Disadvantaged"="socioeconomically_disadvantaged_rate", 
                                                                                           "Migrant"="migrant_rate", "Foster"="foster_rate", "ELL"="ell_rate", "Homeless"="homeless_rate", 
                                                                                           "Female" = "female_rate", "Male" = "male_rate")
av_table1$geoname <- 'Total Rate'

av_table2 <- df_wide%>% dplyr::select(geoname, ends_with("_raw"))  %>% 
  mutate_at(vars(-geoname), funs(round(., 0)))%>% janitor::remove_empty()%>% dplyr::rename("Total"="total_raw", "Asian"="nh_asian_raw",
                                                                                           "Black"="nh_black_raw","Filipino"="nh_filipino_raw","Latinx"="latino_raw",
                                                                                           "Two or More Races"="nh_twoormor_raw","White"="nh_white_raw", "AIAN"="nh_aian_raw",
                                                                                           "Disability"="disabilities_raw", "Socioeconomically Disadvantaged"="socioeconomically_disadvantaged_raw", 
                                                                                           "Migrant"="migrant_raw", "Foster"="foster_raw", "ELL"="ell_raw", "Homeless"="homeless_raw", 
                                                                                           "Female" = "female_raw", "Male" = "male_raw")

av_table2$geoname <- 'Total Count'

av_table <- rbind(av_table1, av_table2)%>% 
  dplyr::rename("Adjusted 5-Year 2023-2024"="geoname")
# View(av_table)


