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
con_shared <- connect_to_db("rda_shared_data")
con<- connect_to_db("cancel_the_contract")

############### PREP DATA ########################

#Get HS Grad, handle nas, ensure DistrictCode reads in right
# Data Dictionary: https://www.cde.ca.gov/ds/ad/filesacgr.asp

df<-dbGetQuery(con_shared, "SELECT * FROM education.cde_multigeo_calpads_graduation_2023_24")

#dplyr::filter for county and state rows, all types of schools, and racial categories
df_subset <- df %>% filter(aggregatelevel %in% c("D") & countycode == "19" & charterschool == "All" & dass == "All") %>%
  # filter for Antelope Valley Union High
  filter(districtcode=="64246") %>%
  
  #select just the fields we need
  select(districtname, reportingcategory, cohortstudents, regularhsdiplomagraduatescount, regularhsdiplomagraduatesrate)
# View(df_subset)

# recode reporting categories

df_subset<-df_subset%>%
  mutate(geography="Antelope Valley Union High School District")%>%
  mutate(reportingcategory_re=ifelse(reportingcategory %in% "RB", "nh_black",
                                              ifelse(reportingcategory %in% "RI", "nh_aian", 
                                                     ifelse(reportingcategory %in% "RA", "nh_asian", 
                                                            ifelse(reportingcategory %in% "RF", "nh_filipino",  
                                                                   ifelse(reportingcategory %in% "RH", "latinx", 
                                                                          ifelse(reportingcategory %in% "RD", "missing_race",
                                                                                 ifelse(reportingcategory %in% "RP", "nh_nhpi",  
                                                                                        ifelse(reportingcategory %in% "RT", "nh_twoormor",  
                                                                                               ifelse(reportingcategory %in% "RW", "nh_white", 
                                                                                                      ifelse(reportingcategory %in% "GM", "male", 
                                                                                                             ifelse(reportingcategory %in% "GF", "female",
                                                                                                                    ifelse(reportingcategory %in% "GX", "nonbinary",
                                                                                                                           ifelse(reportingcategory %in% "GZ", "missing_gender",
                                                                                                                                  ifelse(reportingcategory %in% "SE", "student_english_learner",
                                                                                                                                         ifelse(reportingcategory %in% "SD", "student__disability",
                                                                                                                                                ifelse(reportingcategory %in% "SS", "student_ses_disadv",
                                                                                                                                                       ifelse(reportingcategory %in% "SM", "student_migrant",  
                                                                                                                                                              ifelse(reportingcategory %in% "SF", "student_foster",
                                                                                                                                                                  ifelse(reportingcategory %in% "SH", "student_homeless", 
                                                                                                                                                                            "total"))))))))))))))))))))

# Continue cleaning up df columns

df_subset <- df_subset %>% 
 filter(!is.na(df_subset$regularhsdiplomagraduatescount) & !is.na(df_subset$cohortstudents) & !is.na(df_subset$regularhsdiplomagraduatesrate))

#format for column headers
df_subset <- rename(df_subset,
                    graduation_count = regularhsdiplomagraduatescount,
                    enrollment_total = cohortstudents,
                    graduation_rate = regularhsdiplomagraduatesrate)
# #View(df_subset)


#pop screen. suppress raw/rate for groups with fewer than 0 graduating students.
threshold = 0
df_subset <- df_subset %>%
  mutate(graduation_rate = ifelse(graduation_count < threshold, NA, graduation_rate), 
         graduation_count = ifelse(graduation_count < threshold, NA, graduation_count))


# Add label column that has spelled out versions of the student group categories

rep_str = c('RB' = 'Black',
            'RI' = 'AIAN',
            'RA' = 'Asian',
            'RF' = 'Filipinx',
            'RH' = 'Latinx',
            'RD' = 'Not Reported',
            'RP' = 'Pacific Islander',
            'RT' = 'Multiracial',
            'RW' = 'White',
            'GM' = 'Male',
            'GF' = 'Female',
            'GX' = 'Non-Binary Gender',
            'GZ' = 'Missing Gender',
            'SE' = 'English Learners',
            'SD' = 'Students with Disabilities',
            'SS' = 'Socioeconomically Disadvantaged',
            'SM' = 'Migrant',
            'SF' = 'Foster',
            'SH' = 'Homeless',
            'TA' = 'Total')

