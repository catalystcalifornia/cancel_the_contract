# Get/calculate suspension rates by race for Antelope Valley Union High School District
# borrow code from W:/Project/RDA Team/Region 5 State of the Child/GitHub/CR/State_of_the_Child_Region_5/R_Scripts/suspension.R

# set up

library(RPostgres)
library(tidyverse)
library(data.table)

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")

con_shared <- connect_to_db("rda_shared_data")
con<- connect_to_db("cancel_the_contract")

suspensions<-dbGetQuery(con_shared, "SELECT * FROM education.cde_multigeo_suspension_2023_24")

dbDisconnect(con)


# calculate suspension rates by student group, including race, for Antelope Valley Union High School District

# filter for District level enrollment and unduplicated suspensions in LA County (for charters and non-charters)
la_suspensions <- suspensions %>% dplyr::filter(aggregatelevel=="D" & countyname=="Los Angeles" & charteryn=="All") %>%
  dplyr::select(districtname, reportingcategory, cumulative_enrollment, unduplicated_countof_students_suspended_total)

# in Antelope Valley Union High School District
av_suspensions <- dplyr::filter(suspensions, districtname %in%  c('Antelope Valley Union High'))

# remove suppression asterisks
av_suspensions$cumulative_enrollment <- stringr::str_replace(av_suspensions$cumulative_enrollment, '\\*', '')
av_suspensions$unduplicated_countof_students_suspended_total <- stringr::str_replace(av_suspensions$unduplicated_countof_students_suspended_total, '\\*', '')

# change column type
av_suspensions$cumulative_enrollment <- as.numeric(av_suspensions$cumulative_enrollment)
av_suspensions$unduplicated_countof_students_suspended_total <- as.numeric(av_suspensions$unduplicated_countof_students_suspended_total)

# group by student group and summarize
susp_table <- av_suspensions %>% group_by(reportingcategory) %>%
  dplyr::summarize(enrollment = sum(cumulative_enrollment, na.rm = TRUE),
                   suspensions = sum(unduplicated_countof_students_suspended_total, na.rm = TRUE))  %>%
  
  # and remove categories with no cumulative enrollment
  filter(enrollment > 0)

#calculate suspension rates
susp_table$rate <- round(susp_table$suspensions/susp_table$enrollment*100, 1)

#order rate descending
susp_table <- susp_table %>% arrange((desc(rate)))

# Format column and category names for postgres table---------------------

# Replace multiple strings at a time
rep_str = c('RB' = 'African American',
            'RI' = 'American Indian or Alaska Native',
            'RA' = 'Asian',
            'RF' = 'Filipino',
            'RH' = 'Hispanic or Latino',
            'RD' = 'Not Reported',
            'RP' = 'Pacific Islander',
            'RT' = 'Two or More Races',
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

df<-susp_table%>%
  mutate(label=reportingcategory,
         geography="Antelope Valley Union High School District")

df$label <- str_replace_all(df$label, rep_str) # make the spelled out categories into a label column

df<-df%>%
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
                                       "total"))))))))))))))))))))%>%
  select(geography, reportingcategory, reportingcategory_re, enrollment, suspensions, rate, label)
    


# Push table to postgres--------------------------

table_name <- "analysis_suspensions"
schema <- "data"
indicator <- "Suspension rates by race/CDE reporting cateogry for AV Union High School District level using 2023-24 academic year data"
source <- "R Script: W:\\Project\\RJS\\CTC\\Github\\JZ\\cancel_the_contract\\Analysis\\suspension.R"
qa_filepath <- " W:\\Project\\RJS\\CTC\\Documentation\\QA_Sheet_Suspensions.docx" 
table_comment <- paste0(indicator, source)

dbWriteTable(con, Id(schema, table_name), df, overwrite = TRUE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(df) # Get column names
column_comments <- c(
  "geography level",
  "CDE reporting category raw",
  "CDE reporting category recoded",
  "cumulative total enrollment",
  "cumulative total suspensions -students suspendid counted only once",
  "rate of suspensions",
  "label for reporting categories"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
