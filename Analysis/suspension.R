# Get/calculate suspension rates by race for Antelope Valley Union High School District
# borrow code from W:/Project/RDA Team/Region 5 State of the Child/GitHub/CR/State_of_the_Child_Region_5/R_Scripts/suspension.R

# set up

library(RPostgres)
library(tidyverse)
library(data.table)

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("rda_shared_data")

suspensions<-dbGetQuery(con, "SELECT * FROM education.cde_multigeo_suspension_2023_24")

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

# format category names

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
susp_table$reportingcategory <- str_replace_all(susp_table$reportingcategory, rep_str)

names(susp_table) <- c("Student group", "Student enrollment", "Unduplicated count of students suspended", "Suspension rate")
View(susp_table)