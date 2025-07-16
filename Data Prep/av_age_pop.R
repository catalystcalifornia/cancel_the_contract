#### Calculate population estimates by age for Antelope Valley #### 

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

# age table
age<- dbGetQuery(con_shared, "SELECT geoid, name, 
        s0101_c01_001e,  s0101_c01_001m,
         s0101_c01_022e, s0101_c01_022m,
         s0101_c01_023e, s0101_c01_023m,
         s0101_c01_007e, s0101_c01_007m,
         s0101_c01_008e ,s0101_c01_008m,
         s0101_c01_009e, s0101_c01_009m,
         s0101_c01_010e, s0101_c01_010m,
         s0101_c01_011e, s0101_c01_011m,
         s0101_c01_012e, s0101_c01_012m,
         s0101_c01_013e, s0101_c01_013m,
         s0101_c01_014e, s0101_c01_014m, 
         s0101_c01_030e, s0101_c01_030m
                FROM demographics.acs_5yr_s0101_multigeo_2023 WHERE geolevel='tract'")

# grab SPA-tract xwalk so that we can get all tracts within SPA 1 which we are using as our AV geography

spa<-dbGetQuery( con_shared, "SELECT * FROM crosswalks.lacounty_health_district_spa_tract_2022")%>%
  filter(spa_num==1)

# pull out the tracts for spa 1

ct_av<-spa$ct_geoid

# filter age data to be only for spa 1 tracts

age_av<-age%>%
  filter(geoid %in% ct_av)

# Rename columns, then create additional age bracket groupings via aggregating for age 25-34, 35-44, and 45-54 

age_av<-age_av%>%
  rename("total"="s0101_c01_001e",
         "total_moe"="s0101_c01_001m",
         "age0_17"="s0101_c01_022e",
         "age0_17_moe"="s0101_c01_022m",
         "age18_24"="s0101_c01_023e",
         "age18_24_moe"="s0101_c01_023m",
         "age25_29"= "s0101_c01_007e",
         "age25_29_moe"="s0101_c01_007m",
         "age30_34" ="s0101_c01_008e",
         "age30_34_moe"="s0101_c01_008m",
         "age35_39"="s0101_c01_009e",
         "age35_39_moe"="s0101_c01_009m",
         "age40_44"="s0101_c01_010e",
         "age40_44_moe"="s0101_c01_010m",
         "age45_49"= "s0101_c01_011e",
         "age45_49_moe"="s0101_c01_011m",
         "age50_54"= "s0101_c01_012e",
         "age50_54_moe"="s0101_c01_012m",
         "age55_59"= "s0101_c01_013e",
         "age55_59_moe"="s0101_c01_013m",
         "age60_64"= "s0101_c01_014e",
         "age60_64_moe"= "s0101_c01_014m",
         "age65over"= "s0101_c01_030e",
         "age65over_moe"="s0101_c01_030m"
  )%>%
  mutate(age25_34=age25_29+age30_34,
         age25_34_moe=sqrt(age25_29_moe^2 + age30_34_moe^2),
         age35_44=age35_39+age40_44,
         age35_44_moe=sqrt(age35_39_moe^2 + age40_44_moe^2),
         age45_54=age45_49+age50_54,
         age45_54_moe=sqrt(age45_49_moe^2 + age50_54_moe^2),
         age55_64=age55_59+age60_64,
         age55_64_moe=sqrt(age55_59_moe^2 + age60_64_moe^2)
  )%>%
  select(name, geoid, total, total_moe, age0_17, age0_17_moe, age18_24, age18_24_moe,
         age25_34, age25_34_moe, age35_44, age35_44_moe, age45_54, age45_54_moe, 
         age55_64, age55_64_moe, age65over,   age65over_moe)


## pivot table to long format

df_e<-age_av %>%
  pivot_longer(3:18, names_to = "variable", values_to = "age")%>%
  filter(!grepl("moe", variable))

# QA check that all age categories were grabbed
unique(df_e$variable)

df_m<-age_av %>%
  pivot_longer(3:18, names_to = "variable", values_to = "moe")%>%
  filter(grepl("moe", variable))
# QA check that all age categories were grabbed
unique(df_m$variable)

df_long<-cbind(df_e, df_m)

# QA check that the cbind is okay to do, given that the geoid and name columns are the same
all(df_long$geoid==df_long$geoid.1)
all(df_long$name==df_long$name.1)
# QA check that the age category matches the right moe age category
all(mapply(function(x, y) str_detect(x, fixed(y)), df_long$variable.1, df_long$variable))

df_long<-df_long%>%
  select(1:4, 8)%>%
  rename("count"="age")

# recode age variables
df_long<-df_long%>%
  mutate(age_re=ifelse(variable %in% "total", "Total",
                       ifelse(variable %in% "age0_17", "17 and under",
                              ifelse(variable %in% "age18_24", "18-24",
                                     ifelse(variable %in% "age25_34", "25-34",
                                            ifelse(variable %in% "age35_44", "35-44",
                                                   ifelse(variable %in% "age45_54", "45-54",
                                                          ifelse(variable %in% "age55_64", "55-64",
                                                                 ifelse(variable %in% "age65over", "65 and older",
                                                                        "NULL"
                                                                 )))))))))%>%
  filter(age_re != "NULL")


# set order of age_re column manually

x <- c("17 and under", "18-24", "25-34","35-44","45-54","55-64","65 and older","Total")

df_long<-df_long%>%
  mutate(age_re =  factor(age_re, levels = x)) %>%
  arrange(age_re)%>%
  select(-c(name))%>%
  select(geoid, age_re, count, moe)

# Calculate rates for each age bracket and aggregate up so it is for entire AV -------------------------
 
# create a column for the total value for each tract

# First extract totals
totals <- df_long %>%
  filter(age_re == "Total") %>%
  select(geoid, total = count)

# Join back to original data
df_long <- df_long %>%
  left_join(totals, by = "geoid")%>%
  ungroup()

# Aggregate counts and totals and rates to the AV level

df_final<-df_long %>%
group_by(age_re) %>%
  summarise(
    count = sum(count),
    moe = sqrt(sum(moe^2)),        # Aggregate MOE using square root of sum of squares
    .groups = "drop"
  ) %>%
  mutate(
    total = count[age_re == "Total"],
    rate = count / total*100,
    cv = moe / (1.645 * count),   # Recalculate CV from new MOE and count
    geography = "Antelope Valley"
  ) %>%
  select(geography, age_re, total, count, rate, moe, cv)%>%
  filter(age_re!="Total")

# QA check that AV's population totals in df_final matches population totals from age_av
qa <- age_av %>%
  summarise(total_av_pop = sum(total),
            total_17under = sum(age0_17),
            total_55_64 = sum(age55_64))


# Finalize and push table to postgres --------------------------------
# set field types for postgresql db
charvect = rep('varchar', ncol(df_final)) #create vector that is "numeric" for the number of columns in df

charvect <- replace(charvect, c(3,4,5,6,7), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(df_final)

table_name <- "av_population_age"
schema <- 'data'
indicator <- "2023 ACS Age estimates and rates for Antelope Valley"
source <- "SPA to tract xwalk filtering for SPA 1."

 dbWriteTable(con, Id(schema, table_name), df_final,
              overwrite = TRUE, row.names = FALSE)

qa_filepath <- "W:\\Project\\RJS\\CTC\\Documentation\\QA_av_age_pop.docx" 

# comment on table and columns
column_names <- colnames(df_final)
column_comments <- c('geo level',
                     'age bracket',
                     'total AV population',
                     'count based on age group',
                     'rate of age groups',
                     'MOE',
                     'CV')

# add comment on table and columns using add_table_comments() (accessed via credentials script) 
add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments) 
