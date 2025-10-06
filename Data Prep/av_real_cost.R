# Calculate AV (SPA 1) and get LA County real cost measure data to Postgres

library(RPostgres)
library(tidyverse)
library(data.table)

# Connect to postgres 
source("W:\\RDA Team\\R\\credentials_source.R")
con_ctc <- connect_to_db("cancel_the_contract")
con_rda_shared <- connect_to_db("rda_shared_data")
con_bv <- connect_to_db("bold_vision")

# get LA County and PUMA data
la_county <- dbGetQuery(con_rda_shared, "SELECT * FROM economic.uw_2025_county_state_real_cost_measure")
pumas <- dbGetQuery(con_rda_shared, "SELECT * FROM economic.uw_2025_puma_real_cost_measure")

# get PUMAs that are in SPA 1
puma_spa_xwalk <- dbGetQuery(con_bv, "SELECT * FROM bv_2023.crosswalk_puma_spas_2023")
av_spas <- puma_spa_xwalk %>% filter(spa_id == "1") %>% rename(geoname = puma_name)

# select pumas in the AV. Note I use geoname because the Castaic geoid does not match, I'm guessing due to vintage
# These are the thre PUMAs we want though: "Los Angeles County (North Central)--Lancaster City", 
# "Los Angeles County (North Central)--Palmdale City", and "Los Angeles County (North/Unincorporated)--Castaic"
av_subset <- av_spas %>% left_join(pumas, by = "geoname") %>%
  
  # select fields I want
  select(geoname, num_hh, num_hh_below_rcm, num_hh_above_rcm) %>%
  
  # group for AV
  summarize(geoname = "SPA 1",
            num_hh = sum(num_hh),
            num_hh_below_rcm = sum(num_hh_below_rcm),
            num_hh_above_rcm = sum(num_hh_above_rcm))

# select matching fields for LA County
la_county_subset <- la_county %>% filter(geoname == "Los Angeles County, California") %>% 
  select(geoname, num_hh, num_hh_below_rcm, num_hh_above_rcm)

# combine AV and County and calculate percentages
df <- rbind(av_subset, la_county_subset) %>% 
  mutate(pct_hh_below_rcm = num_hh_below_rcm / num_hh,
         pct_hh_above_rcm = num_hh_above_rcm / num_hh)


# Finalize and push table to postgres --------------------------------
# set field types for postgresql db
charvect = rep('varchar', ncol(df)) #create vector that is "numeric" for the number of columns in df
charvect <- replace(charvect, c(2:6), c("numeric"))

# add df colnames to the character vector
names(charvect) <- colnames(df)

table_name <- "av_real_cost_measure"
schema <- 'data'
indicator <- "Numbers and percentages of households below the Real Cost Measure for Antelope Valley and LA County"
source <- "United Ways of California 2025"

dbWriteTable(con_ctc, Id(schema, table_name), df,
             overwrite = TRUE, row.names = FALSE)

qa_filepath <- "W:\\Project\\RJS\\CTC\\Documentation\\QA_analysis_real_cost.docx" 

# comment on table and columns
column_names <- colnames(df)
column_comments <- c('geography',
                     'number of households below the real cost measure of living',
                     'number of households above the real cost measure of living',
                     'percentage of households below the real cost measure of living',
                     'percentage of households above the real cost measure of living')

# add comment on table and columns using add_table_comments() (accessed via credentials script) 
add_table_comments(con_ctc, schema, table_name, indicator, source, qa_filepath, column_names, column_comments) 

# Disconnect from Postgres
dbDisconnect(con_ctc)
dbDisconnect(con_rda_shared)
dbDisconnect(con_bv)