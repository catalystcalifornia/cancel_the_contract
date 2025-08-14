# Send AV (SPA 1) and LA County infant mortality data to Postgres

library(RPostgres)
library(tidyverse)

# Connect to postgres 

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("cancel_the_contract")

# Create an empty dataframe with specified column names and types

empty_df <- data.frame(
  geography = character(),
  race = character(),
  rate = numeric(),
  year = character()
)

# add AV = SPA 1 Black & White rows from page 10 of https://www.lacare.org/sites/default/files/la6729_birth_health_equity_20250308.pdf

# Create a new row as a data frame or vector (ensure column names/order match)
new_row1 <- data.frame(geography = "SPA 1", race = "Black", rate = "13.2", year = "2020-2022")
new_row2 <- data.frame(geography = "SPA 1", race = "White", rate = "4.5", year = "2020-2022")

# Add the new row
df <- rbind(empty_df, new_row1, new_row2)

# add AV = SPA 1 and LA County Total rows from theme 8 -> Infant_D_m of this downloaded file: https://ph-lacounty.hub.arcgis.com/datasets/b2d4d3c03f114440af6e3088ee612328/about
new_row3 <- data.frame(geography = "SPA 1", race = "Total", rate = "5.7", year = "2018-2022")
new_row4 <- data.frame(geography = "LA County", race = "Total", rate = "3.9", year = "2018-2022")

# Add the new rows
df <- rbind(df, new_row3, new_row4)

# add LA County Black and White infant mortality from page 8 of https://www.lacare.org/sites/default/files/la6729_birth_health_equity_20250308.pdf
new_row5 <- data.frame(geography = "LA County", race = "Black", rate = "8.4", year = "2020-2022")
new_row6 <- data.frame(geography = "LA County", race = "White", rate = "2.9", year = "2020-2022")

# Add the new rows
df_final <- rbind(df, new_row5, new_row6)

print(df_final)

# Finalize and push table to postgres --------------------------------
# set field types for postgresql db
charvect = rep('varchar', ncol(df_final)) #create vector that is "numeric" for the number of columns in df

charvect <- replace(charvect, c(3), c("numeric"))

# add df colnames to the character vector

names(charvect) <- colnames(df_final)

table_name <- "av_infant_mortality_by_race"
schema <- 'data'
indicator <- "Black, White, and total infant mortality rates for Antelope Valley and LA County"
source <- "LA County Department of Public Health"

dbWriteTable(con, Id(schema, table_name), df_final,
             overwrite = TRUE, row.names = FALSE)

qa_filepath <- "W:\\Project\\RJS\\CTC\\Documentation\\QA_analysis_infant_mortality.docx" 

# comment on table and columns
column_names <- colnames(df_final)
column_comments <- c('geography',
                     'race',
                     'infant deaths per 1,000 births',
                     'data years')

# add comment on table and columns using add_table_comments() (accessed via credentials script) 
add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments) 

# Disconnect from Postgres
dbDisconnect(con)