
library(tidyverse)
library(RPostgres)
library(readxl)
library(stringr)
library(DBI)

# ---- Connect to Postgres ----
source("W:\\RDA Team\\R\\credentials_source.R")
con_ctc <- connect_to_db("cancel_the_contract")

# ---- Import and filter data ----
root <- "W:/Data/Health/Deaths/Downloaded CDPH Tables/Community_Health_Profiles_-_Data_Download"
my_data <- read_excel(paste0(root, "/Excel CHP Theme 3 - Physical Activity and Nutrition - Metadata and Data.xlsx"), sheet = 2)

my_data_filtered <- my_data %>%
  filter(Geography_Name %in% c(
    "Los Angeles County",
    "SPA 1: Antelope Valley",
    "City of Lancaster",
    "City of Palmdale"
  )) %>%
  select(
    geo_id = Geo_ID,
    geo_feature = Geo_Feature,
    geography_name = Geography_Name,
    geography_type = Geography_Type,
    percent_children_good_access = Comm_fruitveg,
    lower_ci = Comm_fruitveg_LCL,
    upper_ci = Comm_fruitveg_UCL,
    rse = Comm_fruitveg_RSE,
    method = Comm_fruitveg_EST
  ) %>%
  mutate(across(where(is.character), str_trim))

# ---- Define Postgres field types ----
charvect <- rep("varchar", ncol(my_data_filtered))
charvect[5:8] <- "numeric"
names(charvect) <- colnames(my_data_filtered)

# ---- Table metadata ----
table_name <- "analysis_healthy_food"
schema <- "data"
indicator <- "Percentage of Children with Good or Excellent Community Access to Fresh Fruits and Vegetables"
source_info <- "R script: W:/Project/RJS/CTC/Github/CR/cancel_the_contract/Analysis/analysis_healthy_food.R"
qa_filepath <- "W://Project//RJS//CTC//Documentation//QA_analysis_healthy_food.docx"

# ---- Export to Postgres ----
dbWriteTable(con_ctc, table_name, my_data_filtered, overwrite = TRUE, row.names = FALSE, field.types = charvect)

# ---- Add table/column metadata ----
column_comments <- c(
  "GEOID",
  "Geography Feature",
  "Geography Name",
  "Geography Type",
  "Percentage of Children with Good or Excellent Community Access to Fresh Fruits and Vegetables",
  "Lower 95% Confidence Interval",
  "Upper 95% Confidence Interval",
  "Relative Standard Error",
  "Estimation Method"
)
add_table_comments(con_ctc, schema, table_name, indicator, source_info, qa_filepath, colnames(my_data_filtered), column_comments)

# ---- Verify table in Postgres ----
check_table <- dbGetQuery(con_ctc, paste0("SELECT * FROM ", schema, ".", table_name, " ORDER BY geography_name"))
print(check_table)

# ---- Disconnect ----
dbDisconnect(con_ctc)


