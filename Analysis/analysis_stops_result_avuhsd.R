# AVUHSD stops by stop result


############### SET UP ########################

#install packages if not already installed
list.of.packages <- c("tidyverse","janitor", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(janitor)
library(data.table)

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con_shared <- connect_to_db("rda_shared_data")
con<- connect_to_db("cancel_the_contract")


############### GET DATA ########################

result <- dbGetQuery(con, "SELECT * FROM rel_school_result")
lasd_persons <- dbGetQuery(con_shared, "SELECT * FROM crime_and_justice.lasd_stops_person_2018_2023")

# Join result table with persons table to get race

df<-result%>%
  left_join(lasd_persons%>%select(contact_id, person_id, ))