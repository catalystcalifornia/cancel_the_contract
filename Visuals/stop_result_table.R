# Visualize top stop results

library(dplyr)

#connect to postgres
# source("W:\\RDA Team\\R\\credentials_source.R")
# con<- connect_to_db("cancel_the_contract")

# connect to function script
source("./Visuals/visual_fx.R")

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con_ctc <- connect_to_db("cancel_the_contract")


#load in data
 
df<-dbGetQuery(con_ctc, "SELECT * FROM analysis_stops_result_avuhsd")%>%
  mutate(rate=as.numeric(rate))%>%
  rename("label"="stop_result")%>%
  select(-geography)%>%
  mutate(label = str_to_title(label))

# # NOTE: The indicator field needs to match the way it is in the data dictionary indicator_short column
# ## i.e.) for suspensions by race, I need to set indicator_short== "Suspensions by race"
# 
 indicator="Police Stops by Result of Stop AVUHSD"
 title_text="Police Mainly Issue Citations or Rely on Schools and Guardians During Stops"
# 
# # Apply function
# 
  static_table(df=df,
              indicator=indicator,
              group_col="Stop Result",
           title_text=title_text)
