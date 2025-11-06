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

df<-dbGetQuery(con_ctc, "SELECT * FROM analysis_stops_reason_rs_codes_avuhsd")%>%
  rename("label"="statute_literal_25")%>%
  select(-1,-2,-3,-5)%>%
  mutate(label = str_to_title(label))


  # create the new "Marijuana related stop reasons" row
df_combined<-df%>%
  filter(!str_detect(label, regex("marij", ignore_case = TRUE)))%>% 
  filter(!str_detect(label, regex("fight", ignore_case = TRUE)))%>% 
  
  bind_rows(
    df %>%
      filter(str_detect(label, regex("marij", ignore_case = TRUE)))%>% 
    summarise(
        label = "Marijuana related stop reasons",
        count = sum(count, na.rm = TRUE),
        total = first(total)  # assuming total = 904 for all rows
      ) %>%
      mutate(rate = count / total * 100)
  )%>%
  
  # Create a combined row for the 'fight' stop reasons
  bind_rows(
    df %>%
      filter(str_detect(label, regex("fight", ignore_case = TRUE)))%>% 
      summarise(
        label = "Fighting related stop reasons",
        count = sum(count, na.rm = TRUE),
        total = first(total)  # assuming total = 904 for all rows
      ) %>%
      mutate(rate = count / total * 100)
  )%>%
  arrange(-rate)%>%
  slice(1:9)

# # NOTE: The indicator field needs to match the way it is in the data dictionary indicator_short column
# ## i.e.) for suspensions by race, I need to set indicator_short== "Suspensions by race"
# 
indicator="Police Stops by Reason of Stop AVUHSD"
title_text="Police Mainly Stop Students for Fighting or Marijuana Despite Limited Effectiveness"
# 
# # Apply function
# 
static_table(df=df_combined,
             indicator=indicator,
             group_col="Stop Reason",
             title_text=title_text)
