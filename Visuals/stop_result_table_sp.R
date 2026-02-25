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

# reformat spanish data dictionary specifically for this visual

result_sp<-dict_sp%>%
  filter(indicator_short=="Police Stops by Result of Stop AVUHSD")%>%
  pivot_longer(3:22, names_to="label", values_to = "translation")

# subset translations for the actual stop results

df_sp<-result_sp%>%slice(6:17)

#load in data

df<-dbGetQuery(con_ctc, "SELECT * FROM analysis_stops_result_avuhsd")%>%
  mutate(rate=as.numeric(rate))%>%
  rename("label"="stop_result")%>%
  select(-geography)%>%
  mutate(label = str_to_title(label),
         label = ifelse(label=="In Field Cite And Release", "In-Field Cite-And-Release", label))%>%
  select(-total)

# add row ID columns to the df and the df_sp so I can join them together on the ID column

df$id <- seq_len(nrow(df))

df_sp$id <- seq_len(nrow(df_sp))

# join the df and spanish df together so we have a df with translations and data

df_combined<-df%>%left_join(df_sp, by=c("id"="id"))


# # NOTE: The indicator field needs to match the way it is in the data dictionary indicator_short column
# ## i.e.) for suspensions by race, I need to set indicator_short== "Suspensions by race"
# 
 

# Visualize manually----------------------------------

indicator="Police Stops by Result of Stop AVUHSD"


 ## Title text:
 title_text<-result_sp$translation[result_sp$label=="title"]
 
## Set up subtitle text: This will be from the data dictionary

subtitle_text<-result_sp$translation[result_sp$label=="subtitle"]

# # set caption text to use values from the data dictionary

footnote_text<-paste0("Recurso: ",dict$source[dict$indicator_short==indicator],", ", dict$year[dict$indicator_short==indicator],". ") 
footnote_text <- str_wrap(footnote_text, width = 110)

# define 'group_col'

group_col<-result_sp$translation[result_sp$label=="Label1"]

# Make rates out of 100 and reset as df

df<-df_combined%>%
  mutate(across(contains("rate", ignore.case = TRUE), ~ .x / 100))

# Clean up columns: add spanish translation here for 'Count' and "Rate' and "Stop result' column

df<-df%>%
  select(translation, count, rate, label.x)%>% # keep En translation just for one last comparison between EN/SP make sure they match
  rename("Resultados de paradas"="translation",
         "Contar"="count",
         "Tasa"="rate")
  
  # now remove english translation
  
  df<-df%>%
    select(-label.x)

  # rename column names in the df you are visualizing to be title case and have spaces

colnames(df) <- colnames(df) %>%
  str_replace_all("_", " ") %>%
  str_to_title()



# visualize your gt table

final_visual<-gt(df) %>% 
  opt_all_caps() %>%
  
  
  # use CSS file for fonts
  opt_css(css = readLines(".\\Visuals\\styling.css"), add = TRUE) %>%
  opt_table_font(font = font_table_text) %>%
  
  tab_header(title = md(title_text),
             subtitle = md(paste0("**",subtitle_text,"**"))) %>%
  tab_footnote (footnote = md(footnote_text))%>% 
  
  #title font and text styling
  
  tab_style(
    style = cell_text(font = font_table_title),
    locations = cells_title(groups = "title")
  ) %>%
  
  #subtitle font and text styling
  
  tab_style(
    style = cell_text(font = font_table_title),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  fmt_number(
    columns = where(is.numeric), # This will find and format ALL numeric columns to have comma separators
    use_seps = TRUE,
    decimals = 0 # Use this if you don't want decimal places
  ) %>%
  
  # Add percent signs to rate column
  
  fmt_percent(
    columns = matches("Tasa", ignore.case = TRUE),
    decimals = 0
  )%>%
  # Style the column headers using the custom header font
  tab_style(
    style = cell_text(font = font_table_title),
    locations = cells_column_labels()
  ) %>%
  cols_align(
    align = c("left"),
    columns = everything()
  )%>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = matches("Tasa"),
      rows = `group_col` == "TOTAL"
    ))%>%
  data_color(
    columns = matches("Tasa"),
    colors = scales::col_numeric(
      palette = c("white", teal),
      domain = NULL,
      na.color = textgrey
    )
  ) %>%  
  tab_options(table.font.names = font_table_text,
              column_labels.background.color = "white",
              table.border.top.width = px(3),
              table.border.top.color = "transparent",
              table.border.bottom.color = "transparent",
              table.border.bottom.width = px(3),
              column_labels.border.top.width = px(3),
              column_labels.border.top.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = black,
              data_row.padding = px(3),
              source_notes.font.size = 8,
              heading.title.font.size = px(24),
              heading.subtitle.font.size = px(22),
              column_labels.font.size = px(18),
              table.font.size = 16,
              heading.align = "left",
              container.width = 500
  ) 

# Define base file path for saving visuals
# Folder to save tables
export_dir <- here("Visuals", "Exports SP")
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

# Clean indicator name for files
clean_ind <- gsub("[^A-Za-z0-9_]+", "_", indicator)

# Save PNG and HTML
gtsave(final_visual, filename = file.path(export_dir, paste0(clean_ind, "_table_sp.png")))

  