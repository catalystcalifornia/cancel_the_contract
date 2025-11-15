# Create static visual functions for CTC report 
# Author: JZ

# Set up workspace----------------------------------

list.of.packages <- c("RPostgres", "tidyverse", "gt", "showtext", "scales", "forcats") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

#### Loading Libraries ####
library(RPostgres)
library(tidyverse)
library(gt)
library(showtext)
library(scales)
library(here)
library(forcats)
library(ragg)

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con<- connect_to_db("cancel_the_contract")

# grab data dictionary

dict<-dbGetQuery(con, "SELECT * FROM data_dictionary")

#### Set up style guide---------------------

yellow <- "#d7a24b"
magenta <- "#af3c6d"
teal <- "#46756f"
navy<-"#3f444e"
mauve<-"#c492b1"
lightblue<-"#bae7fc"
black <- "#000000"
textgrey <- "#919191"

# Add fonts

# Steps: 
# 1) Download the zip file from https://font.download/font/gill-sans-2 & https://font.download/font/placard-mt-std
# 2) Extract the zip file and drag them into your local C:\\Windows\\Fonts folder
# 3) Run this code to get the exact file name:
#####systemfonts::system_fonts() |> dplyr::filter(grepl("Gill", family, ignore.case = TRUE))
# 4) Use the exact file names for the font_add() code to add the fonts to R


## Adding the gillsans fonts:

# systemfonts::system_fonts() |> dplyr::filter(grepl("Gill", family, ignore.case = TRUE))

font_add("gillsans", regular = "C:\\Windows\\Fonts\\GIL_____.TTF")

font_add("gillsans_bold", regular = "C:\\Windows\\Fonts\\GILB____.TTF")

## Adding the placard fonts:

# library(systemfonts)
# 
# system_fonts_df <- system_fonts() %>%
#   as.data.frame() %>%
#   filter(grepl("Placard", family, ignore.case = TRUE)) %>%
#   mutate(full_path = normalizePath(path, winslash = "/", mustWork = FALSE))
# 
# system_fonts_df

font_add("placard", regular = "W:\\Project\\RJS\\CTC\\CTC Fonts\\PlacardNextRegular.TTF")

font_add("placard_bold", regular = "W:\\Project\\RJS\\CTC\\CTC Fonts\\PlacardNextBold.TTF")

# Use showtext_auto() to get fonts to load

showtext_auto()

# define fonts in chart

font_title <- "placard"
font_subtitle <- "placard_bold"
font_caption <- "gillsans"
font_bar_label <- "gillsans"
font_axis_label <- "gillsans"

font_table_title<-"placard"
font_table_subtitle<-"placard_bold"
font_table_text<-"gillsans"

# Race Recoding FX-------------------------------------

# This is so we don't have to constantly recode the race labels to be forward-facing

race_recode<-function(df){
  
  df<-df%>%
    filter(!label %in% c("nh_aian", "nh_nhpi", "nh_asian_wo_sa", "nh_sswana"))%>% # we can filter these groups out
    mutate(label=ifelse(label %in% "latino", "Latinx",
                        ifelse(label %in% "latinx", "Latinx",
                        ifelse(label %in% "nh_white", "White",
                               ifelse(label %in% "nh_black", "Black",
                                      ifelse(label %in% "nh_asian", "Asian",
                                             ifelse(label %in% "aian", "AIAN",
                                                    ifelse(label %in% "AIAN AOIC", "AIAN",
                                                    ifelse(label %in% "nh_twoormor", "Multiracial",
                                                           ifelse(label %in% "sswana", "SSWANA",
                                                                  ifelse(label %in% "swana", "SWANA",
                                                                  ifelse(label %in% "SSWANA AOIC", "SSWANA",
                                                                         ifelse(label %in% "SWANA AOIC", "SWANA",
                                                                  ifelse(label %in% "nh_other", "Other",
                                                                         ifelse(label %in% c("nhpi", "nh_nhpi", "pacisl"), "NHPI",
                                                                                ifelse(label %in% "NHPI AOIC", "NHPI",
                                                                                       ifelse(label %in% "total", "Total",
                                                                                       label)
                                                                  ))))))))))))))))
}
                        
# EX) RACE RECODING----------------------------------

# df<-dbGetQuery(con, "SELECT * FROM analysis_stops_race")%>%
#   rename(label=reportingcategory_re)
#   
#   
#  df_new<- race_recode(df)
  


# STATIC TABLE FX-----------------------------


static_table <- function(df, indicator, group_col, title_text)
{
 
  # Divide rate by 100
  
  df<-df%>%
    mutate(across(contains("rate", ignore.case = TRUE), ~ .x / 100))
  
  ## Set up subtitle text: This will be from the data dictionary
  
  subtitle_text<-paste0(dict$indicator[dict$indicator_short==indicator])
    
  # # set caption text to use values from the data dictionary
  
  footnote_text<-paste0("Source: Catalyst California calculations of ",dict$source[dict$indicator_short==indicator]," data, ", dict$year[dict$indicator_short==indicator],". ",dict$race_note[dict$indicator_short==indicator]) 
  footnote_text <- str_wrap(footnote_text, width = 110)

  # rename column to what you specify in the group_col arguement for labeling on the table
  df<-df%>%
    rename(!!group_col := label)
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
     columns = matches("rate", ignore.case = TRUE),
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
       columns = matches("Rate"),
       rows = `group_col` == "TOTAL"
     ))%>%
    data_color(
      columns = matches("Rate"),
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
 export_dir <- here("Visuals", "Exports")
 dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
 
 # Clean indicator name for files
 clean_ind <- gsub("[^A-Za-z0-9_]+", "_", indicator)
 
 # Save PNG and HTML
 gtsave(final_visual, filename = file.path(export_dir, paste0(clean_ind, "_table.png")))
 # gtsave(final_visual, filename = file.path(export_dir, paste0(clean_ind, "_table.html")))
 
 return(final_visual)
 
}
  
# EX) STATIC TABLE--------------------------

# Example: Suspensions (in English)

#load in data
# 
#  df<-dbGetQuery( con, "SELECT * FROM analysis_suspensions")%>%
#    select(label, enrollment_total, suspension_count, suspension_rate) # select columns you want in the table
# 
# # NOTE: The indicator field needs to match the way it is in the data dictionary indicator_short column
# ## i.e.) for suspensions by race, I need to set indicator_short== "Suspensions by race"
# 
#  indicator="Suspensions by race"
#  title_text="Marginalized Students are Suspended at Disproportionately High Rates"
# 
# # Apply function
# 
#  static_table(df=df,
#               indicator=indicator,
#               group_col="Student Group", # specify the way you want the category column to be labeled as on the visual
#            title_text=title_text)
# 

# SINGLE BAR GRAPH FUNCTION -------------------------------------

single_bar<-function(df, indicator, title_text){
  
  # rename 'rate' column for function and arrange by rate descending
  df<-df%>%
    rename_with(~ "rate", .cols = contains("rate"))
  
  # Define max value
  max_y = 1.15 * max(df$rate)
  
  # Dynamically adjust dimensions of the output
  
  # Base width/height
  base_width <- 7   # inches
  base_height <- 5  # inches
  
  # Adjust height based on number of rows (bars)
  num_bars <- nrow(df %>% filter(label != "Total"))
  height <- base_height + 0.2 * num_bars  # each bar adds 0.2 inches
  
  # Adjust width based on title length
  title_length <- nchar(title_text)
  width <- base_width + 0.05 * title_length  # long titles get extra width
  
  ## Set up subtitle text: This will be from the data dictionary
  
  subtitle_text<-paste0(dict$indicator[dict$indicator_short==indicator])
  
  # # set caption text to use values from the data dictionary
  
  caption_text<-paste0("Source: Catalyst California calculations of ",dict$source[dict$indicator_short==indicator]," data, ", dict$year[dict$indicator_short==indicator],". ",dict$race_note[dict$indicator_short==indicator]) 
  wrap_width <- round(width * 12)
  caption_text <- str_wrap(caption_text, width = wrap_width)

    # Graph
  
  final_visual <-  ggplot(df, aes(x= reorder(label, rate), y=rate)) +   
    geom_bar(stat="identity", position = position_dodge(0.7), show.legend = FALSE) +
    
    # define the bars
    
    geom_col(fill = teal) +
    
    # bar labels
    
    geom_text(aes(label = paste0(round(rate, 1), "%")),
              family = font_bar_label, 
              hjust = -0.1,   # small negative number pushes text to the right of the bar
              vjust = 0.5,
              colour = "black",
              size=7) +
    
    labs(title = title_text,
         subtitle = subtitle_text,
         caption=caption_text) + 
    
    scale_x_discrete(labels = function(label) str_wrap(label, width = 20)) +            # wrap long labels
    xlab("") +
    ylab("") +
    expand_limits(y = c(0, max_y))+
  coord_flip()+
    theme_minimal()+
    theme(legend.title = element_blank(), # no legend--modify if necessary
          
          # define style for axis text
          axis.text.y = element_text(size = 18, margin = margin(0, -10, 0, 0), # margins for distance from y-axis labels to bars
                                     colour = black, family= font_axis_label),
          axis.text.x = element_blank(),
          plot.caption = element_text(hjust = 0.0, size = 18, colour = black, family = font_caption),
          plot.title =  element_text(hjust = 0.0, size = 30, colour = black, family = font_title), 
          plot.subtitle = element_text(hjust = 0.0, size = 25, colour = black, family = font_subtitle),
          axis.ticks = element_blank(),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.25),
          panel.grid.major.y = element_blank())
  
  
  # Define base file path for saving visuals
  export_dir <- here::here("Visuals", "Exports")
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  
  outfile <- file.path(export_dir, paste0(indicator, "_singlebar.png"))
  
  ragg::agg_png(outfile, width = width, height = height, units = "in", res = 150)
  print(final_visual)
  dev.off()


  return(final_visual)
}

# EX) SINGLE BAR GRAPH LINE------------------------------------

# 
# df<-dbGetQuery(con, "SELECT * FROM av_population_race")
# 
# # first apply the race_recode function if you are visualizing something disaggregated by race
# # for it to work you need to rename your column that needs to be recoded to 'label'
# 
#  df<-df%>%
#    rename("label"="race")%>% # This is the column that needs to get renamed
#   race_recode() # apply race recoding
# 
# # NOTE: The indicator field needs to match the way it is in the data dictionary indicator_short column
# ## i.e.) for suspensions by race, I need to set indicator== "Suspensions by race"
# 
#  indicator<-"Race"
#  title_text<-"The Majority of the Antelope Valley Population is Latinx, White or Black"
# 
# # Apply function
# 
#  single_bar(df=df,
#            indicator=indicator,
#             title_text=title_text
#                )

# SINGLE BAR GRAPH W/ TOTAL LINE FUNCTION -------------------------------------

single_bar_tot<-function(df, indicator, title_text){
  
  message("single_bar_tot: version updated 2025-11-07")
  
  
  # rename 'rate' column for function and arrange by rate descending
  df<-df%>%
    rename_with(~ "rate", .cols = contains("rate"))
  
# Define max value
 max_y = 1.15 * max(df$rate)

 # Define annotation
 annotate_y = 1.12 * (df$rate[df$label=="Total"])
 
 # set total value
 total_value<- subset(df, label=="Total")$rate
 
 # Dynamically adjust dimensions of the output
 
 # Base width/height
 base_width <- 7   # inches
 base_height <- 5  # inches
 
 # Adjust height based on number of rows (bars)
 num_bars <- nrow(df %>% filter(label != "Total"))
 height <- base_height + 0.2 * num_bars  # each bar adds 0.2 inches
 
 # Adjust width based on title length
 title_length <- nchar(title_text)
 width <- base_width + 0.05 * title_length  # long titles get extra width
 
 ## Set up subtitle text: This will be from the data dictionary
 
 subtitle_text<-paste0(dict$indicator[dict$indicator_short==indicator])
 
 # # set caption text to use values from the data dictionary
 
 caption_text<-paste0("Source: Catalyst California calculations of ",dict$source[dict$indicator_short==indicator]," data, ", dict$year[dict$indicator_short==indicator],". ",dict$race_note[dict$indicator_short==indicator]) 
 wrap_width <- round(width * 12)
 caption_text <- str_wrap(caption_text, width = wrap_width)

 # Graph
 
  final_visual <-  ggplot(subset(df, label !='Total' ), aes(x= reorder(label, rate), y=rate)) +   
    geom_bar(stat="identity", position = position_dodge(0.7), show.legend = FALSE) +
    
    # define the bars
    
    geom_col(fill = teal) +
    
  # vertical line for Total %
    geom_hline(yintercept = subset(df, label =="Total")$rate, linetype = "dotted", color = black, size = 0.75) +    
   
    # label for vertical Total % line
    
    annotate(geom = "text",
             x = 1.0,
             y = subset(df, label=="Total")$rate,
             label = sprintf("Overall Rate: %.1f%%", subset(df, label == "Total")$rate),
             hjust =-0.1, vjust = 0,
             color = black, size = 7, family = font_axis_label) +
    
    # bar labels
    
    geom_text(aes(label = paste0(round(rate, 1), "%")),
              family = font_bar_label, 
              hjust = -0.1,   # small negative number pushes text to the right of the bar
              vjust = 0.5,
              colour = "black",
              size = 7) +
    
    labs(title = title_text,
         subtitle = str_wrap(subtitle_text, width = 80),
         caption=caption_text) + 
  
    scale_x_discrete(labels = function(label) str_wrap(label, width = 20)) +            # wrap long labels
    xlab("") +
    ylab("") +
    expand_limits(y = c(0, max_y))+
  coord_flip()+
    theme_minimal()+
    theme(legend.title = element_blank(), # no legend--modify if necessary
          
          # define style for axis text
          axis.text.y = element_text(size = 18, margin = margin(0, -10, 0, 0), # margins for distance from y-axis labels to bars
                                     colour = black, family= font_axis_label),
          axis.text.x = element_blank(),
          plot.caption = element_text(hjust = 0.0, size = 18, colour = black, family = font_caption),
          plot.title =  element_text(hjust = 0.0, size = 30, colour = black, family = font_title), 
          plot.subtitle = element_text(hjust = 0.0, size = 25, colour = black, family = font_subtitle),
          axis.ticks = element_blank(),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.25),
          panel.grid.major.y = element_blank())
  
  
  # Define base file path
  export_dir <- here::here("Visuals", "Exports")
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  
  outfile <- file.path(export_dir, paste0(indicator, "_singlebartot.png"))
  
  ragg::agg_png(outfile, width = width, height = height, units = "in", res = 150)
  print(final_visual)
  dev.off()
  

  # Save with dynamic dimensions
  # ggsave(outfile, plot = final_visual, width = width, height = height)

  
}

# EX) SINGLE BAR GRAPH W/ TOTAL LINE------------------------------------

# # 
#  df<-dbGetQuery(con, "SELECT * FROM analysis_rent_burden")
# 
# # check your table and do any prep necessary prior to visual
# ## if you have rate_Se and rate_moe type columns, remove prior to visualizing!
# 
#  df<-df%>%
#    rename(label=subgroup)%>%
#    select(-rate_se, -rate_moe, -rate_cv)%>%
#    race_recode()
# 
# 
#  indicator<-"Police Stops by Race"
#  title_text<-"Findings based title"
# 
# # Apply function
# 
#  single_bar_tot(df=df,
#               indicator=indicator,
#               title_text=title_text)


# SINGLE BAR TOTAL LINE FX ONLY FOR SEARCHES BY RACE IN AVUHSD INDICATOR----------

single_bar_search<-function(df, indicator, title_text){
  
  
  # rename 'rate' column for function and arrange by rate descending
  df<-df%>%
    rename_with(~ "rate", .cols = contains("rate"))
  
  # Define max value
  max_y = 1.15 * max(df$rate)
  
  # Define annotation
  annotate_y = 1.12 * (df$rate[df$label=="Total"])
  
  # set total value
  total_value<- subset(df, label=="Total")$rate
  
  # Dynamically adjust dimensions of the output
  
  # Base width/height
  base_width <- 7   # inches
  base_height <- 5  # inches
  
  # Adjust height based on number of rows (bars)
  num_bars <- nrow(df %>% filter(label != "Total"))
  height <- base_height + 0.2 * num_bars  # each bar adds 0.2 inches
  
  # Adjust width based on title length
  title_length <- nchar(title_text)
  width <- base_width + 0.05 * title_length  # long titles get extra width
  scale_factor <- width / 7  # compare to base width
  
  ## Set up subtitle text: This will be from the data dictionary
  
  subtitle_text<-paste0(dict$indicator[dict$indicator_short==indicator])
  
  # # set caption text to use values from the data dictionary
  
  caption_text<-paste0("Source: Catalyst California calculations of ",dict$source[dict$indicator_short==indicator]," data, ", dict$year[dict$indicator_short==indicator],". ",dict$race_note[dict$indicator_short==indicator]) 
  wrap_width <- round(width * 12)
  caption_text <- str_wrap(caption_text, width = wrap_width)
  
  # Graph
  
  final_visual <-  ggplot(subset(df, label !='Total' ), aes(x= reorder(label, rate), y=rate)) +   
    geom_bar(stat="identity", position = position_dodge(0.7), show.legend = FALSE) +
    
    # define the bars
    
    geom_col(fill = teal) +
    
    # vertical line for Total %
    geom_hline(yintercept = subset(df, label =="Total")$rate, linetype = "dotted", color = black, size = 0.75) +    
    
    # label for vertical Total % line
    
    annotate(geom = "text",
             x = 1.0,
             y = subset(df, label=="Total")$rate,
             label = sprintf("Overall Rate: %.1f%%", subset(df, label == "Total")$rate),
             hjust =-0.1, vjust = 0,
             color = black, size = 7, family = font_axis_label) +
    
    # bar labels
    
    geom_text(aes(label = paste0(round(rate, 1), "%")),
              family = font_bar_label, 
              hjust = -0.1,   # small negative number pushes text to the right of the bar
              vjust = 0.5,
              colour = "black",
              size = 7) +
    
    labs(title = title_text,
         subtitle = str_wrap(subtitle_text, width = 80),
         caption=caption_text) + 
    
    scale_x_discrete(labels = function(label) str_wrap(label, width = 20)) +            # wrap long labels
    xlab("") +
    ylab("") +
    expand_limits(y = c(0, max_y))+
    coord_flip()+
    theme_minimal()+
    theme(legend.title = element_blank(), # no legend--modify if necessary
          
          # define style for axis text
          plot.title = element_text(size = 30 * scale_factor),
          plot.subtitle = element_text(size = 25 * scale_factor),
          plot.caption = element_text(size = 18 * scale_factor),
          axis.text.y = element_text(size = 18 * scale_factor),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.25),
          panel.grid.major.y = element_blank())
  
  
  # Define base file path
  export_dir <- here::here("Visuals", "Exports")
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  
  outfile <- file.path(export_dir, paste0(indicator, "_singlebartot.png"))
  
  ragg::agg_png(outfile, width = width, height = height, units = "in", res = 150)
  print(final_visual)
  dev.off()
  
  
  # Save with dynamic dimensions
  # ggsave(outfile, plot = final_visual, width = width, height = height)
  
  
}



# Disconnect from postgres--------------------------
dbDisconnect(con)