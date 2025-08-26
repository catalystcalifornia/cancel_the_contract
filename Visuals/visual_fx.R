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
library(forcats)

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con<- connect_to_db("cancel_the_contract")

# read in data dictionary
dict<-dbGetQuery(con, "SELECT * FROM data_dictionary")


#### Set up style guide: SUBSTITUTE WITH CTC OR WHATEVER WE END UP USING----------------------

## COLORS## Taken from W:\Project\RDA Team\Region 5 State of the Child\Documentation\F5LA_BrandGuidelines_COLORS.pdf
#primary
yellow <- "#d7a24b"
magenta <- "#af3c6d"
teal <- "#46756f"
navy<-"#3f444e"
mauve<-"#c492b1"
lightblue<-"#bae7fc"
black <- "#000000"
textgrey <- "#919191"


## FONTS ##
# Step 1: Download fonts (Gotham Bold and Gotham Book) 
# Step 2: Load the downloaded fonts into the R script:
# General: See tutorial here, under "THE SHOWTEXT PACKAGE" section: https://r-coder.com/custom-fonts-r/#The_showtext_package 
# Step 3: Run the code below in each R script

font_add(family = "GothamBold", regular = "C:/Windows/Fonts/Gotham-Bold 700.otf")
font_add(family = "GothamBook", regular = "C:/Windows/Fonts/Gotham-Book 325.otf")

showtext_auto()

# define fonts in chart
font_title <- "GothamBold"
font_subtitle <- "GothamBook"
font_caption <- "GothamBook"
font_bar_label <- "GothamBold"
font_axis_label <- "GothamBook"
font_table_text<-"GothamBook"

#Reference: W:\Project\RDA Team\LAFed\R\LAFed\visual_functions_static.R

# RACE RECODING FX-----------------------------

# Use this to automatically recode your race column to be spelled out labels for the graph

race_recode<-function(df, col_recode){
  df<-df%>%
    filter(!grepl("nh_aian|nh_nhpi|nh_asian_wo_sa|nh_sswana",{{col_recode}}))%>%
    mutate(label = case_when(
      {{col_recode}} == "nh_white"     ~ "White",
      {{col_recode}} == "aian"         ~ "AIAN",
      {{col_recode}} == "AIAN AOIC"    ~ "AIAN",
      {{col_recode}} == "latino"       ~ "Latinx",
      {{col_recode}} == "latinx"       ~ "Latinx",
      {{col_recode}} == "nh_asian"     ~ "Asian",
      {{col_recode}} == "nh_black"     ~ "Black",
      {{col_recode}} == "nh_other"     ~ "Other",
      {{col_recode}} == "nh_twoormor"  ~ "Multiracial",
      {{col_recode}} == "nhpi"         ~ "NHPI",
      {{col_recode}} == "NHPI AOIC"   ~ "NHPI",
      {{col_recode}} == "sswana"       ~ "SSWANA",
      {{col_recode}} == "SSWANA AOIC" ~ "SSWANA",
      {{col_recode}} == "swana"        ~ "SWANA",
      TRUE                             ~ as.character({{col_recode}})
    ))
}

######  EX) RACE RECODING ###### 

df<-dbGetQuery(con, "SELECT * FROM analysis_stops_race")

df_recode<-race_recode(df=df,
                       col_recode=reportingcategory_re)

# STATIC TABLE FX-----------------------------


static_table <- function(df, indicator, title_text, footnote_text)
{
  
  # Conditionally rename 'label' column to 'Student Group' ONLY if the table is with education data
  
  if ("geography" %in% names(df) &&
      any(df$geography == "Antelope Valley Union High School District", na.rm = TRUE)) {
    df <- df %>% rename(`Student Group` = label)
  }
  
  # rename column names in the df you are visualizing to be title case and have spaces
  
  colnames(df) <- colnames(df) %>%
    str_replace_all("_", " ") %>%
    str_to_title()
  
# set footnote text to use values from the data dictionary
  
footnote_text<-paste0("Source: Catalyst California calculations of ",dict$source[dict$indicator_short==indicator]," data, ", dict$year[dict$indicator_short==indicator],". ",dict$method_note[dict$indicator_short==indicator])
  
  # visualize your gt table
  
 final_visual<-gt(df) %>% 
    opt_all_caps() %>%
    tab_header(title = md(paste0("**",title_text,"**"))) %>%
    tab_footnote (footnote = md(footnote_text))%>% 
    cols_align(
      align = c("left"),
      columns = everything()
    )%>%
   tab_style(
     style = cell_text(weight = "bold"),
     locations = cells_body(
       columns = matches("Rate"),
       rows = `Label` == "TOTAL"
     ))%>%
    data_color(
      columns = matches("Rate"),
      colors = scales::col_numeric(
        palette = c("white", magenta),
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
                table.font.size = 16,
                heading.align = "left",
                container.width = 500
    ) %>%
    opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts()))

 # Define base file path for saving visuals
 base_path <- paste0("W:/Project/RJS/CTC/Visuals/",indicator, "_table")
 showtext_opts(dpi=300)
  
 # save as PNG
 
 gtsave(final_visual, filename = paste0(base_path, ".png"))

  return(final_visual)
}
  
######  EX) STATIC TABLE ###### 

# Example: Suspensions (in English)

#load in data

df<-dbGetQuery( con, "SELECT * FROM analysis_suspensions")%>%
  select(label, enrollment_total, suspension_count, suspension_rate) # select columns you want in the table

# define parameters outside of function

indicator="Suspensions" # THIS NEEDS TO MATCH the way it is spelled out in the data_dictionary df
title_text="Suspension Rates by Student Group, Antelope Valley Union High School District, 2023-24"

# Apply function

static_table(df=df, 
             indicator=indicator, 
             title_text=title_text,
             footnote_text=footnote_text)


# SINGLE BAR GRAPH FUNCTION -------------------------------------


single_bar<-function(df, label, indicator, title_text){
  
  # rename 'rate' column for function and arrange by rate descending
  df<-df%>%
    rename_with(~ "rate", .cols = contains("rate"))
  
  # Define max value
  max_y = 1.15 * max(df$rate)
  
  # # set caption text to use values from the data dictionary
  
 caption_text<-paste0("Source: Catalyst California calculations of ",dict$source[dict$indicator_short==indicator]," data, ", dict$year[dict$indicator_short==indicator],". ",dict$method_note[dict$indicator_short==indicator])
 caption_text <- str_wrap(caption_text, width = 110)
 
 # # set subtitle text to use values from the data dictionary
 
subtitle_text<-paste0(dict$indicator[dict$indicator_short==indicator])

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
              fontface = "bold",  
              colour = "black") +
    
    labs(title = str_wrap(title_text, width = 65),
         subtitle = str_wrap(subtitle_text, width = 80),
         caption=caption_text) + 
    
    scale_x_discrete(labels = function(label) str_wrap(label, width = 20)) +            # wrap long labels
    xlab("") +
    ylab("") +
    expand_limits(y = c(0, max_y))  +  
    coord_flip()+
    theme_minimal()+
    theme(legend.title = element_blank(), # no legend--modify if necessary
          
          # define style for axis text
          axis.text.y = element_text(size = 10, margin = margin(0, -10, 0, 0), # margins for distance from y-axis labels to bars
                                     colour = black, family= font_axis_label),
          axis.text.x = element_blank(),
          plot.caption = element_text(hjust = 0.0, size = 8, colour = black, family = font_caption),
          plot.title =  element_text(hjust = 0.0, size = 15, colour = black, family = font_title), 
          plot.subtitle = element_text(hjust = 0.0, size = 12, colour = black, family = font_axis_label),
          axis.ticks = element_blank(),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.25),
          panel.grid.major.y = element_blank())
  
  # Define base file path
  base_path <- paste0("W:/Project/RJS/CTC/Visuals/",indicator, "_singlebar")
  
  showtext_opts(dpi=300)
  
  # Save in SVG
  ggsave(plot = final_visual, filename = paste0(base_path, ".svg"),
         device = "svg", width = 9, height = 6.5)
  
  # Save in PNG
  ggsave(plot = final_visual, filename = paste0(base_path, ".png"),
         device = "png", width = 9, height = 6.5)
  
  
  return(final_visual)
}

###### EX 1) SINGLE BAR GRAPH LINE BY RACE ######

### This is an example of visualizing an analysis that is by race: 

df<-dbGetQuery(con, "SELECT * FROM av_population_race")
df_recode<-race_recode(df=df, col_recode = race)

title_text<-"Findings based title"
indicator<-"Race"

# Apply function

# You can use the race_recode function to automatically recode your race column to be spelled out if your visual is by race
# if your visual is NOT by race, make sure you rename whatever your categorical column is to have the colname=="label"

single_bar(df=df_recode, 
              label="label", 
              indicator="Race", # THIS HAS TO BE spelled out the way it is in the data_dictionary df
               title_text=title_text)


###### EX 2) SINGLE BAR GRAPH LINE NOT BY RACE ######

### This is an example of visualizing an analysis that is NOT by race: 
# you need to rename your categorical variable column to be named 'label' for the function to work

df<-dbGetQuery(con, "SELECT * FROM av_population_age")%>%
  rename("label"="age_re")

title_text<-"Findings based title"
indicator<-"Population"

# Apply function

single_bar(df=df, 
           label="label",
           indicator=indicator,
           title_text=title_text)


# SINGLE BAR GRAPH W/ TOTAL LINE FUNCTION -------------------------------------

single_bar_tot<-function(df, label, indicator, title_text){
  
  # rename 'rate' column for function and arrange by rate descending
  df<-df%>%
    rename_with(~ "rate", .cols = contains("rate"))
  
# Define max value
 max_y = 1.15 * max(df$rate)

 # Define annotation
 annotate_y = 1.12 * (df$rate[df$label=="Total"])
 
 # set total value
 total_value<- subset(df, label=="Total")$rate
 
 # # set caption text to use values from the data dictionary
 
 caption_text<-paste0("Source: Catalyst California calculations of ",dict$source[dict$indicator_short==indicator]," data, ", dict$year[dict$indicator_short==indicator],". ",dict$method_note[dict$indicator_short==indicator])
 caption_text <- str_wrap(caption_text, width = 110)
 
 # # set subtitle text to use values from the data dictionary
 
subtitle_text<-paste0(dict$indicator[dict$indicator_short==indicator])

 # Graph
 
  final_visual <-  ggplot(subset(df, label !='Total' ), aes(x= reorder(label, rate), y=rate)) +   
    geom_bar(stat="identity", position = position_dodge(0.7), show.legend = FALSE) +
    
    # define the bars
    
    geom_col(fill = teal) +
    
  # vertical line for Total %
    geom_hline(yintercept = subset(df, label =="Total")$rate, linetype = "dotted", color = black, size = 0.75) +    
   
    # label for vertical Total % line
    
    annotate(geom = "text",
             x = 0.75,
             y = subset(df, label=="Total")$rate,
             label = paste0("Total: ", subset(df, label=="Total")$rate,"%"),
             hjust =0, vjust = 0,
             color = black, size = 4, family = font_axis_label) +
    
    # bar labels
    
    geom_text(aes(label = paste0(round(rate, 1), "%")),
              family = font_bar_label, 
              hjust = -0.1,   # small negative number pushes text to the right of the bar
              vjust = 0.5,
              fontface = "bold",  
              colour = "black") +
    
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
          axis.text.y = element_text(size = 10, margin = margin(0, -10, 0, 0), # margins for distance from y-axis labels to bars
                                     colour = black, family= font_axis_label),
          axis.text.x = element_blank(),
          plot.caption = element_text(hjust = 0.0, size = 9, colour = black, family = font_caption),
          plot.title =  element_text(hjust = 0.0, size = 21, colour = black, family = font_title), 
          plot.subtitle = element_text(hjust = 0.0, size = 14, colour = black, family = font_axis_label),
          axis.ticks = element_blank(),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.25),
          panel.grid.major.y = element_blank())
  
  # Define base file path
  base_path <- paste0("W:/Project/RJS/CTC/Visuals/",indicator, "_singlebartot")

  showtext_opts(dpi=300)
  
  # Save in SVG
  ggsave(plot = final_visual, filename = paste0(base_path, ".svg"),
         device = "svg", width = 9, height = 6.5)
  
  # Save in PNG
  ggsave(plot = final_visual, filename = paste0(base_path, ".png"),
         device = "png", width = 9, height = 6.5)

  
  return(final_visual)
}

######  EX) SINGLE BAR GRAPH W/ TOTAL LINE ###### 


df<-dbGetQuery(con, "SELECT * FROM analysis_suspensions")

indicator<-"Suspensions by race"
title_text<-"Findings based title"

# Apply function

single_bar_tot(df=df, 
              label="label",
             indicator=indicator, 
             title_text=title_text)

# Disconnect from postgres--------------------------
dbDisconnect(con)