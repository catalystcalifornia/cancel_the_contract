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


#### Set up style guide: SUBSTITUTE WITH CTC OR WHATEVER WE END UP USING----------------------

## COLORS## Taken from W:\Project\RDA Team\Region 5 State of the Child\Documentation\F5LA_BrandGuidelines_COLORS.pdf
#primary
lightblue <- "#009CDB"
darkblue <- "#332985"
tealblue <- "#22BCB8"
black <- "#000000"
textgrey <- "#919191"
#secondary 
green <- "#54B847"
orange <- "#F58326"
hotpink <- "#EC098C"
red <- "#EF4034"

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
        palette = c("white", lightblue),
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
  
# EX) STATIC TABLE--------------------------


# Example: Suspensions (in English)

#load in data

df<-dbGetQuery( con, "SELECT * FROM analysis_suspensions")%>%
  select(label, enrollment_total, suspension_count, suspension_rate) # select columns you want in the table

# define parameters outside of function ( these  can also be defined in the function)

indicator="suspensions"
title_text="Suspension Rates by Student Group, Antelope Valley Union High School District, 2023-24"
footnote_text="Source: Catalyst California calculations of California Department of Education data, 2023-24.
Note: The student group category for non-binary students was unavailable because of a lack of data. AIAN stands for American Indian Alaskan Native."

# Apply function

static_table(df=df, 
             indicator=indicator, 
             title_text=title_text,
             footnote_text=footnote_text)


# Spanish version ------

#load in data

# susp_table_sp <- df %>% rename('Grupo de estudiantes' = "label", 
#                                            'Matr?cula de estudiantes'="enrollment_total", 
#                                            'Conteo no duplicado de estudiantes suspendidos'="suspension_count", 
#                                            'Tasa de suspensi?n'="suspension_rate"
# )
# 
# #rename to what we want to use in the visuals
# susp_table_sp$`Grupo de estudiantes` <- gsub("Total", "TOTAL",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("African American", "Negrx (Negro)",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Hispanic or Latino", "Latinx (Latino)",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("American Indian or Alaska Native", "AIAN",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Foster", "Adoptivo",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Students with Disabilities", "Estudiantes con discapacidades",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Pacific Islander", "Isle?x del Pac?fico (Isle?o del Pac?fico)",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Male", "Hombre",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Socioeconomically Disadvantaged", "Desfavorecido socioecon?micamente",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Two or More Races", "Dos o m?s razas",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Homeless", "Sin Hogar",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Migrant", "Migrante",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("English Learners", "Estudiantes de ingl?s",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Female", "Mujer",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("White", "Blancx (Blanco)",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Asian", "Asi?ticx (Asi?tico)",susp_table_sp$`Grupo de estudiantes`)
# susp_table_sp$`Grupo de estudiantes` <- gsub("Filipino", "Filipinx (Filipino)",susp_table_sp$`Grupo de estudiantes`)
# 
# susp_table_sp <- subset(susp_table_sp, susp_table_sp$`Grupo de estudiantes` != "Non-Binary Gender" & susp_table_sp$`Grupo de estudiantes` != "Not Reported") 
# # View(susp_table_sp)
# 
# suspensions_sp <- susp_table_sp %>%
#   mutate(across(c(`Matr?cula de estudiantes`, `Conteo no duplicado de estudiantes suspendidos`), comma)) %>%
#   gt() %>% opt_all_caps() %>% 
#   tab_header(title = md("**Tasas de suspensi?n por grupo de estudiantes, distrito escolar de Antelope Valley Union High, 2023-24**")) %>% 
#   tab_footnote (footnote = md("Fuente: C?lculos de Catalyst California de datos del Departamento de Educaci?n de California, 2023-24.<br>
#                                 Nota: La categor?a de grupo de estudiantes para estudiantes g?nero neutral, no estaba disponible debido 
#                               a la falta de datos. AIAN (por sus siglas en ingl?s) significa Indix Americanx y Nativx de Alaska (Indio Americano y 
#                               Nativo de Alaska). "))%>% #, https://www.cde.ca.gov/ds/ 
#   cols_align(
#     align = c("left"),
#     columns = everything()
#   )%>%
#   tab_style(style = cell_text(weight = "bold"),
#             locations = cells_body(
#               columns = `Grupo de estudiantes`,
#               rows = `Grupo de estudiantes` == "TOTAL"))%>% 
#   data_color(
#     columns = c(`Tasa de suspensi?n`),
#     colors = scales::col_numeric(
#       palette = c("white", lightblue),
#       domain = NULL,
#       na.color = textgrey
#     )
#   ) %>%  
#   tab_options(table.font.names = font_table_text,
#               column_labels.background.color = "white",
#               table.border.top.width = px(3),
#               table.border.top.color = "transparent",
#               table.border.bottom.color = "transparent",
#               table.border.bottom.width = px(3),
#               column_labels.border.top.width = px(3),
#               column_labels.border.top.color = "transparent",
#               column_labels.border.bottom.width = px(3),
#               column_labels.border.bottom.color = black,
#               data_row.padding = px(3),
#               source_notes.font.size = 8,
#               table.font.size = 16,
#               heading.align = "left",
#               container.width = 500
#   ) %>% 
#   opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts()))
# gtsave(suspensions, "suspensions_table_esp.png",path = "W:/Project/RJS/CTC/Github/CR/cancel_the_contract/Images/Spanish/", vwidth = 4000, vheight = 6000)

# SINGLE BAR GRAPH FUNCTION -------------------------------------

df<-dbGetQuery(con, "SELECT * FROM analysis_graduation")

title_text<-"High School Graduation Rates by Student Subgroup, <br>2023-24 School Year, Antelope Valley Union High School District"
caption_text<-"Source: California Department of Education, Adjusted Cohort Graduation Rate and Outcome Data,
2023-2024. Note: Rates are out of 100 students. AIAN stands for American Indian and Alaskan Native."

single_bar<-function(df, indicator, title_text, footnote_text){
  
  # Conditionally rename 'label' column to 'Student Group' ONLY if the table is with education data
  
  if ("geography" %in% names(df) &&
      any(df$geography == "Antelope Valley Union High School District", na.rm = TRUE)) {
    df <- df %>% rename(`Student Group` = label)
  }
  
# Define max value
 max_y = 1.15 * max(df$rate)

 # Define annotation
 annotate_y = 1.12 * (df$rate[df$label=="Total"])
 
 # Define total stat annotation
 engtot_stat <- paste("Overall Rate =", min(df$rate[df$label=="Total"]))   
 
 # Graph
 
  final_visual <-  df%>%
    rename_with(~ "rate", .cols = contains("rate"))%>%
    
    ggplot(aes(x= reorder(label, -rate), y=rate)) +   
    geom_bar(stat="identity", position = position_dodge(0.7), show.legend = FALSE) +
    
  
  # set up manual fill using 'test', add "-" before value to order bars when MAX is best
   
     annotate("text", x=df$label, y =68, label = str_wrap(paste0(engtot_stat, "%"), width = 15), 
              hjust=0, lineheight = 0.3,
             vjust = 0.35, family= font_table_text, color = darkblue, size = 20) +
    
    labs(title = paste0("**", title_text,"**"),
         caption=caption_text) + 
    geom_col(fill = lightblue) +
    geom_hline(yintercept = round(df$rate[df$label=="Total"], 1), color = darkblue, size = .5) +
    geom_text(aes(label = paste0(round(rate, 1), "%")),
              family = font_table_text, hjust = -0.2, size = 10) +       # format data labels, adjust hjust to avoid overlap w/ total line
    scale_x_discrete(labels = function(label) str_wrap(label, width = 20)) +            # wrap long labels
    theme_void()+
    xlab("") +
    ylab("") +
    expand_limits(y = c(0,91))+
    coord_flip()+
  
    theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y =element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 52, family= font_axis_label, lineheight = 0.3, hjust=0),
    axis.ticks = element_blank(),
    plot.title= element_markdown(family = font_title, face = "bold", size = 72, hjust = 0, lineheight = 0.4, margin=margin(0,0,4,-155)),
    plot.caption = element_text(family = font_caption, size = 40, hjust = 0, lineheight = 0.3),
    plot.caption.position = "plot",
    plot.margin = margin(t = 3,
                         b = 3,
                         r = 3,
                         l = 3)
  )
  
  # Define base file path
  base_path <- paste0("W:/Project/RJS/CTC/Visuals/",indicator, "_singlebar")

  showtext_opts(dpi=300)
  
  # Save in SVG
  ggsave(plot = final_visual, filename = paste0(base_path, ".svg"),
         device = "svg", width = 8, height = 5.5)
  
  # Save in PNG
  ggsave(plot = final_visual, filename = paste0(base_path, ".png"),
         device = "png", width = 8, height = 5.5)

  
  return(final_visual)
}
  

# Disconnect from postgres--------------------------
dbDisconnect(con)