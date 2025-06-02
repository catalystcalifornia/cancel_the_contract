# install packages if not already installed ----
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

#### First 5 LA Style Guide ####
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

# Suspensions in English ----

#load in data

con<- connect_to_db("cancel_the_contract")
df<-dbGetQuery( con, "SELECT * FROM analysis_suspensions")

# filter
suspensions <- df %>%
  mutate(label=ifelse(label %in% "Total", "TOTAL", label))%>%
 filter(label== "Foster" | label== "Black" |
                  label== "Students with Disabilities"| label== "AIAN"|
                  label=="Pacific Islander"| label=="Male"| label=="Socioeconomically Disadvantaged"|
                  label=="Two or More Races"| label=="TOTAL"| label=="Homeless"|
                  label=="Migrant"| label=="English Learners"| label=="Latinx"|
                  label=="Female"| label=="White"| label=="Filipinx"| label=="Asian") %>%
  select(label, enrollment_total, suspension_count, suspension_rate)%>%
  mutate(across(c(enrollment_total, suspension_count), comma)) %>%
  rename("Student Group"="label",
         "Enrollment" = "enrollment_total",
         "Unduplicated Count of Students Suspended"="suspension_count",
         "Suspension Rate"="suspension_rate")%>%
  gt() %>% 
  opt_all_caps() %>%
  tab_header(title = md("**Suspension Rates by Student Group, Antelope Valley Union High School District, 2023-24**")) %>%
  tab_footnote (footnote = md("Source: Catalyst California calculations of California Department of Education data, 2023-24.<br>
                                Note: The student group category for non-binary students was unavailable because of a lack of data.
                                AIAN stands for American Indian Alaskan Native."))%>% #, https://www.cde.ca.gov/ds/
  cols_align(
    align = c("left"),
    columns = everything()
  )%>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = `Student Group`,
              rows = `Student Group` == "TOTAL"))%>%
  data_color(
    columns = c("Suspension Rate"),
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
gtsave(suspensions, "suspensions_table.png",path = "W:/Project/RJS/CTC/Github/JZ/cancel_the_contract/Images/English/", vwidth = 4000, vheight = 6000)

# view table
suspensions

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

dbDisconnect(con)