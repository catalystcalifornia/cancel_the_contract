# install packages if not already installed ----
list.of.packages <- c("dplyr", "gt", "showtext", "tidyr") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

#### Loading Libraries ####
library(dplyr)
library(tidyr)
library(gt)
library(showtext)
library(scales)
library(forcats)
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
source("W:/Project/RJS/CTC/Github/CR/cancel_the_contract/Analysis/suspension.R")


#rename to what we want to use in the visuals
susp_table$`Student group` <- gsub("Total", "TOTAL",susp_table$`Student group`)
susp_table$`Student group` <- gsub("African American", "Black",susp_table$`Student group`)
susp_table$`Student group` <- gsub("Hispanic or Latino", "Latinx",susp_table$`Student group`)
susp_table$`Student group` <- gsub("American Indian or Alaska Native", "AIAN",susp_table$`Student group`)
susp_table$`Student group` <- gsub("Filipino", "Filipinx",susp_table$`Student group`)

suspensions <- susp_table %>%
  dplyr::filter(`Student group`== "Foster" | `Student group`== "Black" |
                  `Student group`== "Students with Disabilities"| `Student group`== "AIAN"|
                  `Student group`=="Pacific Islander"| `Student group`=="Male"| `Student group`=="Socioeconomically Disadvantaged"|
                  `Student group`=="Two or More Races"| `Student group`=="TOTAL"| `Student group`=="Homeless"|
                  `Student group`=="Migrant"| `Student group`=="English Learners"| `Student group`=="Latinx"|
                  `Student group`=="Female"| `Student group`=="White"| `Student group`=="Filipinx"| `Student group`=="Asian") %>%
  mutate(across(c(`Student enrollment`, `Unduplicated count of students suspended`), comma)) %>%
  gt() %>% opt_all_caps() %>%
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
              columns = `Student group`,
              rows = `Student group` == "TOTAL"))%>%
  data_color(
    columns = c(`Suspension rate`),
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
gtsave(suspensions, "suspensions_table.png",path = "W:/Project/RJS/CTC/Github/CR/cancel_the_contract/Images/English/", vwidth = 4000, vheight = 6000)



# Spanish version ------

#load in data
source("W:/Project/RJS/CTC/Github/CR/cancel_the_contract/Analysis/suspension.R")
susp_table <- susp_table %>% dplyr::rename('Grupo de estudiantes' = "Student group", 
                                           'Matr?cula de estudiantes'="Student enrollment", 
                                           'Conteo no duplicado de estudiantes suspendidos'="Unduplicated count of students suspended", 
                                           'Tasa de suspensi?n'="Suspension rate"
)

#rename to what we want to use in the visuals
susp_table$`Grupo de estudiantes` <- gsub("Total", "TOTAL",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("African American", "Negrx (Negro)",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Hispanic or Latino", "Latinx (Latino)",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("American Indian or Alaska Native", "AIAN",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Foster", "Adoptivo",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Students with Disabilities", "Estudiantes con discapacidades",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Pacific Islander", "Isle?x del Pac?fico (Isle?o del Pac?fico)",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Male", "Hombre",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Socioeconomically Disadvantaged", "Desfavorecido socioecon?micamente",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Two or More Races", "Dos o m?s razas",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Homeless", "Sin Hogar",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Migrant", "Migrante",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("English Learners", "Estudiantes de ingl?s",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Female", "Mujer",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("White", "Blancx (Blanco)",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Asian", "Asi?ticx (Asi?tico)",susp_table$`Grupo de estudiantes`)
susp_table$`Grupo de estudiantes` <- gsub("Filipino", "Filipinx (Filipino)",susp_table$`Grupo de estudiantes`)

susp_table <- subset(susp_table, susp_table$`Grupo de estudiantes` != "Non-Binary Gender" & susp_table$`Grupo de estudiantes` != "Not Reported") 
# View(susp_table)

suspensions <- susp_table %>%
  mutate(across(c(`Matr?cula de estudiantes`, `Conteo no duplicado de estudiantes suspendidos`), comma)) %>%
  gt() %>% opt_all_caps() %>% 
  tab_header(title = md("**Tasas de suspensi?n por grupo de estudiantes, distrito escolar de Antelope Valley Union High, 2023-24**")) %>% 
  tab_footnote (footnote = md("Fuente: C?lculos de Catalyst California de datos del Departamento de Educaci?n de California, 2023-24.<br>
                                Nota: La categor?a de grupo de estudiantes para estudiantes g?nero neutral, no estaba disponible debido 
                              a la falta de datos. AIAN (por sus siglas en ingl?s) significa Indix Americanx y Nativx de Alaska (Indio Americano y 
                              Nativo de Alaska). "))%>% #, https://www.cde.ca.gov/ds/ 
  cols_align(
    align = c("left"),
    columns = everything()
  )%>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = `Grupo de estudiantes`,
              rows = `Grupo de estudiantes` == "TOTAL"))%>% 
  data_color(
    columns = c(`Tasa de suspensi?n`),
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
gtsave(suspensions, "suspensions_table_esp.png",path = "W:/Project/RJS/CTC/Github/CR/cancel_the_contract/Images/Spanish/", vwidth = 4000, vheight = 6000)
