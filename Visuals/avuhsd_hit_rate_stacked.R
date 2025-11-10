# Create a static visual to show hit rate: out of searches how many yielded contraband
# Author: CR

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

# source visual script
source("./Visuals/visual_fx.R")

#connect to postgres
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("cancel_the_contract")


# Grab df
df<-dbGetQuery(con, "SELECT * FROM analysis_stops_hitrate_avuhsd")

# Try visualizing with a waffle graph----------------------

library(waffle)

waffle(
  c('No contraband found=73%' = 72.88136, 'Contraband found=27%' = 27.11864), 
  rows = 10, colors = c(teal, mauve),
  title = 'The majority of searches conducted by police in AVUHSD yield no contraband', legend_pos="bottom"
)

# Try single bar graph stacked--------------------------

#### Pre-information:

# Set up pre-information:
title_text<-"More than 70% of searches made by police yield no contraband"
indicator<-"Police Hit Rate AVUHSD"
subtitle_text<-paste0(dict$indicator[dict$indicator_short==indicator])
caption_text<-paste0("Source: Catalyst California calculations of ",dict$source[dict$indicator_short==indicator]," data, ", dict$year[dict$indicator_short==indicator],". ",dict$race_note[dict$indicator_short==indicator]) 

# Dynamically adjust dimensions of the output

# Base width/height
base_width <- 7   # inches
base_height <- 5  # inches

# Adjust height based on number of rows (bars)
height <- base_height + 0.9

# Adjust width based on title length
title_length <- nchar(title_text)
width <- base_width + 0.05 * title_length  # long titles get extra width
wrap_width <- round(width * 12)
caption_text <- str_wrap(caption_text, width = wrap_width)


#### Graph

final_visual<-df%>%
  mutate(reportingcategory_re=ifelse(reportingcategory_re %in% "total", "Students searched by police", reportingcategory_re))%>%
  ggplot(aes(x = reportingcategory_re, y = rate, fill = contraband_evidence_discovered_none)) +
  geom_col() +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 8)  # wrap at ~15 characters
  )+
  
  geom_text(
    aes(label = paste0(round(rate, 1), "%")),
    position = position_stack(vjust = 0.5),
    family = font_bar_label, 
    size=10)+
  
scale_fill_manual(
  name = "Contraband Found",              # legend title
  values = c("1" = teal, "0" = mauve),  # manual colors
  labels = c("1" = "No", "0" = "Yes")    # legend item labels
)+
  
  labs(title = str_wrap(title_text, width=65),
       subtitle = str_wrap(subtitle_text, width=75),
       caption=caption_text) + 
  xlab("") +
  ylab("")+
  coord_flip()+
  theme_minimal()+
  theme(
    legend.title = element_text(
      size = 8,        # font size
      family = font_caption,  # optional: set your font
      colour = black        # optional: set color
    ),
    legend.text = element_text(
      size = 8,     
      family = font_axis_label,  
      colour = black
    ),
        axis.text.x = element_text( size = 12,         
                                    family = font_axis_label,  
                                    colour = black),
        plot.caption = element_text(hjust = 0.0, size = 12, colour = black, family = font_caption),
        plot.title =  element_text(hjust = 0.0, size = 25, colour = black, family = font_title), 
        plot.subtitle = element_text(hjust = 0.0, size = 22, colour = black, family = font_subtitle),
        axis.ticks = element_blank(),
        # grid line style
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        panel.grid.major.y = element_blank())

# Define base file path for saving visuals
export_dir <- here::here("Visuals", "Exports")
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

outfile <- file.path(export_dir, paste0(indicator, "_stackedbar.png"))

ragg::agg_png(outfile, width = width, height = height, units = "in", res = 150)
print(final_visual)   # <- draw the plot

dev.off()


final_visual

