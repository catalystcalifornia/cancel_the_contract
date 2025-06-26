# Script modified from Bold Vision 2023 to crosswalk 2023 LASD stops to
# LA County SPAs to isolate SPA 1 (Antelope Valley) for analysis.
# The crosswalk won't be applicable for all 2023 stops, so unmatched stops
# will need to be attributed to SPA 1 through geocoding or some other method.


##### Workspace Setup ######
library(sf)
library(rmapshaper) # plot()
library(stringr)

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("cancel_the_contract")
con_rda <- connect_to_db("rda_shared_data")
con_bv <- connect_to_db("bold_vision")


##### Import Data #####
# Get AV/SPA 1 geom
lac_spa <- st_read(con_rda, 
                   query = "SELECT * FROM geographies_la.la_county_service_planning_areas_2022") %>%
  st_set_crs(4326) %>%
  st_transform(3310)
spa1_geom <- lac_spa %>% filter(SPA=="1")

# Get LAPD public safety station geoms
station_boundaries <-  st_read(con_rda, 
                           query = "SELECT st_name, geom FROM geographies_la.la_public_safety_station_boundaries_2023") %>%
  st_transform(3310)

# plot(st_geometry(station_boundaries, col="blue")) +
#   plot(st_geometry(spa1_geom), col=adjustcolor("magenta4", alpha.f = 0.5), add=TRUE)

# get patrol stations-SPA xwalk
station_spa_xwalk <- dbGetQuery(con_bv, "SELECT station, spa, spa_name, prc_area FROM bv_2023.crosswalk_pubsafety_stations_spas_2022") %>% arrange(station)
station_spa_xwalk$station <- gsub(" Police", "", station_spa_xwalk$station)
station_spa_xwalk$station <- gsub(" Sheriff", "", station_spa_xwalk$station)
  
# visualize SPA 1/AV public safety station boundaries
spa1_station_geoms <- station_boundaries %>% 
  left_join(station_spa_xwalk,
            by = c("st_name" = "station"),
            relationship = "many-to-many") %>%
  filter(spa=='1')

# # Note: SCT station accounts for ~23k stops but only overlaps SPA 1 about 23%
# plot(st_geometry(station_boundaries)) +
#   plot(st_geometry(spa1_station_geoms), col=adjustcolor("blue", alpha.f = 0.5), add=TRUE) +
#   plot(st_geometry(spa1_geom), col=adjustcolor("magenta4", alpha.f = 0.5), add=TRUE)

# get 2023 LASD RIPA stop address data - # 225753 unique stops
lasd_stops <- dbGetQuery(con_rda, statement="SELECT * FROM crime_and_justice.lasd_stops_incident_2018_2023
WHERE date_time LIKE '%/2023%';")

# get 2023 AV cts and shapes
ct_geos <- st_read(con_rda, query="SELECT ct_geoid as tract_geoid, geom_3310 as geom FROM geographies_ca.cb_2023_06_tract_500k WHERE countyfp='037';") %>%
  mutate(ct_area=st_area(geom))

# get AV CT place names
ct_places <- dbGetQuery(con_rda, statement = "SELECT ct_geoid as tract_geoid, place_geoid, place_name, namelsad FROM crosswalks.ct_place_2023") %>%
  right_join(ct_geos, by="tract_geoid")

# get neighborhoods
neighborhoods <- st_read(con_rda, query="SELECT name, type, geom FROM geographies_la.latimes_lacounty_neighborhoods_2020") %>%
  mutate(neighborhood_area=st_area(geom))

# Get zip shapes
zips <- st_read(con_rda, query="SELECT zipcode, geom FROM geographies_la.lacounty_zipcodes_2022;") %>%
  st_transform(3310) %>%
  mutate(zip_area = st_area(geom)) 

plot(st_geometry(zips), col="red")

##### 1)	IDENTIFY AV STOPS  #####
#### Get AV place names, and ZIP Codes ---------------------------------------------------------------
## AV Neighborhoods - keep names where at least 50% of neighborhood intersects SPA1
av_nb_intersect <- st_intersection(spa1_geom, neighborhoods) %>%
  mutate(overlap=st_area(geometry)) %>%
  mutate(prc_overlap=as.numeric(overlap)/as.numeric(neighborhood_area)*100) %>% 
  filter(as.numeric(prc_overlap)>=50) %>%
  select(name)

av_neighborhoods <- neighborhoods %>% 
  filter(name %in% av_nb_intersect$name)

# # Note some of SPA 1 not covered by an LA Time neighborhood (e.g., Gorman)
# # List is still more expansive then using census tract to place crosswalk for names
# plot(st_geometry(station_boundaries)) +
#   plot(st_geometry(spa1_geom), col=adjustcolor("magenta4", alpha.f = 0.5), add=TRUE) +
#   plot(st_geometry(av_neighborhoods), col=adjustcolor("green", alpha.f = 0.5), add=TRUE) 

# City names as reported by the County of LA Public Health website: http://publichealth.lacounty.gov/chs/services.htm
# and here: http://publichealth.lacounty.gov/cardio/docs/2012-08-01%20SPA%20Map%20with%20cities_all.pdf
cities <- c("Acton", "Del Sur", "Gorman", "Green Valley", "Hi Vista", "Lake Hughes", "Lake Los Angeles",
            "Lancaster", "Leona Valley", "Littlerock", "Llano", "Palmdale",
            "Pearblossom", "Quartz Hill", "Redman", "Sandberg", "South Antelope Valley", 
            "Valyermo")

av_cities <- toupper(unique(append(cities, av_neighborhoods$name)))


# HK QA: Check is place names is good to use (compare place names to stop cities)
# missing AV place names included: Hasley Canyon, Sun Village, and Desert View Highlands
# last two are unincorporated communities
# based on place names we can estimate 47540 stops
av_places_string <- paste(av_cities, collapse="|")
lasd_cities <- as.data.frame(table(lasd_stops$city, useNA = "ifany"))
lasd_av_cities <- lasd_cities %>%
  filter(str_detect(Var1, av_places_string))
sum(lasd_av_cities$Freq) # 28662
av_nomatch <- as.data.frame(av_cities) %>%
  full_join(lasd_av_cities, by=c("av_cities"="Var1"), keep = TRUE) %>%
  filter(is.na(Var1))
  

## AV ZIPs
spa1_zips <- st_intersection(spa1_geom, zips) %>%
  mutate(overlap=as.numeric(st_area(geometry))) %>%
  mutate(prc_overlap=as.numeric(overlap)/as.numeric(zip_area)*100) %>%
  filter(prc_overlap >1 )

av_zips <- zips %>% filter(zipcode %in% spa1_zips$zipcode)


# Create the plot
plot(st_geometry(station_boundaries)) +
plot(st_geometry(spa1_geom), col=adjustcolor("magenta4", alpha.f = 0.3), add=TRUE) +
plot(st_geometry(av_zips), col=adjustcolor("green", alpha.f = 0.5), border="red", add=TRUE)

# Optional: Add a legend
legend("bottomright", 
       legend = c("Full", "Partial"), # legend = c("Full", "None", "Partial"),
       fill = c(adjustcolor("blue", alpha.f = 0.5),
                # adjustcolor("grey", alpha.f = 0.5),
                adjustcolor("orange", alpha.f = 0.5)),
       title = "SPA1 Coverage")
# #### BV Crosswalk ---------------------------------------------------------------
# # recode patrol station to full name
# lasd_recode <- lasd_stops 
# 
# View(as.data.frame(table(lasd_recode$patrol_station_recode, useNA = "ifany")))
# 
# lasd_spa <- lasd_recode %>% left_join(pubsafety_stations_spas_xwalk, 
#                                       by = c("patrol_station_recode" = "station"),
#                                       relationship = "many-to-many") 
# 
# # filter for SPA 1/AV LASD stops - 50274 stops
# lasd_spa1 <- lasd_spa %>%
#   filter(spa=="1") %>%
#   mutate(source="xwalk")
# 
# # these don't have a SPA and probably need to be geo-coded - 9954
# lasd_na_agency <- lasd_spa %>% 
#   filter(is.na(spa))
# 
# View(as.data.frame(table(lasd_na_agency$patrol_station_recode, useNA = "ifany")))



### Filter LASD stops that didn't match to a SPA using AV place names and/or zipcodes - 1059 stops
av_lasd_na_stops <- lasd_na_agency %>%
  filter(str_detect(city, av_places_string) | 
           zip_code %in% av_zips$zipcode) %>%
  mutate(source="filter")

# # HK QA: Check which stops are in "partial" SPA 1 ZIP codes
# # Cross checking against LA County geography that parts of the 
# # ZIP Codes outside of SPA 1 overlap the San Gabriel Mountains
# # Additionally only 2 stops are connected with the partial ZIPS
# # Checked in google maps and they coordinates are in SPA 1
# partial_zips <- av_zips %>%
#   filter(spa1_coverage=="partial") %>%
#   select(zipcode)
# av_partial_stops <- av_lasd_na_stops %>%
#   filter(zip_code %in% partial_zips$zipcode)

View(as.data.frame(table(av_lasd_na_stops$patrol_station_recode, useNA = "ifany")))

#### Combine all SPA 1 LASD stops and export to pg ------------------------------------------------------
all_av <- rbind(lasd_spa1, av_lasd_na_stops) %>%
  select(-c(spa, spa_name, prc_area)) # 51,489 (prev. 51,227)

# # ## HK QA: check where 262 additional stops came from
# # # Changes in counts for: 
# # # NEWHALL (+82) - added to places filter
# # # CASTAIC - VAL VERDE (+44) - fixed place filter to use str_detect
# # # GORMAN (+40) - added to places filter
# # # CANYON COUNTRY (+21) - added to places filter
# # # VALENCIA (+7) - added to places filter
# # # UNINCORPORATED LANCASTER (+4) - fixed place filter to use str_detect
# # # UNINCORPORATED SANTA CLARITA (+3) - fixed place filter to use str_detect
# # # PEARBLOSSOM (+2) - added to places filter
# # # SAUGUS (+2) - added to places filter
# # # UNINCORPORATED PALMDALE (+2) - fixed place filter to use str_detect
# # # CANYON COUNTRY EAST(+1) - added to places filter
# # # DEL SUR(+1) - added to places filter
# # # HI VISTA(+1) - added to places filter
# # # LLANO (+1) - added to places filter
# # # SOUTH ANTELOPE VALLEY (+1) - added to places filter
# 
# old <- dbGetQuery(con, "SELECT * FROM data.lasd_stops_spa1_2023_old;")
# old_count <- old%>%
#   select(city) %>%
#   group_by(city) %>%
#   summarise(count = n())
# 
# new <- all_av
# 
# new_count <- new %>%
#   select(city) %>%
#   group_by(city) %>%
#   summarise(count = n())
# 
# compare_counts <- full_join(old_count, new_count, by="city", keep=TRUE, suffix = c("_old", "_new")) %>%
#   filter(is.na(count_old) | is.na(count_new) | count_old != count_new) %>%
#   mutate(difference = count_new - count_old)
# 
# compare_stops <- new %>%
#   left_join(old, suffix = c("_new", "_old"), keep=TRUE) %>%
#   filter(is.na(contact_id_old))

# ### HK QA: Check incidence rates of SPA 1 patrols to see if other cities should be in places filter
# incidence <- as.data.frame(table(all_av$city, all_av$source))
# incidence <- incidence %>% 
#   pivot_wider(names_from = Var2, values_from = Freq, names_prefix = "source_") %>%
#   mutate(total_source = source_filter + source_xwalk) %>%
#   left_join(lasd_cities, by="Var1") %>%
#   rename(total_lasd = Freq) %>%
#   mutate(prc=total_source/total_lasd*100,
#          in_filter=ifelse(Var1 %in% lasd_av_cities$Var1, "yes", "no")) 
# # Filter for just names not already in our place filter with incidence rate > 80% and < 100%
# add_cities <- incidence %>%
#   filter(total_source != total_lasd) %>%
#   filter(in_filter=="no" & prc>80 & prc<100)

table_name <- "lasd_stops_spa1_2023"
schema <- 'data'
indicator <- "2023 LASD stops identified as taking place in SPA 1 (Antelope Valley)"
source <- "County of Los Angeles Sheriff Officer Contacts Incident Details imported to rda_shared_data."

dbWriteTable(con, Id(schema, table_name), all_av,
           overwrite = FALSE, row.names = FALSE)

qa_filepath <- "W:\\Project\\RJS\\CTC\\Documentation\\QA_lasd_geocode.docx" 

# comment on table and columns
column_names <- colnames(all_av)
column_comments <- c('Contact ID',
                     'Date and time',
                     'Date reformatted',
                     'Patrol station',
                     'Number of minutes of stop',
                     'Street number',
                     'Direction',
                     'Street name',
                     'Street type (e.g., RD, ST, DR, AVE)',
                     'Suite number or unit',
                     'Cross street',
                     'Landmark',
                     'Full Street',
                     'City',
                     'State',
                     'ZIP Code',
                     'K through 12 school',
                     'School name',
                     'Call for service',
                     'Civilians contacted',
                     'Object ID',
                     'Patrol Station Recoded',
                     'Method of attributing stop to SPA 1: patrol station to SPA ("xwalk") or by place data like city or ZIP Code ("filter")')

# add comment on table and columns using add_table_comments() (accessed via credentials script) 
add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments) 


#### Check LASD Stop Address Data Quality ---------------------------------------------------------------
# ## check if full_street is best column to use for geocoding
# # Compare "street", "cross_street", and "landmark" fields
# # Notes: each stop has at least one of street, cross_street, or landmark data
# # If a stop has ANY street data, that will be the stop's "full_street" (cross_street and landmark are ignored)
# # If there is cross_street and landmark data, full_street = cross_street + landmark
# # Otherwise full_street is whichever type is exclusive to the stop.
# 
# # Most (72.8%) stops  have exclusively cross_street data
# # 23.7% have exclusively "street" data
# # 2% have exclusively "landmark" data
# # <1% have landmark and cross_street data
# # the remaining stops (<1% are some combination that includes street)
# # There are no stops missing all 3
# 
# lasd_stops <- lasd_stops %>%
#   mutate(is_cross_street = !is.na(cross_street),
#          is_street = !is.na(street),
#          is_landmark = !is.na(landmark))
# 
# check_cross_streets <- lasd_stops %>%
#   filter(is_cross_street==FALSE & is_street==FALSE & is_landmark==FALSE)
# # nrow(check_cross_streets) # 0; no stops missing all 3
# 
# check_cross_streets <- lasd_stops %>%
#   filter(is_cross_street==TRUE & is_street==TRUE & is_landmark==TRUE)
# # nrow(check_cross_streets) # 1526; stops have "street", "cross_street" and "landmark"
# # full_street is street (and accompanying cols like "direction", "suite", etc.)
# 
# check_cross_streets <- lasd_stops %>%
#   filter(is_cross_street==TRUE & is_street==FALSE & is_landmark==FALSE)
# # nrow(check_cross_streets) # 767044; stops exclusively reported at "cross_street"
# # full_street is ONLY cross_street
# 
# check_cross_streets <- lasd_stops %>%
#   filter(is_cross_street==TRUE & is_street==FALSE & is_landmark==TRUE)
# # nrow(check_cross_streets) # 4063; stops have "cross_street" and "landmark"
# # full_street is cross_street + landmark
# 
# check_cross_streets <- lasd_stops %>%
#   filter(is_cross_street==TRUE & is_street==TRUE & is_landmark==FALSE)
# # nrow(check_cross_streets) # 6044; stops have "cross_street" and "street"
# # full_street is street (and accompanying cols like "direction", "suite", etc.)
# 
# check_cross_streets <- lasd_stops %>%
#   filter(is_cross_street==FALSE & is_street==TRUE & is_landmark==FALSE)
# # nrow(check_cross_streets) # 249296; stops exclusively reported at "street"
# # full_street is street (and accompanying cols like "direction", "suite", etc.)
# 
# check_cross_streets <- lasd_stops %>%
#   filter(is_cross_street==FALSE & is_street==FALSE & is_landmark==TRUE)
# # nrow(check_cross_streets) # 21556; stops exclusively reported at "landmark"
# # full_street is ONLY landmark
# 
# 
# ## Look at data quality and availability of ZIP Code and City fields
# # Notes: city fields have no missing data, however "unincorporated areas" have 
# # have a concerning lack of information. For example, unincorporated LAC is
# # associated with at least 60 different ZIP Codes and 86% (7596) of these stops 
# # have no ZIP code provided.
# # "UNINCORPORATED AREA-LOS ANGELES COUNTY" is not well understood as a "city"
# # by geocoders so we should be particularly wary of these geocoding results.
# # We could attempt to impute ZIPs but it would be very time-intensive.
# 
# lasd_stop_cities <- as.data.frame(table(lasd_stops$city, useNA = "ifany"))
# # Sometimes multiple places are provided in city, or an ambigious name like "Unincorporated area"; all have a "-":
# # Looking more closely at ZIP Code availability for these cities, ZIP Code availability doesn't seem to be an issue  
# # for named places, however unincorporated is an umbrella term and can refer to many places
# lasd_multi_places <- lasd_stops %>%
#   filter(grepl("-", city)==TRUE) %>%
#   select(city, zip_code) %>%
#   group_by(city) %>%
#   summarise(
#     count_na_zip = sum(is.na(zip_code)),
#     count_zip = sum(!is.na(zip_code)),
#     .groups = 'drop'
#   )
# 
# # Drill down a bit further on UNINCORPORATED AREA-LOS ANGELES COUNTY
# unincorporated_lac <- lasd_stops %>%
#   filter(city=="UNINCORPORATED AREA-LOS ANGELES COUNTY") 
# 
# unincorporated_lac_zip <- unincorporated_lac %>%
#   pull(zip_code) %>%
#   table(useNA = "ifany") %>%
#   as.data.frame() # 7596 have no ZIP Code (86%)
# 
# nrow(unincorporated_lac_zip)-1 # 60 different ZIP Codes associated with unincorporated LAC County areas
# 
# # Back up and look generally at ZIP Codes across all stops
# lasd_stop_zips <- lasd_stops %>%
#   pull(zip_code) %>%
#   table(useNA = "ifany") %>%
#   as.data.frame() %>%
#   rename(zip_code=".") %>%
#   mutate(zip_code=as.character(zip_code))
# 
# # ZIP data entry issue:
# # zip codes not associated with California 478 (450 are 5 digits, 28 have less than 5 digits)
# # looks like typos or sometimes dropping the leading "9"
# ca_zips <- read.csv("W:\\Data\\Geographies\\ZIP Code Database\\2024\\zip_code_database.csv") %>%
#   filter(state=="CA") %>%
#   mutate(zip = as.character(zip))
# unknown_zips <- lasd_stop_zips %>%
#   left_join(ca_zips, by=c("zip_code"="zip")) %>%
#   filter(is.na(state) & !is.na(zip_code)) # 242 unique ZIPs not associated with CA (impacts 478 stops;28 less than 5 digits, 450 are 5 digits)
# sum(unknown_zips$Freq)-28
# 
# lasd_stops_zip_unknown <- lasd_stops %>%
#   filter(zip_code %in% unknown_zips$zip_code)
# 
# ## Additional data quality notes:
# # Landmark-exclusive stops are largely missing ZIPs but ALL have city data
# table(check_cross_streets$zip_code,useNA = "ifany") # 21349 don't include ZIP
# table(check_cross_streets$city,useNA = "ifany") # all have a city
# 
# # Some stops have "N/A" strings (can occur in any of the address-related cols), 
# # in the respective contexts above, the "N/A" is added to "full_street" even though it shouldn't be
# 
# # Some stops have cross streets recorded under "street"; may want to recode as is_cross_street 
# check_cross_streets <- lasd_stops %>%
#   filter(is_cross_street==FALSE & grepl("/", street, fixed=TRUE)) 
