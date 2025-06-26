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
# Get LAPD public safety station table
saf_boundaries <-  st_read(con_rda, 
                           query = "SELECT st_name, geom FROM geographies_la.la_public_safety_station_boundaries_2023") %>%
  st_transform(3310)
st_crs(saf_boundaries)

plot(st_geometry(saf_boundaries))

# get public safety stations-SPA xwalk
pubsafety_stations_spas_xwalk <- dbGetQuery(con_bv, "SELECT station, spa, spa_name, prc_area FROM bv_2023.crosswalk_pubsafety_stations_spas_2022") %>% arrange(station)
pubsafety_stations_spas_xwalk$station <- gsub(" Police", "", pubsafety_stations_spas_xwalk$station)
pubsafety_stations_spas_xwalk$station <- gsub(" Sheriff", "", pubsafety_stations_spas_xwalk$station)

# visualize SPA 1/AV public safety station boundaries
av_saf_spa_geom <- saf_boundaries %>% left_join(pubsafety_stations_spas_xwalk, 
                                             by = c("st_name" = "station"),
                                             relationship = "many-to-many") %>%
  filter(spa=='1') %>%
  mutate(saf_area=st_area(geom))

plot(st_geometry(saf_boundaries)) +
  plot(st_geometry(av_saf_spa_geom), col="blue", add=TRUE)


# get 2023 LASD RIPA stop address data - # 225753 unique stops
lasd_stops <- dbGetQuery(con_rda, statement="SELECT * FROM crime_and_justice.lasd_stops_incident_2018_2023
WHERE date_time LIKE '%/2023%';")


# get 2023 AV cts and shapes
ct_geos <- st_read(con_rda, query="SELECT ct_geoid as tract_geoid, geom_3310 as geom FROM geographies_ca.cb_2023_06_tract_500k WHERE countyfp='037';") %>%
  mutate(ct_area=st_area(geom))

# get AV CT place names
ct_places <- dbGetQuery(con_rda, statement = "SELECT ct_geoid as tract_geoid, place_geoid, place_name, namelsad FROM crosswalks.ct_place_2023") %>%
  right_join(ct_geos, by="tract_geoid")

# Get zip shapes
zips <- st_read(con_rda, query="SELECT zipcode, geom FROM geographies_la.lacounty_zipcodes_2022;") %>%
  st_transform(3310) %>%
  mutate(zip_area = st_area(geom)) 

plot(st_geometry(zips), col="red")

##### 1)	IDENTIFY AV STOPS  #####
#### BV Crosswalk ---------------------------------------------------------------
# recode patrol station to full name
lasd_recode <- lasd_stops %>% 
  mutate(patrol_station_recode = recode(patrol_station,
                                        AIR = "Airborne", 
                                        AVA = "Avalon",
                                        CAS = "Carson",
                                        CCS = "Unknown", 
                                        CEN = "Century",
                                        CER = "Cerritos",
                                        CPT = "Compton", 
                                        CSB = "County Services Bureau", 
                                        CVS = "Crescenta Valley",
                                        DET = "Unknown", 
                                        ELA = "East Los Angeles",
                                        EOB = "Emergency Outreach Bureau", 
                                        FPK = "Firestone Park",  
                                        IDT = "Industry",
                                        LAN = "Lancaster",
                                        LHS = "Malibu / Lost Hills",
                                        LKD = "Lakewood",
                                        LMT = "Lomita",
                                        LNX = "South Los Angeles", 
                                        MDR = "Marina Del Rey",
                                        NWK = "Norwalk",
                                        OCS = "Office of County Services",  
                                        OPS = "Office of Public Safety",  
                                        OTH = "Other", 
                                        PKB = "Parks Bureau", 
                                        PLM = "Palmdale", 
                                        PRV = "Pico Rivera",
                                        SCT = "Santa Clarita Valley",
                                        SDM = "San Dimas",
                                        SLA = "South Los Angeles",
                                        TEM = "Temple",
                                        TSB = "Transit Services Bureau", 
                                        WAL = "Walnut / Diamond Bar",
                                        WHD = "West Hollywood"))

View(as.data.frame(table(lasd_recode$patrol_station_recode, useNA = "ifany")))

lasd_spa <- lasd_recode %>% left_join(pubsafety_stations_spas_xwalk, 
                                      by = c("patrol_station_recode" = "station"),
                                      relationship = "many-to-many") 

# filter for SPA 1/AV LASD stops - 50274 stops
lasd_spa1 <- lasd_spa %>%
  filter(spa=="1")

# these don't have a SPA and probably need to be geo-coded - 9954
lasd_na_agency <- lasd_spa %>% 
  filter(is.na(spa))

View(as.data.frame(table(lasd_na_agency$patrol_station_recode, useNA = "ifany")))

#### Get AV CT Shapes, place names, and ZIP Codes ---------------------------------------------------------------
## AV CT Shapes
av_ct_geos <- st_intersection(av_saf_spa_geom, ct_geos) 
av_ct_places <- av_ct_geos %>%
  left_join(ct_places, by="tract_geoid",
            relationship = "many-to-many") %>%
  filter(!is.na(place_geoid))

# visually confirmed against https://public.tableau.com/app/profile/luz3725/viz/2020CensusData-AVBESTSTARTREGION5/MainPanel
plot(st_geometry(saf_boundaries)) +
  plot(st_geometry(av_saf_spa_geom), col="blue", add=TRUE) +
  plot(st_geometry(av_ct_geos), col=adjustcolor("green", alpha.f = 0.5), add=TRUE)

## AV Place names
av_place_names <- c(rbind(av_ct_places$place_name, av_ct_places$namelsad, av_ct_places$tract_name))

# # HK QA: Check if AV place names are associated with non-AV census tracts - not the case
# qa_cts <- ct_places %>%
#   select(tract_geoid, place_geoid, place_name) %>%
#   st_drop_geometry %>%
#   filter(place_name %in% av_place_names) %>%
#   left_join(av_ct_geos, by ="tract_geoid", relationship = "many-to-many") %>%
#   filter(is.na(st_name))


av_place_names <- toupper(unique(av_place_names))
av_place_names
# # HK QA: Check is place names is good to use (compare place names to stop cities)
# # missing AV place names included: Hasley Canyon, Sun Village, and Desert View Highlands
# # last two are unincorporated communities
# # based on place names we can estimate 47540 stops
av_places_string <- paste(av_place_names, collapse="|")
lasd_cities <- as.data.frame(table(lasd_stops$city, useNA = "ifany"))
lasd_av_cities <- lasd_cities %>%
  filter(str_detect(Var1, av_places_string))
# sum(lasd_av_cities$Freq) # 48330
# av_places_match <- as.data.frame(av_place_names) %>%
#   full_join(lasd_av_cities, by=c("av_place_names"="Var1"), keep = TRUE)


## AV ZIPs
av_saf_zips <- st_intersection(av_saf_spa_geom, zips)
av_saf_zips <- av_saf_zips %>%
  mutate(intersect_area = st_area(av_saf_zips)) %>%
  select(zipcode, intersect_area, zip_area) %>%
  group_by(zipcode) %>%
  summarise(total_intersect=sum(intersect_area)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  left_join(zips, by="zipcode") %>%
  st_drop_geometry() %>%
  mutate(
         zip_overlap = round(as.numeric(total_intersect)/as.numeric(zip_area)*100, 4),
         spa1_coverage = case_when(zip_overlap<=10 ~ "none",
                                   zip_overlap>=90 ~ "full", 
                                   zip_overlap>10 & zip_overlap <90 ~"partial",
                                   .default=NA))

av_zips <- zips %>% 
  filter(zipcode %in% av_saf_zips$zipcode) %>%
  left_join(av_saf_zips, by="zipcode") %>%
  filter(spa1_coverage=="full" | spa1_coverage=="partial") # 91342 drops out, should make sure that's ok

# visually confirmed ZIP and ZCTA matches to SPA 1
colors <- case_when(av_zips$spa1_coverage == "full" ~ adjustcolor("blue", alpha.f = 0.5), 
                    # av_zips$spa1_coverage == "none" ~ adjustcolor("grey", alpha.f = 0.5),
                    av_zips$spa1_coverage == "partial" ~ adjustcolor("orange", alpha.f = 0.5))

# Create the plot
plot(st_geometry(saf_boundaries))
plot(st_geometry(av_saf_spa_geom), col="black", add=TRUE)
plot(st_geometry(av_zips), col=colors, border = "green", add=TRUE)

# Optional: Add a legend
legend("bottomright", 
       legend = c("Full", "Partial"), # legend = c("Full", "None", "Partial"),
       fill = c(adjustcolor("blue", alpha.f = 0.5),
                # adjustcolor("grey", alpha.f = 0.5),
                adjustcolor("orange", alpha.f = 0.5)),
       title = "SPA1 Coverage")

### Filter LASD stops that didn't match to a SPA using AV place names and/or zipcodes - 1059 stops
av_lasd_na_stops <- lasd_na_agency %>%
  filter(city %in% lasd_av_cities$Var1 | 
           zip_code %in% av_zips$zipcode)

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
  select(-c(spa, spa_name, prc_area)) # 51,333 (prev. 51,227)

# ## HK QA: check where 56 additional stops came from
# # Increased counts for: CASTAIC - VAL VERDE (+44)
# # GORMAN (+1) - Not a place name in our list, matching on ZIP
# # LLANO (+1) - Not a place name in our list, matching on ZIP
# # PEARBLOSSOM (+1) - Not a place name in our list, matching on ZIP
# # UNINCORPORATED LANCASTER (+4)
# # UNINCORPORATED PALMDALE (+2)
# # UNINCORPORATED SANTA CLARITA (+3) - These were missing from old table
# # I think the old method was not using robust string matching, for example
# # it was only pulling CASTAIC-VAL VERDE when a ZIP Code in our list was also 
# # recorded; after fixing it pulls ALL CASTAIC-VAL VERDE even is ZIP is NA.
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
#   filter(is.na(count_old) | is.na(count_new) | count_old != count_new)
# 
# compare_stops <- new %>% 
#   left_join(old, suffix = c("_new", "_old"), keep=TRUE) %>%
#   filter(is.na(contact_id_old))

table_name <- "lasd_stops_spa1_2023"
schema <- 'data'
indicator <- "2023 LASD stops identified as taking place in SPA 1 (Antelope Valley)."
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
                     'Patrol Station Recoded')

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
