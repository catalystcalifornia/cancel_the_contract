## LASD 2023 RIPA data Import##
# Referencing code used in BV for 2022 LASD RIPA data import: https://github.com/catalystcalifornia/boldvision_2023/blob/main/Analysis/Systems%20Impact/si_youth_arrest.R


# packages
library(tidyverse)
library(readxl)
library(sf)
library(httr)
library(jsonlite)
library(sp)
#remotes::install_github("catalystcalifornia/RC")
library(RC) # RaceCounts package created by RDA
library(janitor)
library(lubridate)

## data setup ##
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")

# # UPLOAD LA County PERSON-level RIPA Data to Postgres ---------------------------------------

lasd_person <- read_csv("W:/Data/Crime and Justice/LASD/2023/lasd_stops_person_2023.csv")
df <- lasd_person
# 
# # clean up column names

df <- df %>% clean_names()
colnames(df) <- tolower(colnames(df))
colnames(df) <- gsub("\\?.*", "", colnames(df))
colnames(df) <- gsub("\\#.*", "", colnames(df))
colnames(df) = gsub("-", "", colnames(df)) # remove hyphens
colnames(df) = gsub(" ", "_", colnames(df))
 
# # rename columns
df <- df %>%

rename("reasonable_suspicion_officer_witnessed_commission_crime" = "reasonable_suspicion_that_the_officer_witnessed_commission_of_a_crime") %>%

rename("reasonable_suspicion_action_indicative_casing_victimlocation" = "reasonable_suspicion_that_the_persons_actions_was_indicative_of_casing_a_victim_or_location") %>%

rename("reasonable_suspicion_action_indicative_drugtransaction" = "reasonable_suspicion_that_the_persons_actions_was_indicative_of_a_drug_transaction") %>%

rename("reasonable_suspicion_action_indicative_violentcrime" = "reasonable_suspicion_that_the_persons_actions_was_indicative_of_engaging_in_a_violent_crime") %>%

rename("reasonable_suspicion_that_the_person_matched_description" = "reasonable_suspicion_that_the_person_matched_suspect_description") %>%

rename("reasonable_suspicion_person_witness_or_victim_ofsuspect" = "reasonable_suspicion_that_the_person_was_a_witness_or_victim_id_of_suspect_at_the_scene") %>%

rename("reasonable_suspicion_person_carrying_suspicious_object" = "reasonable_suspicion_that_the_person_may_be_carrying_suspicious_object") %>%

rename("reasonable_suspicion_lookout" = "reasonable_suspicion_that_the_person_was_suspected_of_acting_as_a_lookout") %>%

rename("contraband_evidence_discovered_cell_phone_electronics" = "contraband_evidence_discovered_cell_phone_s_or_electronic_device_s") %>%

rename("result_of_contact_custodial_arrest_warrant" = "result_of_contact_custodial_arrest_pursuant_to_outstanding_warrant") %>%

rename("result_of_contact_custodial_arrest_no_warrant" = "result_of_contact_custodial_arrest_without_warrant_offense_codes") %>%

rename("result_of_contact_noncriminal_caretaking_transport" = "result_of_contact_noncriminal_transport_or_care_taking_transport") %>%

rename("result_of_contact_contacted_legal_guardian" = "result_of_contact_contacted_parent_legal_guardian_or_other_person_responsible_for_minor") %>%

rename("result_of_contact_contacted_dept_homeland_security" = "result_of_contact_contacted_u_s_department_of_homeland_security") %>%

rename("result_of_contact_referral_school_staff" = "result_of_contact_referral_to_school_counselor_or_other_support_staff")%>%
  
rename("property_seized_cell_phones_or_electronic_devices"="property_seized_cell_phone_s_or_electronic_device_s")

# view column names
colnames<-as.data.frame(colnames(df))

table_name <- "lasd_stops_person_2018_2023"
schema <- 'crime_and_justice'
source <- "County of Los Angeles Sheriff Officer Contacts Person Details downloaded here: https://data.lacounty.gov/datasets/lacounty::sheriff-officer-contacts-person-details-/about
Years for data are: 2018-2023. R script: W:\\Project\\RJS\\CTC\\Github\\JZ\\cancel_the_contract\\Data Prep\\ripa_lasd_2023_import.R"
# 
dbWriteTable(con, Id(schema, table_name), df,
           overwrite = TRUE, row.names = FALSE)
# 
comment <- paste0("
              COMMENT ON TABLE ", schema, ".", table_name,  " IS '"," from ", source, ".';")

dbSendQuery(con, comment)

# #comment on columns

column_comment <- paste0("
                COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contact_id IS 'Contact ID';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. person_id IS 'Person ID';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. street_number IS 'Street #';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. street_direction IS 'Street Direction';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. street_name IS 'Street Name';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. street_type IS 'Street Type';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. suite IS 'Suite';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. cross_street IS 'Cross Street';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. landmark IS 'Landmark';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. full_address IS 'Full Address';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. city IS 'City';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. state IS 'State';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. zip_code IS 'Zip Code';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. age IS 'Age';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. k12_student IS 'K12 Student';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. limited_or_no_english_fluency IS 'Limited or No English Fluency';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. perception_made IS 'Perception Made';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. homeless IS 'Homeless';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. male IS 'Male';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. female IS 'Female';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. transgender_man IS 'Transgender Man';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. transgender_woman IS 'Transgender Woman';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. gender_non_conforming IS 'Gender Non-Conforming';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. lgbt IS 'LGBT';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. asian IS 'Asian';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. black_african_american IS 'Black/African American';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. hispanic_latino_latina IS 'Hispanic/Latino/Latina';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. middle_eastern_south_asian IS 'Middle Eastern/South Asian';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. native_american IS 'Native American';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. pacific_islander IS 'Pacific Islander';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. white IS 'White';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. deaf_or_difficulty_hearing IS 'Deaf or Difficulty Hearing';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. speech_impaired_or_limited_use_of_language IS 'Speech Impaired or Limited Use of Language';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. blind_or_limited_vision IS 'Blind or Limited Vision';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. mental_health_condition IS 'Mental Health Condition';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. intellectually_or_developmental_disability IS 'Intellectually or Developmental Disability';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. other_disabilities IS 'Other Disabilities';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. hyperactive_or_impulsive_behavior IS 'Hyperactive or Impulsive Behavior';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. no_disabilities IS 'No Disabilities';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reason_for_contact IS 'Reason for Contact';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reason_for_contact_narrative IS 'Reason for Contact Narrative';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. traffic_violation_type IS 'Traffic Violation Type';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. traffic_violation_offense_code IS 'Traffic Violation Offense Code';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reasonable_suspicion_officer_witnessed_commission_crime IS 'Reasonable Suspicion that the Officer Witnessed Commission of a Crime';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reasonable_suspicion_that_the_person_matched_description IS 'Reasonable Suspicion that the Person Matched Suspect Description';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reasonable_suspicion_person_witness_or_victim_ofsuspect IS 'Reasonable Suspicion that the Person was a Witness or Victim ID of Suspect at the Scene';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reasonable_suspicion_person_carrying_suspicious_object IS 'Reasonable Suspicion that the Person may be Carrying Suspicious Object';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reasonable_suspicion_action_indicative_casing_victimlocation IS 'Reasonable Suspicion that the Persons Actions was Indicative of Casing a Victim or Location';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reasonable_suspicion_lookout IS 'Reasonable Suspicion that the Person was Suspected of Acting as a Lookout';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reasonable_suspicion_action_indicative_drugtransaction IS 'Reasonable Suspicion that the Persons Actions was Indicative of a Drug Transaction';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. reasonable_suspicion_action_indicative_violentcrime IS 'Reasonable Suspicion that the Persons Actions was Indicative of Engaging in a Violent Crime';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. officer_had_other_reasonable_suspicions_of_a_crime IS 'Officer had Other Reasonable Suspicions of a Crime';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. offense_code_of_the_reasonable_suspicion IS 'Offense Code of the Reasonable Suspicion';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. education_code_section IS 'Education Code Section';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. education_code_subdivision IS 'Education Code Subdivision';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. person_removed_from_vehicle_by_order IS 'Person Removed from Vehicle by Order';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. person_removed_from_vehicle_by_physical_contact IS 'Person Removed from Vehicle by Physical Contact';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. field_sobriety_test_conducted IS 'Field Sobriety Test Conducted';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. curbside_detention IS 'Curbside Detention';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. person_handcuffed_or_flex_cuffed IS 'Person Handcuffed or Flex Cuffed';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. patrol_car_detention IS 'Patrol Car Detention';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. canine_removed_from_vehicle_or_used_to_search IS 'Canine Removed from Vehicle or Used to Search';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. firearm_pointed_at_person IS 'Firearm Pointed at Person';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. firearm_discharged_or_used IS 'Firearm Discharged or Used';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. electronic_control_device_used IS 'Electronic Control Device Used';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. impact_projectile_discharged_or_used IS 'Impact Projectile Discharged or Used';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. canine_bit_or_held_person IS 'Canine Bit or Held Person';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. baton_or_other_impact_weapon_used IS 'Baton or Other Impact Weapon Used';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. chemical_spray_used IS 'Chemical Spray Used';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. other_physical_or_vehicle_contact IS 'Other Physical or Vehicle Contact';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. person_photographed IS 'Person Photographed';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_of_person_conducted IS 'Search of Person Conducted';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. asked_consent_to_search_person IS 'Asked Consent to Search Person';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. asked_consent_to_search_person_given IS 'Asked Consent to Search Person Given';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_of_property_conducted IS 'Search of Property Conducted';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. asked_consent_to_search_property IS 'Asked Consent to Search Property';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. asked_consent_to_search_property_given IS 'Asked Consent to Search Property Given';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_was_seized IS 'Property was Seized';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. vehicle_impounded IS 'Vehicle Impounded';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. admission_or_written_statement_obtained_from_student IS 'Admission or Written Statement Obtained from Student';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. no_action_taken IS 'No Action Taken';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_consent_given IS 'Search Consent Given';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_officer_safety_safety_of_others IS 'Search Basis- Officer Safety/Safety of Others';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_search_warrant IS 'Search Basis- Search Warrant';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_condition_of_parole_probation_prcs_mandatory_supervision IS 'Search Basis- Condition of Parole/Probation/PRCS/Mandatory Supervision';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_suspected_weapons IS 'Search Basis- Suspected Weapons';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_visible_contraband IS 'Search Basis- Visible Contraband';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_odor_of_contraband IS 'Search Basis- Odor of Contraband';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_canine_detection IS 'Search Basis- Canine Detection';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_evidence_of_crime IS 'Search Basis- Evidence of Crime';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_incident_to_arrest IS 'Search Basis- Incident to Arrest';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_exigent_circumstances_emergency IS 'Search Basis- Exigent Circumstances/Emergency';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_vehicle_inventory IS 'Search Basis- Vehicle inventory';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_suspected_of_violating_of_school_policy IS 'Search Basis- Suspected of Violating of School Policy';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. search_basis_reason IS 'Search Basis- Reason';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seizure_basis_safekeeping_as_allowed_by_law_statute IS 'Property Seizure Basis- Safekeeping as Allowed by Law/Statute';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seizure_basis_contraband IS 'Property Seizure Basis- Contraband';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seizure_basis_evidence IS 'Property Seizure Basis- Evidence';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seizure_basis_impound_of_vehicle IS 'Property Seizure Basis- Impound of Vehicle';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seizure_basis_abandoned_property IS 'Property Seizure Basis- Abandoned Property';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seizure_basis_suspected_of_violating_of_school_policy IS 'Property Seizure Basis- Suspected of Violating of School Policy';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_firearm_s IS 'Property Seized- Firearm(s)';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_ammunition IS 'Property Seized- Ammunition';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_weapon_s_other_than_a_firearm IS 'Property Seized- Weapon(s) Other than a Firearm';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_drug_narcotics IS 'Property Seized- Drug/Narcotics';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_alcohol IS 'Property Seized- Alcohol';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_money IS 'Property Seized- Money';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_drug_paraphernalia IS 'Property Seized- Drug Paraphernalia';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_suspected_stolen_property IS 'Property Seized- Suspected Stolen Property';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_cell_phones_or_electronic_devices IS 'Property Seized- Cell Phone(s) or Electronic Device(s)';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_vehicle IS 'Property Seized- Vehicle';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. property_seized_other_contraband_or_evidence IS 'Property Seized- Other Contraband or Evidence';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_none IS 'Contraband Evidence Discovered- None';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_firearm_s IS 'Contraband Evidence Discovered- Firearm(s)';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_ammunition IS 'Contraband Evidence Discovered- Ammunition';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_weapon_s_other_than_a_firearm IS 'Contraband Evidence Discovered- Weapon(s) Other than a Firearm';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_drugs_narcotics IS 'Contraband Evidence Discovered- Drugs/Narcotics';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_alcohol IS 'Contraband Evidence Discovered- Alcohol';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_money IS 'Contraband Evidence Discovered- Money';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_drug_paraphernalia IS 'Contraband Evidence Discovered- Drug Paraphernalia';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_suspected_stolen_property IS 'Contraband Evidence Discovered- Suspected Stolen Property';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_cell_phone_electronics IS 'Contraband Evidence Discovered- Cell Phone(s) or Electronic Device(s)';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. contraband_evidence_discovered_other IS 'Contraband Evidence Discovered- Other';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_no_action IS 'Result of Contact- No Action';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_warning IS 'Result of Contact- Warning';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_warning_offense_codes IS 'Result of Contact- Warning Offense Codes';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_citation_for_infraction IS 'Result of Contact- Citation for infraction';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_citation_for_infraction_offense_codes IS 'Result of Contact-Citation for Infraction Offense Codes';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_in_field_cite_and_release IS 'Result of Contact-  In-field Cite and Release';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_in_field_cite_and_release_offense_codes IS 'Result of Contact-  In-field Cite and Release Offense Codes';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_custodial_arrest_warrant IS 'Result of Contact- Custodial Arrest Pursuant to Outstanding Warrant ';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_custodial_arrest_without_warrant IS 'Result of Contact- Custodial Arrest Without Warrant';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_custodial_arrest_no_warrant IS 'Result of Contact- Custodial Arrest Without Warrant Offense Codes';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_field_interview_card_completed IS 'Result of Contact- Field Interview Card Completed';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_noncriminal_caretaking_transport IS 'Result of Contact- Noncriminal Transport or Care Taking Transport';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_contacted_legal_guardian IS 'Result of Contact- Contacted Parent/Legal Guardian or other Person Responsible for Minor';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_psychiatric_hold IS 'Result of Contact- Psychiatric Hold';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_contacted_dept_homeland_security IS 'Result of Contact- Contacted U.S. Department of Homeland Security';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_referral_to_school_administrator IS 'Result of Contact- Referral to School Administrator';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. result_of_contact_referral_school_staff IS 'Result of Contact- Referral to School Counselor or Other Support Staff';
COMMENT ON COLUMN crime_and_justice.lasd_stops_person_2018_2023. object_id IS 'ObjectId';

")

dbSendQuery(con, column_comment)

# UPLOAD LA County INCIDENT-level RIPA Data to Postgres ---------------------------------------

lasd_incident <- read_csv("W:/Data/Crime and Justice/LASD/2023/lasd_stops_incident_2023.csv") %>% clean_names()

df <- lasd_incident

### clean up time-zone

df_final <- df %>% mutate(date_reformatted = parse_date_time(date_time, '%m/%d/%Y %I:%M:%S %p'),
date_reformatted = as_datetime(date_reformatted, tz = "America/Los_Angeles")) %>% select(contact_id, date_time, date_reformatted, everything())

# grab column names to help with column metadata

colnames<-as.data.frame(colnames(df_final))

table_name <- "lasd_stops_incident_2018_2023"
schema <- 'crime_and_justice'
source <- "County of Los Angeles Sheriff Officer Contacts Incident Details downloaded here: https://data.lacounty.gov/datasets/lacounty::sheriff-officer-contacts-incident-details-/about Years for data are: 2018-2023.
R script: W:\\Project\\RJS\\CTC\\Github\\JZ\\cancel_the_contract\\Data Prep\\ripa_lasd_2023_import.R."
 
dbWriteTable(con, Id(schema, table_name), df_final, 
             overwrite = TRUE, row.names = FALSE)
            
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '"," from ", source, ".';")

dbSendQuery(con, comment)

# #comment on columns

column_comment <- paste0("
                COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. contact_id IS 'Contact ID';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. date_time IS 'Date and time';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. date_reformatted IS 'Date reformatted';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. patrol_station IS 'Patrol station';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. number_of_minutes IS 'Number of minutes of stop';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. street_number IS 'Street number';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. direction IS 'Direction';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. street IS 'Street number';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. type IS 'Type';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. suite IS 'Suite';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. cross_street IS 'Cross street';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. landmark IS 'Landmark';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. full_street IS 'Full Street';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. city IS 'City';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. state IS 'State';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. zip_code IS 'Zip code';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. k_12_school IS 'K through 12 school';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. school_name IS 'School name';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. call_for_service IS 'Call for service';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. civilians_contacted IS 'Civilians contacted';
COMMENT ON COLUMN crime_and_justice.lasd_stops_incident_2018_2023. object_id IS 'Object ID';


")

dbSendQuery(con, column_comment)

