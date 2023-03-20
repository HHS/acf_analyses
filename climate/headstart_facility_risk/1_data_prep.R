###############################################################################
# PURPOSE: prepare Head Start facility and FEMA national risk index data
# LAST EDITED: 16 mar 2023
############################################################################### . 

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")

# load libraries
library(tidyverse) # makes loading and munging data easier
library(readxl) # for reading excel files
library(sf) #  for geospatial file handling

# define directories
wd <- if_else(
    str_detect(getwd(), "Github"),
    getwd(),
    paste(
        getwd(), 
        "GitHub/acf_analyses/climate/headstart_facility_risk", 
        sep = "/"
    )
)
dd <- paste(wd, "data", sep = "/")
od <- paste(wd, "output", sep = "/")

#### define custom functions ####
# summarize counts by group and return results arranged in descending order
count_by_group <- function(data, arrange_var, ...){
    group_vars <- enquos(...)
    arrange_var <- enquo(arrange_var)
    data %>% 
        group_by(!!!group_vars) %>%
        summarize(n = n()) %>%
        ungroup() %>%
        mutate(pct = n / sum(n)) %>%
        arrange(!!arrange_var %>% desc())
}

#### facilities data: Head Start Enterprise System (HSES) ####
# load raw data (exported from HSES: All Grants -> Reports -> Facilities)
# n.b. need to first manually "calculate sheet" in Excel before loading 
# because of the formulas for program number and zip fields and how Excel
# doesn't store formula values by default
d_facilities_raw <- read_excel(
    path = paste(dd, "ohs_facilities_20230307.xlsx", sep = "/"),
    sheet = "Facilities by Program",
    col_types = "text",
    )
glimpse(d_facilities_raw) # bummer, no facilities ID...

# basic cleaning
d_facilities <- d_facilities_raw %>%
    # 1. rename variables
    # 2. drop unneeded fields
    #       - program number (focus on program type instead)
    #       - phone    
    #       - congressional district
    #       - Federal Interest Filed, which indicates if grantee has filed
    #         notice of federal funding tot heir local government
    select(
        grant_id = `Grant Number`,
        grantee = `Grantee Name`,
        program_type = `Program\r\nType`,
        program = Program,
        facility = `Facility Name`,
        address_line1 = `Address Line 1`,
        address_line2 = `Address Line 2`,
        city = City,
        state_code = State,
        zip = ZIP,
        zip4 = `ZIP+4`,
        county = County,
        data_grantee_verification_status = Status,
        is_federal_interest_site = `Federal\r\nInterest`,
        is_owned_by_program = `Owned By\r\nProgram`,
        is_child_care_partner_site = `Child Care\r\nPartner Site`,
    ) %>%
    # transform data for easier analysis
    mutate(
        # 1. turn Yes/No fields into True/False for easier math
        across(
            .cols = starts_with("is_"), 
            .fns = ~case_when(
                . == "No" ~ FALSE,
                . == "Yes" ~ TRUE,
                is.na(.) ~ TRUE, # applicable to is_child_care_partner_site
            )
        ),
        # 2. drop "County" from county name    
        county = str_replace(county, " County", ""),
    )

# quality check: identify, review, and remove duplicates
d_facilities %>%
    group_by_all() %>%
    summarise(n = n()) %>%
    filter(n > 1)

d_facilities <- d_facilities %>% distinct()

nrow(d_facilities_raw) - nrow(d_facilities) # 8 duplicate rows dropped

# reshape data to facility-level with program data preserved
d_facilities <- d_facilities %>%
    mutate(
        program_type = str_to_lower(program_type) %>% str_replace(" ", "_"),
        program_flag = TRUE,
    ) %>%
    pivot_wider(
        names_from = "program_type",
        names_glue = "is_{program_type}_site",
        names_sort = TRUE,
        values_from = "program_flag",
        values_fill = FALSE,
    ) %>%
    # create a pseudo-facilities id
    group_by(grant_id) %>%
    mutate(facility_id = paste(grant_id, row_number(), sep = "-")) %>%
    ungroup()

glimpse(d_facilities)

# quality check: missing address data?
# >>> nope!
d_facilities %>%
    summarize(across(
        .cols = c(address_line1, city, state_code, zip), 
        .fns = ~sum(is.na(.))
    ))

#### risk data: FEMA National Risk Index (NRI) ####
# https://hazards.fema.gov/nri/data-resources

# load county-level geodatabase
# N.B. will need more computing power to do census-tract level
f_nia <- paste(dd, "NRI_GDB_Counties.gdb", sep = "/")
st_layers(f_nia)

d_nia_codes <- st_read(
    dsn = f_nia,
    layer = "NRI_HazardInfo"
); d_nia_codes

d_nia_raw <- st_read(
    dsn = f_nia,
    layer = "NRI_Counties"
); glimpse(d_nia_raw)

# focus data to priority risk indicators
d_nia <- d_nia_raw %>%
    select(
        nia_id = NRI_ID,
        state_code = STATEABBRV,
        county = COUNTY,
        county_type = COUNTYTYPE,
        starts_with("RISK"), # overall risk assessments
        starts_with("EAL"),  # expected economic annualized lost
        starts_with("SOVI"), # social vulnerability index
        starts_with("RESL"), # community resilience index
        starts_with("HRCN"), # hurricane
        starts_with("TRND"), # tornado
        starts_with("WFIR"), # wildfire
    )
names(d_nia) <- str_to_lower(names(d_nia))
st_geometry(d_nia) <- "shape"
glimpse(d_nia)

# explore county types
# >>> interesting, there are types like "City", "Borough", "Census Area"...
#     given FEMA's published this highly-polished data tool, assume these are 
#     clean data. if there's wonkiness later, will revisit.
d_nia_data <- d_nia %>% st_drop_geometry() # data sans polygons = faster processing
count_by_group(d_nia_data, n, county_type)

# pull in descriptive field names from data dictionary
d_nia_dictionary <- read_csv(paste(dd, "NRI_DataDictionary.csv", sep = "/")) %>%
    select(
        field_name = `Field Name`,
        field_description = `Field Alias`,
    )

#### check for and address inconsistencies between datasets ####
# states included?
# >>> NIA doesn't include DC or the five territories (PR, VI, AS, PW, MP)
states_facilities <- d_facilities %>% pull(state_code) %>% unique()
states_nia <- d_nia_data$state_code %>% unique()
states_facilities[!(states_facilities %in% states_nia)]

# county names?
# >>> HS facility data has 37 counties that don't exist in exact form in NIA
create_county_state_code <- function(d) {
    res <- d %>% 
        mutate(county_state_code = paste(county, state_code, sep = "-"))
}

focus_on_county_state_codes <- function(d) {
    res <- d %>%
        select(county, state_code) %>%
        st_drop_geometry() %>%
        unique()
}
    
counties_facilities <- d_facilities %>% 
    filter(state_code %in% states_nia) %>%
    focus_on_county_state_codes() %>%
    create_county_state_code()

counties_nia <- d_nia_data %>% 
    focus_on_county_state_codes() %>%
    create_county_state_code()

counties_to_address <- counties_facilities %>%
    filter(!(county_state_code %in% counties_nia$county_state_code))
counties_to_address %>% print(n = Inf)

# sort out "missing" counties
# most have geographic descriptors like "city", "Parish", "Borough"...
# check if these counties exist if we move those indicators over into a 
# "county_type" field like the NIA data has
# >>> yup!
# counties_nia %>% filter(county == "Norfolk") # "city" example
# counties_nia %>% filter(county == "St. Landry") # "Parish" example
# counties_nia %>% filter(state_code == "NM") # mismatch due to tilde
# counties_nia %>% filter(county == "Matanuska-Susitna") # Borough example
# counties_nia %>% filter(county == "Anchorage") # Municipality example
# counties_nia %>% filter(county == "Prince of Wales-Hyder") # Census Area example

d_facilities <- d_facilities %>%
    create_county_state_code() %>%
    mutate(
        county_type = case_when(
            !(county_state_code %in% counties_to_address$county_state_code) ~
                "County",
            county == "Dona Ana" ~ "County",
            str_detect(county, " city") ~ "City",
            str_detect(county, " Parish") ~ "Parish",
            str_detect(county, " Borough") ~ "Borough",
            str_detect(county, " Municipality") ~ "Municipality",
            str_detect(county, " Census Area") ~ "Census Area",
        ),
        county = case_when(
            county_state_code == "Dona Ana-NM" ~ "Doña Ana",
            county_state_code %in% counties_to_address$county_state_code ~ 
                county %>% 
                    str_replace(" city", "") %>% 
                    str_replace(" Parish", "") %>%
                    str_replace(" Borough", "") %>%
                    str_replace(" Municipality", "") %>%
                    str_replace(" Census Area", ""),
            TRUE ~ county
        )
    ) %>%
    create_county_state_code()

# check if there are still "missing" counties
# >>> nope!
counties_facilities <- d_facilities %>% 
    filter(state_code %in% states_nia) %>%
    focus_on_county_state_codes() %>%
    create_county_state_code()
counties_nia <- d_nia %>% 
    focus_on_county_state_codes() %>%
    create_county_state_code()

counties_to_address <- counties_facilities %>%
    filter(!(county_state_code %in% counties_nia$county_state_code))
counties_to_address %>% print(n = Inf)

# confirm uniqueness of county-state combos in NIA data
# >>> yes!
count_by_group(counties_nia, n, county_state_code) %>% filter(n > 1)

#### save cleaned data ####
save(
    d_nia,
    d_nia_data,
    d_nia_dictionary,
    d_facilities,
    file = paste(dd, "1_prepped.Rdata", sep = "/")
)

#### notes ####
# FYIs
#  - NRI doesn't include US territories yet but should come Mar 23, 2023. When
#  - incorporating that data, will need to clean up addresses for territories
#  - NRI includes census-level data but will need more computing power to
#    utilize it. If we go that route, we will need to code the HS facilites by
#    census tract: 
#    https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html
# Potential datasets for follow-on analyses:
#  1. National Weather Service Climate Prediction Center 
#     https://www.cpc.ncep.noaa.gov/
#     could be useful for predictive tool
#  2. National Oceanic and Atmospheric Administration
#     Service Records Retention Systems (SRRS)
#     https://www.ncei.noaa.gov/products/weather-climate-models/service-records-retention
#     includes past weather warnings and advisories
#  3. FEMA Geospatial Resource Center
#     https://gis-fema.hub.arcgis.com/
#     contains data on current and historic extreme weather events