###############################################################################
# PURPOSE: prepare Head Start facility and FEMA national risk index data
# LAST EDITED: 12 apr 2023
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
    str_detect(getwd(), "GitHub"),
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

#### load grantee and location data: Head Start Enterprise System (HSES) ####
# load raw data (exported from HSES: Active Grants -> Reports)
# n.b. need to first manually "calculate sheet" in Excel before loading 
# because of the formulas for program number and zip fields and how Excel
# doesn't store formula values by default

# grantee details
d_grantees_raw <- read_excel(
    path = paste(dd, "ohs_grantee_details_20230406.xlsx", sep = "/"),
    sheet = "Grantee Locations and Contacts",
    col_types = "text",
)
glimpse(d_grantees_raw)

d_grantees <- d_grantees_raw %>%
    select(
        grant_id = `Grant Number`,
        agency_type = `Head Start Program  Agency Type`,
        agency_description = `Head Start Program  Agency Description`,
    ) %>%
    replace_na(list(
        agency_type = "Missing Data", 
        agency_description = "Missing Data"
    ))
d_grantees

count_by_group(d_grantees, n, agency_type)
count_by_group(d_grantees, n, agency_description)

## centers = classrooms
d_centers_raw <- read_excel(
    path = paste(dd, "ohs_centers_20230406.xlsx", sep = "/"),
    sheet = "Centers by Program",
    col_types = "text",
)
glimpse(d_centers_raw) # no location ID

# basic cleaning
d_centers <- d_centers_raw %>%
    # 1. rename variables
    # 2. drop unneeded fields
    #       - program number (focus on program type instead)
    #       - phone    
    #       - congressional district
    #       - Federal Interest Filed, which indicates if grantee has filed
    #         notice of federal funding to their local government
    #       - region
    select(
        grant_id = `Grant Number`,
        grantee = `Grantee Name`,
        program_type = `Program\r\nType`,
        program = Program,
        total_slots = `Total Slots`,
        location_name = `Center Name`,
        address_line1 = `Address Line 1`,
        address_line2 = `Address Line 2`,
        city = City,
        state_code = State,
        zip = ZIP,
        zip4 = `ZIP+4`,
        county = County,
        lat = Latitude,
        lon = Longitude,
        data_grantee_verification_status = Status,
        is_federal_interest_site = `Federal\r\nInterest`,
        is_owned_by_program = `Owned By\r\nProgram`,
        is_child_care_partner_site = `Child Care\r\nPartner Site`,
        state_license_status = `Center has state\r\nlicense to operate?`,
        state_license_comments = `Additional\r\nComments...28`,
        qris_status = `Center participates in the state or local\r\nQuality Rating and Improvement System (QRIS)?`,
        qris_comments = `Additional\r\nComments...26`,
    ) %>%
    # transform data for easier analysis
    mutate(
        # 1. turn Yes/No fields into True/False for easier math
        across(
            .cols = starts_with("is_"), 
            .fns = ~case_when(
                . == "No" ~ FALSE,
                . == "Yes" ~ TRUE,
            )
        ),
        # 2. drop "County" from county name    
        county = str_replace(county, " County", ""),
        # 3. clean up category labels for state license and QRIS status
        state_license_status = case_when(
            state_license_status == "Yes" ~ "State-licensed",
            state_license_status == "No, this center is exempt" ~ "Exempt",
            state_license_status == "No, this center is not licensed for another reason" ~ "Not licensed or exempt",
            is.na(state_license_status) ~ "Unknown",
        ),
        qris_status = case_when(
            qris_status == "Yes" ~ "Participates",
            qris_status == "No" ~ "Does not participate",
            qris_status == "QRIS is not yet available" ~ qris_status,
            is.na(qris_status) ~ "Unknown",
        ),
        # 4. add location type
        location_type = "center",
    ) %>%
    # slots, lat/lon make numeric
    mutate(across(c(total_slots, lat, lon), ~ as.numeric(.)))

# quality check: identify, review, and remove duplicates
d_centers %>%
    group_by_at(vars(grant_id:county)) %>%
    summarise(n = n()) %>%
    filter(n > 1) # no duplicates
    # n.b. there are some locations that have same program name, program type,
    # but different funded slots. for now, not de-duping those since the total
    # slot numbers match what HSES has.dropped

## facilities = administrative buildings
d_facilities_raw <- read_excel(
    path = paste(dd, "ohs_facilities_20230406.xlsx", sep = "/"),
    sheet = "Facilities by Program",
    col_types = "text",
    )
glimpse(d_facilities_raw)

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
        location_name = `Facility Name`,
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
        # 3. add location type
        location_type = "administrative facility",
        # 4. add empty columns for variables that don't apply to facilities
        # (to allow combining data)
        total_slots = NA,
        lat = NA,
        lon = NA,
        state_license_status = NA,
        state_license_comments = NA,
        qris_status = NA,
        qris_comments = NA,
    ) %>%
    # arrange columns in same order as d_centers
    select(
        grant_id:program,
        total_slots,
        location_name:county,
        lat, lon,
        data_grantee_verification_status:is_child_care_partner_site,
        state_license_status:qris_comments,
        location_type,
    )

    # quality check: identify, review, and remove duplicates
d_facilities %>%
    group_by_all() %>%
    summarise(n = n()) %>%
    filter(n > 1)

d_facilities <- d_facilities %>% distinct()

nrow(d_facilities_raw) - nrow(d_facilities) # 8 duplicate rows dropped

#### combine: location data ####
# combine facility and center data
d_locations <- bind_rows(d_centers, d_facilities)

# reshape data to facility-level with program data preserved
d_locations <- d_locations %>%
    mutate(
        program_type = str_to_lower(program_type) %>% str_replace(" ", "_"),
    ) %>%
    pivot_wider(
        names_from = "program_type",
        names_glue = "{program_type}_slots",
        names_sort = TRUE,
        values_from = "total_slots",
        values_fn = ~ sum(.x, na.rm = TRUE),
        values_fill = 0,
    )
glimpse(d_locations)
n_locations_prededup <- nrow(d_locations)

# de-duplicate facilities and centers at same address
d_locations <- d_locations %>%
    group_by(
        grant_id, grantee, program, 
        address_line1, city, state_code, zip, zip4, county,
    ) %>%
    summarize(
        address_line2 = str_flatten(address_line2, collapse = " ", na.rm = TRUE),
        location_name = str_flatten(location_name, collapse = " / ", na.rm = TRUE),
        location_type = str_flatten(location_type, collapse = " & ", na.rm = TRUE),
        lat = max(lat, na.rm = TRUE),
        lon = max(lon, na.rm = TRUE),
        # incomplete < needs verification < verified
        data_grantee_verification_status = min(data_grantee_verification_status,
            na.rm = TRUE),
        across(starts_with("is_"), ~max(., na.rm = TRUE)),
        # drops facility NAs
        across(state_license_status:qris_comments, ~str_flatten(., na.rm = TRUE)),
        across(ends_with("_slots"), ~sum(., na.rm = TRUE)),
    ) %>%
    ungroup() %>%
    distinct() %>%
    # replace the Inf lat/lon (from locations without any existing data) to NAs
    mutate(
        lat = if_else(is.infinite(lat), NA, lat),
        lon = if_else(is.infinite(lon), NA, lon),
    ) %>%
    # create a pseudo-facilities id
    group_by(grant_id) %>%
    mutate(location_id = paste(grant_id, row_number(), sep = "-")) %>%
    ungroup() %>%
    # create total slots across all programs
    mutate(total_slots = rowSums(across(ends_with("_slots")))) %>%
    # reorder variables again 
    select(
        grant_id:address_line1,
        address_line2,
        city:county,
        lat, lon,
        location_id,
        location_name,
        location_type,
        ends_with("_slots"),
        data_grantee_verification_status:qris_comments,
    )

glimpse(d_locations)

n_locations_deduped <- nrow(d_locations)
n_locations_prededup - n_locations_deduped # 779 locations de-duped

# quality check: total slots match between raw and processed? Yes!
sum(as.numeric(d_centers_raw$`Total Slots`)) == sum(d_locations$total_slots)

# quality check: missing address data?
# >>> nope!
d_locations %>%
    summarize(across(
        .cols = c(address_line1, city, state_code, zip), 
        .fns = ~sum(is.na(.))
    ))

#### load and prep risk data: FEMA National Risk Index (NRI) ####
# https://hazards.fema.gov/nri/data-resources

# load census tract-level geodatabase
f_nia <- paste(dd, "NRI_GDB_CensusTracts.gdb", sep = "/")
st_layers(f_nia)

d_nia_raw <- st_read(
    dsn = f_nia,
    layer = "NRI_CensusTracts"
); glimpse(d_nia_raw)

# define territory state abbreviations
territories <- c("PR", "GU", "MP", "AS", "VI", "PW")

# focus data to priority risk indicators
d_nia <- d_nia_raw %>%
    select(
        nia_id = NRI_ID,
        state_code = STATEABBRV,
        county = COUNTY,
        county_type = COUNTYTYPE,
        tract_fips = TRACTFIPS,
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

d_nia <- d_nia %>%
    # for territories, only EALs are provided (no SVI or community resilience
    # info, which is needed to calculate risk ratings). This isn't perfect but
    # we're going to sub in the EAL for the overal risk rating in these cases,
    # thereby assuming a f(SVI/community resilience) = 1
    mutate(
        hrcn_riskr = if_else(state_code %in% territories, hrcn_ealr, hrcn_riskr),
        trnd_riskr = if_else(state_code %in% territories, trnd_ealr, trnd_riskr),
        wfir_riskr = if_else(state_code %in% territories, wfir_ealr, wfir_riskr),
    )
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
# >>> NIA doesn't include Palau
states_locations <- d_locations %>% pull(state_code) %>% unique()
states_nia <- d_nia_data$state_code %>% unique()
states_locations[!(states_locations %in% states_nia)]

# county names?
# >>> HS facility data has 205 counties that don't exist in exact form in NIA
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
    
counties_locations <- d_locations %>% 
    filter(state_code %in% states_nia) %>%
    focus_on_county_state_codes() %>%
    create_county_state_code()

counties_nia <- d_nia_data %>% 
    focus_on_county_state_codes() %>%
    create_county_state_code()

counties_to_address <- counties_locations %>%
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
# n.b. there are also mismatches in accents, spacing, and superceded names

d_locations <- d_locations %>%
    create_county_state_code() %>%
    mutate(
        county_type = case_when(
            !(county_state_code %in% counties_to_address$county_state_code) ~
                "County",
            county == "Dona Ana" ~ "County",
            str_detect(county, " city") ~ "City",
            str_detect(county, " Parish") ~ "Parish",
            str_detect(county, " City and Borough") ~ "City and Borough",
            str_detect(county, " Borough") ~ "Borough",
            str_detect(county, " Municipality") ~ "Municipality",
            str_detect(county, " Census Area") ~ "Census Area",
            str_detect(county, " Municipio") ~ "Municipio",
            str_detect(county, " District") ~ "District",
            str_detect(county, " Island") ~ "Island",
        ),
        county = case_when(
            county_state_code == "Dona Ana-NM" ~ "Doña Ana",
            county_state_code == "Bayamon-PR" ~ "Bayamón",
            county_state_code == "Rincon-PR" ~ "Rincón",
            county_state_code == "Mayaguez-PR" ~ "Mayagüez",
            county_state_code == "Juana Diaz-PR" ~ "Juana Díaz",
            county_state_code == "Penuelas-PR" ~ "Peñuelas",
            county_state_code == "San Sebastian-PR" ~ "San Sebastián",
            county_state_code == "Guanica-PR" ~ "Guánica",
            county_state_code == "San German-PR" ~ "San Germán",
            county_state_code == "Rio Grande-PR" ~ "Río Grande",
            county_state_code == "Las Marias-PR" ~ "Las Marías",
            county_state_code == "Anasco-PR" ~ "Añasco",
            county_state_code == "Canovanas-PR" ~ "Canóvanas",
            county_state_code == "Loiza-PR" ~ "Loíza",
            county_state_code == "Comerio-PR" ~ "Comerío",
            county_state_code == "Catano-PR" ~ "Cataño",
            county_state_code == "Manati-PR" ~ "Manatí",
            county_state_code == "La Salle-LA" ~ "LaSalle",
            city == "Saipan" & state_code == "MP" ~ "Saipan",
            city == "Tinian" & state_code == "MP" ~ "Tinian",
            city == "Rota" & state_code == "MP" ~ "Rota",
            # renamed
            county_state_code == "Wade Hampton-AK" ~ "Kusilvak",
            county_state_code == "Kodiak-AK" ~ "Kodiak Island",
            # restructured in 2019
            county_state_code == "Valdez-Cordova-AK" ~ "Copper River",
            # renamed in 2015
            county_state_code == "Shannon-SD" ~ "Oglala Lakota",
            county_state_code %in% counties_to_address$county_state_code ~ 
                county %>% 
                    str_replace(" city", "") %>% 
                    str_replace(" Parish", "") %>%
                    str_replace(" City and Borough", "") %>%
                    str_replace(" Borough", "") %>%
                    str_replace(" Municipality", "") %>%
                    str_replace(" Census Area", "") %>%
                    str_replace(" Municipio", "") %>%
                    str_replace(" District", "") %>%
                    str_replace(" Island", ""),
            TRUE ~ county
        )
    ) %>%
    create_county_state_code()

# check if there are still "missing" counties
# >>> nope!
counties_locations <- d_locations %>% 
    filter(state_code %in% states_nia) %>%
    focus_on_county_state_codes() %>%
    create_county_state_code()

counties_to_address <- counties_locations %>%
    filter(!(county_state_code %in% counties_nia$county_state_code))
counties_to_address %>% print(n = Inf)

# confirm uniqueness of county-state combos in NIA data
# >>> yes!
count_by_group(counties_nia, n, county_state_code) %>% filter(n > 1)

#### combine grantee data ####
# do light clean-up of grantee names
d_locations <- d_locations %>%
    mutate(grantee = case_when(
        grantee == "Child Development Resources Of Ventura County, Inc." ~
            "Child Development Resources of Ventura County, Inc.",
        grantee == "Eckerd Youth Alternatives Inc." ~
            "Eckerd Youth Alternatives, Inc.",
        TRUE ~ grantee
    ))

# combine grantee data
d_locations <- d_locations %>%
    left_join(d_grantees, by = "grant_id") %>%
    select(
        starts_with("grant"),
        starts_with("agency_"),
        program:county_type,
    )

#### save cleaned data ####
save(
    d_nia,
    d_nia_data,
    d_nia_dictionary,
    d_locations,
    file = paste(dd, "1_prepped.Rdata", sep = "/")
)
