###############################################################################
# PURPOSE: geocode head start facilities to enable mapping
# LAST EDITED: 17 mar 2023
############################################################################### . 

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")

# load libraries
library(tidyverse) # the most useful functions!
library(sf) #  for geospatial file handling
library(tmaptools) # for geocoding addresses
library(reticulate) # for calling Python

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

#### load data ####
load(paste(dd, "1_prepped.Rdata", sep = "/"))

#### create unified address field for geocoding ####
# ignore address_line2 and zip4
d_facilities <- d_facilities %>%
    mutate(address = paste0(
        address_line1, " ",
        city, ", ",
        state_code, " ",
        zip
    ))

# quality check: unexpected symbols in address data?
# >>> some # and ()s --> clean up
d_facilities$address[str_detect(d_facilities$address, "[#!@\\^\\(\\)]+")]

d_facilities <- d_facilities %>%
    mutate(
        address = address %>% str_replace_all("[#\\(\\)]+", "")   
    )
d_facilities$address[str_detect(d_facilities$address, "[#!@\\^\\(\\)]+")]

#### get latitude/longitude via Open Street Maps (OSM) API ####
# initialize an empty dataframe with correct structure by geocoding 
# a seed address
d_latlong <- geocode_OSM(
    q = d_facilities %>% slice_head(n = 1) %>% pull(address),
    as.data.frame = TRUE,
    details = TRUE
)
glimpse(d_latlong)
d_latlong <- d_latlong %>% filter(is.na(query))

# filter down to unique addresses
d_addresses <- d_facilities %>%
    select(address) %>%
    unique()

# define batches to get around API call limits
n_facilities <- nrow(d_addresses)
batch_size <- 25
sequence_start <- 1
sequence_end <- ceiling(n_facilities/batch_size) * batch_size

d_batches <- data.frame(
    start = seq(from = 1, to = sequence_end, by = batch_size), 
    end = seq(from = batch_size, to = sequence_end, by = batch_size)
)
d_batches$end[d_batches$end == max(d_batches$end)] <- n_facilities
d_batches

# geocode by batch
for (i in 1:nrow(d_batches)) {
    start_index <- d_batches$start[i]
    end_index <- d_batches$end[i]
    d_latlong_additions <- geocode_OSM(
        q = d_addresses$address[start_index:end_index],
        as.data.frame = TRUE,
        keep.unfound = TRUE,
        details = TRUE
    )
    d_latlong <- bind_rows(d_latlong, d_latlong_additions)
    print(paste("Batch", i, "complete", sep = " "))
}

# save geocodes
save(d_latlong, file = paste(dd, "2_geocoded_interim.rdata", sep = "/"))

#### investigate / fix unfound addresses ####
# >>> if OSM isn't able to locate them, let's try another source
d_latlong_missing <- d_latlong %>% filter(is.na(lat))
write_csv(
    d_latlong_missing, 
    file = paste(dd, "py_addresses_to_geocode.csv", sep = "/")
)

# switch to python since geopy has a handy pre-built package for calling
# other potential geocoder services
use_python("C:/Users/jane.yang/Anaconda3")
source_python(paste(wd, "3_geocode_remaining.py"))

d_latlong_additions <- read_csv(paste(dd, "py_addresses_geocoded.csv", 
    sep = "/")) %>%
    unique() %>%
    mutate(geocode_source = "arcgis")
glimpse(d_latlong_additions)

# join the straggler geocodes with the existing
n_records_before <- nrow(d_latlong)

d_latlong <- d_latlong %>%
    left_join(d_latlong_additions, by = "query") %>%
    mutate(
        lat = if_else(is.na(lat.x), lat.y, lat.x),
        lon = if_else(is.na(lon.x), lon.y, lon.x),
        geocode_source = if_else(is.na(geocode_source), "osm", geocode_source)
    ) %>%
    select(
        query, 
        lat, lon, 
        address_standardized = display_name,
        location_type = type,
        geocode_source,
    )

n_records_after <- nrow(d_latlong)

# check if the join behaved as expected, i.e. didn't create new records
n_records_before == n_records_after

# check for still-missing geocodes
# >>> none!
d_latlong %>% filter(is.na(lat)) %>% glimpse()
d_latlong %>% filter(is.na(lon)) %>% glimpse()

# save final geocoded data
save(d_latlong, file = paste(dd, "3_geocoded_final.rdata", sep = "/"))

#### attach lat/long and hazard risks to d_facilities ####
# join in lat/long
n_records_before <- nrow(d_facilities)

glimpse(d_facilities )
d_facilities <- d_facilities %>%
    left_join(d_latlong, by = c("address" = "query")) %>%
    mutate(
        lat = if_else(is.na(lat.x), lat.y, lat.x),
        lon = if_else(is.na(lon.x), lon.y, lon.x),
        address_standardized = if_else(
            is.na(address_standardized.x),
            address_standardized.y,
            address_standardized.x
        ),
        location_type = if_else(
            is.na(location_type.x),
            location_type.y,
            location_type.x
        ),
        geocode_source = if_else(is.na(geocode_source), "osm", geocode_source)
    ) %>%
    select(
        -lat.x, -lon.x, -lat.y, -lon.y,
        -address_standardized.x, -address_standardized.y,
        -location_type.x, -location_type.y,
    )
glimpse(d_facilities)

n_records_after <- nrow(d_facilities)

n_records_before == n_records_after

# clean up location types
d_facilities <- d_facilities %>%
    mutate(
        location_type = case_when(
            is.na(location_type) ~ "unknown",
            location_type %in% c("yes", "unclassified") ~ "unknown",
            TRUE ~ str_replace(location_type, "_", " ")
        )
    )

# join in hazard risks
n_records_before <- nrow(d_facilities)

d_facilities <- d_facilities %>%
    left_join(
        d_nia %>% st_drop_geometry(), 
        by = c("county", "state_code", "county_type")
    )
glimpse(d_facilities)

n_records_after <- nrow(d_facilities)

n_records_before == n_records_after

# quality check: look for missing hazard risks
# >>> all for territories, as expected
d_facilities %>%
    summarize(across(
        .cols = c(contains("riskr"), contains("ratng")), 
        .fns = ~sum(is.na(.))
    ))

d_facilities %>% 
    filter(is.na(risk_ratng)) %>% 
    count_by_group(n, state_code) %>%
    print(n = Inf)


#### do light clean-up of grantee names (TODO: upstream) ####
d_facilities <- d_facilities %>%
    mutate(grantee = case_when(
        grantee == "Child Development Resources Of Ventura County, Inc." ~
            "Child Development Resources of Ventura County, Inc.",
        grantee == "Eckerd Youth Alternatives Inc." ~
            "Eckerd Youth Alternatives, Inc.",
        TRUE ~ grantee
    ))

#### create long version of facility data by risk rating ####
key_risks <- c(
    "risk_ratng",
    "hrcn_riskr",
    "trnd_riskr",
    "wfir_riskr"
)

d_facilities_byrisk <- d_facilities %>%
    select(
        grant_id:geocode_source,
        any_of(key_risks),
    ) %>%
    pivot_longer(
        cols = key_risks,
        names_to = "risk_type",
        values_to = "risk_rating",
    ) %>%
    mutate(
        risk_type = risk_type %>% 
            str_replace("_ratng", "") %>%
            str_replace("_riskr", ""),
        risk_type = case_when(
            risk_type == "risk" ~ "Overall",
            risk_type == "hrcn" ~ "Hurricane",
            risk_type == "trnd" ~ "Tornado",
            risk_type == "wfir" ~ "Wildfire",
        ),
        risk_type = factor(risk_type, levels = c(
            "Overall",
            "Hurricane",
            "Tornado",
            "Wildfire"
        )),
        risk_rating = case_when(
            is.na(risk_rating) ~ "Insufficient Data",
            risk_rating == "No Rating" ~ "Insufficient Data",
            TRUE ~ risk_rating
        ),
        risk_rating = factor(risk_rating, levels = c(
            "Very High",
            "Relatively High",
            "Relatively Moderate",
            "Relatively Low",
            "Very Low",
            "Not Applicable",
            "Insufficient Data"
        ))
    )
glimpse(d_facilities_byrisk)

#### save final analyzable data #### 
# for Tableau mapping
d_tableau <- d_facilities %>% 
    # slim down given tableau will have multiple map layers
    select(
        grant_id,
        grantee,
        program,
        facility,
        facility_id,
        address,
        state_code,
        county,
        county_type,
        lat,
        lon,
        location_type,
        data_grantee_verification_status,
        starts_with("is_"),
        contains("_ratng"),
        contains("_riskr"),
    ) %>%
    # reshape for easier plotting of site type
    pivot_longer(
        cols = c(contains("hs_site"), is_child_care_partner_site),
        names_to = "program_type",
        names_prefix = "is_",
        values_to = "is_program_type"
    ) %>%
    # change T/F to yes/no
    mutate(across(
        .cols = starts_with("is_"),
        .fns = ~if_else(. == TRUE, "Yes", "No")
    )) %>%
    # add a row number to allow filtering down to unique facilities when
    # not using the program type data
    group_by(facility_id) %>%
    mutate(dummy_filter = row_number()) %>%
    ungroup()
glimpse(d_tableau)    

write_csv(d_tableau, paste(dd, "tb_facilities.csv", sep = "/"))

# for future R analysis
save(
    d_nia_data,
    d_nia_dictionary,
    d_facilities,
    d_facilities_byrisk,
    d_tableau,
    file = paste(dd, "4_analyzable.Rdata", sep = "/")
)
