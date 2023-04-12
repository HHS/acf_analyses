###############################################################################
# PURPOSE: geocode head start facilities to enable mapping
# LAST EDITED: 12 apr 2023
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
library(httr) # for api calls

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
td <- paste(wd, "test", sep = "/")

#### load data ####
load(paste(dd, "1_prepped.Rdata", sep = "/"))

#### create unified address field for geocoding ####
# ignore address_line2 and zip4
d_locations <- d_locations %>%
    mutate(
        address = paste0(
            address_line1, ", ",
            city, ", ",
            state_code, " ",
            zip
        ),
        geocode_source = if_else(is.na(lat), NA, "hses")
    )

# quality check: unexpected symbols in address data?
# >>> some # and ()s --> clean up
d_locations$address[str_detect(d_locations$address, "[#!@\\^\\(\\)]+")]

d_locations <- d_locations %>%
    mutate(
        address = address %>% str_replace_all("[#\\(\\)]+", "")   
    )
d_locations$address[str_detect(d_locations$address, "[#!@\\^\\(\\)]+")]

#### get latitude/longitude via Open Street Maps (OSM) API ####
# initialize an empty dataframe with correct structure by geocoding 
# a seed address
d_latlong <- geocode_OSM(
    q = d_locations %>% slice_head(n = 1) %>% pull(address),
    as.data.frame = TRUE,
    details = TRUE
)
glimpse(d_latlong)
d_latlong <- d_latlong %>% filter(is.na(query))

# filter down to unique addresses without lat/longs
d_addresses <- d_locations %>%
    filter(is.na(lat) | is.na(lon)) %>%
    select(address) %>%
    distinct()

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
        building_type = type,
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

#### combine lat/long to d_locations ####
# join in lat/long
n_records_before <- nrow(d_locations)

glimpse(d_locations )
d_locations <- d_locations %>%
    left_join(d_latlong, by = c("address" = "query")) %>%
    mutate(
        lat = if_else(is.na(lat.x), lat.y, lat.x),
        lon = if_else(is.na(lon.x), lon.y, lon.x),
        geocode_source = if_else(is.na(geocode_source), "osm", geocode_source),
    ) %>%
    select(-lat.x, -lon.x, -lat.y, -lon.y)
glimpse(d_locations)

n_records_after <- nrow(d_locations)

n_records_before == n_records_after

# clean up location types
d_locations <- d_locations %>%
    mutate(
        building_type = case_when(
            is.na(building_type) ~ "unknown",
            building_type %in% c("yes", "unclassified") ~ "unknown",
            TRUE ~ str_replace(building_type, "_", " ")
        )
    )

#### match locations to census tracts ####
# define desired projection
sf_use_s2(FALSE)
proj_target <- 4326 # WGS84 (EPSG: 4326) lat/lon projection

# create simple feature object of Head Start locations
d_locations_sf <- d_locations %>%
    select(location_id, state_code, lat, lon) %>%
    st_as_sf(
        crs = proj_target,
        coords = c("lon", "lat")
    )

# create pared down version of census tract geometries
d_tracts <- d_nia %>%
    select(tract_fips, state_code, shape) %>%
    st_transform(crs = proj_target) %>%
    st_make_valid()

# peek to make sure plotting is as expected
plot(d_tracts %>% filter(state_code == "IL") %>% select(tract_fips), reset = FALSE)
plot(d_locations_sf %>% filter(state_code == "IL") %>% select(location_id), add = TRUE)

# find which census tract each location falls in
d_location_tracts <- st_join(
    d_locations_sf, 
    d_tracts %>% select(-state_code), 
    join = st_within)
glimpse(d_location_tracts)

# spot check with results from https://geocoding.geo.census.gov/geocoder/
# using census 2020 boundaries
# >>> yay, checks out; one mis-match but the geocoder said it was 'inexact'
d_location_tracts %>% 
    slice_sample(n = 10) %>%
    left_join(
        d_locations %>% 
            select(location_id, address_line1, city, zip), 
        by = "location_id"
    ) %>%
    select(address_line1, city, state_code, zip, location_id, tract_fips) %>%
    write_csv(paste(td, "census_tract_geocode_sample.csv", sep = "/"))

# handle any missing census tract mappings
# 12 are the Palau locations, which we knew were missing
# >>> fix the 3 others using the census geocoding API
# https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html
d_location_tracts %>% filter(is.na(tract_fips)) %>% glimpse()

d_location_find_tracts <- d_location_tracts %>% 
    filter(
        is.na(tract_fips),
        state_code != "PW",
    ) %>%
    left_join(
        d_locations %>% 
            select(location_id, lat, lon), 
        by = "location_id"
    ) %>%
    st_drop_geometry()

base_url <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"

for (i in 1:nrow(d_location_find_tracts)) {
    d_location_found_tracts <- GET(paste0(
        base_url,
        "benchmark=2020",
        "&vintage=2020",
        "&x=", d_location_find_tracts$lon[i],
        "&y=", d_location_find_tracts$lat[i],
        "&layers=9",
        "&format=json"
    ))
    
    d_location_find_tracts$tract_fips[i] <- 
        content(d_location_found_tracts)$result$geographies$`Census Tracts`[[1]]$GEOID
}

d_location_tracts <- d_location_tracts %>%
    filter(!is.na(tract_fips) | state_code == "PW") %>%
    bind_rows(d_location_find_tracts) %>%
    select(-lat, -lon, -state_code, -nia_id) %>%
    # re-add in nia_ids so that the api-coded records also have them
    left_join(
        d_nia_data %>% select(nia_id, tract_fips),
        by = "tract_fips"
    )

# join in census tract data to d_locations
n_records_before <- nrow(d_locations)
d_locations <- d_locations %>%
    left_join(d_location_tracts %>% st_drop_geometry, by = "location_id")
n_records_after <- nrow(d_locations)

n_records_before == n_records_after
d_locations %>% filter(is.na(nia_id)) %>% nrow()

#### combine with hazard risks ####
# join in hazard risks
n_records_before <- nrow(d_locations)

d_locations <- d_locations %>%
    left_join(
        d_nia_data %>% select(-state_code, -county, -county_type), 
        by = c("nia_id")
    )
glimpse(d_locations)

n_records_after <- nrow(d_locations)

n_records_before == n_records_after

# quality check: look for missing hazard risks
# >>> all for Palau, as expected
d_locations %>%
    summarize(across(
        .cols = c(contains("riskr"), contains("ratng")), 
        .fns = ~sum(is.na(.))
    ))

d_locations %>% 
    filter(is.na(risk_ratng)) %>% 
    count_by_group(n, state_code) %>%
    print(n = Inf)

#### create long version of facility data by risk rating ####
key_risks <- c(
    "risk_ratng",
    "hrcn_riskr",
    "trnd_riskr",
    "wfir_riskr"
)

d_locations_byrisk <- d_locations %>%
    select(
        grant_id:nia_id,
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
glimpse(d_locations_byrisk)

#### save final analyzable data #### 
# for Tableau mapping
d_tableau <- d_locations %>% 
    # slim down given tableau will have multiple map layers
    select(
        grant_id,
        grantee,
        program,
        location_name,
        location_id,
        address,
        state_code,
        county,
        county_type,
        lat,
        lon,
        tract_fips,
        location_type,
        building_type,
        data_grantee_verification_status,
        starts_with("is_"),
        ends_with("_slots"),
        any_of(key_risks),
    ) %>%
    # reshape for easier plotting of site type
    pivot_longer(
        cols = c(contains("_slots")),
        names_to = "program_type",
        names_pattern = "(.*)_slots",
        values_to = "program_slots"
    ) %>%
    mutate(
        # change T/F to yes/no
        across(
            .cols = starts_with("is_"),
            .fns = ~if_else(. == TRUE, "Yes", "No")
        )
    ) %>%
    # add a row number to allow filtering down to unique facilities when
    # not using the program type data
    group_by(location_id) %>%
    mutate(dummy_filter = row_number()) %>%
    ungroup()
glimpse(d_tableau)    

write_csv(d_tableau, paste(dd, "tb_facilities.csv", sep = "/"))

# for future R analysis
save(
    d_nia_data,
    d_nia_dictionary,
    d_locations,
    d_locations_byrisk,
    d_tableau,
    file = paste(dd, "4_analyzable.Rdata", sep = "/")
)
