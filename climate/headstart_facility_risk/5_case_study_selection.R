###############################################################################
# PURPOSE: identify potential case studies
# LAST EDITED: 1 may 2023
############################################################################### . 

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")

# load libraries
library(tidyverse) # the most useful functions!
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

#### load location data with risk hazards ####
load(paste(dd, "4_analyzable.Rdata", sep = "/"))

#### prep poverty data by census tract ####
# load
d_poverty_raw <- read_csv(
    file = paste(dd, "acs_2017-2021_poverty-ACSST5Y2021.S1701.csv", sep = "/"),
    col_select = c("GEO_ID", "NAME", "S1701_C03_003E", "S1701_C03_003M"),
    na = c("null", "-", "**")
)

# prep
d_poverty <- d_poverty_raw %>%
    slice(-1) %>% # drop second header %>%
    rename(
        tract_fips = GEO_ID,
        tract_name = NAME,
        pct_under5_below_poverty_level_middle = S1701_C03_003E,
        pct_under5_below_poverty_level_moe = S1701_C03_003M,
    ) %>%
    mutate(
        tract_fips = str_replace(tract_fips, "1400000US", ""),
        across(starts_with("pct_"), ~ as.numeric(.)/100),
        pct_under5_below_poverty_level_high = pmin(pct_under5_below_poverty_level_middle +
            pct_under5_below_poverty_level_moe, 1),
        pct_under5_below_poverty_level_low = pmax(pct_under5_below_poverty_level_middle -
            pct_under5_below_poverty_level_moe, 0),
        pct_under5_below_poverty_moe_relative = if_else(
            pct_under5_below_poverty_level_middle == 0, 
            NA, 
            pct_under5_below_poverty_level_moe/
                pct_under5_below_poverty_level_middle
        )
    )
glimpse(d_poverty)

# check: all tract_fips in d_locations in d_poverty?
# ACS doesn't include AS, GU, MP, PW, VI
d_locations %>%
    filter(!(tract_fips %in% d_poverty$tract_fips)) %>%
    count_by_group(n, tract_fips, state_code) %>%
    print(n = Inf)

# check: any fips missing poverty percentages?
summary(d_poverty$pct_under5_below_poverty_level_middle) # 2578 NAs

d_poverty %>%
    filter(is.na(pct_under5_below_poverty_level_middle)) %>%
    mutate(state = str_extract(tract_name, "[[:alpha:] ]+$")) %>%
    count_by_group(n, state) %>%
    print(n = Inf)
    # some missing in all states / DC -- hundreds of census tracts in FL, NY, CA,
    # TX, MI, AZ...

#### prep urban area definitions ####
# define desired projection
proj_target <- 4326 # WGS84 (EPSG: 4326) lat/lon projection

# load data
# n.b. ideally would use the DoAg's rural/urban commuting area designations
# that differentiates isolated rural areas but they haven't updated to 2020
# census tracts yet
f_urban <- paste(dd, "census_bureau_urban_areas_2020.gdb", sep = "/")
st_layers(f_urban)

d_urban_raw <- st_read(
    dsn = f_urban,
    layer = "Urban_Area_20"
); glimpse(d_urban_raw)

d_urban <- d_urban_raw %>%
    select(
        ua_id = GEOID,
        ua_name = NAMELSAD,
        ua_geometry = SHAPE
    ) %>%
    st_transform(crs = proj_target)

# create simple feature object of Head Start locations
d_locations_sf <- d_locations %>%
    select(location_id, state_code, lat, lon) %>%
    st_as_sf(
        crs = proj_target,
        coords = c("lon", "lat")
    )

# peek to make sure plotting is as expected
plot(d_urban %>% filter(str_detect(ua_name, "IL")) %>% select(ua_name), reset = FALSE)
plot(d_locations_sf %>% filter(state_code == "IL") %>% select(location_id), add = TRUE)

# find which locations fall in urban areas
d_location_urban <- st_join(
    d_locations_sf, 
    d_urban, 
    join = st_within)
glimpse(d_location_urban)

# further code isolate vs. rural areas
d_location_uri <- d_location_urban %>%
    st_drop_geometry %>%
    mutate(
        area_type = case_when(
            !is.na(ua_id) ~ "urban",
            state_code %in% c(
                "PR", "GU", "MP", "AS", "VI", "PW",
                "HI", "AK"
            ) ~ "isolated",
            TRUE ~ "rural"
        )
    ) %>%
    select(location_id, area_type, urban_area = ua_name)
count_by_group(d_location_uri, n, area_type)

#### tag each location by poverty percentage and urban/rural/isolated #### 
# join in data
n_records_before <- nrow(d_locations)
d_locations <- d_locations %>%
    left_join(d_location_uri, by = "location_id") %>%
    left_join(d_poverty, by = "tract_fips")
n_records_after <- nrow(d_locations)
n_records_before == n_records_after
glimpse(d_locations)

# check for any weirdness
count_by_group(d_locations, n, area_type)

summary(d_locations$pct_under5_below_poverty_level_middle) #NAs
d_locations %>%
    filter(is.na(pct_under5_below_poverty_level_middle)) %>%
    count_by_group(n, state_code) %>% 
    print(n = Inf)
    # >>> gaps mostly in territories, though a few in states
    #     due to gaps in ACS-provided data

#### identify illustrative locations ####
# focus on federal interest locations with highest level of risk for at 
# least one hazard type
d_programs_vhighrisk <- d_locations %>% 
    select(
        location_id,
        ends_with("_riskr"),
        ends_with("_slots"),
        starts_with("pct_"),
        area_type,
        state_code,
        is_federal_interest_site,
    ) %>%
    filter(
        is_federal_interest_site,
        hrcn_riskr == "Very High" | 
            trnd_riskr == "Very High" | 
            wfir_riskr == "Very High"
    ) %>%
    pivot_longer(
        cols = c(ends_with("_slots")),
        names_to = "program_type",
        names_pattern = "(.*)_slots",
        values_to = "program_slots"
    ) %>%
    filter(program_slots > 0)

# get a sense of poverty levels and relative margin of errors by program type
pov_threshold <- d_programs_vhighrisk %>%
    group_by(program_type) %>%
    summarize(
        pov_median = median(pct_under5_below_poverty_level_low, na.rm = TRUE),
        pov_mean = mean(pct_under5_below_poverty_level_low, na.rm = TRUE),
        pov_75pctl = quantile(pct_under5_below_poverty_level_low, probs = 0.75,
            na.rm = TRUE)
    ) %>%
    ungroup()
pov_threshold

pov_moe_threshold <- d_programs_vhighrisk %>%
    left_join(pov_threshold, by = "program_type") %>%
    filter(pct_under5_below_poverty_level_low > pov_75pctl) %>%
    group_by(program_type) %>%
    summarize(
        n = n(),
        moe_median = median(pct_under5_below_poverty_moe_relative, na.rm = TRUE),
        moe_mean = mean(pct_under5_below_poverty_moe_relative, na.rm = TRUE),
        moe_25pctl = quantile(pct_under5_below_poverty_moe_relative, probs = 0.25,
            na.rm = TRUE)
    ) %>%
    ungroup()
pov_moe_threshold

# narrow by location-based priorities
d_case_study_options <- d_programs_vhighrisk %>%
    left_join(pov_threshold, by = "program_type") %>%
    left_join(pov_moe_threshold, by = "program_type") %>%
    filter(
        pct_under5_below_poverty_level_low >= pov_75pctl,
        pct_under5_below_poverty_moe_relative <= moe_25pctl
    )

# CHECKS:
# how many locations have we narrowed to so far? < 50
d_case_study_options %>% pull(location_id) %>% unique() %>% length()

# are there are locations in all area types? yes
count_by_group(d_case_study_options, n, area_type) 

# are there locations of all program types? yes
d_case_study_options %>%
    group_by(program_type) %>%
    summarize(n = n_distinct(location_id)) %>%
    ungroup()

# are there locations for each risk type? yes
d_case_study_options %>%
    summarize(across(ends_with("_riskr"), ~ sum(. == "Very High")))

# narrow by portfolio-based priorities --> pull examples from here
d_case_studies <- d_locations %>%
    filter(location_id %in% c(
        # aian (e)hs,  tornado + wildfire + rural
        "90CI010032-3",
        "90CI010032-12",
        # aian (ehs), wildfire + rural
        "09CH010861-13", # picking out CA site though doesn't meet pov criteria
        # migrant (e)hs, hurricane + urban
        "90HM000016-12",
        # migrant (e)hs, tornado + rural
        "90CM009842-1",
        # (e)hs, hurricane + isolated
        "02CH012204-28",
        # (e)hs, hurricane + rural
        "04CH011336-8",
        # (e)hs, tornado + urban
        "04CH011230-1",
        "04CH011010-10",
        # (e)hs, wildfire + rural
        "90CM009846-3"
    ))

# estimate roughly how many locations are *roughly* similar to the proposed case 
# studies in terms of area type and being at-risk (moderate -> very high)
# n.b. this is regardless of federal interest and area poverty rates
s_locations_similarish <- d_locations %>%
    pivot_longer(
        cols = c(ends_with("_riskr")),
        names_to = "risk_type",
        names_pattern = "(.*)_riskr",
        values_to = "risk_rating"
    ) %>%
    filter(risk_rating %in% c(
        "Very High", 
        "Relatively High", 
        "Relatively Moderate")) %>%
    group_by(risk_type, area_type) %>%
    summarize(
        n_locations = n_distinct(location_id),
        n_states = n_distinct(state_code),
        median_pov = median(pct_under5_below_poverty_level_middle, na.rm = TRUE),
    ) %>%
    ungroup()
s_locations_similarish

#### save data for future reference ####
save(
    d_case_studies,
    d_case_study_options,
    d_locations,
    s_locations_similarish,
    focus_states,
    file = paste(dd, "5_illustrative_examples.Rdata", sep = "/")
)

write_csv(d_case_studies, paste(od, "illustrative_examples.csv", sep = "/"))
write_csv(s_locations_similarish, paste(od, "at_risk_breakdown.csv", sep = "/"))
