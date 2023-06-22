###############################################################################
# PURPOSE: prepare HSES enrollment data export into analyzable format
# LAST EDITED: 22 june 2023
# NOTES:
#   - Data is from January 2022 through May 2023, inclusive
#   - We do not consider enrollment in June, July and August for entry into FEI
#   - Exclude Region 13 (interim) grants
############################################################################### . 

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")

# load libraries
library(tidyverse) # the most useful functions!
library(janitor) # helpful for speeding up column renaming
library(readxl) # reading Excel
library(lubridate) # making it easy to work with dates

# define directories
wd <- if_else(
    str_detect(getwd(), "GitHub"),
    getwd(),
    paste(
        getwd(), 
        "GitHub/acf_analyses/ecd/full_enrollment_initiative", 
        sep = "/"
    )
)
dd <- paste(wd, "data", sep = "/")
od <- paste(wd, "output", sep = "/")

#### load data ####
# enrollment report by grant (rows) and month (grouped by columns)
d_enrollment_raw <- read_excel(
    path = paste(dd, "Enrollment Jan22_May23.xlsx", sep = "/"),
    sheet = "Enrollment",
    range = "a1:rc2425",
    col_names = FALSE,
    col_types = "text",
    .name_repair = "universal",
    na = "N/A",
)

# grant recipients in Full Enrollment Initiative (FEI)
d_fei_raw <- read_excel(
    path = paste(dd, "Enrollment Jan22_May23.xlsx", sep = "/"),
    sheet = "Recipients entered into FEI",
    range = "a2:k611",
    col_types = c(
        "numeric", rep("text", 3), "numeric", "text",
        rep("date", 2), "text", rep("date", 2)
    ),
    .name_repair = "unique_quiet",
    na = "N/A",
)

#### munge enrollment report variable names ####
n_rows_header <- 3

# generate monthly data column names
col_monthly_start <- 13
col_monthly_end <- 471
n_cols_monthly <- col_monthly_end - col_monthly_start + 1

d_enrollment_col_names_monthly <- d_enrollment_raw %>%
    # focus on monthly data columns 
    select(all_of(col_monthly_start:col_monthly_end)) %>%
    # take header information
    slice(1:n_rows_header) %>% 
    # reshape to map months, program types, and variables against column number
    pivot_longer(
        cols = everything(), 
        cols_vary = "slowest"
    ) %>%
    mutate(col_type = rep(c("month", "program", "data"), n_cols_monthly)) %>% 
    # remap month and program to columns
    pivot_wider(
        names_from = col_type, 
        values_from = value
    ) %>% 
    fill(
        month, program,
        .direction = "down"
    ) %>%
    # collapse month, program, and variables into a single string
    mutate(col_name = paste(data, month, program, sep = ","))

col_meta_start <- 1
col_meta_end <- 12

# generate grant metadata column names
d_enrollment_col_names_meta <- d_enrollment_raw %>% 
    select(all_of(col_meta_start:col_meta_end)) %>%
    slice(n_rows_header) %>%
    pivot_longer(cols = all_of(col_meta_start:col_meta_end))

# apply rescued variable names to raw data
d_enrollment_col_names <- c(
    d_enrollment_col_names_meta$value, 
    d_enrollment_col_names_monthly$col_name
)

d_enrollment_renamed <- d_enrollment_raw
names(d_enrollment_renamed) <- d_enrollment_col_names

# make variable names easier to work with
d_enrollment <- d_enrollment_renamed %>% 
    # drop original header rows and empty rows
    slice(-(1:3)) %>% 
    filter(!is.na(`Grant Number`)) %>%
    # drop lookback period aggregates
    select(-contains("Total")) %>%
    # reshape so that each row corresponds to a grant-program-month combination
    pivot_longer(!d_enrollment_col_names_meta$value) %>%
    separate_wider_delim(
        cols = "name",
        names = c("col", "month", "program"), 
        delim = ","
    ) %>% 
    pivot_wider(names_from = col, values_from = value) %>% 
    clean_names() %>%
    # move metadata to front for easier reference
    relocate(grant_number, month, program)

# check we have the expected number of rows
n_grants <- d_enrollment_raw %>%
    # drop original header rows and empty rows
    slice(-(1:3)) %>% 
    filter(!is.na(`...1`)) %>%
    nrow()

n_months <- time_length(
    interval(ymd("2022-01-01"), ymd("2023-05-01")), 
    unit = "month"
) + 1 # to be inclusive of May

n_programs <- 2

n_rows_expected <- n_grants * n_months * n_programs

nrow(d_enrollment) == n_rows_expected # TRUE, yay!

#### finish cleaning up enrollment report data ####

# convert data types

# remove Region 13 (interim grants)

#### clean up FEI grant recipient data ####
# rename variables
d_fei_renamed <- d_fei_raw

names(d_fei_renamed) <- c(
    "region",
    "state",
    "grant_id",
    "grantee",
    "annual_funding_month",
    "fei_status_hs",
    "fei_effective_date_hs",
    "fei_action_date_hs",
    "fei_status_ehs",
    "fei_effective_date_ehs",
    "fei_action_date_ehs"
)

# factorize categorical variables
d_fei <- d_fei_renamed %>%
    mutate(across(c("region", "state", "annual_funding_month"), ~as.factor(.)))

#### add fei context to enrollment report data ####

#### save cleaned data ####
save(
    file = paste(dd, "1_clean_data.Rdata", sep = "/"),
    d_fei,
    d_enrollment
)
