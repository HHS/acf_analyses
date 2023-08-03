###############################################################################
# PURPOSE: pull sample of comments to manually code
# LAST EDITED: 3 august 2023
############################################################################### . 

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")

# load libraries
library(tidyverse)
library(lubridate)
library(readxl)

# define directories
wd <- if_else(
    str_detect(getwd(), "GitHub"),
    getwd(),
    paste(
        getwd(), 
        "GitHub/acf_analyses/", 
        sep = "/"
    )
)
dd <- paste(wd, "data", sep = "/")
od <- paste(wd, "output", sep = "/")

#### load data ####
# cleaned comments
load(paste(dd, "1_clean_data.Rdata", sep = "/"))

# active grants
d_grants_raw <- read_excel(paste(dd, "Grant Project Period Report.xlsx", sep = "/"))

#### create subset of comments based on stratified sample of 400 grants ####
# determine share of grants per region 
d_grants <- d_grants_raw %>%
    select(
        region = Region,
        grant_id = `Grant Number`
    ) %>%
    filter(!(region %in% c("Totals", "12", "13")))

s_grants <- d_grants %>%
    count(region) %>%
    mutate(
        pct = n/sum(n),
        n_sample = ceiling(pct * 400)
    )

d_grants <- d_grants %>%
    left_join(s_grants, by = "region")

# randomly arrange grant order
set.seed(20230803)
d_grants <- d_grants[sample(1:nrow(d_grants)), ]

# filter down to sample 
d_grants_sample <- d_grants %>%
    group_by(region) %>%
    mutate(row_id = row_number()) %>%
    filter(row_id <= n_sample) %>%
    ungroup()

# compare region proportions in sample vs. total dataset
# >>> looks good
d_grants_sample %>% 
    count(region) %>% 
    mutate(pct_sample = n/sum(n)) %>%
    select(region, pct_sample) %>%
    left_join(s_grants %>% select(region, pct_grants = pct)) %>%
    mutate(pct_diff = pct_sample - pct_grants)

# create comment sample
d_sample <- d %>%
    filter(grant_id %in% d_grants_sample$grant_id) %>%
    arrange(region, grant_id, report_month)

s_sample <- d_sample %>%
    count(region) %>% 
    mutate(pct = n/sum(n))
s_sample

#### save data ####
write_csv(d_sample, paste(od, "comment_sample.csv", sep = "/"))
