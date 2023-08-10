###############################################################################
# PURPOSE: document B-5 Explorer logic for categorical dimensions
# LAST EDITED: 9 aug 2023
############################################################################### . 

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")

# load libraries
library(tidyverse)

# define directories
wd <- if_else(
    str_detect(getwd(), "GitHub"),
    getwd(),
    paste(
        getwd(), 
        "GitHub/acf_analyses/ecd/b_5_explorer", 
        sep = "/"
    )
)
dd <- paste(wd, "data", sep = "/")
od <- paste(wd, "output", sep = "/")

#### load data ####
# acs data
paste(dd, "b5_2021.Rdata", sep = "/")

# state FIPs and median income data
d_states <- read_csv(paste(dd, "state_fips_and_smi4.csv", sep = "/")) %>%
    rename(
        state_code = Code,
        state = State,
        smi4 = `4 Person SMI`,
        statefip = `State FIP`
    )

#### add bucketed dimensions ####
# calculate necessary variables
d_b5 <- d_b5 %>%
    left_join(d_states, by = "statefip") %>%
    mutate(
        # household income groupings: amount and as % of SMI
        # n.b. there are cases of 9999999 household income
        household_income = case_when(
            hhincome >= 200000 ~ "$200,000 or Greater",
            hhincome > 150000 ~ "$150,000 to $199,999",
            hhincome > 120000 ~ "$120,000 to $149,999",
            hhincome > 100000 ~ "$100,000 to $119,999",
            hhincome > 80000 ~ "$80,000 to $99,999",
            hhincome > 70000 ~ "$70,000 to $79,999",
            hhincome > 60000 ~ "$60,000 to $69,999",
            hhincome > 50000 ~ "$50,000 to $59,999",
            hhincome > 40000 ~ "$40,000 to $49,999",
            hhincome > 30000 ~ "$30,000 to $39,999",
            hhincome > 20000 ~ "$20,000 to $29,999",
            hhincome > 10000 ~ "$10,000 to $19,999",
            hhincome > 0 ~ "$0 to $9999",
            TRUE ~ "None or Unreported"
        ),
        hh_pctof_smi = case_when(
            hhincome > 0 ~ hhincome/smi4,
            TRUE ~ 0
        ),
        household_income_as_percent_of_state_median_income = case_when(
            hh_pctof_smi > 5 ~ "500% or Greater",
            hh_pctof_smi > 4 ~ "400% to 499%",
            hh_pctof_smi > 3 ~ "300% to 399%",
            hh_pctof_smi > 2 ~ "200% to 299%",
            hh_pctof_smi > 1.49 ~ "150% to 199%",
            hh_pctof_smi > 1 ~ "100% to 149%",
            hh_pctof_smi > 0.5 ~ "50% to 99%",
            hh_pctof_smi > 0.25 ~ "25% to 49%",
            hh_pctof_smi > 0 ~ "0% to 24%",
            TRUE ~ "0% OF SMI"
        ),
        # ethnicity
        child_ethnicity = case_when(
            hispan == 0 ~ "Not Hispanic or Latino",
            hispan == 9 ~ "Unknown",
            TRUE ~ "Hispanic or Latino"
        ),
        # race: aligned with proposed Census Bureau update
        child_race = case_when(
            race == 1 ~ "White, including Middle Eastern or North African",
            race == 2 ~ "Black or African American",
            race == 3 ~ "American Indian or Alaska Native",
            race == 7 ~ "Other Race",
            between(raced, 400, 620) ~ "Asian",
            between(raced, 640, 643) ~ "Asian",
            between(raced, 660, 679) ~ "Asian",
            between(raced, 630, 634) ~ "Native Hawaiian or Other Pacific Islander",
            between(raced, 680, 699) ~ "Native Hawaiian or Other Pacific Islander",
            race >= 8 ~ "Multi-Race",
        ),
        # poverty level
        household_income_as_percentage_of_poverty_line = case_when(
            poverty < 50 ~ "Less than 50%",
            poverty < 100 ~ "50% to 99%",
            poverty < 150 ~ "100% to 149%",
            poverty < 200 ~ "150% to 199%",
            poverty < 250 ~ "200% to 249%",
            poverty < 300 ~ "250% to 299%",
            poverty < 350 ~ "300% to 349%",
            poverty < 400 ~ "350% to 399%",
            poverty < 450 ~ "400% to 449%",
            poverty < 500 ~ "450% to 499%",
            poverty >= 500 ~ "500% or Greater"
        ),
        # parent age
        parents_age_score = rowSums(across(starts_with("age_"), ~case_when(. < 18 ~ 1, TRUE ~ 0))),
        parents_age = case_when(
            parents_age_score == 0 ~ "Both Parents Over 18",
            parents_age_score == 1 ~ "One Parent Under 18",
            TRUE ~ "Both Parents Under 18"
        ),
        # parent education
        parents_college_score = rowSums(across(starts_with("educd_"), ~case_when(. > 100 ~ 1, TRUE ~ 0))),
        parents_high_school_score = rowSums(across(starts_with("educd_"), ~case_when(. > 61 ~ 1, TRUE ~ 0))),
        parents_education = case_when(
            parents_college_score > 1 ~ "Bachelors or Greater: Both Parents",
            parents_college_score == 1 ~ "Bachelors or Greater: One Parent",
            parents_high_school_score > 1 ~ "High School Diploma or GED: Both Parents",
            parents_high_school_score == 1 ~ "High School Diploma or GED: One Parent",
            TRUE ~ "No High School or Higher Education Degree: Both Parents"
        ),
        # parent employment
        parents_employment_score = rowSums(across(starts_with("empstat_"), ~case_when(. == 1 ~ 1, TRUE ~ 0))),
        parents_employment = case_when(
            parents_employment_score > 1 ~ "Both Parents Employed",
            parents_employment_score == 1 ~ "One Parent Employed",
            TRUE ~ "Both Parents Unemployed or Not in Labor Force"
        )
    )