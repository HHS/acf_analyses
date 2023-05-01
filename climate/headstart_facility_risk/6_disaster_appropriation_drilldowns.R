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
library(janitor) # more cleaning functions
library(gt) # pretty table formatting
library(extrafont) # custom fonts

# set design factors
theme_set(theme_minimal(base_family = "Gill Sans MT"))

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

#### expand state info to all grantees eligible for disaster funds ####
disaster_appropriation_states <- c("FL", "PR", "SC")

#  light cleanup of grantee names
d_locations <- d_locations %>%
    mutate(
        grantee = case_when(
            grantee == "Episcopal Children's Services, Inc." ~ "Episcopal Children's Services", 
            grantee == "GLEAMNS Human Resources Commission, Inc" ~ "Gleamns Human Resource Commission, Inc.",
            grantee == "Le Jardin Community Center Inc" ~ "Le Jardin Community Center, Inc.",
            grantee == "Orangeburg-Calhoun-Allendale-Bamberg Community Action Agency Inc" ~
                "Orangeburg-Calhoun-Allendale-Bamberg Community Action Agency, Inc.",
            grantee == "Step-Up Suncoast, Inc." ~ "Step Up Suncoast, Inc.",
            TRUE ~ grantee
        )
    )

s_locations <- d_locations %>%
    filter(state_code %in% disaster_appropriation_states) %>%
    group_by(state_code) %>%
    summarize(
        n_grants = n_distinct(grant_id),
        n_grantees = n_distinct(grantee),
        n_locations = n(),
        n_federal_interest = sum(is_federal_interest_site),
        n_owned = sum(is_owned_by_program),
        n_child_care = sum(is_child_care_partner_site),
        across(
            .cols = ends_with("_slots"),
            .fns = sum,
            .names = "n_{.col}"
        )
    ) %>%
    ungroup() %>%
    adorn_totals(where = "row")
glimpse(s_locations)

t_locations <- s_locations %>%
    select(
        State = state_code,
        Grantees = n_grantees,
        Grants = n_grants,
        Total = n_locations,
        `With Federal Interest` = n_federal_interest,
        Owned = n_owned,
        Overall = n_total_slots,
        HS = n_hs_slots,
        EHS = n_ehs_slots,
        `AIAN HS` = n_aian_hs_slots,
        `AIAN EHS` = n_aian_ehs_slots,
        `Migrant HS` = n_migrant_hs_slots,
        `Migrant EHS` = n_migrant_ehs_slots,
        `Child Care Partner` = n_child_care,
    ) %>%
    t() %>%
    row_to_names(row_number = 1) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(across(c(2:5), as.numeric)) %>%
    gt() %>%
    fmt_number(columns = c(2:5), decimals = 0) %>%
    tab_row_group(
        label = md("**Locations**"),
        rows = c("Total", "With Federal Interest", "Owned"),
        id = "rg_locations"
    ) %>%
    tab_row_group(
        label = md("**Grantee Concentration**"),
        rows = starts_with("Grant"),
        id = "rg_grantees"
    ) %>%
    tab_row_group(
        label = md("**Program Slots***"),
        rows = c("Overall",
            "HS", "EHS", "AIAN HS", "AIAN EHS", "Migrant HS", "Migrant EHS",
            "Child Care Partner"),
        id = "rg_programs"
    ) %>%
    row_group_order(groups = c("rg_locations", "rg_grantees", "rg_programs")) %>%
    tab_style(
        style = cell_fill(color = "#BCD9ED"),
        locations = cells_body(columns = "Total")
    )

gtsave(
    t_locations, 
    paste(od, "disaster_overall_summary.png", sep = "/"),
)

# summarize key grantee info by state
s_state_drilldown_overall <- d_locations_byrisk %>%
    filter(
        state_code %in% disaster_appropriation_states,
        risk_type == "Overall",
    ) %>%
    mutate(is_at_risk = risk_rating %in% c(
        "Very High", "Relatively High", "Relatively Moderate")) %>%
    group_by(state_code, risk_type) %>%
    summarize(
        `Total` = n(),
        `With Federal Interest` = sum(is_federal_interest_site),
        `Owned` = sum(is_owned_by_program),
        `Grantees` = n_distinct(grantee),
        `Grants` = n_distinct(grant_id),
        `Overall` = sum(total_slots),
        `HS` = sum(hs_slots),
        `EHS` = sum(ehs_slots),
        `AIAN HS` = sum(aian_hs_slots),
        `AIAN EHS` = sum(aian_ehs_slots),
        `Migrant HS` = sum(migrant_hs_slots),
        `Migrant EHS` = sum(migrant_ehs_slots),
        `Child Care Partner` = sum(is_child_care_partner_site),
    ) %>%
    ungroup()

s_state_drilldown_byrisk <- d_locations_byrisk %>%
    filter(
        state_code %in% disaster_appropriation_states,
        risk_type != "Overall",
    ) %>%
    mutate(is_at_risk = risk_rating %in% c(
        "Very High", "Relatively High", "Relatively Moderate")) %>%
    group_by(state_code, risk_type) %>%
    summarize(
        `Total` = sum(is_at_risk),
        `With Federal Interest` = sum(is_federal_interest_site * is_at_risk),
        `Owned` = sum(is_owned_by_program * is_at_risk),
        `Grantees` = n_distinct(if_else(is_at_risk, grantee, NA)),
        `Grants` = n_distinct(if_else(is_at_risk, grant_id, NA)),
        `Overall` = sum(total_slots * is_at_risk),
        `HS` = sum(hs_slots * is_at_risk),
        `EHS` = sum(ehs_slots * is_at_risk),
        `AIAN HS` = sum(aian_hs_slots * is_at_risk),
        `AIAN EHS` = sum(aian_ehs_slots * is_at_risk),
        `Migrant HS` = sum(migrant_hs_slots * is_at_risk),
        `Migrant EHS` = sum(migrant_hs_slots * is_at_risk),
        `Child Care Partner` = sum(is_child_care_partner_site * is_at_risk),
    ) %>%
    ungroup()

s_state_drilldown <- bind_rows(s_state_drilldown_overall, 
    s_state_drilldown_byrisk) %>%
    mutate(risk_type = if_else(risk_type == "Overall", risk_type, 
        paste("At-risk for", risk_type)))
glimpse(s_state_drilldown)

t_state_drilldown <- s_state_drilldown %>%
    split(.$state_code) %>%
    map(
        . %>%
            select(-state_code) %>%
            t() %>%
            row_to_names(row_number = 1) %>%
            as.data.frame() %>%
            rownames_to_column() %>%
            mutate(across(c(2:5), as.numeric)) %>%
            gt() %>%
            fmt_number(columns = c(2:5), decimals = 0) %>%
            tab_row_group(
                label = md("**Locations**"),
                rows = c("Total", "With Federal Interest", "Owned"),
                id = "rg_locations"
            ) %>%
            tab_row_group(
                label = md("**Grantee Concentration**"),
                rows = starts_with("Grant"),
                id = "rg_grantees"
            ) %>%
            tab_row_group(
                label = md("**Program Slots***"),
                rows = c("Overall",
                    "HS", "EHS", "AIAN HS", "AIAN EHS", "Migrant HS", "Migrant EHS",
                    "Child Care Partner"),
                id = "rg_programs"
            ) %>%
            row_group_order(groups = c("rg_locations", "rg_grantees", "rg_programs"))
    )

fp_state_drilldown <- str_c(
    od,
    str_c("disaster_state_summary_",names(t_state_drilldown), ".png"),
    sep = "/"
)

pwalk(
    .l = list(t_state_drilldown, fp_state_drilldown), 
    .f = gtsave,
    vwidth = 600, 
    vheight = 800
)
