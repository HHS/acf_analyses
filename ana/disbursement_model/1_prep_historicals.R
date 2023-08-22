###############################################################################
# PURPOSE: munge historical award data
# LAST EDITED: 22 august2023
############################################################################### . 

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")

# load libraries
library(tidyverse) # the most useful functions!
library(readxl) # reading Excel
library(lubridate) # making it easy to work with dates
library(fs) # for file navigation
library(scales) # for better scale labeling
library(extrafont) # custom fonts

# set design factors
acf_palette <- c(
    "#264A64", # ACF blue
    "#336A90", # medium blue
    "#BCD9ED", # pale blue
    "#BFB0A3", # neutral
    "#DDE2E8", # light cool gray
    "#475260", # medium gray
    "#63BAB0", # teal
    "#F9E585", # yellow
    "#A12854", # magenta
    "#E29F4D" # orange
)

loadfonts(device = "win")

theme_set(theme_minimal(base_family = "Gill Sans MT"))

# define directories
wd <- if_else(
    str_detect(getwd(), "GitHub"),
    getwd(),
    paste(
        getwd(), 
        "GitHub/acf_analyses/ana/disbursement_model", 
        sep = "/"
    )
)
dd <- paste(wd, "data", sep = "/")
od <- paste(wd, "output", sep = "/")

#### load data ####
# create list of all data files
files <- dir_ls(dd) %>%
    str_subset("Decision Book") %>%
    str_subset("2019", negate = TRUE) %>%
    str_subset("Original", negate = TRUE)

# define function to pull specific fields of interest
# from a decision book sheet
pull_fields <- function(data) {
    cleaned <- data %>%
        # pull and rename approved amounts
        rename_with(
            .fn = ~paste0("amt_", str_sub(., -2, -1)) %>% 
                str_to_lower, 
            .cols = contains("Approved Amount")
        ) %>%
        # pull and rename other needed data elements
        select(
            application_id = contains("Number") & 
                !contains("Serial") &
                !contains("Panel") &
                !contains("Grant"),
            grantee = `Name`,
            decision = starts_with("Fund"),
            starts_with("amt_"),
        ) %>%
        # if there's approved funding and missing decision, then indicate 
        # application is funded
        mutate(decision = if_else(!is.na(amt_y1), "F", as.character(decision))) %>%
        # focus on funded applications and ignore subtotal rows
        filter(
            str_detect(decision, "^F"),
            !is.na(grantee),
        ) %>%
        # do light data clean-up
        mutate(
            grantee = str_to_lower(grantee),
            application_id = as.character(application_id),
            decision = as.character(decision),
            across(starts_with("amt_"), ~as.numeric(.)),
        )
    
    return(cleaned)
}

# define function to load all sheets in a file
load_decision_book <- function(path) {
    
    # save year of decision book
    year <- str_extract(path, "[[:digit:]]+")
    
    # get all data sheet names in workbook
    sheets <- path %>%
        excel_sheets() %>%
        set_names() %>%
        str_subset("[^Dashboard]") %>%
        str_subset("[^Data]") %>%
        str_subset("[^Sheet1]")
    
    # load sheet data into a single list
    data <- map(
        .x = sheets, 
        .f = read_excel, 
        path = path
    )
    names(data) <- sheets
    
    # pull data fields of interest and combine into one data frame
    combined <- map(.x = data, .f = pull_fields)
    
    result <- combined %>%
        bind_rows(.id = "grant") %>%
        mutate(fy_approved = year)
    
    return(result)
}

# load decision books
d_raw <- map(files, load_decision_book) %>%
    bind_rows()

#### prepare data for analysis ####
# create disbursal-level dataset
d_disbursals_partial <- d_raw %>%
    select(-decision) %>%
    mutate(
        # remove inconsistent year labels
        grant = str_extract(grant, "[[:alpha:]&-]+"),
        # make fy approved numeric
        fy_approved = as.numeric(fy_approved)
    ) %>%
    # reshape to have fy amounts in separate rows
    pivot_longer(
        cols = starts_with("amt_"),
        names_to = "yr_n",
        names_prefix = "amt_y",
        values_to = "amt"
    ) %>%
    mutate(
        yr_n = as.numeric(yr_n),
        fy_disbursed = fy_approved + 
            yr_n -
            1, # because the year of award is considered "y1"
        grant_type = if_else(
            grant %in% c("EMI", "P&M"), 
            "language",
            "development"
        ),
        amt = if_else(amt == 0, NA, amt),
    ) %>%
    #reorder columns
    select(
        grant,
        grant_type,
        application_id,
        grantee,
        yr_n,
        fy_approved,
        fy_disbursed,
        amt,
    )
glimpse(d_disbursals_partial)

# add approximate data for 2023 SEDS based on average annual disbursement and
# duration from historicals
s_seds <- d_disbursals_partial %>%
    filter(
        grant == "SEDS",
        !is.na(amt),
    ) %>%
    group_by(application_id) %>%
    summarize(
        yrs = n(),
        amt_total = sum(amt),
    ) %>%
    ungroup() %>%
    summarize(
        amt_yr_avg = mean(amt_total/yrs),
        avg_yrs = mean(yrs), # ~3
    )
s_seds

fy23_total_available <- 49696475
fy23_funded <- d_disbursals_partial %>%
    filter(fy_disbursed == 2023) %>%
    summarize(amt = sum(amt, na.rm = TRUE)) %>%
    pull(amt)
fy23_seds_available <- fy23_total_available - fy23_funded
fy_23_seds_grants <- ceiling(fy23_seds_available/(s_seds %>% pull(amt_yr_avg)))

d_disbursals_seds <- tibble(
    grant = "SEDS",
    grant_type = "development",
    application_id = paste0("assumption", rep(1:fy_23_seds_grants, 3)) %>% sort(),
    grantee = application_id,
    yr_n = rep(1:3, fy_23_seds_grants),
    fy_approved = 2023,
    fy_disbursed = fy_approved + yr_n - 1,
    amt = fy23_seds_available/fy_23_seds_grants, # use all available funding
)

d_disbursals <- bind_rows(d_disbursals_partial, d_disbursals_seds) %>%
    filter(!is.na(amt))

# sanity check: basics
glimpse(d_disbursals)
n_distinct(d_disbursals$application_id)
n_distinct(d_disbursals$grantee)
summary(d_disbursals$amt)

# sanity check: trend by program
s_programs <- d_disbursals %>% 
    group_by(fy_approved, grant) %>%
    summarize(
        n = n_distinct(application_id),
        amt_approved = sum(amt)
    ) %>%
    ungroup()

g_programs <- ggplot(s_programs) +
    geom_bar(
        aes(
            x = fy_approved,
            y = amt_approved/1e6,
            fill = grant,
        ),
        stat = "identity",
    ) +
    scale_fill_manual(values = acf_palette) +
    labs(
        title = "ANA historical approved funding",
        subtitle = "based on decision books",
        x = "FY Approved",
        y = "Approved Amount ($ million)",
        caption = "2023 numbers based on projections",
    ); g_programs
ggsave(
    paste(od, "approved_funding_2020-2023.jpg", sep = "/"),
    unit = "in",
    height = 4,
    width = 6,
)

# create grant-level dataset
d_grants <- d_disbursals %>%
    group_by(grant, grant_type, application_id, grantee, fy_approved) %>%
    summarize(
        duration_yrs = n(),
        amt_total = sum(amt),
    ) %>%
    ungroup()
glimpse(d_grants)

#### save prepped data ####
save(
    d_disbursals, 
    d_grants,
    s_programs,
    file = paste(dd, "data_clean.Rdata", sep = "/")
)

write_csv(d_disbursals, paste(od, "disbursals.csv", sep = "/"))
write_csv(d_grants, paste(od, "grants.csv", sep = "/"))
