###############################################################################
# PURPOSE: munge historical award data
# LAST EDITED: 25 august2023
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
library(here)

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
wd <- here()
dd <- here(wd, "data")
od <- here(wd, "output")

#### load data ####
# create list of all data files
files <- dir_ls(dd) %>%
    str_subset("Decision Book") %>%
    str_subset(regex("2019|Original|Projections|202308"), negate = TRUE)

# define function to pull specific fields of interest
# from a decision book sheet
pull_fields <- function(data) {
    cleaned <- data %>%
        # pull and rename requested and approved amounts
        rename_with(
            .fn = ~paste0("amt_app_", str_sub(., -2, -1)) %>% 
                str_to_lower, 
            .cols = contains("Approved Amount")
        ) %>%
        rename_with(
            .fn = ~paste0("amt_req_y", str_sub(., -1, -1)) %>% 
                str_to_lower, 
            .cols = contains("Amount Requested")
        ) %>%
        # pull and rename other needed data elements
        select(
            application_id = contains("Number") & 
                !contains("Serial") &
                !contains("Panel") &
                !contains("Grant"),
            grant_number = contains("Number") & contains("Grant"),
            grantee = `Name`,
            decision = starts_with("Fund"),
            starts_with("amt_"),
        ) %>%
        mutate(
            # if amount is zero, clear out for consistency with NAs
            across(starts_with("amt_"), ~if_else(.x == 0, NA, .x)),
            # if there's approved funding and missing decision, then indicate 
            # application is funded
            decision = if_else(!is.na(amt_app_y1), "F", as.character(decision))
        ) %>%
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
            # there are some coerced NAs due to "no data" entries
            across(starts_with("amt_"), ~as.numeric(.)),
        )
    
    return(cleaned)
}

# define function to load all sheets in a file
load_decision_book <- function(path) {
    
    # save year of decision book
    year <- str_extract(path, "[[:digit:]]+") %>% as.integer()
    print(year)
    
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
d_disbursals <- d_raw %>%
    select(-decision) %>%
    mutate(
        # remove inconsistent year labels
        grant = str_extract(grant, "[[:alpha:]&-]+")
    ) %>%
    # reshape to have fy requested and approved amounts in separate rows
    pivot_longer(
        cols = starts_with("amt_"),
        names_to = "type_yn",
        values_to = "amt"
    ) %>%
    mutate(
        type = str_extract(type_yn, "[req|app]{3}"),
        yr_n = str_extract(type_yn, "[[:digit:]]") %>% as.numeric
    ) %>%
    # reshape to put requested and approved amounts side-by-side
    select(-type_yn) %>%
    pivot_wider(
        names_from = type,
        names_prefix = "amt_",
        values_from = amt,
    ) %>%
    mutate(
        # fill holes in FY 23 approved amounts for years 2-5 continued funding
        # (the FY23 decision book didn't fill in the approved funding past Y1;
        # per ANA, if Y1 is funded, subsequent requested amounts were too)
        amt_app = if_else(
            fy_approved == 2023 & yr_n > 1 & is.na(amt_app), 
            amt_req, 
            amt_app
        ),
        fy_disbursed = fy_approved + 
            yr_n -
            1, # because the year of award is considered "y1"
        grant_type = if_else(
            grant %in% c("EMI", "P&M"), 
            "language",
            "development"
        )
    ) %>%
    filter(!is.na(amt_app)) %>%
    #reorder columns
    select(
        grant,
        grant_type,
        application_id,
        grantee,
        yr_n,
        fy_approved,
        fy_disbursed,
        amt_app,
    )

# sanity check: basics
glimpse(d_disbursals)
assertr::verify(
  d_disbursals, 
  n_distinct(d_disbursals$application_id) == nrow(d_raw),
  success_fun = assertr::success_logical
)
n_distinct(d_disbursals$application_id)
n_distinct(d_disbursals$grantee)
summary(d_disbursals$amt_app)

# remove grant "approved" amounts that are actually sums of approved amounts
# for a program in a given year
bad_data <- d_disbursals %>% 
    filter(application_id %in% c("NA23004143", "NL23002470", "NL22002310")) %>% 
    pull(application_id)

d_disbursals <- d_disbursals %>% 
    filter(!(application_id %in% bad_data))

# sanity check: trend by program
s_programs <- d_disbursals %>% 
    group_by(fy_approved, grant) %>%
    summarize(
        n = n_distinct(application_id),
        amt_approved = sum(amt_app)
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
        caption = "as of August 17, 2023",
    ); g_programs
ggsave(
    paste(od, "approved_funding_2020-2023.jpg", sep = "/"),
    unit = "in",
    height = 4,
    width = 6,
)

# Confirm that totals align
nccs_final <- here(dd, "2024 NCCsFINALforMia.xlsx")
continuations_model <- readxl::read_excel(
  here(od, "20230825 Projection - Grant Disbursement Change Model.xlsm"),
  sheet="MODEL",
  range = "G2:N32"
) %>%
  janitor::clean_names() %>%
  filter(fy_dispersed == 2024) %>%
  transmute(
    grant = trimws(grant),
    continuations = committed_continuations
  )

continuations_actual <- readxl::read_excel(
  nccs_final,
  sheet = "TOTALS",
  col_names = FALSE
) %>%
  setNames(c("grant", "continuations")) %>%
  select(grant, continuations) %>%
  mutate(grant = trimws(grant))

continuations_diff <- function(list_of_errors, data) {
  data %>%
    transmute(
      grant,
      difference = continuations.y - continuations.x
    ) %>%
    return()
}

d_continuations_diff <- continuations_model %>%
  left_join(
    continuations_actual,
    by = "grant",
    relationship = "one-to-one"
  ) %>%
  filter(!is.na(continuations.y)) %>%
  assertr::verify(continuations.x == continuations.y, error_fun = continuations_diff)

if (!all(d_continuations_diff == TRUE)) {
  
  continuations_diff_check <- function(list_of_errors, data) {
    data %>%
      mutate(difference = revised_fy24_federal_amount - amt_app) %>%
      group_by(grant) %>%
      summarize(difference = sum(difference)) %>%
      left_join(
        d_continuations_diff,
        by = "grant"
      ) %>%
      # {
      #   print(.) %>%
      #     {return(.)}
      # } %>%
      assertr::verify(difference.x == difference.y, error_fun = assertr::just_warn)
    
    return(data)
    
  }
  
  disbursals_actual <- read_excel(nccs_final) %>%
    janitor::clean_names() %>%
    mutate(
      grantee = str_to_lower(grantee_name),
      grant = case_when(
        grepl("90NA", grant_number) ~ "SEDS",
        grepl("90NB", grant_number) ~ "EMI",
        grepl("90NL", grant_number) ~ "PMI",
        grepl("90NK", grant_number) ~ "SEDS-AK",
        grepl("90NR", grant_number) ~ "ERE"
      )
    )
  
  disbursals_model <- d_disbursals %>%
    filter(fy_disbursed == "2024") %>%
    fedmatch::merge_plus(
      disbursals_actual,
      by = c("grantee", "grant"),
      match_type = "multivar",
      unique_key_1 = "application_id",
      unique_key_2 = "grant_number",
      multivar_settings = fedmatch::build_multivar_settings(
        compare_type = c("stringdist", "indicator"),
        wgts = c(.25, .75),
        top = 1
      )
    )
  
  disbursals_model$matches %>%
    filter(grantee_compare < .75) %>%
    select(grant_1, grantee_1, fy_approved)
  
  disbursals_model$matches %>%
    mutate(
      revised_fy24_federal_amount = ifelse(grantee_compare < .75, 0, revised_fy24_federal_amount),
      grant = coalesce(grant_1, grant_2)
    ) %>%
    assertr::verify(amt_app == revised_fy24_federal_amount, error_fun = continuations_diff_check) %>%
    select(amt_app, revised_fy24_federal_amount)


}

emi_differences <- disbursals_model$matches %>%
  filter(grant_1 == "EMI" & amt_app != revised_fy24_federal_amount) %>%
  transmute(
    grant = grant_1,
    grantee = grantee_1,
    amt_app, revised_fy24_federal_amount
  )


# Confirm that disbursals align
d_disbursals %>%
  filter(fy_disbursed == 2024) %>%
  left_join(
    select(d_raw, application_id, grant_number),
    by = "application_id"
  ) %>%
  left_join(
    disbursals_actual,
    by = "grant_number"
  ) %>%
  filter(!is.na(grant_number)) %>%
  mutate(amt_app = round(amt_app)) %>%
  assertr::verify(
    amt_app == approved_fy_24_federal_amount,
    success_fun = assertr::success_logical
  )

# create grant-level dataset
d_grants <- d_disbursals %>%
    group_by(grant, grant_type, application_id, grantee, fy_approved) %>%
    summarize(
        duration_yrs = n(),
        amt_total = sum(amt_app),
    ) %>%
    ungroup() %>%
    mutate(
        fy_end = fy_approved + duration_yrs - 1
    )
glimpse(d_grants)

#### save prepped data ####
save(
    d_disbursals, 
    d_grants,
    s_programs,
    emi_differences,
    file = file.path(dd, "data_clean.Rdata")
)

write_csv(d_disbursals, file.path(od, "disbursals.csv"))
write_csv(d_grants, file.path(od, "grants.csv"))
