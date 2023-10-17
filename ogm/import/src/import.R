library(tidyverse)
library(readxl)
`%nin%` <- Negate(`%in%`)


# USA Spending ----
# data dictionary: https://www.usaspending.gov/data-dictionary
tribal_business_types <- c(
  'INDIAN/NATIVE AMERICAN TRIBAL GOVERNMENT (FEDERALLY-RECOGNIZED)',
  'INDIAN/NATIVE AMERICAN TRIBAL DESIGNATED ORGANIZATION',
  'INDIAN/NATIVE AMERICAN TRIBAL GOVERNMENT (OTHER THAN FEDERALLY-RECOGNIZED)',
  'INDIAN/NATIVE AMERICANTRIBAL GOVERNMENT (FEDERALLY RECOGNIZED)',
  'ALASKA NATIVE AND NATIVE HAWAIIAN SERVING INSTITUTIONS',
  'TRIBALLY CONTROLLED COLLEGE OR UNIVERSITY (TCCU)'
)

end_of_fy <- ymd('2022-09-30')

years <- as.character(c(2015:2020))

get_grant_data <- function(year) {
  temp <- tempfile()
  url <- str_glue('https://files.usaspending.gov/award_data_archive/FY{year}_075_Assistance_Full_20231008.zip')
  download.file(url, temp)
  data <- read_csv(unzip(temp), show_col_types = F)
  unlink(temp)
  return(data)
}

usa_spending_data <- years %>%
  set_names() %>% 
  map( \(x) get_grant_data(x))

acf_tribal_grant_data <- usa_spending_data %>%
  map(\(x) {
    x %>%
     filter(
       awarding_sub_agency_code == "7590",
       period_of_performance_current_end_date <= end_of_fy, 
       business_types_description %in% tribal_business_types
     ) %>% mutate(
       recipient_address_line_2 = as.character(recipient_address_line_2)
     )
  }) %>%
  reduce(bind_rows)

# active ACF grants ----
active_acf_grants <- read_excel(here::here('ogm', 'import', 'input', 'ACF Active Grants - 10132023.xlsx'))

closed_acf_tribal_grant_data <- acf_tribal_grant_data %>%
  filter(award_id_fain %nin% active_acf_grants$award_id_fain)

# export ----
export_path <- here::here('ogm', 'import', 'output', 'grant_data.csv')
write_csv(closed_acf_tribal_grant_data, export_path)
