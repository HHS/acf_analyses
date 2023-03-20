###############################################################################
# PURPOSE: understand HS facility risk of tornadoes, hurricanes, wildfires
# LAST EDITED: 17 mar 2023
# RELATED: https://public.tableau.com/app/profile/janeyang/viz/HeadStartFacilityHazardRisks/NaturalHazardRiskforHeadStartFacility
############################################################################### . 

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")

# load libraries
library(tidyverse) # the most useful functions!
library(janitor) # more cleaning functions
library(ggfittext) # for more plot styling
library(scales) # for pretty graph scales
library(sf) #  for geospatial file handling
library(gt) # pretty table formatting
library(extrafont) # custom fonts

# set design factors
theme_set(theme_minimal(base_family = "Gill Sans MT"))

acf_palette <- c(
    "#264A64", # ACF blue
    "#336A90", # medium blue
    "#BCD9ED", # pale blue
    "#BFB0A3", # neutral
    "#475260", # medium gray
    "#DDE2E8", # light cool gray
    "#63BAB0", # teal
    "#F9E585", # yellow
    "#A12854", # magenta
    "#E29F4D" # orange
)

loadfonts(device = "win")

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

#### load data ####
# core datasets
load(paste(dd, "4_analyzable.Rdata", sep = "/"))

# create version of risk table focused on moderate -> high risk
d_facilities_atrisk <- d_facilities_byrisk %>% 
    filter(risk_rating %in% c(
        "Very High", 
        "Relatively High", 
        "Relatively Moderate")
    )

#### calculate/plot basic stats for baseline understanding of facilities ####
# number of facilities
n_facilities <- nrow(d_facilities)

# interest, ownership, program overview
d_pcts <- d_facilities %>%
    summarize(across(starts_with("is_"), mean)) %>%
    glimpse() %>%
    pivot_longer(
        cols = everything(),
        names_to = "program",
        values_to = "pct_facilities"
    ) %>%
    mutate(
        program = case_when(
            program == "is_federal_interest_site" ~ "Federal Interest",
            program == "is_owned_by_program" ~ "Program-Owned",
            program == "is_child_care_partner_site" ~ "Child Care Partner",
            program == "is_aian_ehs_site" ~ "AIAN Early Head Start",
            program == "is_aian_hs_site" ~ "AIAN Head Start",
            program == "is_migrant_ehs_site" ~ "Migrant Early Head Start",
            program == "is_migrant_hs_site" ~ "Migrant Head Start",
            program == "is_ehs_site" ~ "Early Head Start",
            program == "is_hs_site" ~ "Head Start",
        )
    ); d_pcts

p_program <- ggplot(
        data = d_pcts %>% 
            filter(!(program %in% c("Federal Interest", "Program-Owned"))),
        aes(
            x = fct_reorder(program, pct_facilities) %>% fct_rev,
            y = pct_facilities
        )
    ) +
    geom_bar(
        stat = "identity",
        fill = acf_palette[2]
    ) +
    geom_text(
        mapping = aes(
            label = comma(pct_facilities * n_facilities),
            y = if_else(pct_facilities > 0.1,
                pct_facilities - 0.06,
                pct_facilities + 0.06
            ),
            color = if_else(pct_facilities > 0.1, "white", "black"),
        ),
        stat = "identity",
        place = "top",
        family = "Gill Sans MT",
        size = 3,
    ) +
    geom_text(
        mapping = aes(
            label = percent(pct_facilities, accuracy = 1),
            y = if_else(pct_facilities > 0.1,
                pct_facilities - 0.02,
                pct_facilities + 0.02
            ),
            color = if_else(pct_facilities > 0.1, "white", "black"),
        ),
        stat = "identity",
        place = "top",
        family = "Gill Sans MT",
        size = 3,
    ) +
    # add title, etc.
    labs(
        x = "",
        y = "",
    ) + 
    # customize scales
    scale_x_discrete(
        labels = function(x) str_wrap(x, width = 10),
        expand = c(0, 0)
    ) +
    scale_y_continuous(expand = c(0, .005)) +
    scale_color_manual(
        values = c("white" = "white", "black" = "black"),
        guide = "none"
    ) +
    # more aesthetic tweaking
    theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
    )
p_program
ggsave(
    filename = paste(od, "program.jpg", sep = "/"), 
    plot = p_program,
    width = 8,
    height = 2.8,
)

# share by federal interest and ownership
count_by_group(d_facilities, n, is_federal_interest_site, is_owned_by_program)

# state breakdown by federal interest: percent
d_state_interest_pct <- d_facilities %>%
    group_by(state_code) %>%
    summarize(pct_federal_interest = mean(is_federal_interest_site)) %>%
    ungroup()

p_state_interest_pct <- ggplot(
        data = d_state_interest_pct,
        mapping = aes(
            x = pct_federal_interest,
            y = fct_reorder(state_code, pct_federal_interest),
        )
    ) +
    geom_bar(
        stat = "identity",
        fill = acf_palette[1]
    ) +
    geom_text(
        mapping = aes(
            label = percent(pct_federal_interest, accuracy = 1),
            x = pct_federal_interest - 0.03
        ),
        stat = "identity",
        position = "identity",
        color = "white",
        size = rel(2),
    ) +
    # add title, etc.
    labs(
        x = "",
        y = "",
        title = str_wrap("Share of Head Start facilities with federal interest, 
            by state", width = 55),
        caption = str_wrap("Sources: Facilities pulled from Head Start 
            Enterprise System as of March 7, 2023.", width = 85)
    ) + 
    # customize scales
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0,0)) +
    # more aesthetic tweaking
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = rel(0.8)),
        title = element_text(size = rel(0.8))
    )
p_state_interest_pct
ggsave(
    filename = paste(od, "state_and_interest_pct.jpg", sep = "/"), 
    plot = p_state_interest_pct,
    width = 6,
    height = 8,
)

# state breakdown by federal interest: count
p_state_interest <- ggplot(
        data = d_facilities,
        mapping = aes(
            y = fct_infreq(state_code) %>% fct_rev,
            fill = is_federal_interest_site,
        )
    ) +
    geom_bar(stat = "count") +
    geom_text(
        mapping = aes(
            label = after_stat(count),
            color = if_else(is_federal_interest_site, "black", "white"),
        ),
        stat = "count",
        position = position_stack(reverse = TRUE, vjust = 0.5),
        size = rel(2),
    ) +
    # add title, etc.
    labs(
        x = "",
        y = "",
        title = "Head Start facilities by state",
        caption = str_wrap("Sources: Facilities pulled from Head Start 
            Enterprise System as of March 7, 2023.", width = 85)
    ) + 
    # customize scale
    scale_fill_manual(
        name = "Federal Interest",
        values = c("FALSE" = acf_palette[1], "TRUE" = acf_palette[3]),
        labels = c("No", "Yes"),
        guide = guide_legend(reverse = TRUE)
    ) +
    scale_color_manual(
        values = c("white" = "white", "black" = "black"),
        guide = "none"
    ) +
    # format axes
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    # extra formatting
    theme(
        # format legend
        legend.position = "top",
        legend.justification = "left",
        # get rid of visual distractions
        panel.grid = element_blank(),
        axis.text.x = element_blank()
    )
p_state_interest
ggsave(
    filename = paste(od, "state_and_interest.jpg", sep = "/"), 
    plot = p_state_interest,
    width = 6,
    height = 8,
)
    
# federal interest by ownership
d_state_interest_ownership_pct <- d_facilities %>%
    group_by(state_code) %>%
    summarize(pct_federal_interest = mean(is_federal_interest_site)) %>%
    ungroup()

p_state_interest_pct <- ggplot(
        data = d_state_interest_pct,
        mapping = aes(
            x = pct_federal_interest,
            y = fct_reorder(state_code, pct_federal_interest),
        )
    ) +
    geom_bar(
        stat = "identity",
        fill = acf_palette[1]
    ) +
    geom_text(
        mapping = aes(
            label = percent(pct_federal_interest, accuracy = 1),
            x = pct_federal_interest - 0.03
        ),
        stat = "identity",
        position = "identity",
        color = "white",
        size = rel(1),
    ) +
    # add title, etc.
    labs(
        x = "",
        y = "",
        title = str_wrap("Share of Head Start facilities with federal interest, 
            by state", width = 55),
        caption = str_wrap("Sources: Facilities pulled from Head Start 
            Enterprise System as of March 7, 2023.", width = 85)
    ) + 
    # customize scales
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0,0)) +
    # more aesthetic tweaking
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = rel(0.8)),
        title = element_text(size = rel(0.8))
    )
p_state_interest_pct
ggsave(
    filename = paste(od, "state_and_interest_pct.jpg", sep = "/"), 
    plot = p_state_interest_pct,
    width = 4,
    height = 8,
)

#### Q: what is overall and by-hazard type risk Head Start facilities face? ####
# overall x risk type
g_hazards <- ggplot(
        data = d_facilities_byrisk,
        mapping = aes(
            x = risk_type,
            fill = risk_rating
        )
    ) +
    geom_bar(stat = "count") +
    geom_bar_text(
        mapping = aes(
            label = percent(
                after_stat(count/tapply(count, x, sum)[as.character(x)]),
                accuracy = 0.1
            ),
        ),
        stat = "count",
        position = "stack",
        place = "middle",
        size = 8,
        contrast = TRUE,
        family = "Gill Sans MT"
    ) +
    # add title, etc.
    labs(
        x = "",
        y = "Number of Head Start Facilities",
        title = "Head Start facility risk levels: by natural hazards",
        caption = str_wrap("Sources: Facilities pulled from Head Start 
            Enterprise System as of March 7, 2023. Risk ratings based on FEMA 
            National Risk Index, November 2021 Release.", width = 85)
    ) + 
    # customize scale
    scale_fill_brewer(
        name = "Risk Rating",
        type = "seq",
        palette = "OrRd",
        direction = -1,
    ) +
    # format axes
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    # extra formatting
    theme(
        # format legend
        legend.position = "right",
        legend.justification = "left",
    )
g_hazards
ggsave(
    filename = paste(od, "risk.jpg", sep = "/"), 
    plot = g_hazards,
    width = 9,
    height = 6,
)

#### Q: how does it look by federal interest? ####
# including all risk ratings
g_interest <- ggplot(
        data = d_facilities_byrisk,
        mapping = aes(
            x = risk_rating,
            fill = is_federal_interest_site
        )
    ) +
    geom_bar() +
    geom_bar_text(
        mapping = aes(label = after_stat(count)),
        stat = "count",
        position = "stack",
        place = "middle",
        size = 8,
        contrast = TRUE,
        family = "Gill Sans MT"
    ) +
    # by risk type
    facet_wrap(
        ~risk_type,
        ncol = 2,
    ) +
    # add title, etc.
    labs(
        x = "",
        y = "Number of Head Start Facilities",
        title = "Head Start facility risk levels: by federal interest",
        caption = str_wrap("Sources: Facilities pulled from Head Start 
            Enterprise System as of March 7, 2023. Risk ratings based on FEMA 
            National Risk Index, November 2021 Release.", width = 85)
    ) + 
    # customize scale
    scale_fill_manual(
        name = "Federal Interest",
        values = c("FALSE" = acf_palette[1], "TRUE" = acf_palette[3]),
        labels = c("No", "Yes")
    ) +
    # wrap rating labels
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    # extra formatting
    theme(
        # format legend
        legend.position = "top",
        legend.justification = "left",
        # format facet labels
        strip.text = element_text(face = "bold"),
        # format axis label size
        axis.text.x = element_text(size = rel(0.8))
    )
g_interest
ggsave(
    filename = paste(od, "risk_and_interest.jpg", sep = "/"), 
    plot = g_interest,
    width = 8,
    height = 6,
)

#### Q: for at-risk federal interest buildings, what's ownership breakdown? ####
# TBD DISCUSSION
    
#### Q: for at-risk federal interest buildings, what's program breakdown ####
# TBD DISCUSSION

#### Q: where are there high concentrations of risk? ####
# by state and natural hazard
fi_labels <- c("No Federal Interest", "With Federal Interest")
names(fi_labels) <- c(FALSE, TRUE)

g_state_concentration <- d_facilities_atrisk %>%
    split(.$risk_type) %>%
    map(
        ~ggplot(
            data = .,
            mapping = aes(
                x = fct_infreq(state_code) %>% fct_rev,
                fill = risk_rating
            )
        ) +
        geom_bar(stat = "count") +
        geom_bar_text(
            mapping = aes(label = after_stat(count)),
            stat = "count",
            position = "stack",
            place = "middle",
            size = 6,
            min.size = 4,
            contrast = TRUE,
            family = "Gill Sans MT"
        ) +
        coord_flip() +
        # facet_wrap(
        #     ~is_federal_interest_site,
        #     labeller = labeller(is_federal_interest_site =  fi_labels)
        # ) +
        # add title, etc.
        labs(
            x = "",
            y = "",
            title = str_c("States with moderate-to-high", unique(.$risk_type), "risk", sep = " "),
            caption = str_wrap("Sources: Facilities pulled from Head Start 
            Enterprise System as of March 7, 2023. Risk ratings based on FEMA 
            National Risk Index, November 2021 Release.", width = 85)
        ) + 
        scale_fill_brewer(
            name = "Risk Rating",
            type = "seq",
            palette = "OrRd",
            direction = -1,
            guide = guide_legend(reverse = TRUE)
        ) +
        # format axes
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        # extra formatting
        theme(
            # format legend
            legend.position = "top",
            legend.justification = "left"
        )
    )

fp_state_concentration <- str_c(
    od,
    str_c("state_",names(g_state_concentration), ".jpg"),
    sep = "/"
)

pwalk(
    .l = list(fp_state_concentration, g_state_concentration), 
    .f = ggsave,
    width = 5, 
    height = 6
)

#### state drill-downs ####
# define states of interest
focus_states <- c("CA", "FL", "LA", "SC", "TX")

# summarize key grantee info by state
t_state_drilldown <- d_facilities_atrisk %>%
    filter(state_code %in% focus_states) %>%
    split(.$state_code) %>%
    map(
        . %>%
            group_by(risk_type) %>%
            summarize(
                `Total` = n_distinct(facility_id),
                `With Federal Interest` = n_distinct(
                    if_else(is_federal_interest_site, facility_id, NA)),
                `Owned` = n_distinct(
                    if_else(is_owned_by_program, facility_id, NA)),
                `Grantees` = n_distinct(grantee),
                `Grants` = n_distinct(grant_id),
            ) %>%
            ungroup() %>%
            t() %>%
            row_to_names(row_number = 1) %>%
            as.data.frame() %>%
            rownames_to_column() %>%
            gt() %>%
            tab_spanner(
                label = md("**Risk Type**"),
                columns = everything(),
            ) %>%
            tab_row_group(
                label = md("**At-Risk Facilities**"),
                rows = matches("[Grant]{0}"),
                id = "rg_facilities"
            ) %>%
            tab_row_group(
                label = md("**Grantee Concentration**"),
                rows = starts_with("Grant"),
                id = "rg_grantees"
            ) %>%
            row_group_order(groups = c("rg_facilities", "rg_grantees"))
    )

fp_state_drilldown <- str_c(
    od,
    str_c("state_summary_",names(t_state_drilldown), ".png"),
    sep = "/"
)

pwalk(
    .l = list(t_state_drilldown, fp_state_drilldown), 
    .f = gtsave,
    vwidth = 400, 
    vheight = 600
)

# find grantees with most number of facilities in state
d_facilities_atrisk %>%
    filter(state_code %in% focus_states) %>%
    split(.$state_code) %>%
    map(
        . %>%
            group_by(grantee) %>%
            summarize(n_facilties_per_grantee = n_distinct(facility_id)) %>%
            ungroup() %>%
            arrange(n_facilties_per_grantee %>% desc())
    )
