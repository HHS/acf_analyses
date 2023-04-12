###############################################################################
# PURPOSE: understand HS facility risk of tornadoes, hurricanes, wildfires
# LAST EDITED: 12 apr 2023
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
d_locations_atrisk <- d_locations_byrisk %>% 
    filter(risk_rating %in% c(
        "Very High", 
        "Relatively High", 
        "Relatively Moderate")
    )

#### calculate/plot basic stats for baseline understanding of facilities ####
# number of facilities
n_locations <- nrow(d_locations)

# interest, ownership, and child care overview
d_pcts_characteristics <- d_locations %>%
    summarize(across(starts_with("is_"), mean)) %>%
    pivot_longer(
        cols = everything(),
        names_to = "characteristic",
        values_to = "pct_locations"
    ) %>%
    mutate(
        program = case_when(
            characteristic == "is_federal_interest_site" ~ "Federal Interest",
            characteristic == "is_owned_by_program" ~ "Program-Owned",
            characteristic == "is_child_care_partner_site" ~ "Child Care Partner",
        ),
        n_locations = pct_locations * n_locations,
    ); d_pcts_characteristics

# share by federal interest and ownership combined
count_by_group(d_locations, n, is_federal_interest_site, is_owned_by_program)

# program overview
d_pcts_programs <- d_locations %>%
    summarize(across(ends_with("_slots"), ~mean(.>0))) %>%
    select(-total_slots) %>%
    pivot_longer(
        cols = everything(),
        names_to = "program",
        values_to = "pct_locations"
    ) %>%
    mutate(
        program = program %>% 
            str_replace_all("_", " ") %>%
            str_replace(" slots", "") %>%
            str_replace("ehs", "Early Head Start") %>%
            str_replace("hs", "Head Start") %>%
            str_to_title() %>%
            str_replace("Aian", "AIAN"),
        n_locations = pct_locations * n_locations,
    ); d_pcts_programs

p_program <- ggplot(
        data = d_pcts_programs,
        aes(
            x = fct_reorder(program, pct_locations) %>% fct_rev,
            y = pct_locations
        )
    ) +
    geom_bar(
        stat = "identity",
        fill = acf_palette[2]
    ) +
    geom_text(
        mapping = aes(
            label = comma(n_locations),
            y = if_else(pct_locations > 0.1,
                pct_locations - 0.06,
                pct_locations + 0.06
            ),
            color = if_else(pct_locations > 0.1, "white", "black"),
        ),
        stat = "identity",
        family = "Gill Sans MT",
        size = 3,
    ) +
    geom_text(
        mapping = aes(
            label = percent(pct_locations, accuracy = 1),
            y = if_else(pct_locations > 0.1,
                pct_locations - 0.02,
                pct_locations + 0.02
            ),
            color = if_else(pct_locations > 0.1, "white", "black"),
        ),
        stat = "identity",
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

# state breakdown by federal interest: percent
d_state_interest_pct <- d_locations %>%
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
        title = str_wrap("Share of Head Start locations with federal interest, 
            by state", width = 55),
        caption = str_wrap("Sources: Locations pulled from Head Start 
            Enterprise System as of April 6, 2023.", width = 85)
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
state_order <- d_locations %>%
    filter(is_federal_interest_site) %>%
    group_by(state_code) %>%
    summarize(n_locations = n()) %>%
    ungroup() %>%
    arrange(n_locations %>% desc()) %>%
    pull(state_code) # order by number of federal interest sites
    
p_state_interest <- ggplot(
        data = d_locations,
        mapping = aes(
            x = factor(state_code, levels = state_order),
            fill = is_federal_interest_site,
        ),
    ) +
    geom_bar(
        stat = "count",
        color = acf_palette[2]
    ) +
    geom_text(
        mapping = aes(
            label = after_stat(count),
            color = if_else(is_federal_interest_site, "white", "black"),
        ),
        stat = "count",
        position = position_stack(vjust = 0.5),
        size = rel(2),
    ) +
    # add title, etc.
    labs(
        x = "",
        y = "",
        title = "Head Start locations by state",
        caption = str_wrap("Sources: Locations pulled from Head Start 
            Enterprise System as of April 6, 2023.", width = 85)
    ) + 
    # customize scale
    scale_fill_manual(
        name = "Federal Interest Site",
        values = c("TRUE" = acf_palette[2], "FALSE" = "white"),
        labels = c("No", "Yes"),
        guide = guide_legend(reverse = TRUE)
    ) +
    scale_color_manual(
        values = c("white" = "white", "black" = "black"),
        guide = "none"
    ) +
    # format axes
    scale_x_discrete(
        labels = function(x) str_wrap(x, width = 10),
        expand = c(0, 0)
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    # extra formatting
    theme(
        # format legend
        legend.position = "top",
        legend.justification = "left",
        # get rid of visual distractions
        panel.grid = element_blank()
    )
p_state_interest
ggsave(
    filename = paste(od, "state_and_interest.jpg", sep = "/"), 
    plot = p_state_interest,
    width = 12,
    height = 8
)
    
# federal interest by ownership
d_state_interest_ownership_pct <- d_locations %>%
    group_by(state_code) %>%
    summarize(pct_federal_interest_owned = mean(is_owned_by_program)) %>%
    ungroup()

g_state_interest_ownership_pct <- ggplot(
        data = d_state_interest_ownership_pct,
        mapping = aes(
            x = pct_federal_interest_owned,
            y = fct_reorder(state_code, pct_federal_interest_owned),
        )
    ) +
    geom_bar(
        stat = "identity",
        fill = acf_palette[1]
    ) +
    geom_text(
        mapping = aes(
            label = percent(pct_federal_interest_owned, accuracy = 1),
            x = pct_federal_interest_owned - 0.03
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
        title = str_wrap("Share of Head Start locations with federal interest 
        that are owned by program, by state", width = 55),
        caption = str_wrap("Sources: Locations pulled from Head Start 
            Enterprise System as of April 6, 2023.", width = 85)
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
g_state_interest_ownership_pct
ggsave(
    filename = paste(od, "state_and_interest_pct.jpg", sep = "/"), 
    plot = g_state_interest_ownership_pct,
    width = 4,
    height = 8,
)

#### Q: what is overall and by-hazard type risk Head Start locations face? ####
# overall x risk type
g_hazards <- ggplot(
        data = d_locations_byrisk,
        mapping = aes(
            x = risk_type,
            fill = risk_rating,
            label = percent(
                after_stat(count/tapply(count, x, sum)[as.character(x)]),
                accuracy = 0.1
            )
        )
    ) +
    geom_bar(stat = "count") +
    geom_bar_text(
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
        y = "Number of Head Start locations",
        title = "Head Start facility risk levels: by natural hazards",
        caption = str_wrap("Sources: Locations pulled from Head Start 
            Enterprise System as of April 6, 2023. Risk ratings based on FEMA 
            National Risk Index, March 2023 Release.", width = 85)
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
# prettify federal interest labels
fi_labels <- c("Federal Interest Sites", "Non-Federal Interest Sites")
names(fi_labels) <- c(TRUE, FALSE)

# pre-aggregate
s_locations_byrisk_interest <- d_locations_byrisk %>%
    count(is_federal_interest_site, risk_type, risk_rating) %>%
    group_by(is_federal_interest_site, risk_type) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()

# including all risk ratings
g_interest <- ggplot(
        data = s_locations_byrisk_interest %>% 
            mutate(is_federal_interest_site = 
                    factor(is_federal_interest_site, levels = c(TRUE, FALSE))),
        mapping = aes(
            x = risk_type,
            y = n,
            fill = risk_rating,
            label = percent(pct,accuracy = 0.1)
        )
    ) +
    geom_bar(stat = "identity") +
    geom_bar_text(
        stat = "identity",
        position = "stack",
        place = "middle",
        size = 8,
        contrast = TRUE,
        family = "Gill Sans MT"
    ) +
    # by risk type
    facet_wrap(
        ~is_federal_interest_site,
        ncol = 2,
        scales = "free_y",
        labeller = labeller(is_federal_interest_site = fi_labels)
    ) +
    # add title, etc.
    labs(
        x = "",
        y = "Number of Head Start locations",
        title = "Head Start facility risk levels: by federal interest",
        caption = str_wrap("Sources: Locations pulled from Head Start 
            Enterprise System as of April 6, 2023. Risk ratings based on FEMA 
            National Risk Index, March 2023 Release.", width = 85)
    ) + 
    # customize scales
    scale_fill_brewer(
        name = "Risk Rating",
        type = "seq",
        palette = "OrRd",
        direction = -1,
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_y_continuous(labels = comma_format()) +
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
    width = 10,
    height = 6,
)

#### Q: where are there high concentrations of risk amongst federal interest sites? ####
# by state and natural hazard
g_state_concentration <- d_locations_atrisk %>%
    filter(is_federal_interest_site) %>%
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
            stat = "count",
            position = "stack",
            place = "middle",
            size = 6,
            min.size = 4,
            contrast = TRUE,
            family = "Gill Sans MT"
        ) +
        coord_flip() +
        # add title, etc.
        labs(
            x = "",
            y = "",
            title = str_c("At-risk Federal Interest Head Start sites:", unique(.$risk_type), sep = " "),
            caption = str_wrap("Sources: Locations pulled from Head Start 
            Enterprise System as of April 6, 2023. Risk ratings based on FEMA 
            National Risk Index, March 2023 Release.", width = 80)
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
    str_c("state_hazard_",names(g_state_concentration), ".jpg"),
    sep = "/"
)

pwalk(
    .l = list(fp_state_concentration, g_state_concentration), 
    .f = ggsave,
    width = 5, 
    height = 6
)

#### state drill-downs ####
# define states of interest: in top 5 w/ at-risk federal interest sites
# per risk type, plus a few states getting emergency appropriations
d_focus_states <- d_locations_atrisk %>%
    filter(is_federal_interest_site) %>%
    group_by(risk_type, state_code) %>%
    summarize(n_sites = n()) %>%
    ungroup() %>%
    arrange(risk_type, n_sites %>% desc()) %>%
    group_by(risk_type) %>%
    slice_head(n = 5) %>%
    ungroup()
    
focus_states <- c(d_focus_states$state_code, "SC", "PR", "NC") %>% unique()

# summarize key grantee info by state
t_state_drilldown <- d_locations_atrisk %>%
    filter(
        state_code %in% focus_states,
        risk_type != "Overall"
    ) %>%
    split(.$state_code) %>%
    map(
        . %>%
            group_by(risk_type) %>%
            summarize(
                `Total` = n_distinct(location_id),
                `With Federal Interest` = n_distinct(
                    if_else(is_federal_interest_site, location_id, NA)),
                `Owned` = n_distinct(
                    if_else(is_owned_by_program, location_id, NA)),
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
                label = md("**At-Risk Locations**"),
                rows = matches("[Grant]{0}"),
                id = "rg_locations"
            ) %>%
            tab_row_group(
                label = md("**Grantee Concentration**"),
                rows = starts_with("Grant"),
                id = "rg_grantees"
            ) %>%
            row_group_order(groups = c("rg_locations", "rg_grantees"))
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

# find grantees with most number of at-risk federal interest locations in state
d_grantees_with_concentrated_risk <- d_locations_atrisk %>%
    filter(
        state_code %in% focus_states,
        is_federal_interest_site,
    ) %>%
    split(.$state_code) %>%
    map(
        . %>%
            group_by(grantee) %>%
            summarize(n_locations_per_grantee = n_distinct(location_id)) %>%
            ungroup() %>%
            arrange(n_locations_per_grantee %>% desc()) %>%
            slice_head(n = 10)
    )

d_grantees_with_concentrated_risk <- d_grantees_with_concentrated_risk %>% 
    map_dfr(~as_tibble(.)) %>%
    mutate(state_code = rep(focus_states, 10) %>% sort())

write_csv(d_grantees_with_concentrated_risk,
    paste(od, "state_grantee_concentrations.csv", sep = "/"))
