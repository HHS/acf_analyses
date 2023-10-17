library(tidyverse)
library(tidygraph)
library(ggraph)

data_path <- here::here('ogm', 'import', 'output', 'grant_data.csv')
data <- read_csv(data_path)

# identify unique program and recipient id and name combinations ----
grant_programs <- data %>%
  distinct(cfda_number, .keep_all = T) %>%
  select(cfda_number, cfda_title)

grant_recipients <- data %>%
  distinct(recipient_uei, .keep_all = T) %>%
  select(recipient_uei, recipient_name)

# prepare data for network graph ----
mat_data <- data %>%
  select(cfda_number, recipient_uei) %>%
  left_join(grant_programs) %>%
  left_join(grant_recipients) %>%
  distinct(cfda_title, recipient_name)

mat <- t(
  table(
    mat_data 
  )
)

adj_mat <- t(mat) %*% mat

# network graph data
net <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE,
                                           diag = FALSE) %>%
  as_tbl_graph

### using quantiles to determine which grant programs have the most shared grantees
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble(x = quantile(x, q), q = q)
}

### breaking out percentiles
q_values <- c(.25, .5, .75, .95)

min_edge_weight <- net %>% 
  igraph::as_data_frame() %>%
  reframe(quibble(weight, q_values)) %>%
  filter(q == max(q)) %>%
  pull(x)

## Q1_b viz of grant programs by shared grantees ----
grant_gravity_template_g <- function(graph, label = T, box = NULL) {
  
  p <- ggraph(graph, layout = "kk") +
    geom_edge_link(aes(width = weight), alpha = .75) +
    geom_node_point(size = 6, color = 'steelblue') +
    scale_edge_width_continuous(name = "Shared grantees", range = c(0.5, 3)) +
    guides(alpha = 'none') 
  
  if (label) {
    p <- p + 
      geom_node_label(aes(label = name, alpha = .45), fontface = 'bold')
  } 
  
  if (!is.null(box)) {
    bbox <- c(-box, box)
    p <- p + 
      lims(x = bbox, y = bbox)
  }
  
  p
}

shared_grantees_g <- grant_gravity_template_g(net)
shared_grantees_no_label_g <- grant_gravity_template_g(net, F)

n_high_cross_funding <- net %>% 
  igraph::as_data_frame() %>% 
  filter(weight >= min_edge_weight) %>% 
  pivot_longer(cols = c(from, to), names_to = 'program') %>% 
  distinct(value) %>% 
  nrow

no_cross_funding <- net %>%
  filter(node_is_isolated()) %>%
  pull(name)

### zooming into highly weighted nodes ----
net_high_weight <- net %>%
  activate(edges) %>%
  filter(weight >= min_edge_weight) %>%
  activate(nodes) %>%
  filter(!node_is_isolated())

subset_shared_grantees_g <- grant_gravity_template_g(net_high_weight)

ggsave(filename = here::here('ogm', 'eda', 'output', 'grant_recipient_cross_funding.png'),
       plot = subset_shared_grantees_g,
       width = 1920, height = 1080, units = 'px', dpi = 'screen')
