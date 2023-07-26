###############################################################################
# PURPOSE: Fit topic model to full enrollment initiative data
# LAST EDITED: 22 june 2023
# NOTES:
############################################################################### . 

#### set up ####
# clear environment and console

rm(list = ls())
cat("\014")

# load libraries
library(tidyverse) 
library(tidytext) # for preparing text data for the topic model
library(topicmodels) # functions for LDA topic models
library(ldatuning) # for fitting the model across multiple topic count values

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
pd <- paste(wd, "plots", sep = "/")

#### load data ####
load(paste(dd, "1_clean_data.Rdata", sep = "/"))


#### prepare data for topic model ####

d_enrollment_tokenized <- d %>% 
  # select just unique identifier 
  select(pk, comments_grantee) %>%
  # tokenize by word
  unnest_tokens(word, comments_grantee) %>% 
  # remove stop words
  anti_join(stop_words) %>%
  # filter out NA's
  filter(!is.na(word))

# organize by document, word and count
d_word_counts <- d_enrollment_tokenized %>% 
  count(pk, word, sort = T)

# create document term matrix
dtm_grants <- d_word_counts %>% 
  cast_dtm(pk, word, n)


#### fitting model ####
# fit model across a sequence of topic count (k) values, assess fit with two metrics
# Pulled from this tutorial: https://ladal.edu.au/topicmodels.html
tune <- FindTopicsNumber(
  dtm_grants,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

# fit model
n_topics <- 6

lda_grants <- LDA(
  dtm_grants, 
  k = n_topics, 
  control = list(seed = 1234)
)


# shape data for plotting
d_grants_topics <- tidy(lda_grants, matrix = "beta")

d_topics_by_grant <- tidy(lda_grants, matrix = "gamma")

d_topic_mapping <- d_topics_by_grant %>% 
  pivot_wider(id_cols = document, names_from = topic, values_from = gamma) %>% 
  left_join(d %>% select(pk, comments_grantee), by = join_by(document == pk))
view(d_topic_mapping)
write_csv(d_topic_mapping, file = paste(od, "topic_mapping.csv", sep = "/"))

d_topics_by_region <- d %>% 
  select(pk, region) %>% 
  left_join(d_topics_by_grant, by = join_by(pk == document)) %>% 
  group_by(region, topic) %>% 
  summarize(avg_gamma = mean(gamma)) %>% 
  na.omit()

d_topics_by_fei <- d %>% 
  select(pk, is_fei) %>% 
  left_join(d_topics_by_grant, by = join_by(pk == document)) %>% 
  group_by(is_fei, topic) %>% 
  summarize(avg_gamma = mean(gamma)) %>% 
  na.omit()

d_top_terms <- d_grants_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#### plots ####

FindTopicsNumber_plot(tune)

ggsave(paste(pd, "1_topic_model_tune.png", sep = "/"))

d_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  ggtitle("Top Terms per Topic")

ggsave(paste(pd, "2_top_terms_per_topic.png", sep = "/"))

d_topics_by_region %>%
  mutate(topic = reorder_within(topic, avg_gamma, region)) %>% 
  ggplot(aes(avg_gamma, factor(topic), fill = region)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ region, scales = "free") +
  scale_y_reordered() +
  ggtitle("Average Gamma per Topic by Region")

ggsave(paste(pd, "3_avg_gamma_per_topic_by_region.png", sep = "/"))

d_topics_by_fei %>%
  mutate(topic = reorder_within(topic, avg_gamma, is_fei)) %>% 
  ggplot(aes(avg_gamma, factor(topic), fill = is_fei)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ is_fei, scales = "free") +
  scale_y_reordered() +
  ggtitle("Average Gamma per Topic by FEI")

ggsave(paste(pd, "4_avg_gamma_per_topic_by_fei.png", sep = "/"))
