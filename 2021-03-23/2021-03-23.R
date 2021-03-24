library(tidyverse)
library(lubridate)
library(ggsci)
library(tidytext)
library(patchwork)

# tidy tuesday data -------------------------------------------------------
unvotes_tt <- tidytuesdayR::tt_load("2021-03-23")

# unvotes
unvotes_tt %>% 
  pluck("unvotes") -> unvotes

# roll_calls
unvotes_tt %>% 
  pluck("roll_calls") -> roll_calls

# issues
unvotes_tt %>% 
  pluck("issues") -> issues



# data inspection ---------------------------------------------------------
unvotes_tt %>% 
  map(glimpse)

# unvotes
unvotes %>% 
  map(table)

# roll_calls
roll_calls %>% 
  map(table)

# issues
issues %>% 
  map(table)



# data manipulation -------------------------------------------------------

# count of issues by year
issues_time <- roll_calls %>% 
  left_join(issues, by = "rcid") %>% 
  mutate(year = year(date)) %>% 
  group_by(issue, year) %>% 
  summarise(n = n()) %>% 
  # mutate(decade = year - year %% 10) %>% 
  drop_na()
 
# count of answer behaviour by roll call
answer_country <- unvotes %>%
  # distinct(rcid, country)
  group_by(country, vote) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n)) %>% round(3)) %>% 
  group_by(vote) %>%
  slice_max(freq, n = 10) %>% 
  ungroup %>% 
  mutate(vote = factor(vote, levels = c("yes", "no", "abstain")),
         country = reorder_within(country, freq, vote))
  # purrr approach
  # nest(.key = "vote_data") %>% 
  # mutate(top_10_data = map(vote_data, ~ slice_max(.x, freq, n = 10)))

# average sentiment score by roll call short description
sentiment_rc <- roll_calls %>% 
  unnest_tokens(word, short, to_lower = TRUE) %>% 
  left_join(get_sentiments("afinn"), by = "word") %>% 
  filter(! is.na(value)) %>% 
  group_by(rcid) %>% 
  summarise(mean_value = mean(value), date) %>% 
  ungroup %>% 
  mutate(year = year(date),
         decade = year - year %% 10)



# data visualization ------------------------------------------------------

# ggplot2 theme updates
theme_set(theme_linedraw())

theme_update(strip.background = element_blank(),
             strip.text = element_text(color = "black", size = 12),
             panel.grid.minor = element_blank(),
             legend.position = "top",
             plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5))

# issues over time 
issues_time_plot <- ggplot(issues_time, aes(x = year, y = n, fill = issue)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(breaks = seq(1940, 2020, 10), 
                     limits = c(1945, 2020)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     labels = scales::percent_format(accuracy = 1)) + 
  scale_fill_npg() + 
  guides(fill = guide_legend(ncol = 3)) + 
  theme(legend.title = element_blank()) +
  labs(x = "",
       y = "",
       title = "Share of UN Vote Issues by Year") + 
  annotate("text", 
           x = 1964, 
           y = 1.025, 
           size = 3,
           label = "1964 is missing") + 
  annotate("segment", 
           x = 1964, 
           y = 1.0125, 
           xend = 1964, 
           yend = 0.875, 
           arrow = arrow(angle = 10), 
           color = "red")

# save plot
ggsave(here::here("2021-03-23/issues_over_time.png"), 
       issues_time_plot,
       width = 12,
       height = 6)

# highest share of answer behaviour
answer_top_10 <- ggplot(answer_country, aes(x = freq, y = country, fill = vote)) +
  facet_wrap(~ vote, scales = "free_y") + 
  geom_col() + 
  scale_x_continuous(breaks = seq(0, 1, 0.25), labels = scales::label_percent(1)) +
  scale_y_reordered() + 
  scale_fill_aaas() +
  theme(axis.text.y = element_text(angle = 45),
        legend.position = "none") +
  labs(x = "",
       y = "",
       title = "General Voting Behaviour by Country",
       subtitle = "Top 10 Countries by General Share of Voting")

# save plot
ggsave(here::here("2021-03-23/general_answer_share.png"), 
       answer_top_10,
       width = 12,
       height = 6)

# sentiment analysis
sentiment_plot <- ggplot(sentiment_rc) +
  geom_boxplot(aes(x = decade, y = mean_value, group = decade)) +
  geom_smooth(aes(x = year, y = mean_value)) +
  scale_x_continuous(breaks = seq(1940, 2020, 10)) +
  scale_y_continuous(breaks = seq(-4, 2, 1)) +
  labs(x = "",
       y = "AFINN Sentiment Value",
       title = "Average Sentiment Value of UN Roll Call Descriptions",
       subtitle = "Boxplots by Decade | Using AFINN Lexicon",
       caption = "Positive scores indicate positive sentiment, while negative values indicate the opposite. Score runs between -5 and 5")

# save plot
ggsave(here::here("2021-03-23/sentiment_plot.png"), 
       sentiment_plot,
       width = 9,
       height = 9)

# all plots
all_plots <- (issues_time_plot / answer_top_10) | sentiment_plot

# save plot
ggsave(here::here("2021-03-23/all_plots.png"), 
       all_plots,
       width = 18,
       height = 9)
