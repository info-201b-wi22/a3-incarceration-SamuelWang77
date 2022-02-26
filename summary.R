df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
total_pop_each_state <- df %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(total_pop = sum(total_pop))

most_pop_name <- total_pop_each_state %>%
  filter(total_pop == max(total_pop)) %>%
  pull(state)

most_pop_num <- total_pop_each_state %>%
  filter(total_pop == max(total_pop)) %>%
  pull(total_pop)

total_jail_pop_each_state <- df %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  left_join(total_pop_each_state) %>%
  mutate(percent_in_jail = total_jail_pop / total_pop * 100)



jail_pop_black <- df %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(total_jail_pop_black = sum(black_jail_pop, na.rm = TRUE))

percent_black <- total_jail_pop_each_state %>%
  left_join(jail_pop_black, by = 'state') %>%
  mutate('bper' = total_jail_pop_black / total_jail_pop * 100) %>%
  na.omit()

highest_percent_black <- percent_black %>%
  filter(bper == max(bper))

state_highest_black <- highest_percent_black %>%
  pull(state)

number_highest_black <- highest_percent_black %>%
  pull(bper)

jail_pop_white <- df %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(total_jail_pop_white = sum(white_jail_pop, na.rm = TRUE))

percent_white <- total_jail_pop_each_state %>%
  left_join(jail_pop_white, by = 'state') %>%
  mutate('wper' = total_jail_pop_white / total_jail_pop * 100) %>%
  na.omit()

highest_percent_white <- percent_white %>%
  filter(wper == max(wper))

state_highest_white <- highest_percent_white %>%
  pull(state)

number_highest_white <- highest_percent_white %>%
  pull(wper)


total_pop_2008 <- df %>%
  filter(year == '2008') %>%
  group_by(state) %>%
  summarize(total_pop_08 = sum(total_pop, na.rm = TRUE))

jail_pop_2008 <- df %>%
  filter(year == '2008') %>%
  group_by(state) %>%
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))

jail_percent_08 <- total_pop_2008 %>%
  left_join(jail_pop_2008, by = 'state') %>%
  mutate('percent' = total_jail_pop / total_pop_08 * 100)

max_jail_percent_08 <- jail_percent_08 %>%
  filter(percent == max(percent)) %>%
  pull(percent)

max_jail_name_08 <- jail_percent_08 %>%
  filter(percent == max(percent)) %>%
  pull(state)

jail_wa_08 <- jail_percent_08 %>%
  filter(state == 'WA') %>%
  pull(percent)

jail_wa_18 <- total_jail_pop_each_state %>%
  filter(state == 'WA') %>%
  pull(percent_in_jail)

change_rate <- (jail_wa_18 - jail_wa_08) / jail_wa_08 * 100
