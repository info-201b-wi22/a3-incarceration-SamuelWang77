library(tidyverse)
library(dplyr)
library(usdata)

df_map <- df %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = T), total_pop = sum(total_pop, na.rm = T)) %>%
  mutate(rate = total_jail_pop / total_pop * 100) %>%
  na.omit() %>%
  mutate(region = abbr2state(state))

df_map$region <- tolower(df_map$region)

state_shape <- map_data("state")

state_shape <- left_join(state_shape, df_map)
chart3 <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(long, lat, group = group, fill = rate)
  ) +
  scale_fill_continuous(low = 'yellow', high = 'red', labels = scales::label_number_si()) +
  coord_map() +
  labs(title = 'National Jail Rate 2018', fill = 'Jail Rates(jail pop over total pop)')

