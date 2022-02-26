library(tidyverse)
df_chart2 <- df %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  arrange(-total_pop_15to64) %>%
  slice(1:3) %>%
  summarize(total_pop, land_area, total_jail_dis, region) %>%
  mutate(density = total_pop / land_area)

library(ggplot2)
chart2 <- ggplot(data = df_chart2) +
  geom_smooth(aes(x= density, y = total_jail_dis / 1000, color = region)) +
  xlim(0, 5000) +
  labs(
    title = 'Total Discharge vs. Population Density',
    x = 'Population Density (population per square miles)',
    y = 'Total Discharge Amount (in k)'
  )
