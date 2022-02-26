top_10_state <- df %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(total = sum(total_jail_pop,na.rm = T)) %>%
  arrange(-total)

res_10_state <- df %>%
  filter(year >= 1980) %>%
  filter(state == 'CA' | state == 'WA' | state == 'TX' |state == 'FL' |state == 'GA' |state == 'PA' |state == 'VA' |state == 'TN' |state == 'NY' |state == 'LA')


library(ggplot2)
chart1 <- ggplot(data = res_10_state) +
  geom_smooth(aes(x = year, y = total_jail_pop, color = urbanicity)) +
  facet_wrap(~state) +
  labs(
    title = 'Jail Population Trend by Time from 1970 to 2020 by States in top 10',
    x = 'Time(Year)',
    y = 'total jail population'
  )

