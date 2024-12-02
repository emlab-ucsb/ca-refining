#setwd("G://Shared drives/emlab/projects/current-projects/calepa-cn/outputs/refining-2024/health")
setwd("H://Shared drives/emlab/projects/current-projects/calepa-cn/outputs/refining-2024/health")

mort <- fread("refining_mortality_2023.csv", stringsAsFactors = F)

## pop-weighted pollution exposure

mort %>%
  group_by(disadvantaged, year) %>%
  mutate(weight = pop / sum(pop, na.rm = F)) %>%
  ungroup() %>%
  group_by(scen_id, disadvantaged, year) %>%
  summarise(per_cap = sum(total_pm25 * weight, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = per_cap, group = disadvantaged, color = disadvantaged)) +
  facet_wrap(~scen_id) +
  geom_line() +
  geom_point() +
  theme_cowplot() +
  labs(y = "PM2.5 (pop-weighted)", color = "DAC")

## pop-weighted health cost per cap

mort %>%
  mutate(per_cap = cost_2019_PV / pop) %>%
  group_by(scen_id, disadvantaged, year) %>%
  summarise(per_cap = mean(per_cap, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = per_cap, group = disadvantaged, color = disadvantaged)) +
  facet_wrap(~scen_id) +
  geom_line() +
  geom_point() +
  theme_cowplot() +
  labs(y = "NPV per cap (unweighted)", color = "DAC")

## pop-unweighted health cost per cap

mort %>%
  group_by(disadvantaged, year, scen_id) %>%
  summarize(
    cost_2019_PV = sum(cost_2019_PV, na.rm = F),
    pop = sum(pop, na.rm = F)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = cost_2019_PV / pop, group = disadvantaged, color = disadvantaged)) +
  facet_wrap(~scen_id) +
  geom_line() +
  geom_point() +
  theme_cowplot() +
  labs(y = "NPV per cap (pop-weighted)", color = "DAC")
