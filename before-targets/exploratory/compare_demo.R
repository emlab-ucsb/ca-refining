
library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)

#$tar_target(name = file_population, command = file.path(main_path, "data/benmap/processed/ct_inc_45.csv"), format = "file"),
#tar_target(name = file_dt_ct_inc_pop, command = file.path(main_path, "data/health/processed/ct_inc_45_2020.csv"), format = "file"),

path <- c("G://Shared drives/emlab/projects/current-projects/calepa-cn")

old <- fread(paste0(path,"/data/benmap/processed/ct_inc_45.csv"))
new_old <- fread(paste0(path,"/data/health/processed/ct_inc_45_2020_old.csv"))
new <- fread(paste0(path,"/data/health/processed/ct_inc_45_2020.csv"))

#First compare total pop in 2020

old %>% filter(year==2020) %>% summarize(pop = sum(pop)/1000000)
new_old %>% filter(year==2020) %>% summarize(pop = sum(pop))
new %>% filter(year==2020) %>% summarize(pop = sum(pop))

#Compare age groups
old %>% select(lower_age,upper_age) %>% distinct()
new_old %>% select(start_age,end_age) %>% distinct()
new %>% select(start_age,end_age) %>% distinct()

#figs by age group

old_fig <- old %>%
  filter(year>2019)%>%
  mutate(age_group = ifelse(lower_age<30,"<30",">=30"))%>%
  group_by(age_group,year)%>%
  summarise(pop = sum(pop)/1000000)%>%
  ggplot(aes(x=year,y=pop, color = age_group, group=age_group))+
  geom_line()+
  #scale_y_continuous(limits = c(0,25), breaks = c(0,5,10,15,20,25))+
  scale_y_continuous(limits = c(10,30), breaks = c(10,15,20,25,30))+
  theme_cowplot()+
  labs(y= "Population (millions)", x="Year")+ guides(color="none");old_fig

new_old_fig <- new_old %>%
  filter(year>2019)%>%
  mutate(age_group = ifelse(start_age<30,"<30",">=30"))%>%
  group_by(age_group,year)%>%
  summarise(pop = sum(pop)/1000000)%>%
  ggplot(aes(x=year,y=pop, color = age_group, group=age_group))+
  geom_line()+
  #scale_y_continuous(limits = c(0,25), breaks = c(0,5,10,15,20,25))+
  scale_y_continuous(limits = c(10,30), breaks = c(10,15,20,25,30))+
  theme_cowplot()+
  labs(y= NULL, x="Year")+ guides(color="none");new_old_fig

new_fig <- new %>%
  mutate(age_group = ifelse(start_age<30,"<30",">=30"))%>%
  group_by(age_group,year)%>%
  summarise(pop = sum(pop)/1000000)%>%
  ggplot(aes(x=year,y=pop, color = age_group, group=age_group))+
  geom_line()+
  theme_cowplot()+
  scale_y_continuous(limits = c(10,30), breaks = c(10,15,20,25,30))+
  labs(y= "Population (millions)", color = "Age group", x="Year");new_fig

plot_grid(old_fig, new_old_fig, new_fig, labels = c('A. Old population data', 'B. New (old) population data', 'C. New population data'))

  