# Cal-ref: Matching EIA refineries to NEI 
# vthivier@uottawa.ca
# created: 09/07/2022
# updated: 24/03/2025

# set up environment

rm(list=ls())
gc()

#for interactive runs

source("C:/git/ca-refining/before-targets/analysis/header_setup.R")
source("C:/git/ca-refining/before-targets/analysis/0_matching_helpers_NEI.R")
cat("\f")

#for the batch runs
# source("header_setup.R") 
# source("0_matching_helpers.R")
# cat("\f")

# Set directories

data <- "/projects/data/"

# Load NEI data
write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_to_match_nei.csv", row.names = F)

nei <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_to_match_nei.csv", 
             stringsAsFactors = F,
             colClasses = "character")%>%
  distinct()

# Load EIA data

eia <- fread("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_to_match_eia.csv", 
               stringsAsFactors = F,
                  colClasses = "character")%>%
  distinct()

# Clean establishment name

#define set of common words in establishment names to remove
# name_remove2 <- c(freq_string(eia,"company",50)$string, 
#                   freq_string(nei,"site_name",50)$string,name_remove)
name_remove2 <- c(name_remove)
pattern_temp <- str_to_upper(paste0(" ",name_remove2," ")) %>% as.data.frame() %>% distinct()%>% pull() %>% as.character()
pattern_new <- rep("",length(pattern_temp))
names(pattern_new) <- pattern_temp

# RUN FUNCTION TO CLEAN FACILITY NAME, ADDRESS AND CITY

eia_clean <- clean_name_ad(eia,"company","county",pattern_new)%>%
  select(-name_empty,-street_empty)

nei_clean <- clean_name_ad(nei,"site_name","county",pattern_new)%>%
  select(-name_empty,-street_empty)%>%
  mutate(noxt = 1)

eia_city <- clean_name_city(eia %>% mutate(dummy_name1 = "dummy_name1"),"dummy_name1" ,"site",pattern_new)%>%
  select(site_id,city_clean)

nei_city <- clean_name_city(nei %>% mutate(dummy_name2 = "dummy_name2")%>%rename(city_name = city),"dummy_name2","city_name",pattern_new)%>%
 select(eis_facility_id,city_clean)

eia_clean <- eia_clean%>%
  left_join(eia_city, by = c("site_id"))

nei_clean <- nei_clean %>%
  left_join(nei_city, by = c("eis_facility_id"))


# Using the matching functions #########################

#global parameterers
data1 = eia_clean
data2 = nei_clean
data1_id1 = "site_id" 
data2_id2 = "eis_facility_id"
poll = "noxt"

gc()
cat("\f")

data1 <- data1%>%
  mutate(id1 = get(data1_id1),
         data1_id1 = get(data1_id1))%>%
  rename(name_clean1 = name_clean,
         street_clean1 = street_clean,
         street_remove1 = street_remove,
         city_clean1 = city_clean)%>%
  ungroup()

data2 <- data2 %>%
  mutate(id2 = get(data2_id2),
         data2_id2 = get(data2_id2),
         poll = get(poll))%>%
  rename(name_clean2 = name_clean,
         street_clean2 = street_clean,
         street_remove2 = street_remove,
         city_clean2 = city_clean)%>%
  ungroup()

#1 Functions for exact or fuzzy matching

## exact  ######################################################

#1 exact matching on name, city, and county

select_vars1 <- c("id1","name_clean1","street_clean1", "city_clean1")
select_vars2 <- c("id2","name_clean2","street_clean2", "city_clean2")
join_statement <- c("name_clean1"="name_clean2","street_clean1"="street_clean2","city_clean1"="city_clean2")
fuzzy_statement <- list(dist = 1)

match_1 <- exact_matching(data1,data2,select_vars1,select_vars2,join_statement,match_level = 1, treshold = 0);match_1

#remove matched plants

data1_nest <- data1%>%
  filter(id1 %notin% (match_1 %>% select(id1) %>% distinct() %>% pull()))

data2_nest <- data2%>%
  filter(id2 %notin% (match_1 %>% select(id2) %>% distinct() %>% pull()))

#2 exact match on city, and county and fuzzy treshold on name 

select_vars1 <- c("id1","name_clean1","street_clean1", "city_clean1")
select_vars2 <- c("id2","name_clean2","street_clean2", "city_clean2")
join_statement <- c("street_clean1"="street_clean2","city_clean1"="city_clean2")
fuzzy_statement <- list(dist = quo(stringsim(name_clean1, name_clean2, method= "lv")))

match_2 <- exact_matching(data1_nest,data2_nest,select_vars1,select_vars2,join_statement,
                          match_level = 2, treshold = .15);match_2

#debug
match_2 %>%
  left_join(data1,
            by = c("id1"="site_id"))%>%
  left_join(data2,
            by = c("id2"="eis_facility_id"))

#remove matched plants

data1_nest <- data1_nest %>%
  filter(id1 %notin% (match_2 %>% select(id1) %>% distinct() %>% pull()))

data2_nest <- data2_nest %>%
  filter(id2 %notin% (match_2 %>% select(id2) %>% distinct() %>% pull()))

#3 exact match on county and fuzzy treshold on name 

select_vars1 <- c("id1","name_clean1","street_clean1")
select_vars2 <- c("id2","name_clean2","street_clean2")
join_statement <- c("street_clean1"="street_clean2")
fuzzy_statement <- list(dist = quo(stringsim(name_clean1, name_clean2, method= "lv")))

match_3 <- exact_matching(data1_nest,data2_nest,select_vars1,select_vars2,join_statement,
                          match_level = 3, treshold = .5);match_3

#debug
match_3 %>%
  left_join(data1,
            by = c("id1"="site_id"))%>%
  left_join(data2,
            by = c("id2"="eis_facility_id"))

#remove matched plants

data1_nest <- data1_nest %>%
  filter(id1 %notin% (match_3 %>% select(id1) %>% distinct() %>% pull()))

data2_nest <- data2_nest %>%
  filter(id2 %notin% (match_3 %>% select(id2) %>% distinct() %>% pull()))

#4 exact match on county and fuzzy name (lower treshold)

select_vars1 <- c("id1","name_clean1","street_clean1")
select_vars2 <- c("id2","name_clean2","street_clean2")
join_statement <- c("street_clean1"="street_clean2")
fuzzy_statement <- list(dist = quo(stringsim(name_clean1, name_clean2, method= "lv")))

match_4 <- exact_matching(data1_nest,data2_nest,select_vars1,select_vars2,join_statement,
                          match_level = 4, treshold = .15);match_4

#remove matched plants

data1_nest <- data1_nest %>%
  filter(id1 %notin% (match_4 %>% select(id1) %>% distinct() %>% pull()))

data2_nest <- data2_nest %>%
  filter(id2 %notin% (match_4 %>% select(id2) %>% distinct() %>% pull()))

# Create final cross-walk ##########################################

matched <- rbind(match_1,match_2, match_3, match_4)

matched %>%
  left_join(data1 %>% select("site_id","name_clean1","street_clean1", "city_clean1"),
            by = c("id1"="site_id"))%>%
  left_join(data2 %>% select("eis_facility_id","name_clean2","street_clean2", "city_clean2"),
            by = c("id2"="eis_facility_id"))%>%
  arrange(city_clean2)

data2_nest %>% select("eis_facility_id","name_clean2","street_clean2", "city_clean2")

# MANUALLY MATCHED 

manual_site <- c("343","97","97") #REMOVED IDS
manual_eis <- c("5682211","14055211","18702311") #REMOVED IDS

manual_crosswalk <- as.data.frame(rbind(cbind(manual_site,manual_eis)))%>%
  mutate_at(vars(manual_site,manual_eis),as.character)%>%
  rename(id1 = manual_site, id2 = manual_eis)

matched <- rbind(matched,manual_crosswalk)

matched %>% 
  write.csv("H:/Shared drives/emlab/projects/current-projects/calepa-cn/data-staged-for-deletion/health/raw/ref_match/ref_matches.csv", row.names = F)

