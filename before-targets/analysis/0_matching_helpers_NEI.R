# Efficiency: Helpers for matching algo
# vthivierge@ucsb.edu
# created: 06/18/2022
# updated: 07/02/2024

# Clean names and addresses #####

# temp steps: determine frequency of components (removing proper names and single letters)

names <- babynames %>% filter(prop>0.0005) %>% select(name) %>% distinct() %>% pull(name) #could try different proportions. 

freq_string <- function(data,string,slice){
  
  data_temp <- data %>%
    mutate(string_var = get(string))%>%
    select(string_var)%>%
    #slice_head(n=10)%>%
    mutate(string_var = str_to_upper(string_var),
           string_var = gsub('[[:punct:] ]+',' ', string_var),
           string_var = str_trim(str_squish(string_var)))%>%
    pull(string_var)%>%
    str_split(" ")%>%
    unlist()%>%
    table()%>%
    as.data.frame()%>%
    arrange(-Freq)%>%
    `colnames<-` (c("string","freq"))%>%
    mutate(string = as.character(string))%>%
    filter(nchar(string)>1)%>%
    mutate(names_dummy = ifelse(string %in% str_to_upper(names),1,0))%>%
    filter(names_dummy==0)%>%
    slice_head(n=slice)
  
  return(data_temp)
  
}

# corporate names to remove

#default name removal
name_remove <- c("u  s  a", "USA", "industries", "indust", "incorporated", "inc", "corporation", "corp", "products", "prod",
                 "company","co", "llc","limited", "ltd","l  t  d","manufacturing","mfg","technologies", "lp", "l  p", "l  l  c", "california", "calif","ca",
                 "services", "svc" ,"svs", "international", "int", "partners", "division", "div", "and", "of", "holdings")

#name_remove <- c(freq_string(reclaim_raw,"fname",10)$string,name_remove)
pattern_temp <- str_to_upper(paste0(" ",name_remove," ")) %>% as.data.frame() %>% distinct()%>% pull() %>% as.character()
pattern_new <- rep("",length(pattern_temp))
names(pattern_new) <- pattern_temp
#str_replace_all(str_replace_all("INC CO INC CO CO CORP ", " ", "  "), pattern = pattern_new)

# address names to replace

address_words <-paste0(" ",c("RD","ST","AV","AV","DR","HWY","FWY","PWY","PWY","BWY","BWY",
                             "BLVD","N","N","S","S","E","W","STE","CT","LN","PL", "FLR","CTR","CIR", "BLDG")," ")

names(address_words) <- paste0(" ",c("ROAD","STREET","AVENUE","AVE","DRIVE","HIGHWAY","FREEWAY","PARKWAY","PKWY","BROADWAY","BDWY",
                              "BOULEVARD","NORTH","NO","SOUTH","SO","EAST","WEST", "SUITE","COURT", "LANE","PLACE","FLOOR","CENTER","CIRCLE", "BUILDING")," ")

# address names to remove

address_remove_temp <-paste0(" ",c("RD","ST","AV","AV","DR","HWY","FWY","PWY","PWY","BWY","BWY",
                                   "BLVD","STE","CT","LN","PL", "FLR","CTR","CIR", "BLDG")," ") #REMOVED THE CARDINAL POINTS (N,S,E,W); SINCE MORE IMPORTANT INFO THAN RD OR ST
address_remove <- rep("",length(address_remove_temp))
names(address_remove) <- address_remove_temp

rm(pattern_temp,address_remove_temp)

# function to clean name and address

clean_name_ad <- function(data,name,address,pattern_new){

  data_clean <- data%>%
    mutate(name_temp = str_trim(get(name), side = "both"),
           name_temp = str_to_upper(name_temp),
           name_temp = gsub('[[:punct:] ]+',' ', name_temp), #removes special characters and punctuation
           name_temp = str_squish(name_temp),
           name_temp = paste0(name_temp," ", sep=""), # keep the first word if its one of the common ones
           name_temp = str_replace_all(name_temp, " ", "  "),
           name_temp = str_replace_all(name_temp, pattern = pattern_new), 
           name_clean = str_trim(str_squish(name_temp)))%>%
    mutate(street_temp = str_trim(get(address), side = "both"),
           street_temp = str_to_upper(street_temp),
           street_temp = gsub('[[:punct:] ]+',' ', street_temp), #removes the "&" and other special characters and punctuation
           street_temp = paste0(" ",street_temp," "),
           street_temp = str_replace_all(street_temp, " ", "  "),
           street_temp = str_replace_all(street_temp, pattern = address_words),
           street_remove = str_replace_all(street_temp, pattern = address_remove),
           street_clean = str_trim(str_squish(street_temp)),
           street_remove = str_trim(str_squish(street_remove)))%>%
    select(-street_temp,-name_temp)%>%
    arrange(name_clean)%>%
    distinct()%>%
    #filter(name_clean %notin% "")%>% # MAYBE SHOULD NOT DROP THESE
    mutate(name_empty = ifelse(name_clean %in% "",1,0),
           street_empty = ifelse(street_clean %in% "",1,0))%>%
    ungroup()
  
  return(data_clean)

}

clean_name_city <- function(data,name,city,pattern_new){
  
  data_clean <- data%>%
    mutate(name_temp = str_trim(get(name), side = "both"),
           name_temp = str_to_upper(name_temp),
           name_temp = gsub('[[:punct:] ]+',' ', name_temp), #removes special characters and punctuation
           name_temp = str_squish(name_temp),
           name_temp = paste0(name_temp," ", sep=""), # keep the first word if its one of the common ones
           name_temp = str_replace_all(name_temp, " ", "  "),
           name_temp = str_replace_all(name_temp, pattern = pattern_new), 
           name_clean = str_trim(str_squish(name_temp)))%>%
    mutate(city_temp = str_trim(get(city), side = "both"),
           city_temp = str_to_upper(city_temp),
           city_temp = gsub('[[:punct:] ]+',' ', city_temp), #removes the "&" and other special characters and punctuation
           city_clean = str_trim(str_squish(city_temp)))%>%
    select(-city_temp,-name_temp)%>%
    arrange(name_clean)%>%
    distinct()%>%
    #filter(name_clean %notin% "")%>% # MAYBE SHOULD NOT DROP THESE
    mutate(name_empty = ifelse(name_clean %in% "",1,0),
           city_empty = ifelse(city_clean %in% "",1,0))%>%
    ungroup()
  
  return(data_clean)
  
}

#debugging tests
# clean_name_ad(asmcm_ssl_ca,"name1","street_phy",pattern_new) %>% select(name_empty) %>% table()
# 
# name_remove <- c(freq_string(reclaim_raw,"fname",100)$string,name_remove)
# pattern_temp <- str_to_upper(paste0(" ",name_remove," ")) %>% as.data.frame() %>% distinct()%>% pull() %>% as.character()
# pattern_new <- rep("",length(pattern_temp))
# names(pattern_new) <- pattern_temp
# clean_name_ad(asmcm_ssl_ca,"name1","street_phy",pattern_new) %>% select(name_empty) %>% table()
# 
# clean_name_ad(asmcm_ssl_ca,"name1","street_phy",pattern_new) %>% select(name1, name_clean, street_phy, street_clean) %>%
#   mutate(prop=nchar(name_clean)/nchar(name1))%>% distinct() %>% arrange(-prop)%>% View

# drop duplicates of ids

drop_multiple_ids <- function(data){

  data <- data %>% 
    group_by(id1,year)%>%
    mutate(n_id2 = n_distinct(id2))%>%
    filter(n_id2==1)%>% 
    group_by(id2,year)%>%
    mutate(n_id1 = n_distinct(id1))%>%
    filter(n_id1==1)%>%
    ungroup()
  
  return(data)

}

# MANUALLY MATCHED LBDNUM AND UFACID 

# manual_lbdnum1 <- c(XYZ) #REMOVED IDS
# manual_ufacid1 <- c(XYZ) #REMOVED IDS
# 
# #adding to the matched
# 
# manual_lbdnum2 <- c(XYZ) #REMOVED IDS
# manual_ufacid2 <- c(XYZ) #REMOVED IDS
# 
# #changing the matched
# manual_lbdnum3 <- c(XYZ) #REMOVED IDS
# manual_ufacid3 <- c(XYZ) #REMOVED IDS
# 
# #single ufacid but multiple lbdnum (maybe the analysis of this can be a benefit?)
# 
# manual_lbdnum4 <- c(XYZ) #REMOVED IDS
# manual_ufacid4 <- c(XYZ) #REMOVED IDS
# 
# manual_crosswalk <- as.data.frame(rbind(cbind(manual_lbdnum1,manual_ufacid1),
#                                         cbind(manual_lbdnum2,manual_ufacid2),
#                                         cbind(manual_lbdnum3,manual_ufacid3),
#                                         cbind(manual_lbdnum4,manual_ufacid4)))%>%
#   mutate_at(vars(manual_lbdnum1,manual_ufacid1),as.character)%>%
#   rename(lbdnum = manual_lbdnum1, ufacid = manual_ufacid1)
# 
# rm(manual_lbdnum1,manual_lbdnum2,manual_lbdnum3,manual_lbdnum4,
#    manual_ufacid1,manual_ufacid2,manual_ufacid3,manual_ufacid4)


# exact matching function

exact_matching <- function(data1_temp,data2_temp,select_vars1,select_vars2,join_statement,match_level,treshold=0.75){
    
  ids_temp <- data1_temp %>%
    select(one_of(select_vars1))%>%
    filter_all(all_vars(. %notin% c("",NA,"NA")))%>% 
    distinct()%>%
    left_join(data2_temp %>%
                select(one_of(select_vars2))%>%
                filter_all(all_vars(. %notin% c("",NA,"NA")))%>% 
                distinct(), 
              c(join_statement))%>% 
    drop_na(id1,id2)%>%
    mutate(!!! fuzzy_statement)%>%
    filter(dist>=treshold)%>%
    select(id1,id2)%>%
    distinct()
  
#   match_temp <- data1_temp%>%
#     select(id1,year,tvs)%>%
#     filter(year %in% number_years2)%>%
#     left_join(ids_temp, c("id1"))%>%    
#     left_join(data2_temp %>% select(id2,data2_id2,year,poll), c("id2", "year"))%>%
#     drop_na(data2_id2)%>%
#     select(-data2_id2)%>%
#     filter(poll>0 & tvs>0)%>%
#     distinct()%>%
#     mutate(match_level = match_level)%>%
#     # KEEP TRACK OF DUPLICATES. WILL DEAL WITH THEM LATER BY EITHER DROPPING OR SUMMING THEIR VALUES OR TVS OR poll. 
#     # THEY SHOULD BE GOOD MATCHES. THEY SHOULD NOT BE PUT BACK IN THE POOL FOR WORST MATCHES AFTER.
#     group_by(id1,year)%>%
#     mutate(n_id2=n_distinct(id2),
#            n1=n())%>%
#     ungroup()%>%
#     group_by(id2,year)%>%
#     mutate(n_id1=n_distinct(id1),
#            n2=n())%>%
#     ungroup()
  
  return(ids_temp)
  
}

# fuzzy matching function

fuzzy_matching <- function(data1_temp,data2_temp,select_vars1,select_vars2,join_statement,match_level,treshold=0.75){
  
  ids_temp <- data1_temp %>%
    select(one_of(select_vars1))%>%
    filter_all(all_vars(. %notin% c("",NA,"NA")))%>% 
    distinct()%>%
    group_by(id1)%>%
    left_join(data2_temp %>%
                select(one_of(select_vars2))%>%
                filter_all(all_vars(. %notin% c("",NA,"NA")))%>% 
                distinct(), 
              c(join_statement))%>% 
    ungroup()%>%
    drop_na(id1,id2)%>%
    mutate(!!! fuzzy_statement)%>%
    filter(dist>=treshold)%>%
    group_by(id1)%>%
    arrange(-dist)%>%
    slice_head(n=1)%>% ### should I ??
    ungroup()%>%
    select(id1,id2)%>%
    distinct()
  
  match_temp <- data1_temp%>%
    select(id1,year,tvs)%>%
    filter(year %in% number_years2)%>%
    left_join(ids_temp, c("id1"))%>%    
    left_join(data2_temp %>% select(id2,data2_id2,year,poll), c("id2", "year"))%>%
    drop_na(data2_id2)%>%
    select(-data2_id2)%>%
    filter(poll>0 & tvs>0)%>%
    distinct()%>%
    mutate(match_level = match_level)%>%
    # KEEP TRACK OF DUPLICATES. WILL DEAL WITH THEM LATER BY EITHER DROPPING OR SUMMING THEIR VALUES OR TVS OR poll. 
    # THEY SHOULD BE GOOD MATCHES. THEY SHOULD NOT BE PUT BACK IN THE POOL FOR WORST MATCHES AFTER.
    group_by(id1,year)%>%
    mutate(n_id2=n_distinct(id2),
           n1=n())%>%
    ungroup()%>%
    group_by(id2,year)%>%
    mutate(n_id1=n_distinct(id1),
           n2=n())%>%
    ungroup()
  
  return(match_temp)
  
}
