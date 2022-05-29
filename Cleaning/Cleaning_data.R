rm(list = ls())

# Librerías ---------------------------------------------------------------

library(tidyverse)

# Procesamiento de información --------------------------------------------

# Clean gross_inv 

raw_df  <- readxl::read_excel("Input/Manual/infrastructure-data-may-2020.xlsx", sheet = 2, skip = 2)

gross_inv <- raw_df %>% 
  rename(group = ...1, category = ...2) %>% 
  filter(!is.na(category)) %>% 
  mutate(
    meta_cat = if_else(!is.na(group), category, NA_character_), 
    group_num = group,
    .after = "category"
  ) %>% 
  fill(meta_cat, group_num) %>%
  pivot_longer(names_to = "year", values_to = "gross_inv", cols = `1947`:`2017`,
               names_transform = list(year = as.integer)) %>% 
  filter(is.na(group)) %>% 
  select(-group)

# Clean gross_inv_chained 

raw_df2  <- readxl::read_excel("Input/Manual/infrastructure-data-may-2020.xlsx", sheet = 3, skip = 2)

chain_inv <- raw_df2 %>% 
  rename(group = ...1, category = ...2) %>%
  mutate(
    meta_cat = if_else(!is.na(group), category, NA_character_), 
    group_num = group,
    .after = "category"
  ) %>% 
  
  fill(meta_cat, group_num) %>%
  pivot_longer(names_to = "year", values_to = "gross_inv_chain", cols = `1947`:`2017`,
               names_transform = list(year = as.integer)) %>% 
  filter(is.na(group)) %>% 
  select(-group)

# Clean ipd 

raw_df3  <- readxl::read_excel("Input/Manual/infrastructure-data-may-2020.xlsx", sheet = 4, skip = 2)

ipd <- raw_df3 %>% 
  rename(group = ...1, category = ...2) %>% 
  filter(!is.na(category)) %>% 
  mutate(
    meta_cat = if_else(!is.na(group), category, NA_character_), 
    group_num = group,
    group_num = if_else(category == "GDP", 0, group_num),
    .after = "category"
  ) %>% 
  
  fill(meta_cat, group_num) %>%
  pivot_longer(names_to = "year", values_to = "gross_inv_ipd", cols = `1947`:`2017`,
               names_transform = list(year = as.integer)) %>% 
  filter(is.na(group)) %>% 
  select(-group) %>% 
  mutate(meta_cat = if_else(category == "GDP", "GDP", meta_cat))

#Join infrastructure

infrastucture_data<-gross_inv%>%
  left_join(chain_inv)%>%
  left_join(ipd)

infrastucture_data$meta_cat<-if_else(str_detect(infrastucture_data$meta_cat,"development"),
                                     "Development",
                                     infrastucture_data$meta_cat)

infrastucture_data$category<-if_else(str_detect(infrastucture_data$category,"S&L"),
                                     "S&L",
                                     if_else(str_detect(infrastucture_data$category,"Private"),
                                             "Private",
                                             "Federal"))

#Elimino categor?as duplicadas

conversor<- tribble(
  ~ group_num,~high_cat,
  #- - - - - - - - - - - - - - - - - - - 
  5,'Total basic infrastructure',
  6,'Total basic infrastructure',
  7,'Total basic infrastructure',
  8,'Total basic infrastructure',
  11,'Total basic infrastructure',
  18,'Total social infrastructure',
  19,'Total social infrastructure',
  21,'Total social infrastructure',
  22,'Total digital infrastructure'
)

# Clean geographic data 

geo_gross_inv  <- readxl::read_excel("Input/Manual/infrastructure-data-may-2020.xlsx", sheet = "By state data", skip = 4,n_max = 52) %>% 
  rename(group = ...1, state = ...2)%>%
  gather(`1992`:`2017`,key = year,value = gross_inv)

geo_ipd  <- readxl::read_excel("Input/Manual/infrastructure-data-may-2020.xlsx", sheet = "By state data", skip = 60,n_max = 52) %>% 
  rename(group = ...1, state = ...2) %>%
  gather(`1992`:`2017`,key = year,value = ipd)

geo_gross_inv_chain  <- readxl::read_excel("Input/Manual/infrastructure-data-may-2020.xlsx", sheet = "By state data", skip = 116,n_max = 52) %>% 
  rename(group = ...1, state = ...2) %>%
  gather(`1992`:`2017`,key = year,value = gross_inv_chain)

geo_population <- readxl::read_excel("Input/Manual/infrastructure-data-may-2020.xlsx", sheet = "By state data", skip = 173,n_max = 52) %>% 
  rename(group = ...1, state = ...2)  %>%
  gather(`1992`:`2017`,key = year,value = population)

geo_total <- geo_gross_inv%>%
  left_join(geo_ipd)%>%
  left_join(geo_gross_inv_chain)%>%
  left_join(geo_population)%>%
  mutate(inv_ch_x_pop = gross_inv_chain/population,
         state= tolower(state))%>%
  select(-group)

geo_total<- geo_total[!str_detect(geo_total$state,"s&l"),]

# Guardo información ------------------------------------------------------

write.csv(geo_total,"Input/geo_data.csv",row.names = F)

infrastucture_data%>%
  inner_join(conversor)%>%
  write.csv("Input/infrastucture_data.csv",row.names = F)