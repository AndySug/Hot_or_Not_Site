setwd('C:/Users/AndyS/Documents/Hot or Not/hot_or_not_website/')
libs<-c('tidyverse','tidyr','lubridate','googlesheets4','factoextra','corrplot','clipr','showtext','emojifont', 'scales','svglite','stringdist','jsonlite')
lapply(libs,require,character.only=TRUE)
font_add_google("Nunito Sans","nunito")

data_26 <- read_csv('raw_data/data26.csv') %>%
  rename(item = name) %>%
  mutate(year = 2026, year_int = 7) %>%
  pivot_longer(4:150) %>%
  filter(item != 'weight' & item != 'referrer' & item != 'Hand' & item != 'Gender') %>%
  select(1,4,5,6,7)

data_25 <- read_csv('raw_data/data25.csv') %>% 
  mutate(year = 2025, year_int = 6) %>%
  pivot_longer(2:65) %>%
  filter(item != 'weight')

data_24 <- read_csv('raw_data/data24.csv') %>% 
  mutate(year = 2024, year_int = 5) %>%
  pivot_longer(2:65)

data_23 <- read_csv('raw_data/data23.csv')  %>% 
  mutate(year = 2023, year_int = 4) %>%
  pivot_longer(2:23) %>%
  rename('item' = "What's your name?")

data_22 <- read_csv('raw_data/data22.csv',
                    col_names = c('item','Andy','Caitlin','Kit','Danny','Josie','Scotch','Sophie','Will','Sarah'),
                    col_types = 'ciiiiiiiii',
                    skip = 1)  %>% 
  mutate(year = 2022, year_int = 3) %>%
  na.omit() %>%
  pivot_longer(2:10)

data_21 <- read_csv('raw_data/data21.csv',
                    col_names = c('item','Andy','Caitlin','Kit','Danny','Josie','Sara','Scotch','Sophie','Will'),
                    col_types = 'ciiiiiiiii',
                    skip = 1)  %>% 
  mutate(year = 2021, year_int = 2) %>%
  pivot_longer(2:10)

data_20 <- read_csv('raw_data/data20.csv',
                    col_names = c('item','Kit','Andy','Caitlin','Sophie','Josie','Danny','Scotch','Sarah','Will'),
                    col_types = 'ciiiiiiiii',
                    skip = 1)  %>% 
  mutate(year = 2020, year_int = 1) %>%
  pivot_longer(2:10)

#Vote data final table
data_all_years <- data_26 %>%
  rbind(data_25, data_24, data_23, data_22, data_21, data_20) %>%
  slice_sample(prop = 1) %>%
  mutate(simplified_score = ifelse(as.numeric(value) >= 10, 10, ifelse(as.numeric(value) <= 1, 1, as.numeric(value))))

write_csv(data_all_years,'./raw_data/all_scores.csv')

simplified_all_time <- read_csv('./raw_data/all_time_26.csv')

simplified_all_time <-
  simplified_all_time %>%
  mutate(living_human = case_when(grepl('?',`Living Human`,fixed = TRUE) ~ "Debateable",
                                  grepl('y',`Living Human`,ignore.case = TRUE) ~ "Yes",
                                  TRUE ~ "No"))

data_summ <- data_all_years %>%
  group_by(item, year, year_int, simplified_score) %>%
  summarise(count = n())

score_hist <- data_summ %>% 
  pivot_wider(id_cols = c('item','year','year_int'),
              names_from = simplified_score,
              values_from = count,
              values_fill = 0)

score_nest <- score_hist %>%
  nest(4:14)

#Matching to images
all_img <- list.files('C:/Users/AndyS/Documents/Hot or Not/hot_or_not_website/raw_image/',recursive=TRUE)

no_import <- data.frame(loc = all_img) %>%
  filter(!(str_ends(loc,'.import'))) %>%
  mutate(Year = as.integer(substr(loc,1,4)))

item_year <- simplified_all_time %>%
  filter(is.na(Personal)) %>%
  select(1,6)

top_combo <- item_year %>%
  left_join(no_import,relationship='many-to-many') %>%
  mutate(dist = stringdist(Name,loc,method='lv'),
         list_loc = paste0('./raw_image/',loc)) %>%
  arrange(Year,Name,dist) %>%
  group_by(Year,Name) %>%
  slice_head(n=1) %>%
  ungroup()

loc_changes <- read_csv('./raw_data/loc_changes.csv') %>%
  mutate(Year = as.integer(substr(loc,1,4)),
         dist = NA,
         list_loc = paste0('./raw_image/',loc))

correct_combo <- top_combo %>%
  anti_join(loc_changes,by=c('Name', 'Year')) %>%
  rbind(loc_changes)

correct_combo <- correct_combo %>% 
  filter(Name != 'Josh Gill (transparent PNG).png') %>%
  filter(Name != 'Greg Wallace') %>%
  filter(Name != 'Gregg Wallace (Hair)') %>%
  filter(Name != 'Leader of the Opposition Keir Starmer') %>%
  filter() %>%
  mutate(site_loc = paste0('./raw_image/',loc))





nest_and_slug <- score_nest %>%
  mutate(slug = tolower(gsub('[[:blank:]]+','-',gsub('[[:punct:]]+','',item))),
         url = paste0('/',year,'/',slug))


#Joining
name_changes <- read_csv('./raw_data/name_changes.csv')

scores_and_overall <- simplified_all_time %>%
  left_join(name_changes,by=c("Name"="official_name","Year"="year")) %>%
  mutate(matching_name = coalesce(yearly_name,Name)) %>%
  left_join(nest_and_slug,by=c("matching_name"="item","Year"="year"))

overall_w_image <- scores_and_overall %>%
  inner_join(correct_combo,by=c("Name","Year")) %>%
  select(-5,-8,-10,-12)

write_csv(overall_w_image,'./site_data/full_dataset.csv')

json <- toJSON(overall_w_image)

write_file(json,"./site_data/full_dataset.json")


#Missing - Pacha (new groove) & Chicken Doughnut Dippers







