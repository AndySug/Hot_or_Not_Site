setwd('C:/Users/AndyS/Documents/Hot or Not/hot_or_not_website/')
libs<-c('tidyverse','tidyr','lubridate','googlesheets4','factoextra','corrplot','clipr','showtext','emojifont', 'scales','svglite','stringdist','jsonlite')
lapply(libs,require,character.only=TRUE)
font_add_google("Nunito Sans","nunito")

data_26 <- read_csv('raw_data/data26.csv') %>%
  rename(item = name) %>%
  filter(item != 'weight' & item != 'referrer' & item != 'Hand' & item != 'Gender') %>%
  mutate(year = 2026, year_int = 7, item_id = row_number() + 1803) %>%
  pivot_longer(4:150) %>%
  select(1,4,5,6,7,8)

data_25 <- read_csv('raw_data/data25.csv') %>% 
  arrange(item) %>%
  filter(item != 'weight') %>%
  mutate(year = 2025, year_int = 6, item_id = row_number() + 1503) %>%
  pivot_longer(2:65)

data_24 <- read_csv('raw_data/data24.csv') %>% 
  arrange(item) %>%
  mutate(year = 2024, year_int = 5, item_id = row_number() + 1202) %>%
  pivot_longer(2:65)

data_23 <- read_csv('raw_data/data23.csv')  %>% 
  arrange(`What's your name?`) %>%
  mutate(year = 2023, year_int = 4, item_id = row_number() + 902) %>%
  pivot_longer(2:23) %>%
  rename('item' = "What's your name?")

data_22 <- read_csv('raw_data/data22.csv',
                    col_names = c('item','Andy','Caitlin','Kit','Danny','Josie','Scotch','Sophie','Will','Sarah'),
                    col_types = 'ciiiiiiiii',
                    skip = 1)  %>% 
  arrange(item) %>%
  filter(!is.na(item)) %>%
  mutate(year = 2022, year_int = 3, item_id = row_number() + 600) %>%
  pivot_longer(2:10)

data_21 <- read_csv('raw_data/data21.csv',
                    col_names = c('item','Andy','Caitlin','Kit','Danny','Josie','Sara','Scotch','Sophie','Will'),
                    col_types = 'ciiiiiiiii',
                    skip = 1)  %>% 
  arrange(item) %>%
  mutate(year = 2021, year_int = 2, item_id = row_number() + 300) %>%
  pivot_longer(2:10)

data_20 <- read_csv('raw_data/data20.csv',
                    col_names = c('item','Kit','Andy','Caitlin','Sophie','Josie','Danny','Scotch','Sarah','Will'),
                    col_types = 'ciiiiiiiii',
                    skip = 1)  %>%
  arrange(item) %>%
  mutate(year = 2020, year_int = 1, item_id = row_number()) %>%
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
  group_by(item, item_id, year, year_int, simplified_score) %>%
  summarise(count = n())

score_hist <- data_summ %>% 
  pivot_wider(id_cols = c('item','item_id','year','year_int'),
              names_from = simplified_score,
              values_from = count,
              values_fill = 0)

score_nest <- score_hist %>%
  nest(data = 5:15)

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
  inner_join(correct_combo,by=c("Name","Year"))

overall_named <- overall_w_image %>%
  rename(item = Name,
         score = Score,
         rank_year_original = `Rank (year)`,
         rank_alltime_original = `Rank (all time)`,
         year = Year,
         tag = `Same Thing?`,
         gender = Gender)

overall_rerank <- overall_named %>%
  group_by(year) %>%
  mutate(rank_year = rank(desc(score),na.last = "keep",ties.method = "min")) %>%
  ungroup() %>%
  mutate(rank_alltime = rank(desc(score),na.last = "keep",ties.method = "min")) %>%
  select(-5,-10)

write_csv(overall_rerank,'./site_data/full_dataset.csv')

json_formatting <- overall_rerank %>%
  mutate(score = format(score,nsmall = 2))

json <- toJSON(json_formatting)

write_file(json,"./site_data/full_dataset.json")


#Missing - Pacha (new groove) & Chicken Doughnut Dippers







