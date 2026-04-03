setwd('C:/Users/AndyS/Documents/Hot or Not/hot_or_not_website/')
libs<-c('tidyverse','tidyr','lubridate','googlesheets4','factoextra','corrplot','clipr','showtext','emojifont', 'scales','svglite','stringdist','jsonlite')
lapply(libs,require,character.only=TRUE)
font_add_google("Nunito Sans","nunito")
font_add("noto-emoji","C:\\Users\\AndyS\\Documents\\R\\win-library\\4.1\\emojifont\\emoji_fonts\\NotoEmoji.ttf")

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

data_all_years <- data_25 %>%
  rbind(data_24, data_23, data_22, data_21, data_20) %>%
  slice_sample(prop = 1)

data_summ <- data_all_years %>%
  group_by(item, year, year_int, value) %>%
  summarise(count = n())

score_hist <- data_summ %>% 
  pivot_wider(id_cols = c('item','year','year_int'),
              names_from = value,
              values_from = count,
              values_fill = 0)

score_hist %>%
  select(1:3) %>%
  write_clip()

fixed_loc_to_25 <- read_csv('raw_data/fixed_loc.csv') %>%
  transmute(item = Name,
            year = Year,
            score = Score,
            tags = `Same Thing?`,
            loc = loc)

fixed_loc_to_25 <- fixed_loc_to_25 %>% 
  filter(item != 'Josh Gill (transparent PNG).png') %>%
  filter(item != 'Greg Wallace') %>%
  filter(item != 'Gregg Wallace (Hair)') %>%
  filter(item != 'Leader of the Opposition Keir Starmer') %>%
  filter(!(str_ends(loc,'gif'))) %>%
  mutate(site_loc = paste0('./raw_image/',loc))

### Add 2026 in here

### Need to fix a bunch of missing things as well

loc_and_scores <- fixed_loc_to_25 %>%
  inner_join(score_nest)

write_csv(loc_and_scores,'./site_data/full_dataset.csv')

json <- toJSON(loc_and_scores)

write_file(json,"./site_data/full_dataset.json")





