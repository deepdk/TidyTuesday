library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(reactablefmtr)
library(webshot2)

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
species <- read_excel("D:/species codes.xlsx")

species <- species %>% 
  clean_names()
head(species)

species <- species %>% 
  rename('species_code' = 'e_bird_taxonomy_updated_june_2018') %>% 
  rename('sci_name' = 'x2') %>% 
  rename('primary_com_name' = 'x3') %>% 
  slice(-1)

head(species)

feederwatch <- feederwatch %>% 
  clean_names()

df <- left_join(feederwatch, species, on = 'species_code')

head(df)

month <- df %>% 
  filter(year == 2021,
         valid == 1,
         reviewed == 1,
  ) %>% 
  select(month,species_code, primary_com_name, how_many) %>% 
  group_by(month, primary_com_name) %>%
  summarise(total = sum(how_many)) %>% 
  ungroup()
month

m1 <- month %>% 
  filter(month == 1) %>% 
  arrange(desc(total)) %>% 
  head(5)
m1

m2 <- month %>% 
  filter(month == 2) %>% 
  arrange(desc(total)) %>% 
  head(5)
m2

m3 <- month %>% 
  filter(month == 3) %>% 
  arrange(desc(total)) %>% 
  head(5)
m3

m4 <- month %>% 
  filter(month == 4) %>% 
  arrange(desc(total)) %>% 
  head(5)
m4

df_new <- rbind(m1,m2,m3,m4)
df_new

df_new <- df_new %>%
  mutate(image = case_when(primary_com_name =="Pine Siskin"~"https://celebrateurbanbirds.org/wp-content/uploads/2016/05/pine-siskin-1024x682.jpg",
                           primary_com_name =="Common Redpoll"~"https://www.allaboutbirds.org/guide/assets/photo/124706311-1900px.jpg",
                           primary_com_name == "Purple Finch"~"http://media.cleveland.com/neobirding_impact/photo/purplefinchjpeg-b542b9d470a2ae03.jpeg",
                           primary_com_name == "Canada Goose"~"https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Kanadagans_Branta_canadensis.jpg/1200px-Kanadagans_Branta_canadensis.jpg",
                           primary_com_name == "House Sparrow"~"https://www.saga.co.uk/contentlibrary/saga/publishing/verticals/home-and-garden/gardening/garden-wildlife/david-chapman/birds/house-sparrow-david-chapman.jpg",
                           primary_com_name == "Black-bellied Whistling-Duck"~"http://www.tgonaturecenter.org/wp-content/uploads/2015/03/MAF8878.jpg",
                           primary_com_name == "rosy-finch sp."~"https://birdscoo.com/wp-content/uploads/2020/08/Brown-Capped-Rosy-Finch-1-2048x1365.jpg",
                           primary_com_name == "Northern Cardinal"~"https://4.bp.blogspot.com/_eDsYe_W-c-E/S_vXoPTSY6I/AAAAAAAAEXQ/Y-tVvAZLGok/s1600/Cardinal-Northern,+male+IMG_2614.jpg",
                           primary_com_name == "Common Raven"~"https://cdn.birdwatchingdaily.com/2018/10/i-common-raven.jpg",
                           primary_com_name == "Painted Bunting"~"https://members.ntmn.org/wp-content/uploads/2019/05/Painted-bunting-1.jpg",
                           primary_com_name == "Mallard (Northern)"~"https://dl.id.au/photo_birds/2007-05-27_2993.jpg",
                           primary_com_name == "Wood Duck"~"https://www.captainmitchs.com/wp-content/uploads/2018/01/wood-duck-PFYHVZN.jpg",
                           primary_com_name == "Rose-breasted Grosbeak"~"https://birdscoo.com/wp-content/uploads/2020/05/Rose-Breasted-Grosbeak.jpg",
                           primary_com_name == "Gray Catbird"~"https://www.birdguides.com/cdn/gallery/birds/GreyCatbirdcopycrop2-1.jpg",
                           primary_com_name == "Black-headed Grosbeak"~"https://www.birdguides-cdn.com/cdn/gallery/birdguides/91d63495-f907-4326-96c5-0b18b4ff79f5.jpg",
                           primary_com_name == "Indigo Bunting"~"http://www.ozarksnatureguide.com/wp-content/uploads/2014/04/indigo-bunting.jpg"))
(df_new)

df_new <- df_new %>%
  mutate(month = case_when(month ==1~"January",
                           month ==2~"February",
                           month == 3~"March",
                           month == 4~"April"))
df_new

bird_table <- df_new %>%
  select(month, image, primary_com_name, total) %>% 
  reactable(.,
            theme = sanfran(centered = TRUE, header_font_size = 11),
            pagination = FALSE,
            compact = TRUE,
            # add border between groups when sorting by the Division column
            rowStyle = group_border_sort("month"),
            columns = list(
              month = colDef(
                name = "Month",
                maxWidth = 75,
                # hide rows containing duplicate values on sort 
                style = group_merge_sort("month")
              ),
              primary_com_name = colDef(
                name = "Name of Bird",
                maxWidth = 75,
                # render team logos from their image address and increase their size
              ),
              image = colDef(
                name = "Bird",
                sortable = FALSE,
                maxWidth = 75,
                # render team logos from their image address and increase their size
                style = background_img(height = "125%")
              ),
              total = colDef(
                maxWidth = 400,
                name = "Total no. birds seen",
                # add data bars with a shadow
                cell = data_bars(., text_size = 13, box_shadow = FALSE, fill_color = "#556B2F")
              ))
  ) 
  
bird_table

save_reactable_test(bird_table,"bird_table.png")
