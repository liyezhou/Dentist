#### Libraries ####
library(skimr)
library(rstatix)
library(ggpubr)
library(glue)
library(ggsignif)
library(magrittr)
library(tidyverse)
library(YZUtils)

#### Import Dentist ####
dent <- read_csv("/Users/yezhouli/Documents/Project/Dentist/Dentists Full.csv") %>% as_tibble()
dent %>% names
dent %>% skim
dent %>% view

#### Narrow Down accepting practices ####
table(dent$`Accepting new adult NHS patients`, dent$`Value  (Accepting new adult NHS patients)`)
gdRaw <- dent %>% filter(`Accepting new adult NHS patients` == "Adults 18 and over")

#### Process Location to Find Nearest ####
library(PostcodesioR)
homeRes <- PostcodesioR::postcode_lookup("M13 9PL") %T>% print_tibble_row()
gdRaw %>% print_tibble_row()
gd <- gdRaw %>% transmute(id = `NACS Code`, 
                 name = `Organisation Name`, 
                 postcode = PostCode,
                 tel = `Telephone Number`,
                 rating = `NHS.UK users rating`,
                 acceptNewAdult = `Accepting new adult NHS patients`,
                 parking = `Car parking (Is there car parking at this organisation?)`,
                 Address1, Address2, Address3, Address4, Address5)
gd <- gd %>% mutate(pcRes = postcode %>% map(postcode_lookup))
gd <- gd %>% mutate(
  lat = pcRes %>% map_dbl("latitude"),
  long = pcRes %>% map_dbl("longitude"),
  distance = ((lat - homeRes$lat)^2 + (long - homeRes$long)^2) ^ 0.5 * 76,
  .after = postcode
)
gd %>% arrange(distance) %>% view
gd %>% arrange(distance) %>% select(-pcRes) %>% write_csv("/Users/yezhouli/Documents/Project/Dentist/Dentists.csv")
