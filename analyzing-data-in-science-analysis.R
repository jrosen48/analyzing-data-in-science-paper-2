library(tidyverse)
library(googlesheets4)

d <- read_sheet("https://docs.google.com/spreadsheets/d/1gLyAAp6N_5g-Uo-eb61O-eW2apMzzqNdM8fLcBV7V3g/edit#gid=46458265",
                  sheet = 1)

d %>% 
  count(gender) %>% 
  mutate(prop = n /330) # other is prefer not to say

d %>% 
  mutate(years_worked = unlist(years_worked)) %>% 
  mutate(years_worked = as.numeric(years_worked)) %>% 
  select(years_worked, elem, middle, high) %>% 
  filter(years_worked < 100) %>% # 2 outliers here
  summarize(mean_years_worked = mean(years_worked, na.rm = T),
            sd_years_worked = sd(years_worked, na.rm = T))

d %>% 
  count(underrepresented) %>% 
  mutate(prop = n / 330)

d %>% 
  count(with_which_groups) %>% 
  arrange(desc(n)) %>% 
  filter(with_which_groups != "NA") # 9 African American/black (2.7%), 4 hispanic (1.2%), 2 asian (0.60%), 2 american indian, 1 muslim (0.30%)

d %>% count(state_work) %>%  # PR, outside US, 2 NA
  arrange(desc(n))

qual_q80 <- read_sheet("https://docs.google.com/spreadsheets/d/1gLyAAp6N_5g-Uo-eb61O-eW2apMzzqNdM8fLcBV7V3g/edit#gid=46458265",
                sheet = "Q 80 - MASTER")

qual_q80 %>% 
  select(3:9) %>% 
  summarize_all(sum, na.rm = T) %>% 
  gather(key, val) %>% 
  mutate(prop = val/328) %>% 
  arrange(desc(val))
