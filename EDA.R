library(tidyverse)
library(lubridate)

age_gender <- read_csv("age_gender_bkts.csv")
country <- read_csv("countries.csv")
sessions <- read_csv("sessions.csv")
user <- read_csv("train_users_2.csv")

sessions %>% head()

user %>% summary()

user %>%
  count(gender) #need to impute

user %>% filter(age <= 85, age >= 15) %>%
  ggplot(aes(age)) +
  geom_histogram(color = 'white', bins = 71) #need to impute

user %>%
  mutate(book.y = year(date_first_booking)) %>%
  count(book.y)

user %>%
  mutate(book.y = year(date_first_booking),
         book.m = month(date_first_booking)) %>%
  filter(book.y != 2015) %>%
  count(book.m)

user %>%
  count(signup_method, sort = TRUE)

user %>%
  count(signup_flow)

user %>%
  count(signup_method, signup_flow) %>% spread(signup_method, n)

user %>%
  count(language, sort = TRUE)

user %>%
  count(affiliate_channel, sort = TRUE)

user %>%
  count(affiliate_provider, sort = TRUE)

user %>%
  count(first_affiliate_tracked, sort = TRUE)

user %>%
  count(signup_app, sort = TRUE)

user %>%
  count(first_device_type, sort = TRUE)

user %>%
  count(first_browser, sort = TRUE)

user %>%
  count(country_destination, sort = TRUE)

user$timestamp_first_active %/% 1000000 %>% ymd()
user$timestamp_first_active %% 1000000

user %>%
  mutate(is_booking = ifelse(is.na(date_first_booking), 0, 1),
         date_first_active = timestamp_first_active %/% 1000000 %>% ymd()) %>%
  filter(is_booking == 1) %>%
  mutate(journey = date_first_booking - date_first_active) %>%
  select(id, date_account_created, date_first_active, date_first_booking, journey) %>%
  arrange(journey) %>%
  select(journey) %>%
  mutate(journey = as.numeric(journey)) %>% summary()
#what we find? In the early days, we can book without account(before 2013 yrs)
#besides, 50% people will book within 3 days since first active time

#country growth curve
user %>%
  mutate(is_booking = ifelse(is.na(date_first_booking), 0, 1),
         book_yr = year(date_first_booking)) %>%
  filter(is_booking == 1) %>%
  select(id, book_yr, country_destination) %>%
  count(book_yr, country_destination) %>%
  group_by(book_yr) %>%
  mutate(sum_yr = sum(n)) %>%
  ungroup() %>%
  mutate(ratio = n / sum_yr) %>%
  arrange(book_yr, desc(ratio)) %>% 
  filter(country_destination != 'US') %>%
  ggplot(aes(book_yr, ratio, color = country_destination)) +
  geom_line() +
  facet_grid(~country_destination)

breaks <- c(0, seq(14,85,5), 999)

#check sex prorportion among age_bucket
user %>%
  mutate(age_bucket = cut(user$age, breaks)) %>%
  filter(!age_bucket %in% c('(0,14]', '(84,999]', NA)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(gender), gender != '-unknown-', gender != 'OTHER') %>%
  select(id, gender, age_bucket, country_destination) %>%
  count(age_bucket, gender) %>%
  #filter(n >= 10) %>%
  #group_by(age_bucket) %>%
  #mutate(tot = sum(n), ratio = n/tot) %>%
  ggplot(aes(age_bucket, n, fill = gender)) +
  geom_bar(stat = 'identity', position = 'dodge')

#check sex prorportion among age_bucket across countries
user %>%
  mutate(age_bucket = cut(user$age, breaks)) %>%
  filter(!age_bucket %in% c('(0,14]', '(84,999]', NA)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(gender), gender != '-unknown-', gender != 'OTHER') %>%
  select(id, gender, age_bucket, country_destination) %>%
  count(age_bucket, country_destination, gender) %>%
  filter(n >= 10) %>%
  group_by(age_bucket, country_destination) %>%
  mutate(tot = sum(n), ratio = n/tot) %>%
  ggplot(aes(age_bucket, ratio, fill = gender)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  facet_wrap(~ country_destination)

#check sessions----------------
sessions %>%
  count(action, sort = TRUE)

sessions %>%
  count(action_type, sort = TRUE)

sessions %>%
  count(action_detail, sort = TRUE)

sessions %>%
  count(action, action_type, action_detail, sort = TRUE) %>%
  #filter(action_type == 'view') %>%
  arrange(action) %>% View()

sessions %>% 
  filter(user_id == 'd1mm9tcy42') %>% View()

sessions %>%
  count(user_id, sort = TRUE) %>%
  summary(n) #eliminate user_id = NA

sessions %>%
  filter(is.na(user_id))

breaks <- c(0, seq(1,10,1), Inf)
sessions %>%
  inner_join(user, by = c("user_id"='id')) %>%
  mutate(is_booking = ifelse(country_destination=='NDF', 0, 1)) %>%
  filter(action == 'search_results') %>%
  count(user_id, is_booking) %>%
  mutate(logn = log2(n+1),
         ngroup = cut(logn, breaks)) %>%
  group_by(ngroup) %>%
  summarise(booking_rate = mean(is_booking), nn = n()) %>%
  filter(nn >= 10) %>%
  ggplot(aes(ngroup, booking_rate, fill = booking_rate)) +
  geom_bar(stat = 'identity') +
  xlab('search_results使用次數(log2)')
  

#demographic chart-------------------------
age_gender$age_bucket <- age_gender$age_bucket %>% 
  factor(
    levels = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39',
               '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79',
               '80-84', '85-89', '90-94', '95-99','100+')
  )
tmp1 <- age_gender %>%
  group_by(age_bucket, gender, country_destination) %>%
  summarise(n = sum(population_in_thousands)) %>%
  mutate(n = ifelse(gender == 'female', -n, n))

tmp1 %>%
  ggplot(aes(age_bucket, n, group = gender, fill = gender)) +
  geom_bar(stat="identity", position = 'dodge', data = subset(tmp1, gender == 'male')) +
  geom_bar(stat="identity", position = 'dodge', data = subset(tmp1, gender == 'female')) +
  coord_flip() +
  facet_wrap(~ country_destination)

tmp2 <- age_gender %>%
  group_by(age_bucket, gender, country_destination) %>%
  summarise(n = sum(population_in_thousands)) %>%
  ungroup() %>%
  group_by(country_destination) %>%
  mutate(tot = sum(n), ratio = n / tot) %>%
  select(-tot, -n) %>%
  mutate(ratio = ifelse(gender == 'female', -ratio, ratio))

tmp2 %>%
  ggplot(aes(age_bucket, ratio, group = gender, fill = gender)) +
  geom_bar(stat="identity", position = 'dodge', data = subset(tmp2, gender == 'male')) +
  geom_bar(stat="identity", position = 'dodge', data = subset(tmp2, gender == 'female')) +
  coord_flip() +
  facet_wrap(~ country_destination)

age_gender %>%
  group_by(age_bucket, country_destination) %>%
  summarise(n = sum(population_in_thousands)) %>%
  ungroup() %>%
  group_by(age_bucket) %>%
  mutate(tot = sum(n),
         ratio = n / tot) %>%
  ggplot(aes(age_bucket, ratio, fill = country_destination)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  facet_wrap(~ country_destination)
  