library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)

select(surveys, plot_id, species_id, weight)
select(surveys, -record_id, -species_id)

filter(surveys, year ==1995, sex =="M")
surveys2 <- filter(surveys, weight <5)
surveys_sml <- select(surveys2, species_id, sex, weight)
surveys_sml2 <- select(filter(surveys, weight < 5), species_id, sex, weight)

# cmd shift m for pipe 
surveys %>%
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg *2.2) %>% 
  view()

# split-apply-combine paradigm
surveys %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE))

surveys %>% 
  filter (!is.na(weight),!is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE)) %>% 
  print(n=15)

surveys %>% 
  filter (!is.na(weight),!is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE), min_weight = min(weight)) %>% 
  arrange(min_weight)

surveys %>% 
  count(sex, species)

# for exporting a table with specific filtering, assign new name before the first surveys line,
# all the following steps will be in the new table 

surveys %>% 
  count(plot_type)

# n for counting number of observations
surveys %>% 
  filter(!is.na(hindfoot_length), !is.na(species_id)) %>% 
  group_by(species_id) %>% 
  summarise(
    mean_hindfoot_length = mean(hindfoot_length),
    min_hindfoot_length = min(hindfoot_length), 
    max_hindfoot_length =max(hindfoot_length), n=n()) %>%
  view()

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>% 
  select(year, genus, species_id, weight) %>% 
  arrange(year) %>% 
  unique()
  
# Rearrange according to genus 
surveys_gw <- surveys  %>% 
  filter(!is.na(weight)) %>% 
  group_by(plot_id,genus) %>% 
  summarise(mean_weight = mean(weight))

# converting to a wide format
surveys_wide <- surveys_gw %>% 
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0)

# converting back to a long format (useful for plotting data)
# columns have to be indicated 
surveys_long <- surveys_wide %>% 
  pivot_longer(names_to = "genus", values_to = "mean_weight", cols = -plot_id)

# measurement hindfoot_length and weight
# mean values of each measurement per year in different plot types 
surveys_long_1 <- surveys %>% 
  filter(!is.na(weight), !is.na(hindfoot_length)) %>% 
  pivot_longer(names_to = "measurement", values_to = "value", cols = c(hindfoot_length, weight))

surveys_long_1 %>% 
  group_by(year, measurement, plot_type) %>%
  summarise(mean_value = mean(value)) %>% 
  pivot_wider(names_from = measurement, values_from = mean_value)

surveys_complete <- surveys %>% 
  filter(!is.na(weight), !is.na(sex), !is.na(hindfoot_length))

write_csv(surveys_complete, file = "suvreys_complete.csv")  
