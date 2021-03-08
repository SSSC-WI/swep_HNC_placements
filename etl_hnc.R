# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# ETL for HNC placements
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)


# read data ---------------------------------------------------------------

# 0 in NA to account for mandatory fields.
# 0 should be dropped when subsequent data come in.

df = read_excel("Placement Provision.xlsx", na = c("-99", "0")) %>%
  janitor::clean_names()

college_postcode = read_excel("colleges.xlsx") %>%
  janitor::clean_names()

postcode = read_csv("~/GIS/OS/postcode.csv")


# geo-code ----------------------------------------------------------------

postcode = postcode %>%
  select(postcode = V1, easting = V3, northing = V4) %>%
  mutate(postcode = str_remove_all(postcode, " "))

college_xy = college_postcode %>%
  mutate(postcode = str_remove_all(centre_postcode, " ")) %>%
  left_join(postcode) %>%
  select(centre_name, easting, northing)


# clean -------------------------------------------------------------------

x = df %>%
  select(-id, -completion_time, -email, -name, -your_name) %>%
  rename(centre_name = which_college_are_you_responding_for) %>%
  left_join(college_xy)

x_enrol = x %>%
  select(centre_name, start_time, contains("enrolled")) %>%
  pivot_longer(contains("enrolled"), values_to = "enrolled") %>%
  drop_na(enrolled) %>%
  group_by(centre_name, name) %>%
  filter(start_time == max(start_time)) %>%
  ungroup() %>%
  mutate(name = str_remove(name, "how_many_"),
         name = str_remove(name, "_students_are_enrolled")) %>%
  separate(name, c("course", "option"), sep = "_on_")


x_place = x %>%
  select(centre_name, start_time, contains("placement")) %>%
  pivot_longer(contains("placement"), names_to = "course", values_to = "not_placed") %>%
  drop_na(not_placed) %>%
  group_by(centre_name, course) %>%
  filter(start_time == max(start_time)) %>%
  ungroup() %>%
  mutate(course = str_remove(course, "how_many_"),
         course = str_remove(course, "_students_do_not_have_a_placement"),
         course = str_remove(course, "_option_2"))

df_clean = x %>%
  select(centre_name, start_time, easting, northing) %>%
  full_join(x_enrol) %>%
  full_join(x_place) %>%
  mutate(placement_req = if_else(option %in% c("option_1",
                                               "option_3",
                                               "theoretical_group_award"),
                                 "no", "yes"),
         not_placed = replace(not_placed,
                              placement_req == "no",
                              NA),
         enrolled = as.numeric(enrolled),
         not_placed = as.numeric(not_placed),
         not_placed = if_else(is.na(not_placed) & placement_req == "yes",
                              enrolled, not_placed)) %>%
  drop_na(enrolled)


# transform for outliers --------------------------------------------------

df_clean = df_clean %>%
  mutate(enrolled_log = log(enrolled),
         enrolled_log = replace(enrolled_log,
                                enrolled_log == -Inf, 0),
         not_placed_log = log(not_placed),
         not_placed_log = replace(not_placed_log,
                                  not_placed_log == -Inf, 0))


# IO ----------------------------------------------------------------------

df_clean %>%
  write_csv("placements_clean.csv")

