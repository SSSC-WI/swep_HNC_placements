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

df = read_excel("Placement Provision.xlsx", na = c("-99", "0"))

college_postcode = read_excel("colleges.xlsx") %>%
  janitor::clean_names()

postcode = read_csv("~/GIS/OS/postcode.csv")


# geo-code ----------------------------------------------------------------

postcode = postcode %>%
  select(postcode = V1, easting = V3, northing = V4) %>%
  mutate(postcode = str_remove_all(postcode, " "))

college_xy = college_postcode %>%
  mutate(postcode = str_remove_all(centre_postcode, " "),
         centre_name = replace(centre_name,
                               centre_name == "Borders College (formerly BC Consultants)",
                               "Borders College")) %>%
  left_join(postcode) %>%
  select(centre_name, easting, northing)


# clean -------------------------------------------------------------------

# Location
x = df %>%
  select(-ID, -`Completion time`, -Email, -Name, -`Your name`,
         start_time = `Start time`) %>%
  rename(centre_name = `Which college are you responding for?`) %>%
  left_join(college_xy)

# How many are enrolled
x_enrol = x %>%
  select(centre_name, start_time, contains("enrolled"), contains(" on ")) %>%
  pivot_longer(c(contains("enrolled"), contains(" on ")), values_to = "students") %>%
  drop_na(students) %>%
  group_by(centre_name, name) %>%
  filter(start_time == max(start_time)) %>%
  ungroup() %>%
  mutate(name = str_remove(name, "\\?"),
         name = str_remove(name, "How many "),
         name = str_remove(name, " students are enrolled"),
         name = str_remove(name, " students are"),
         name = str_remove(name, "\\(i.e. without a placement arranged\\)")) %>%
  separate(name, c("course", "option"), sep = " on ")

# How many are without a placement
x_place = x %>%
  select(centre_name, start_time, contains("option 2"), contains("do not")) %>%
  pivot_longer(c(contains("option 2"), contains("do not")), names_to = "course", values_to = "not_placed") %>%
  drop_na(not_placed) %>%
  group_by(centre_name, course) %>%
  filter(start_time == max(start_time)) %>%
  ungroup() %>%
  mutate(course = str_remove(course, "\\?"),
         course = str_remove(course, "How many "),
         course = str_remove(course, " students do not have a placement"),
         course = str_remove(course, " students are"),
         course = str_remove(course, " on option 2 \\(i.e. without a placement arranged\\)"))

# How many have a delay to their placement
x_delay = x %>%
  select(centre_name, start_time, contains("delay")) %>%
  pivot_longer(contains("delay"), names_to = "course", values_to = "delayed") %>%
  drop_na(delayed) %>%
  group_by(centre_name, course) %>%
  filter(start_time == max(start_time)) %>%
  ungroup() %>%
  mutate(course = str_remove(course, "\\?"),
         course = str_remove(course, "How many "),
         course = str_remove(course, " students have a delay to their arranged placement"))

# Stick them all together
# NAs exist from older entries in full table, these are removed drop_na
df_clean = x %>%
  select(centre_name, start_time, easting, northing) %>%
  full_join(x_enrol) %>%
  full_join(x_place) %>%
  full_join(x_delay) %>%
  mutate(placement_req = if_else(option %in% c("option 3",
                                               "theoretical group award"),
                                 "no", "yes"),
         not_placed = replace(not_placed,
                              placement_req == "no",
                              NA),
         students = as.numeric(students),
         not_placed = as.numeric(not_placed),
         delayed = as.numeric(delayed),
         not_placed = if_else(is.na(not_placed) & placement_req == "yes",
                              students, not_placed),
         delayed = replace_na(delayed, 0),
         start_time = str_sub(start_time, 1, 10)) %>%
  drop_na(students) %>%
  mutate(not_placed = if_else(option == "option 1" &
                                !is.na(option),
                              0, not_placed))


# transform for outliers --------------------------------------------------

df_clean = df_clean %>%
  mutate(students_log = log(students),
         students_log = replace(students_log,
                                students_log == -Inf, 0),
         not_placed_log = log(not_placed),
         not_placed_log = replace(not_placed_log,
                                  not_placed_log == -Inf, 0),
         delayed_log = log(delayed),
         delayed_log = replace(delayed_log,
                               delayed_log == -Inf, 0))


# IO ----------------------------------------------------------------------

df_clean %>%
  write_csv("placements_clean.csv")

