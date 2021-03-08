# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# ETL for HNC placements
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)


# read data ---------------------------------------------------------------

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
  select(-completion_time, -email, -name, -your_name) %>%
  rename(centre_name = which_college_are_you_responding_for) %>%
  left_join(college_xy)

x_enrol = x %>%
  select(id, contains("enrolled")) %>%
  pivot_longer(!id, values_to = "enrolled") %>%
  mutate(name = str_remove(name, "how_many_"),
         name = str_remove(name, "_students_are_enrolled")) %>%
  separate(name, c("course", "option"), sep = "_on_") %>%
  drop_na(enrolled)

x_place = x %>%
  select(id, contains("placement")) %>%
  pivot_longer(!id, names_to = "course", values_to = "not_placed") %>%
  mutate(course = str_remove(course, "how_many_"),
         course = str_remove(course, "_students_do_not_have_a_placement"),
         course = str_remove(course, "_option_2")) %>%
  drop_na(not_placed)

df_clean = x %>%
  select(id, start_time, centre_name, easting, northing) %>%
  full_join(x_enrol) %>%
  full_join(x_place) %>%
  mutate(not_placed = replace(not_placed,
                              option %in% c("option_1",
                                            "option_3",
                                            "theoretical_group_award"),
                              NA),
         enrolled = as.numeric(enrolled),
         not_placed = as.numeric(not_placed))


# QA ----------------------------------------------------------------------

x = df %>%
  select(contains("placement")) %>%
  pivot_longer(everything())

x = sum(as.numeric(x$value), na.rm = T)
y = sum(df_clean$not_placed, na.rm = T)

stopifnot("Incorect unplaced students" = x == y)

x = df %>%
  select(contains("enrolled")) %>%
  pivot_longer(everything())

x = sum(as.numeric(x$value), na.rm = T)
y = sum(df_clean$enrolled, na.rm = T)

stopifnot("Incorect enrolled students" = x == y)


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

