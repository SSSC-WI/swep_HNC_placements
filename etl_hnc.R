# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# ETL for HNC placements
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)


# read data ---------------------------------------------------------------

social = read_excel("SLWG HNC Social Services.xlsx") %>%
  janitor::clean_names()

# early = read_excel("Childhood Practice college data 9th Nov.xlsx") %>%
#   janitor::clean_names()

college_names = read_csv("college_name_lookup.csv")

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


# clean social ------------------------------------------------------------

social %>%
  select(college) %>%
  filter(!is.na(college) &
           college != "Total") %>%
  left_join(college_names, by = c(college = "hnc_social_name")) %>%
  left_join(college_xy)


# clean early years -------------------------------------------------------


# early %>%
#   mutate(college = str_replace_all(college, "–", "-"),
#          college = str_replace_all(college, "Lewes", "Lewis")) %>%
#   filter(college != "UHI - general comment") %>%
#   left_join(x, by = c(college = "centre_name"))





























































