# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# ETL for HNC placements
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)


# read data ---------------------------------------------------------------

social = read_excel("SLWG HNC Social Services (1).xlsx") %>%
  janitor::clean_names()

early = read_excel("Childhood Practice college data 9th Nov.xlsx") %>%
  janitor::clean_names()

colleges = read_excel("colleges.xlsx") %>%
  janitor::clean_names()

postcode = read_csv("~/GIS/OS/postcode.csv")


# geo-code ----------------------------------------------------------------

postcode = postcode %>%
  select(postcode = V1, easting = V3, northing = V4) %>%
  mutate(postcode = str_remove_all(postcode, " "))

colleges = colleges %>%
  mutate(postcode = str_remove_all(centre_postcode, " ")) %>%
  left_join(postcode)


# clean social ------------------------------------------------------------

x = colleges %>%
  mutate(centre_name = replace(centre_name,
                           centre_name == "Borders College (formerly BC Consultants)",
                           "Borders College"),
         centre_name = replace(centre_name,
                 centre_name == "Dumfries & Galloway College",
                 "Dumfries and Galloway"),
         centre_name = replace(centre_name,
                           centre_name == "Dundee & Angus College",
                           "Dundee and Angus"),
         centre_name = replace(centre_name,
                           centre_name == "Forth Valley College",
                           "Forth Valley"),
         centre_name = replace(centre_name,
                           centre_name == "Glasgow Kelvin College",
                           "Glasgow Kelvin"),
         centre_name = replace(centre_name,
                           centre_name == "City of Glasgow College",
                           "City of Glasgow"),
         centre_name = replace(centre_name,
                           centre_name == "",
                           "Lothian College"),
         centre_name = replace(centre_name,
                           centre_name == "North East Scotland College",
                           "NESCOL"),
         centre_name = replace(centre_name,
                           centre_name == "New College Lanarkshire Coatbridge Campus",
                           "New College Lanarkshire"),
         centre_name = replace(centre_name,
                           centre_name == "Orkney College UHI",
                           "Orkney College"),
         centre_name = replace(centre_name,
                           centre_name == "West College Scotland",
                           "West College"),
         centre_name = replace(centre_name,
                           centre_name == "Shetland College UHI",
                           "Shetland College"),
         centre_name = replace(centre_name,
                           centre_name == "West Highland College UHI",
                           "West Highland College"),
         centre_name = replace(centre_name,
                           centre_name == "Argyll College UHI",
                           "Argyll College"))

social = social %>%
  filter(!is.na(college) &
           college != "Total") %>%
  mutate(college = replace(college,
                           college == "Argyl College",
                           "Argyll College")) %>%
  left_join(x, by = c(college = "centre_name"))


# clean early years -------------------------------------------------------

x = colleges %>%
  mutate(centre_name = str_remove_all(centre_name, " College"),
         centre_name = replace(centre_name,
                               centre_name == "Borders (formerly BC Consultants)",
                               "Borders"),
         centre_name = replace(centre_name,
                               centre_name == "Dundee & Angus",
                               "Tayside - Dundee & Angus"),
         centre_name = replace(centre_name,
                               centre_name == "Forth Valley",
                               "Forth Valley"),
         centre_name = replace(centre_name,
                               centre_name == "Glasgow Kelvin",
                               "Glasgow - Kelvin"),
         centre_name = replace(centre_name,
                               centre_name == "City of Glasgow",
                               "Glasgow - City"),
         centre_name = replace(centre_name,
                               centre_name == "Glasgow Clyde",
                               "Glasgow - Clyde"),
         centre_name = replace(centre_name,
                               centre_name == "North East Scotland",
                               "Aberdeen - Nescol"),
         centre_name = replace(centre_name,
                               centre_name == "New Lanarkshire Coatbridge Campus",
                               "Lanarkshire - NCLAN"),
         centre_name = replace(centre_name,
                               centre_name == "Inverness",
                               "UHI - Inverness"),
         centre_name = replace(centre_name,
                               centre_name == "Moray",
                               "UHI - Moray"),
         centre_name = replace(centre_name,
                               centre_name == "Perth",
                               "UHI - Perth"),
         centre_name = replace(centre_name,
                               centre_name == "Orkney UHI",
                               "UHI - Orkney"),
         centre_name = replace(centre_name,
                               centre_name == "West Scotland",
                               "West College Scotland"),
         centre_name = replace(centre_name,
                               centre_name == "Shetland UHI",
                               "UHI - Shetland"),
         centre_name = replace(centre_name,
                               centre_name == "West Highland UHI",
                               "UHI - West Highland"),
         centre_name = replace(centre_name,
                               centre_name == "North Highland",
                               "UHI - North Highland"),
         centre_name = replace(centre_name,
                               centre_name == "Argyll UHI",
                               "UHI - Argyll"),
         centre_name = replace(centre_name,
                               centre_name == "Lewis Castle",
                               "UHI - Lewis Castle"),
         centre_name = replace(centre_name,
                               centre_name == "South Lanarkshire",
                               "Lanarkshire - SLC"))

early %>%
  mutate(college = str_replace_all(college, "–", "-"),
         college = str_replace_all(college, "Lewes", "Lewis")) %>%
  filter(college != "UHI - general comment") %>%
  left_join(x, by = c(college = "centre_name"))





























































