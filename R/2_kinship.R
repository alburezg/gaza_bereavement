
rm(list=ls())
# Create average kinship structure in Palestine 1950:2024

library(tidyverse)
library(tidyr)
library(data.table)
library(DemoKin)

source("R/UNWPP_data.R")

# 1. Get data -----

start_year <- 1950
end_year <- 2023

rates_f <- 
  UNWPP_data(
    country = "State of Palestine"
    , start_year =  start_year
    , end_year = end_year
    , sex = "Female"
    )

pop_f <- UNWPP_pop(
  country = "State of Palestine"
  , start_year = start_year
  , end_year = end_year
  , sex = "Female"
  )

pop_m <- UNWPP_pop(
  country = "State of Palestine"
  , start_year = start_year
  , end_year = end_year
  , sex = "Male"
)

# Reshape fertility
asfr_f <- 
  rates_f %>%
  select(age, year, fx) %>%
  pivot_wider(names_from = year, values_from = fx) %>%
  select(-age) %>%
  as.matrix()

# Reshape survival
px_f <- 
  rates_f %>%
  select(age, year, px) %>%
  pivot_wider(names_from = year, values_from = px) %>%
  select(-age) %>%
  as.matrix()

# Reshape survival
px_m <- 
  rates_m %>%
  select(age, year, px) %>%
  pivot_wider(names_from = year, values_from = px) %>%
  select(-age) %>%
  as.matrix()

# Export data

save(asfr_f, px_f, px_m, pop_f, pop_m, file = "data_int/input_kin.rdata")

load("data_int/input_kin.rdata")

# 2. Run Kinship models ---------

kin_out <- 
  kin2sex(
  pf = px_f,
  pm = px_m,
  ff = asfr_f,
  fm = asfr_f,
  nf = pop_f,
  nm = pop_m,
  output_kin = c("c", "d", "gd", "gm", "m", "n", "a", "s"),
  time_invariant = F,
  sex_focal = "f",
  )

# kin_out$kin_summary %>%
#   filter(year == 2023) %>% 
#   rename_kin(sex = "2sex") %>% 
#   ggplot(aes(age_focal, count_living, fill=sex_kin))+
#   geom_area()+
#   theme_bw() +
#   labs(y = "Expected number of living kin by sex and Focal's age",
#        x = "Age of Focal",
#        fill = "Sex of Kin") +
#   facet_wrap(~kin_label)

# Save kin_full only

kin_full <- 
  kin_out$kin_full %>% 
  mutate(sex_focal ="f") %>% 
  filter(year >= 2023)

# save(kin_full, file = "data_int/kin_full.rdata")
fwrite(kin_full, "data_int/kin_full.csv", row.names = FALSE)
