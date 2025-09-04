
rm(list = ls())

library(tidyverse)
library(data.table)
library(DemoKin)
library(knitr)

# 1. Load data ------

dts <- read.csv("data_int/dts_gaza.csv", stringsAsFactors = F)

kin_full <- fread("data_int/kin_full.csv", stringsAsFactors = F)

# hack: for now, I don't have 2024 in kinship data, so let's replicate values
# for 2023 and assume they are the same for 2024

kin_full <- 
  kin_full %>% 
  bind_rows(
    kin_full %>% 
      filter(year == 2023) %>% 
      mutate(year = 2024)
  )

# 2. Analysis ------

y_keep <- 2023:2024
sex_focal <- "f"

cnf_kin <- 
  dts %>% 
  filter(sex != "t") %>% 
  rename(sex_kin = sex, age_kin = age, mx_kin = mx) %>% 
  select(-dx)

# 2.1. Bereavement probabilities =========

kin <- 
  kin_full %>% 
  # filter(kin != "c") %>%
  filter(year %in% y_keep) %>% 
  rename(sex = sex_focal, age = age_focal)

brv_temp <- 
  kin %>% 
  left_join(cnf_kin, by = join_by(year, sex_kin, age_kin)) %>% 
  mutate(
    p0_dt = (1-mx_kin)^living, # probability that no relatives of the focal die
  ) %>% 
  select(year, sex, age, kin, sex_kin, age_kin, living, mx_kin, pop, p0_dt)

# adding together all kin losses and combining probabilities 
# across all ages and sexes of relatives. estimates per year, sex, and age of focal
brv <- 
  brv_temp %>% 
  summarise(p0_dt = prod(p0_dt), .by = c(year, sex, age, kin))

# 2.2. BEreaved persons=========

# multiplying each each probability of bereavement by the 
# exposed population in each age-sex combination

pop <- 
  dts %>% 
  filter(year == 2024) %>%
  select(-mx)

# accumulation of bereavement risk over time 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# probability that none relative is killed accumulates over life
# we calculate the cumulative product of the probability by age over the 
# same cohort 

brv_cum <- 
  brv %>% 
  select(year, sex, age, kin, p0 = p0_dt) %>% 
  mutate(cohort = year - age) %>% 
  arrange(kin, cohort, sex, age) %>% 
  group_by(sex, cohort, kin) %>% 
  arrange(age) %>% 
  mutate(p0_cum = cumprod(p0)) %>% 
  ungroup() 

# persons bereaved by the conflict in 2024
brv_pop <- 
  brv_cum %>%
  filter(year == 2024) %>%
  left_join(pop) %>% 
  mutate(q0_cum = 1 - p0_cum,
         b_per = q0_cum * pop)

brv_kin <- 
  brv_pop %>% 
  filter(year == 2024) %>%
  reframe(b_per = sum(b_per),
          .by = c(kin)) %>% 
  arrange(kin) %>% 
  rename_kin("2sex") %>% 
  select(-kin) %>% 
  rename(kin = kin_label)


# people with at least one kin loss in the extended family

brv_ext <- 
  brv_cum %>% 
  filter(year == 2024) %>%
  reframe(p0_cum = prod(p0_cum),
          .by = c(age, sex)) %>% 
  left_join(pop) %>% 
  # filter(year == 2024) %>%
  mutate(q0_cum = 1 - p0_cum,
         b_per = q0_cum * pop,
         kin = "Extended") %>%
  reframe(b_per = sum(b_per),
          .by = c(kin))

brv_ncl <- 
  brv_cum %>% 
  filter(kin %in% c("d", "m", "s")) %>% 
  filter(year == 2024) %>%
  reframe(p0_cum = prod(p0_cum),
          .by = c(age, sex, cohort)) %>% 
  left_join(pop) %>% 
  # filter(year == 2024) %>%
  mutate(q0_cum = 1 - p0_cum,
         b_per = q0_cum * pop,
         kin = "Nuclear") %>%
  reframe(b_per = sum(b_per),
          .by = c(kin))

# adding together nuclear and extended bereavement
pop2024 <- sum(pop$pop)

brv_all <- 
  bind_rows(brv_kin,
            brv_ncl,
            brv_ext) %>% 
  mutate(b_per_perc = round(b_per / pop2024 * 100)) %>% 
  select(kin, bereaved = b_per, bereaved_perc = b_per_perc)

kable(brv_all)

# 3. Diagnosis -----

# Age pyramid of deaths

dts %>% 
  mutate(value = ifelse(sex == "f", -dx, dx)) %>% 
  ggplot(aes(x = age, y = value, fill = sex)) +
  geom_col() +
  facet_wrap(~year) +
  coord_flip() +
  theme_bw()

# Cumulative deaths, starting at 0
dts %>% 
  group_by(year, sex) %>%
  mutate(dx = cumsum(dx)) %>% 
  mutate(value = ifelse(sex == "f", -dx, dx)) %>% 
  ungroup() %>%
  ggplot(aes(x = age, y = value, fill = sex)) +
  geom_col() +
  facet_wrap(~year) +
  coord_flip() +
  theme_bw()

# prob of having a living kin over age

kin_keep <- c("d", "c", "a", "gd", "s", "n")

kp <- 
  kin_full %>% 
  filter(
    kin %in% kin_keep
    , year == 2024
  ) %>% 
  rename_kin("2sex")

kp2 <- 
  kp %>% 
  summarise(value = sum(living), .by = c(kin_label, age_focal, year)) %>% 
  mutate(name = "1. Number of living kin")

# mean age of kin over age
kp3 <- 
  kp %>% 
  summarise(
    value = sum(age_kin*living)/sum(living)
    , .by = c(age_focal, year, kin_label)
  ) %>% 
  mutate(name = "2. Mean age of Kin")

bind_rows(kp2, kp3) %>% 
  ggplot(aes(x = age_focal, y = value, colour = kin_label)) +
  geom_line() +
  facet_wrap(~name) +
  theme_bw()
