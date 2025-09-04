# TODO  ----
# 1. Improve ungrouping for dx
# 2. Improve ungrouping for pop

rm(list=ls())

library(tidyverse)
library(ungroup)
library(DemoTools)
library(data.table)

# This scripts gets mx from the models ran by Ana for the PopHealthMetrics paper
# for Gaza and combines it with pop data forecasted by Ugo to estimate numbers
# of deaths in Gaza in 2023 and 2024. 
# Mx estimates are given for various models and LC forecasts for UN and PCBS data. 
# For now, just take UN model and PCBS-data forecast as an example. 

# Functions -----

# From Kike
ung_age <- function(chunk){
  dt_in <- 
    tibble(age = chunk$age,
           dx = chunk$dx,
           dx_mt = dx*1e5) %>% 
    mutate(dx_mt = ifelse(dx_mt == 0, 1, dx_mt))
  
  nl <- 101 - max(dt_in$age)
  # nl <- 6
  dxs <- pclm(x = dt_in$age,
              y = dt_in$dx_mt, 
              nlast = nl)$fitted
  
  fit <- tibble(age = 0:100, dx = dxs/1e5)
  out <- 
    chunk %>% 
    select(year, sex) %>% 
    unique() %>%
    cross_join(fit)
  # left_join(fit,
  #           by = character())
  return(out)
}

# 1. Get mx ----

# The ASMR come from this Repo: 
# https://github.com/realirena/uncertainty_quantification/tree/main/R/model/diff_reporting/samples

# And were extracted by a script prepared bi kike on 20250903

# Get data ====

# ages <- c(0,1,seq(5,80,5))

mx <- 
  read.csv("data/irena_rates_mean.csv", stringsAsFactors = F) %>% 
  mutate(region = "Gaza Strip") %>% 
  filter(
    cause == "conflict"
    , source == "moh"
    ) %>%
    # filter(cause == "all") %>% 
  select(-cause) %>% 
  rename(mx_model = mx_m)

mx %>% 
  ggplot(aes(x = age, y = mx_model, group = sex)) +
  geom_line(linewidth = 1, col = "red")+
  scale_y_log10()+
  facet_grid(year~sex)+
  theme_bw()

# 2. Get number of deaths ----

# Lo población por edad y sexo está en este archivo, el archivo tiene las 
# diferentes fuentes de información y las proyecciones que hizo Ugo: 
# https://github.com/realirena/uncertainty_quantification/blob/main/R/lc/data_plus_forecasts_v2.rds  

# Use PCBS population for now (not sure what WPP population for Gaza is)

if(file.exists("data/data_plus_forecasts_v2.rds")){
  dat <- readRDS("data/data_plus_forecasts_v2.rds")
} else {
  dat <- readRDS(url("https://raw.githubusercontent.com/realirena/uncertainty_quantification/main/R/lc/data_plus_forecasts_v2.rds"))
}

y_keep <- 2023:2024

dts <- 
  dat %>% 
  select(region, year, sex, age, pop, dx_cnf, source) %>% 
  filter(
    region == "Gaza Strip"
    # Using PBCS LC projections for now
    , source == "pcbs"
  )  %>% 
  left_join(mx, by = c("region", "year", "sex", "age")) %>% 
  filter(!is.na(mx_model)) %>% 
  # Estimate deaths
  mutate(
    dx_model = mx_model * pop
  ) %>% 
  # Clean up
  select(region, year, sex, age, dx = dx_model, mx = mx_model, pop)

# Check if numbers make sense
dts %>% 
  summarise(dx = sum(dx), .by = year)

# Deaths over sexes
dts %>% 
  ggplot(aes(x = age, y = dx, fill = sex)) +
  geom_area(data = . %>% filter(sex != "t")) +
  # geom_line(data = . %>% filter(sex == "t"), size = 1) +
  facet_wrap(~year) +
  theme_bw()

# 3. Ungroup deaths ----

# chunk <- dts %>% filter(year == 2023, sex == "f")

dts2 <- 
  dts %>% 
  group_by(year, sex) %>% 
  do(ung_age(chunk = .data)) %>% 
  ungroup() 

# testing consistency between ungrouped and originally grouped
comp <- 
  dts2 %>% 
  mutate(dx2 = round(dx),
         age = age - age%%5,
         age = ifelse(age >=80, 80, age)) %>% 
  reframe(dx1 = sum(dx),
          dx2 = sum(dx2),
          .by = c(year, sex, age)) %>% 
  left_join(
    dts %>% 
      mutate(age = ifelse(age == 1, 0, age)) %>% 
      summarise(dx = sum(dx), .by = c(year, sex, age))
  ) %>% 
  select(year, sex, age, dx1, dx2, original = dx) %>%
  pivot_longer(cols = c(dx1, dx2, original))

comp %>% 
  ggplot(aes(x = age, y = value, colour = name)) +
  geom_line() +
  facet_wrap(~sex+year) +
  theme_bw()

# scaling ungrouped deaths to grouped values
dts3 <- 
  dts2 %>% 
  mutate(age2 = age - age%%5,
         age2 = ifelse(age2 >=80, 80, age2)) %>% 
  group_by(year, sex, age2) %>% 
  mutate(dx_sum = sum(dx)) %>% 
  left_join(
    dts %>% 
      rename(age2 = age, dx_grp = dx)
  ) %>% 
  mutate(adj_fc = dx_grp/dx_sum,
         dx = dx * adj_fc,
         dx_sum = sum(dx),
         diff = dx_grp - dx_sum) %>% 
  ungroup() %>% 
  select(year, sex, age, dx) %>% 
  mutate(dx = round(dx, 5))

# 4. Ungroup pop -----

pop <- 
  dat %>% 
  filter(
    sex != "t"
    , year %in% y_keep 
    , region == "Gaza Strip"
    # Using PBCS LC projections for now
    , source == "pcbs"
  ) %>% 
  select(year, sex, age, dx = pop)

# chunk <- pop %>% filter(year == 2023, sex == "f")

# This is not really working, but just to get something for now
pop2 <- 
  pop %>% 
  group_by(year, sex) %>% 
  do(ung_age(chunk = .data)) %>% 
  ungroup() 

# testing consistency between ungrouped and originally grouped
comp <- 
  pop2 %>% 
  mutate(dx2 = round(dx),
         age = age - age%%5,
         age = ifelse(age >=80, 80, age)) %>% 
  reframe(dx1 = sum(dx),
          dx2 = sum(dx2),
          .by = c(year, sex, age)) %>% 
  left_join(
    pop %>% 
      mutate(age = ifelse(age == 1, 0, age)) %>% 
      summarise(dx = sum(dx), .by = c(year, sex, age))
    ) %>% 
  select(year, sex, age, dx1, dx2, original = dx) %>%
  pivot_longer(cols = c(dx1, dx2, original))

comp %>% 
  ggplot(aes(x = age, y = value, colour = name)) +
  geom_line() +
  facet_wrap(~sex+year) +
  theme_bw()

# Sum to same pop?
comp %>% 
  summarise(value = sum(value), .by = c(name))

# scaling ungrouped deaths to grouped values
# pop3 <- 
#   pop2 %>% 
#   mutate(age2 = age - age%%5,
#          age2 = ifelse(age2 >=80, 80, age2)) %>% 
#   group_by(year, sex, age2) %>% 
#   mutate(dx_sum = sum(dx)) %>% 
#   left_join(
#     pop %>% 
#       rename(age2 = age, dx_grp = dx)
#   ) %>% 
#   mutate(adj_fc = dx_grp/dx_sum,
#          dx = dx * adj_fc,
#          dx_sum = sum(dx),
#          diff = dx_grp - dx_sum) %>% 
#   ungroup() %>% 
#   select(year, sex, age, dx) %>% 
#   mutate(dx = round(dx, 5))

# Test

# comp2 <- 
#   pop3 %>% 
#   mutate(name = "scaled") %>% 
#   rename(value = dx) %>%
#   bind_rows(comp) 

# comp2 %>% 
#   ggplot(aes(x = age, y = value, colour = name)) +
#   geom_line() +
#   facet_wrap(~sex+year) +
#   theme_bw()

pop3 <- 
  pop2 %>% 
  rename(pop = dx)

# 5. Get rates -----

dts4 <- 
  dts3 %>% 
  left_join(pop3) %>% 
  mutate(mx = dx/pop)

# Export deaths ----

write.csv(dts4, "data_int/dts_gaza.csv", row.names = FALSE)
