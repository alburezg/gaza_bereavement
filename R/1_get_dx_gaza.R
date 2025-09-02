# TODO  ----
# 1. Improve ungrouping for dx
# 2. Improve ungrouping for pop

rm(list=ls())

library(tidyverse)
library(ungroup)
library(DemoTools)

# This scripts gets mx from the models ran by Ana for the PopHealthMetrics paper
# for Gaza and combines it with pop data forecasted by Ugo to estimate numbers
# of deaths in Gaza in 2023 and 2024. 
# Mx estimates are given for various models and LC forecasts for UN and PCBS data. 
# For now, just take UN model and PCBS-data forecast as an example. 

# 1. Get mx ----

# De Ana:
# Ya los decifré, las tasas de mortalidad están en los archivos que se llaman 
# “XXXXXXXX_lifetable_f_le0.csv”. Estos archivos estan en “R\model\diff_reporting\samples\...”
# Las tasas de mortalidad aparecen en las columnas que se llama X1 – X18 (o de
# la columna 3 a la 20, depende como lo veas). Cada columna corresponde a una
# edad (las edades son c(0,1,seq(5,80,5)) y cada renglón a una simulación.
# Entonces para obtener el promedio de las tasas tienes que obtener el promedio
# de cada columna, y eso te va a dar la tasa de mortalidad promedio para cada
# edad.
# Repo: 
# https://github.com/realirena/uncertainty_quantification/tree/main/R/model/diff_reporting/samples

# Functino to transform data
get_mx <- function(df, sex = "m", ages = c(0,1,seq(5,80,5)), year = 2023){
  
  cols_keep <- c(paste0("X", seq(1,18)))
  
  df %>%
    select(any_of(cols_keep)) %>%
    summarise(across(everything(), mean)) %>% 
    pivot_longer(everything(), names_to = "age", values_to = "mx_model") %>%
    mutate(
      age = ages
      , sex = sex
      , year = year
    )
}

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

# Get data ====

ages <- c(0,1,seq(5,80,5))

if(file.exists("data/un_conflict23_lifetable_f_le0.csv")){
  mx <-
    read.csv("data/un_conflict23_lifetable_f_le0.csv", stringsAsFactors = FALSE) %>%
    get_mx(sex = "f", ages, year = 2023) %>%
    bind_rows(
      read.csv("data/un_conflict24_lifetable_f_le0.csv", stringsAsFactors = FALSE) %>%
        get_mx(sex = "f", ages, year = 2024)
    ) %>%
    bind_rows(
      read.csv("data/un_conflict23_lifetable_m_le0.csv", stringsAsFactors = FALSE) %>%
        get_mx(sex = "m", ages, year = 2023)
    ) %>%
    bind_rows(
      read.csv("data/un_conflict24_lifetable_m_le0.csv", stringsAsFactors = FALSE) %>%
        get_mx(sex = "m", ages, year = 2024)
    ) %>%
    bind_rows(
      read.csv("data/un_conflict23_lifetable_t_le0.csv", stringsAsFactors = FALSE) %>%
        get_mx(sex = "t", ages, year = 2023)
    ) %>%
    bind_rows(
      read.csv("data/un_conflict24_lifetable_t_le0.csv", stringsAsFactors = FALSE) %>%
        get_mx(sex = "t", ages, year = 2024)
    ) %>%
    mutate(region = "Gaza Strip")
} else {
  # Read from internet
  # Base URL for the raw files on GitHub
  base_url <- "https://raw.githubusercontent.com/realirena/uncertainty_quantification/main/R/model/diff_reporting/samples/gaza/"
  
  # Create a data frame that lists each file and its corresponding parameters (sex and year)
  files_to_process <- tibble::tribble(
    ~filename,                             ~sex, ~year,
    "un_conflict23_lifetable_f_le0.csv",   "f",  2023,
    "un_conflict24_lifetable_f_le0.csv",   "f",  2024,
    "un_conflict23_lifetable_m_le0.csv",   "m",  2023,
    "un_conflict24_lifetable_m_le0.csv",   "m",  2024,
    "un_conflict23_lifetable_t_le0.csv",   "t",  2023,
    "un_conflict24_lifetable_t_le0.csv",   "t",  2024
  )
  
  # Read and process all files
  # We use pmap_dfr to iterate over each row of the 'files_to_process' data frame.
  # For each file, it constructs the full URL, reads the CSV, and processes it.
  # The results are automatically combined into a single data frame called 'mx'.
  mx <- pmap_dfr(files_to_process, function(filename, sex, year) {
    
    # Construct the full URL for the raw CSV file
    full_url <- paste0(base_url, filename)
    
    # Read the data and process it
    read.csv(full_url, stringsAsFactors = FALSE) %>%
      get_mx(sex = sex, ages = ages, year = year)
    
  }) %>%
    # Add the region column at the end
    mutate(region = "Gaza Strip")
}

mx %>% 
  ggplot(aes(age, mx_model, colour = sex)) +
  facet_wrap(~year) +
  geom_line() +
  theme_bw()

# 2. Get number of deaths ----

# Lo población por edad y sexo está en este archivo, el archivo tiene las 
# diferentes fuentes de información y las proyecciones que hizo Ugo: 
# https://github.com/realirena/uncertainty_quantification/blob/main/R/lc/data_plus_forecasts_v2.rds  

# Use PCBS population for now (not sure what WPP population for Gaza is)

if(file.exists("data/un_conflict23_lifetable_f_le0.csv")){
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
  left_join(mx) %>% 
  filter(!is.na(mx_model)) %>% 
  # Estimate deaths
  mutate(
    dx_model = mx_model * pop
  ) %>% 
  # Clean up
  select(region, year, sex, age, dx = dx_model, mx = mx_model, pop)

# Check if numbers make sense
dts %>% 
  filter(sex %in% "t") %>%
  pull(dx) %>%
  sum()

# Deaths are consistent over sexes
dts %>% 
  ggplot(aes(x = age, y = dx, fill = sex)) +
  geom_area(data = . %>% filter(sex != "t")) +
  geom_line(data = . %>% filter(sex == "t"), size = 1) +
  facet_wrap(~year) +
  theme_bw()

# 3. Ungroup deaths ----

# chunk <- dts %>% filter(year == 2023, sex == "f")

dts2 <- 
  dts %>% 
  filter(sex != "t") %>% 
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
  left_join(dts) %>% 
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

# Not very happy with this, but just for testing purposes it will do.

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
  left_join(pop) %>% 
  select(year, sex, age, dx1, dx2, original = dx) %>%
  pivot_longer(cols = c(dx1, dx2, original))

comp %>% 
  ggplot(aes(x = age, y = value, colour = name)) +
  geom_line() +
  facet_wrap(~sex+year) +
  theme_bw()

# scaling ungrouped deaths to grouped values
pop3 <- 
  pop2 %>% 
  mutate(age2 = age - age%%5,
         age2 = ifelse(age2 >=80, 80, age2)) %>% 
  group_by(year, sex, age2) %>% 
  mutate(dx_sum = sum(dx)) %>% 
  left_join(
    pop %>% 
      rename(age2 = age, dx_grp = dx)
  ) %>% 
  mutate(adj_fc = dx_grp/dx_sum,
         dx = dx * adj_fc,
         dx_sum = sum(dx),
         diff = dx_grp - dx_sum) %>% 
  ungroup() %>% 
  select(year, sex, age, dx) %>% 
  mutate(dx = round(dx, 5))

# Test

comp2 <- 
  pop3 %>% 
  mutate(name = "scaled") %>% 
  rename(value = dx) %>%
  bind_rows(comp) 

comp2 %>% 
  ggplot(aes(x = age, y = value, colour = name)) +
  geom_line() +
  facet_wrap(~sex+year) +
  theme_bw()

# Sum to same pop?
comp2 %>% 
  summarise(value = sum(value), .by = c(name))


pop4 <- 
  pop3 %>% 
  rename(pop = dx)

# 5. Get rates -----

dts4 <- 
  dts3 %>% 
  left_join(pop4) %>% 
  mutate(mx = dx/pop)

# Export deaths ----

write.csv(dts4, "data_int/dts_gaza.csv", row.names = FALSE)
