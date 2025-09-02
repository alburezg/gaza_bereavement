library(tidyverse)
library(data.table)
# library(patchwork)

# 1. Load data -----

# ASFR
f <- 
  read.csv("../data/Palestine historical rates - asfr.csv", stringsAsFactors = F)

# TEMP Consolidate age groups

f$age[f$age == "0-19"] <- "15-19"
f$age[f$age == "45+"] <- "45-49"

# Extract Palestine from Un
# fread("../data/WPP2024_Fertility_by_Age5.csv") %>%
#   filter(ISO3_code == "PSE") %>%
#   select(year = Time, age = AgeGrpStart, value = ASFR) %>%
#   mutate(source = "UNWPP", region = "State of Palestine") %>%
#   write.csv(., "../data/un_fert.csv", row.names=FALSE)

f_un <- 
  read.csv("../data/un_fert.csv", stringsAsFactors = F) 

# TFR
tfr <- read.csv("../data/Palestine historical rates - tfr.csv", stringsAsFactors = F)

# Format 

tfr <-
  tfr %>%
    separate(
      year,
      into = c("start_year", "end_year"),
      sep = "-",
      fill = "right",
      remove = FALSE
    ) %>%
    # Convert columns to numeric; any non-numeric text becomes NA
    mutate(
      start_year = as.numeric(start_year),
      end_year = as.numeric(end_year)
    ) %>% 
    # Remove rows where the start_year is not a valid number
    filter(!is.na(start_year)) %>%
    # Apply the calculation row by row
    rowwise() %>%
    # If 'end_year' is missing, use 'start_year'; otherwise, find the mean
    mutate(
      year = ifelse(is.na(end_year), start_year, mean(c(start_year, end_year)))
    ) %>%
    # Ungroup to prevent issues with later operations
    ungroup() %>% 
    select(-start_year, -end_year) %>% 
    # Remove non-numeric values in 'value' column
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value)) %>%
    filter(region != "Palestine (Combined)")


# Extract Palestine from Un
# fread("../data/WPP2024_Demographic_Indicators_Medium.csv") %>%
#   filter(ISO3_code == "PSE") %>%
#   select(year = Time, value = TFR) %>%
#   mutate(source = "UNWPP", region = "State of Palestine") %>%
#   write.csv(., "../data/un_tfr.csv", row.names=FALSE)

tfr_un <- 
  read.csv("../data/un_tfr.csv", stringsAsFactors = F) 

# 2. Plot ----

# 2.1. ASFR ====

# Ungroup

ages_f <- unique(f$age)
years_asfr <- unique(f$year)
years_asfr_small <- c(1968, 1978, 1985, 1995)

f %>% 
  mutate(age = factor(age, levels = ages_f)) %>% 
  ggplot(aes(x = year, y = value, colour = age)) +
  geom_line() +
  facet_wrap(.~region) +
  theme_bw()

# Plot together

f_both <- 
  f %>% 
  mutate(
    age = as.numeric(str_extract(age, "^[0-9]+"))
    , year = as.factor(as.character(year))
  ) %>% 
  bind_rows(
    f_un %>% 
      mutate(
        # age = as.numeric(factor(age, levels = ages_f))
        year = as.factor(as.character(year))
      )
    ) 

# f_both %>% 
#   filter(year %in% years_asfr_small) %>% 
#   ggplot(aes(x = age, y = value, colour = year)) +
#   geom_line() +
#   facet_wrap(.~region) +
#   theme_bw() +
#   theme(legend.position = "bottom")

# ggsave("../output/asfr_palestine.pdf", width = 10, height = 6, units = "in")

# Together faceting by year
f_both %>% 
  filter(year %in% years_asfr) %>%
  ggplot(aes(x = age, y = value, colour = region)) +
  geom_line() +
  facet_wrap(.~year) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("../output/asfr_palestine-region.pdf", width = 10, height = 10, units = "in")

# 2.2. TFR ============

years_tfr <- unique(tfr$year)

tfr_both <- 
  tfr %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value)) %>% 
  bind_rows(tfr_un)

tfr_both %>%
  filter(year <= max(years_tfr)) %>% 
  ggplot(aes(x = year, y = value, colour = region, linetype = source)) +
  geom_line() +
  geom_hline(yintercept = 7.5, linetype = "dashed", colour = "grey") +
  labs(x = "Year", y = "TFR", colour = "") +
  coord_cartesian(ylim = c(0, 9)) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("../output/tfr_palestine.pdf", width = 10, height = 6, units = "in")
