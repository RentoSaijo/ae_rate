# Load libraries.
suppressMessages(library(tidyverse))

# Read SV.
SV_raw <- read_csv(
  'data/SV001.csv',
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>% 
  select(-any_of('...1'))
SV <- SV_raw %>%
  mutate(
    Site = parse_integer(Site),
    isPlanned = Visits %in% c('Visit1', 'Visit2', 'Visit3')
  ) %>%
  select(-Visits)
rm(SV_raw)

# Read AE.
AE_raw <- read_csv(
  'data/AE001.csv',
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>% 
  select(-any_of('...1'))
AE <- AE_raw %>%
  mutate(
    Site       = parse_integer(Site),
    ofInterest = AE_Category == 'AE OF SPECIAL INTEREST',
    isSerious  = SeriousAE   == 'YES'
  ) %>%
  select(-AE_Category, -SeriousAE)
rm(AE_raw)

# Summarize per site.
SV_per_site <- SV %>% 
  filter(isPlanned == TRUE) %>% 
  group_by(Site) %>% 
  summarise(PlannedVisits = n(), SVSubjects = length(unique(Subject)))
AE_per_site <- AE %>% 
  group_by(Site) %>% 
  summarise(AEs = n(), AESubjects = length(unique(Subject)))

# Calculate AE rate per visit using binomial distribution.
AE_rate_per_visit <- SV_per_site %>% 
  left_join(AE_per_site, by = 'Site') %>% 
  filter(AEs != 0) %>% 
  mutate(Rate = AEs/PlannedVisits) %>% 
  select(-AESubjects, -SVSubjects)
p <- sum(AE_rate_per_visit$AEs)/sum(AE_rate_per_visit$PlannedVisits)
AE_rate_per_visit <- AE_rate_per_visit %>%
  rowwise() %>%
  mutate(
    p_x_greater_than_success = 1-sum(dbinom(0:AEs, PlannedVisits, p)),
    p_x_within_plus_minus_1  = sum(dbinom(AEs-1:AEs+1, PlannedVisits, p))
  )

# Calculate AE rate per subject using binomial distribution.
AE_rate_per_subject <- SV_per_site %>% 
  left_join(AE_per_site, by = 'Site') %>% 
  filter(AEs != 0) %>% 
  mutate(Rate = AESubjects/SVSubjects) %>% 
  select(-AEs, -PlannedVisits)
p <- sum(AE_rate_per_subject$AESubjects)/sum(AE_rate_per_subject$SVSubjects)
AE_rate_per_subject <- AE_rate_per_subject %>%
  rowwise() %>%
  mutate(
    p_x_greater_than_success = 1-sum(dbinom(0:AESubjects, SVSubjects, p))
  )

# Calculate AE rate per subject using multinomial distribution.
xs <- AE %>%
  group_by(Site, Subject) %>%
  summarise(n_ae = n(), .groups = 'drop') %>%
  mutate(bin = paste0('x', n_ae + 1L)) %>% 
  count(Site, bin, name = 'n') %>%
  pivot_wider(names_from = bin, values_from = n, values_fill = 0) %>%
  right_join(distinct(AE_per_site, Site), by = 'Site') %>%
  mutate(across(c(x2, x3, x4, x5), ~ replace_na(.x, 0))) %>%
  select(Site, x2, x3, x4, x5) %>%
  arrange(Site) %>% 
  left_join(SV_per_site, by = 'Site') %>% 
  mutate(x1 = SVSubjects - (x2 + 2 * x3 + 3 * x4 + 4 * x5)) %>% 
  select(Site, SVSubjects, x1, x2, x3, x4, x5)
