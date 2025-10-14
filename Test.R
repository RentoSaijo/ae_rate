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
    p_x_greater_than_success = 1-sum(dbinom(0:AEs, PlannedVisits, p))
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
  mutate(x1 = SVSubjects - (x2 + x3 + x4 + x5)) %>% 
  select(Site, SVSubjects, x1, x2, x3, x4, x5)
p1 <- sum(xs$x1)/sum(xs$SVSubjects)
p2 <- sum(xs$x2)/sum(xs$SVSubjects)
p3 <- sum(xs$x3)/sum(xs$SVSubjects)
p4 <- sum(xs$x4)/sum(xs$SVSubjects)
p5 <- sum(xs$x5)/sum(xs$SVSubjects)
ps <- c(p1, p2, p3, p4, p5)

# ---- helper: P(Y_i <= U_i for all i), Y ~ Multinomial(n, q) ----
upper_leq_mult <- function(n, q, U) {
  K <- length(q)
  if (sum(U) < n) return(0)
  if (K == 1) return(as.numeric(n <= U[1]))
  
  q1 <- q[1]; U1 <- U[1]
  lo <- max(0L, n - sum(U[-1]))   # leave enough for the rest
  hi <- min(U1, n)
  if (hi < lo) return(0)
  
  if (1 - q1 < 1e-15) return(as.numeric(n <= U1))  # handle q1 ~ 1
  
  s <- 0
  for (y in lo:hi) {
    s <- s + dbinom(y, size = n, prob = q1) *
      upper_leq_mult(n - y, q[-1] / (1 - q1), U[-1])
  }
  s
}

# ---- exact union: P(any reporting category exceeds its observed count) ----
# This equals 1 - P(X2<=x2, X3<=x3, X4<=x4, X5<=x5).
# (Note X1>=x1 then follows automatically from the sum constraint.)
union_reporting_prob_exact <- function(n, p, x) {
  stopifnot(length(p) == 5, length(x) == 5, abs(sum(p) - 1) < 1e-10)
  p1 <- p[1]
  q  <- p[-1] / (1 - p1)  # conditional probs for 2..5 given not in 1
  U  <- x[-1]             # caps for 2..5 in the complement
  
  # Sum over X1 = t. Only t >= n - sum(U) contributes; that's t >= x1.
  tmin <- max(0L, n - sum(U))     # equals x1 when x1 = n - sum_{i>=2} x_i
  comp <- 0
  for (t in tmin:n) {
    m <- n - t
    comp <- comp + dbinom(t, size = n, prob = p1) * upper_leq_mult(m, q, U)
  }
  1 - comp
}

# Per-site p-values for "any reporting category > observed"
xs <- xs %>%
  rowwise() %>%
  mutate(
    probability_union = union_reporting_prob_exact(
      n = SVSubjects,
      p = ps,
      x = c(x1, x2, x3, x4, x5)
    )
  ) %>%
  ungroup()
