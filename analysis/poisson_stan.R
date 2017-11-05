# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
library(rstan)
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ------------------------------------------------------------------------------
# 2015 acs data
# ------------------------------------------------------------------------------
acs <- tbl(db, 'acs_sector') %>% collect()

# convert race, hispanic, and education to percentages
E1 <- c('B15003_017', 'B15003_018')
E4 <- str_c('B15003_0', 21:25)
acs <- acs %>%
  rename(
    W = B02001_002,
    B = B02001_003,
    I = B02001_004,
    A = B02001_005,
    H = B03003_003
  ) %>%
  mutate(
    den = B01001_001 / B25001_001,
    inc = B19013_001,
    pctW = W / B02001_001,
    pctB = B / B02001_001,
    pctI = I / B02001_001,
    pctA = A / B02001_001,
    pctH = H / B03003_001
  ) %>%
  within({
    pctE1 <- rowSums(acs[E1]) / B15003_001
    pctE4 <- rowSums(acs[E4]) / B15003_001
  })

# sector characteristics
sec <- acs %>% select(sector, W, B, I, A, H, den, inc, contains('pct'))
sec %<>% gather(subjectrace, P, -sector, -7:-15)
sec %<>% 
  filter(!sector %in% c('9', 'H')) %>%
  filter(subjectrace != 'H') %>%
  mutate(
    log_inc = log(inc), 
    c_inc = arm::rescale(inc),
    c_pctB = arm::rescale(pctB),
    c_pctE4 = arm::rescale(pctE4)
  ) %>%
  arrange(sector, subjectrace)

# ------------------------------------------------------------------------------
# 2016 terry data
# ------------------------------------------------------------------------------
terry_in <- tbl(db, 'terry') %>% collect()

terry <- terry_in %>%
  mutate(
    dt = ymd_hms(str_c(reported_date, reported_time), tz = 'US/Pacific')
  ) %>%
  filter(!sector %in% c('-', '99', 'NULL')) %>%
  filter(!subjectrace %in% c('M', 'O', 'U')) %>%
  count(year(dt), sector, subjectrace) %>%
  group_by(`year(dt)`, sector) %>%
  mutate(
    N = sum(n),
    p = n / N
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 2015 nibr/sibr matched records
# ------------------------------------------------------------------------------
mibr_in <- tbl(db, 'mibr') %>% collect()

mibr <- mibr_in %>%
  filter(!sector %in% c('-', '99', 'NULL')) %>%
  mutate(race = replace(race, race == 'U' & ethn == 'H', 'H')) %>%
  rename(subjectrace = race) %>%
  filter(subjectrace != 'U') %>%
  count(year(dt), sector, subjectrace) %>%
  complete(`year(dt)`, sector, subjectrace, fill = list(n = 0)) %>%
  group_by(`year(dt)`, sector) %>%
  mutate(
    N = sum(n),
    p = n / N
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# plot to look at rates over time
# ------------------------------------------------------------------------------

# organize race as factor
# terry$subjectrace %<>% factor()
# levels(terry$subjectrace) <- c('I', 'A', 'B', 'H', 'W')
# terry$subjectrace <- fct_rev(terry$subjectrace)

# organize race as factor
# mibr$subjectrace %<>% factor(levels = levels(terry$subjectrace))

# plot
ggplot(mibr) +
  geom_bar(
    aes(`year(dt)`, p, fill = subjectrace),
    stat = 'identity',
    position = 'dodge'
    ) +
  facet_wrap(~sector, nrow = 6, ncol = 3)

# ------------------------------------------------------------------------------
# join data
# ------------------------------------------------------------------------------
dat <- full_join(
  terry, mibr, 
  by = c('year(dt)', 'sector', 'subjectrace'),
  suffix = c('_terry', '_mibr')
)

dat %<>%
  rename(year = `year(dt)`) %>%
  group_by(sector, subjectrace) %>%
  mutate(
    ln_mibr = lag(n_mibr, order_by = year),
    lN_mibr = lag(N_mibr, order_by = year)
  ) %>%
  ungroup() %>%
  filter(year %in% c(2015:2016)) %>%
  filter(subjectrace != 'H') %>% 
  arrange(year, sector, subjectrace) %>%
  mutate(
    SR = interaction(sector, subjectrace, lex.order = T)
    )

# ------------------------------------------------------------------------------
# stan - poission regression
# ------------------------------------------------------------------------------
inits <- function(chain, N = NULL, S = NULL, R = NULL) {
  set.seed(10 ^ chain)
  
  # model a
  x <- list(
    mu = rnorm(1, 0, 1),
    raw_a = rnorm(R, 0, 1),
    raw_b = rnorm(S, 0, 1),
    raw_e = rnorm(N, 0, 1),
    sigma_a = abs(rnorm(1, 0, 1)),
    sigma_b = abs(rnorm(1, 0, 1)),
    sigma_e = abs(rnorm(1, 0, 1))
  )
  
  # model b
  x <- append(x, list(
    gamma = rnorm(1, 0, 1),
    rho = rnorm(1, 0, 1)
  ))
  
  # model c / d
  x <- append(x, list(
    nu = rnorm(1, 0, 1),
    raw_d = rnorm(R, 0, 1),
    raw_z = rnorm(S, 0, 1),
    raw_u = rnorm(R * S, 0, 1),
    raw_w = rnorm(N, 0, 1),
    sigma_d = abs(rnorm(1, 0, 1)),
    sigma_z = abs(rnorm(1, 0, 1)),
    sigma_u = abs(rnorm(1, 0, 1)),
    sigma_w = abs(rnorm(1, 0, 1)),
    phi = rnorm(1, 0, 1),
    psi = rnorm(1, 0, 1)
  ))
  
  return(x)
}

pars1 <- c('mu', 'alpha', 'beta', 'sigma_a', 'sigma_b', 'sigma_e', 'lp')
pars2 <- c('nu', 'delta', 'zeta', 'sigma_d', 'sigma_z', 'theta')
pars <- list(
  a1 = pars1,
  a2 = c(pars1, 'gamma'),
  a3 = c(pars1, 'rho'),
  b1 = pars1,
  b2 = c(pars1, 'gamma'),
  b3 = c(pars1, 'rho'),
  c1 = c(pars1, pars2, 'sigma_u'),
  c2 = c(pars1, pars2, 'sigma_u', 'gamma'),
  c3 = c(pars1, pars2, 'sigma_u', 'phi', 'psi'),
  d1 = c(pars1, pars2, 'sigma_w'),
  d2 = c(pars1, pars2, 'sigma_w', 'gamma'),
  d3 = c(pars1, pars2, 'sigma_w', 'phi', 'psi')
)

stan_dat <- list(
  S = 17,
  R = 4,
  K = 17 * 4,
  N = 2 * 17 * 4,
  y = dat$n_terry,
  b = dat$ln_mibr,
  X = select(sec, P, c_inc, c_pctB, c_pctE4),
  yS = as.numeric(factor(dat$sector)),
  yR = as.numeric(factor(dat$subjectrace)),
  yC = as.numeric(factor(dat$SR)),
  cS = as.numeric(factor(dat$sector[1:68])),
  cR = as.numeric(factor(dat$subjectrace[1:68]))
)

stan_fit <- function(m) {
  stan(
    file = str_c('analysis/models/poisson_', m, '.stan'),
    data = stan_dat,
    iter = 1000,
    chains = 4,
    init = map(1:4, inits, N = 2 * 17 * 4, S = 17, R = 4),
    pars = pars[[m]],
    seed = 314159,
    control = list(
      adapt_delta = 0.99,
      max_treedepth = 13
    )
  )
}
stan_fit_q <- quietly(stan_fit)

m <- c('a1', 'a2', 'a3', 'b1', 'b2', 'b3', 'c1', 'c2', 'c3', 'd1', 'd2', 'd3')
fit <- list()
for (i in seq_along(m)) {
  fit[[i]] <- stan_fit_q(m[i])
}
names(fit) <- m

# add data to list
fit$data <- stan_dat

# save
save(fit, file = 'analysis/poisson_stan_fits_nu.Rda')

xxx <- stan_fit_q('c3')
yyy <- stan_fit_q('d3')
