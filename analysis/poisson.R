# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
library(RSocrata)
library(rstan)
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ------------------------------------------------------------------------------
# 2016 terry data
# ------------------------------------------------------------------------------
terry_in <- tbl(db, 'terry') %>% collect()

terry <- terry_in %>%
  mutate(
    dt = ymd_hms(str_c(reported_date, reported_time), tz = 'US/Pacific')
  ) %>%
  filter(!sector %in% c('-', '99', 'NULL')) %>%
  filter(!subjectrace %in% c('-', 'Multi-Racial', 'Other', 'Unknown')) %>%
  count(year(dt), sector, subjectrace) %>%
  group_by(`year(dt)`, sector) %>%
  mutate(
    N = sum(n),
    p = n / N
  )

# organize race as factor
terry$subjectrace %<>% factor()
levels(terry$subjectrace) <- c('I', 'A', 'B', 'H', 'W')
terry$subjectrace <- fct_rev(terry$subjectrace)

# ------------------------------------------------------------------------------
# 2015 nibr/sibr matched records
# ------------------------------------------------------------------------------
mibr_in <- tbl(db, 'mibr') %>% collect()

mibr <- mibr_in %>%
  ungroup() %>%
  filter(sector != '9') %>%
  mutate(
    race = if_else(race == '', 'U', race),
    ethn = if_else(ethn == '', 'U', ethn),
    subjectrace = race,
    subjectrace = if_else(race == 'U', ethn, race),
    subjectrace = if_else(subjectrace == 'N', 'U', subjectrace)
  ) %>%
  filter(subjectrace != 'U') %>%
  count(year(dt), sector, subjectrace) %>%
  complete(`year(dt)`, sector, subjectrace, fill = list(n = 0)) %>%
  group_by(`year(dt)`, sector) %>%
  mutate(
    N = sum(n),
    p = n / N
  )

# organize race as factor
mibr$subjectrace %<>% factor(levels = levels(terry$subjectrace))

# plot to look at rates over time
ggplot(mibr) +
  geom_bar(
    aes(`year(dt)`, p, fill = subjectrace),
    stat = 'identity',
    position = 'dodge'
    ) +
  facet_wrap(~sector, nrow = 6, ncol = 3)

dat <- left_join(terry, mibr, by = c('year(dt)', 'sector', 'subjectrace'))
dat %<>%
  group_by(sector, subjectrace) %>%
  mutate(
    n.yl = lag(n.y, order_by = `year(dt)`),
    N.yl = lag(N.y, order_by = `year(dt)`),
    p.yl = lag(p.y, order_by = `year(dt)`)
  )

# ------------------------------------------------------------------------------
# glm - poission regression
# ------------------------------------------------------------------------------
reg1 <- glm(
  n.x ~ 1,
  offset = log(n.yl),
  family = poisson(link = 'log'),
  data = filter(dat, subjectrace != 'H')
)
summary(reg1)

reg2 <- glm(
  n.x ~ factor(subjectrace),
  offset = log(n.yl),
  family = poisson(link = 'log'),
  data = filter(dat, subjectrace != 'H')
)
summary(reg2)

reg3 <- glm(
  n.x ~ factor(subjectrace) + factor(sector),
  offset = log(n.yl),
  family = poisson(link = 'log'),
  data = filter(dat, subjectrace != 'H')
)
summary(reg3)

# ------------------------------------------------------------------------------
# stan - poission regression
# ------------------------------------------------------------------------------
inits <- function(chain) {
  set.seed(10 ^ chain)
  
  # model a
  x <- list(
    mu = rnorm(1, 0, 1),
    alpha_raw = rnorm(4, 0, 1),
    beta_raw  = rnorm(17, 0, 1),
    epsilon_raw = rnorm(17 * 4, 0, 1),
    sigma_a = abs(rnorm(1, 0, 1)),
    sigma_b = abs(rnorm(1, 0, 1)),
    sigma_e = abs(rnorm(1, 0, 1))
  )
  
  # model b
  x <- append(x, list(
    tau = rnorm(1, 0, 1)
  ))
  
  # model c / d
  x <- append(x, list(
    l2_alpha_raw = rnorm(4, 0, 1),
    l2_beta_raw  = rnorm(17, 0, 1),
    l2_epsilon_raw = rnorm(17 * 4, 0, 1),
    l2_sigma_a = abs(rnorm(1, 0, 1)),
    l2_sigma_b = abs(rnorm(1, 0, 1)),
    l2_sigma_e = abs(rnorm(1, 0, 1))
  ))
  
  return(x)
}

z <- filter(dat, `year(dt)` == 2016 & subjectrace != 'H')

fit <- stan(
  file = 'analysis/models/poisson_d.stan',
  data = list(
    S = 17,
    R = 4,
    N = 17 * 4,
    y = z$n.x,
    b = z$n.yl,
    p = round(runif(17 * 4, 1.5, 3) * z$n.yl),
    yS = as.numeric(factor(z$sector)),
    yR = as.numeric(factor(z$subjectrace))
  ),
  iter = 2000,
  chains = 4,
  init = map(1:4, inits),
  pars = c('mu_adj', 'alpha_adj', 'beta', 'sigma_a', 'sigma_b', 'sigma_e'),
  seed = 314159,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 11
  )
)
print(fit)
