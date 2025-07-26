## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE, echo = FALSE--------------------------------------------
  library(ggplot2)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(forcats)
  library(sitar)

## -----------------------------------------------------------------------------
knitr::kable(optimal_design(z = -4:4*2/3, N = 10000), digits = c(2, 2, 0, 3, 0, 2, 2))

## ----fig.width = 7------------------------------------------------------------
minage <- 0
maxage <- 20
N <- 1e4
map_dfr(c(1, 0.9, 0.38), ~{
  tibble(age = (runif(N, minage^.x, maxage^.x))^(1/.x),
         p = .x)
}) %>% 
  mutate(lambda = fct_reorder(factor(p), -p))  %>%
  ggplot(aes(age, group = lambda)) +
  geom_histogram(fill = 'gray', binwidth = 1) +
  facet_wrap(. ~ lambda, labeller = label_both) +
  xlab('age (years)') + ylab('frequency')

## ----plot---------------------------------------------------------------------
  n_table <- map_dfc(-4:4*2/3, ~{
    n_agegp(z = .x, N = 10000) %>% 
      select(!!z2cent(.x) := n_varying)
  }) %>% 
  bind_cols(tibble(age = paste(0:19, 1:20, sep = '-')), .)
knitr::kable(n_table)

## -----------------------------------------------------------------------------
knitr::kable(n_agegp(z = 2, N = 3506, minage = 0, maxage = 5, n_groups = 5) %>% 
  select(age, n_varying))

## ----fig.width = 7------------------------------------------------------------
# define CGS age distribution
  tibble(age = c(1/6, 1/2, 5/6, 5/4, 7/4, 19/8, 25/8, 4:10,
                 c(22:29/2 - 1/4), 15:18, 77/4),
         n = c(rep(12, 3), rep(10, 4), 13, rep(11, 6), rep(6, 3),
               rep(11, 5), 15, rep(8, 3), 13) * 100,
         span = c(rep(1/3, 3), rep(1/2, 2), rep(3/4, 2), rep(1, 7),
                  rep(1/2, 8), rep(1, 4), 3/2)) %>% 
    ggplot(aes(x = age, y = n/span, width = span-0.02)) +
    xlab('age (years)') + ylab('frequency') +
    geom_bar(fill = 'gray', stat = 'identity') +
    geom_bar(aes(y = n_varying, width = NULL), 
             data = n_agegp(z = qnorm(0.03), SEz = 0.3 / 5.75) %>% 
               mutate(n_varying = n_varying * 1.1), 
             width = 0.98, fill = 'gray50', stat = 'identity')

