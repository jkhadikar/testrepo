# R TIPS ----
# TIP 009 | Must-Know Tidyverse Features: Summarise + Across ----
#
# 👉 For Weekly R-Tips, Sign Up Here: https://mailchi.mp/business-science/r-tips-newsletter

# LIBRARIES ----
library(tidyverse)

# DATA ----
mpg %>% View()

# SUMMARIZE W/ ACROSS ----
# - Group By + Summarize: Super common summarization pattern
# - Summarize + Across: Scale your summarizations:
#    - Multiple columns
#    - Multiple Summary Functions (e.g. mean, sd)

# 1.0 BASIC USAGE ----

# * AVERAGE CITY FUEL CONSUMPTION BY VEHICLE CLASS ----
mpg %>%
  group_by(class) %>%
  summarise(
    across(cty, .fns = mean),
    .groups = "drop"
  )

# * AVERAGE & STDEV CITY FUEL CONSUMPTION BY VEHICLE CLASS
mpg %>%
  group_by(class) %>%
  summarise(
    across(cty, .fns = list(mean = mean, stdev = sd)), .groups = "drop"
  )

# * AVERAGE & STDEV CITY + HWY FUEL CONSUMPTION BY VEHICLE CLASS WITH LAMBDAS
mpg %>%
  group_by(class) %>%
  summarise(
    across(c(cty, hwy), .fns = list(mean = mean, stdev = sd, max = max, min = min, 
                                    n_miss = ~ sum(is.na(.x)),
                                    "Range LO" = ~ (mean(.x) - 2*sd(.x)),
                                    "Range HI" = ~ (mean(.x) + 2*sd(.x))
                                    )), .groups = "drop"
  )

mpg

# 2.0 ADVANCED ----

# * CUSTOMIZE NAMING SCHEME ----
mpg %>%
  group_by(class) %>%
  summarise(
    across(
      c(cty, hwy),
      .fns = list(mean = mean, stdev = sd),
      .names = "{.fn} {.col} Consumption"
    ),
    .groups = "drop"
  ) %>%
  rename_with(.fn = str_to_upper)

# * COMPLEX FUNCTIONS ----
mpg %>%
  group_by(class) %>%
  summarise(
    across(
      c(cty, hwy),
      .fns = list(
        "mean"     = ~ mean(.x),
        "range lo" = ~ (mean(.x) - 2*sd(.x)),
        "range hi" = ~ (mean(.x) + 2*sd(.x))
      ),
      .names = "{.fn} {.col}"
    ),
    .groups = "drop"
  ) %>%
  rename_with(.fn = str_to_upper)



dist_summary <- function(df, var) {
  df %>%
    summarise(n = n(), min = min({{ var }}), max = max({{ var }}), stdev =  sd({{var}}))
}
mtcars %>% dist_summary(mpg)
mtcars %>% group_by(cyl) %>% dist_summary(mpg)


#System Help - across
iris %>%
  group_by(Species) %>%
  summarise(across(starts_with("Sepal"), mean))


iris %>%
  as_tibble() %>%
  mutate(across(where(is.factor), as.character))

# A purrr-style formula
iris %>%
  group_by(Species) %>%
  summarise(across(starts_with("Sepal"), ~mean(.x, na.rm = TRUE)))

# A named list of functions
iris %>%
  group_by(Species) %>%
  summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd)))

# Use the .names argument to control the output names
iris %>%
  group_by(Species) %>%
  summarise(across(starts_with("Sepal"), mean, .names = "mean_{.col}"))
iris %>%
  group_by(Species) %>%
  summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd), .names = "{.col}.{.fn}"))
iris %>%
  group_by(Species) %>%
  summarise(across(starts_with("Sepal"), list(mean, sd), .names = "{.col}.fn{.fn}"))

# c_across() ---------------------------------------------------------------
df <- tibble(id = 1:4, w = runif(4), x = runif(4), y = runif(4), z = runif(4))
df %>%
  rowwise() %>%
  mutate(
    sum = sum(c_across(w:z)),
    sd = sd(c_across(w:z))
  )





