data_manipulation
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## PULSE data

``` r
pulse_df =
  haven::read_sas("data/public_pulse_data.sas7bdat") |>
  janitor::clean_names() |>
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    values_to = "bdi_score",
    names_prefix = "bdi_score_"
  ) |>
  mutate(
    visit = replace(visit, visit == "bl", "00m"),
    visit = factor(visit)
  )

print(pulse_df, n =12)
```

    ## # A tibble: 4,348 × 5
    ##       id   age sex   visit bdi_score
    ##    <dbl> <dbl> <chr> <fct>     <dbl>
    ##  1 10003  48.0 male  00m           7
    ##  2 10003  48.0 male  01m           1
    ##  3 10003  48.0 male  06m           2
    ##  4 10003  48.0 male  12m           0
    ##  5 10015  72.5 male  00m           6
    ##  6 10015  72.5 male  01m          NA
    ##  7 10015  72.5 male  06m          NA
    ##  8 10015  72.5 male  12m          NA
    ##  9 10022  58.5 male  00m          14
    ## 10 10022  58.5 male  01m           3
    ## 11 10022  58.5 male  06m           8
    ## 12 10022  58.5 male  12m          NA
    ## # ℹ 4,336 more rows

Learning Assessment

``` r
litters_df =
  read_csv("data/FAS_litters.csv") |>
  janitor::clean_names() |>
  select(litter_number, ends_with("weight")) |>
  pivot_longer(
    gd0_weight:gd18_weight,
    names_to = "gd",
    values_to = "weight") |>
  mutate(
    gd = case_match(
      gd,
      "gd0_weight" ~ 0,
      "gd18_weight" ~ 18,
      ))
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## LoTR

Import LoTR words data

``` r
fellowship_df =
  readxl::read_excel("data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship")

two_towers_df =
  readxl::read_excel("data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two towers")

return_of_the_king_df =
  readxl::read_excel("data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "return of the king")


lotr_df = 
  bind_rows(fellowship_df, two_towers_df, return_of_the_king_df) |>
  janitor::clean_names() |>
  pivot_longer(
    male:female,
    names_to = "gender",
    values_to = "word"
    ) |>
  relocate(movie)
```

## Revisit FAS

``` r
litters_df =
  read_csv("data/FAS_litters.csv") |>
  janitor::clean_names() |>
  mutate(wt_gain = gd18_weight - gd0_weight) |>
  select(litter_number, group, wt_gain) |>
  separate(group, into = c("dose", "day_of_tx"), 3)
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df =
  read_csv("data/FAS_pups.csv") |>
  janitor::clean_names() |>
  mutate(
    sex = case_match(
      sex,
      1 ~ "male",
      2 ~ "female"
    )
  )
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fas_df =
  left_join(pups_df, litters_df, by = "litter_number")
```
