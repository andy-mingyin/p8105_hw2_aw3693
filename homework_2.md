Homework 1
================
Mingyin Wang
2024-09-28

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(haven)
```

## Problem 2

load and clean mr trash wheel dataset

``` r
mr_trash_wheel = read_excel("./202409 Trash Wheel Collection Data.xlsx", sheet = "Mr. Trash Wheel", skip = 1) |>
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
    sports_balls = as.integer(round(sports_balls)), 
    source = "Mr. Trash Wheel"
  )
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
mr_trash_wheel
```

    ## # A tibble: 651 × 17
    ##    dumpster month year  date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <chr> <dttm>                    <dbl>              <dbl>
    ##  1        1 May   2014  2014-05-16 00:00:00        4.31                 18
    ##  2        2 May   2014  2014-05-16 00:00:00        2.74                 13
    ##  3        3 May   2014  2014-05-16 00:00:00        3.45                 15
    ##  4        4 May   2014  2014-05-17 00:00:00        3.1                  15
    ##  5        5 May   2014  2014-05-17 00:00:00        4.06                 18
    ##  6        6 May   2014  2014-05-20 00:00:00        2.71                 13
    ##  7        7 May   2014  2014-05-21 00:00:00        1.91                  8
    ##  8        8 May   2014  2014-05-28 00:00:00        3.7                  16
    ##  9        9 June  2014  2014-06-05 00:00:00        2.52                 14
    ## 10       10 June  2014  2014-06-11 00:00:00        3.76                 18
    ## # ℹ 641 more rows
    ## # ℹ 11 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, sports_balls <int>, homes_powered <dbl>, x15 <lgl>,
    ## #   x16 <lgl>, source <chr>

load and clean prof trash wheel dataset

``` r
prof_trash_wheel = read_excel("./202409 Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel", skip = 1) |>
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
   year = as.character(year), 
    source = "Professor Trash Wheel"
  )
```

load and clean Gwynnda trash wheel dataset

``` r
gwynnda_trash_wheel = read_excel("./202409 Trash Wheel Collection Data.xlsx", sheet = "Gwynnda Trash Wheel", skip = 1) |>
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
   year = as.character(year), 
    source = "Gwynnda Trash Wheel"
  )
gwynnda_trash_wheel
```

    ## # A tibble: 263 × 13
    ##    dumpster month  year  date                weight_tons volume_cubic_yards
    ##       <dbl> <chr>  <chr> <dttm>                    <dbl>              <dbl>
    ##  1        1 July   2021  2021-07-03 00:00:00        0.93                 15
    ##  2        2 July   2021  2021-07-07 00:00:00        2.26                 15
    ##  3        3 July   2021  2021-07-07 00:00:00        1.62                 15
    ##  4        4 July   2021  2021-07-16 00:00:00        1.76                 15
    ##  5        5 July   2021  2021-07-30 00:00:00        1.53                 15
    ##  6        6 August 2021  2021-08-11 00:00:00        2.06                 15
    ##  7        7 August 2021  2021-08-14 00:00:00        1.9                  15
    ##  8        8 August 2021  2021-08-16 00:00:00        2.16                 15
    ##  9        9 August 2021  2021-08-16 00:00:00        2.6                  15
    ## 10       10 August 2021  2021-08-17 00:00:00        3.21                 15
    ## # ℹ 253 more rows
    ## # ℹ 7 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, plastic_bags <dbl>, wrappers <dbl>,
    ## #   homes_powered <dbl>, source <chr>

combine three dataset

``` r
trash_wheel_data = 
  bind_rows(mr_trash_wheel, prof_trash_wheel, gwynnda_trash_wheel) |> 
  relocate(source)
trash_wheel_data
```

    ## # A tibble: 1,033 × 17
    ##    source          dumpster month year  date                weight_tons
    ##    <chr>              <dbl> <chr> <chr> <dttm>                    <dbl>
    ##  1 Mr. Trash Wheel        1 May   2014  2014-05-16 00:00:00        4.31
    ##  2 Mr. Trash Wheel        2 May   2014  2014-05-16 00:00:00        2.74
    ##  3 Mr. Trash Wheel        3 May   2014  2014-05-16 00:00:00        3.45
    ##  4 Mr. Trash Wheel        4 May   2014  2014-05-17 00:00:00        3.1 
    ##  5 Mr. Trash Wheel        5 May   2014  2014-05-17 00:00:00        4.06
    ##  6 Mr. Trash Wheel        6 May   2014  2014-05-20 00:00:00        2.71
    ##  7 Mr. Trash Wheel        7 May   2014  2014-05-21 00:00:00        1.91
    ##  8 Mr. Trash Wheel        8 May   2014  2014-05-28 00:00:00        3.7 
    ##  9 Mr. Trash Wheel        9 June  2014  2014-06-05 00:00:00        2.52
    ## 10 Mr. Trash Wheel       10 June  2014  2014-06-11 00:00:00        3.76
    ## # ℹ 1,023 more rows
    ## # ℹ 11 more variables: volume_cubic_yards <dbl>, plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   plastic_bags <dbl>, wrappers <dbl>, sports_balls <int>,
    ## #   homes_powered <dbl>, x15 <lgl>, x16 <lgl>

total weight collected by prof trash wheel

``` r
total_weight_pro = 
  trash_wheel_data |>
  filter(source == "Professor Trash Wheel") |> 
  summarize(total_weight = sum(weight_tons, na.rm = TRUE))
total_weight_pro
```

    ## # A tibble: 1 × 1
    ##   total_weight
    ##          <dbl>
    ## 1         247.

The total weight collected by prof trash wheel is `total_weight_pro`.

Total cigarette butts collected by Gwynnda in June 2022

``` r
tot_butts_june_2022=
   trash_wheel_data |> 
    filter(source == "Gwynnda Trash Wheel",month =="June", year == "2022") |> 
  summarize(total_weight = sum(weight_tons, na.rm = TRUE))

tot_butts_june_2022
```

    ## # A tibble: 1 × 1
    ##   total_weight
    ##          <dbl>
    ## 1         40.5

The total cigarette butts collected by Gwynnda in June 2022 is
`tot_butts_june_2022` tons.
