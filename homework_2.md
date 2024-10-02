Homework 1
================
Mingyin Wang
2024-09-28

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

## Problem 2

load and clean mr trash wheel dataset

``` r
mr_trash_wheel = read_excel("data/202409 Trash Wheel Collection Data.xlsx", sheet = "Mr. Trash Wheel", skip = 1) |>
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
prof_trash_wheel = read_excel("data/202409 Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel", skip = 1) |>
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
   year = as.character(year), 
    source = "Professor Trash Wheel"
  )
prof_trash_wheel
```

    ## # A tibble: 119 × 14
    ##    dumpster month    year  date                weight_tons volume_cubic_yards
    ##       <dbl> <chr>    <chr> <dttm>                    <dbl>              <dbl>
    ##  1        1 January  2017  2017-01-02 00:00:00        1.79                 15
    ##  2        2 January  2017  2017-01-30 00:00:00        1.58                 15
    ##  3        3 February 2017  2017-02-26 00:00:00        2.32                 18
    ##  4        4 February 2017  2017-02-26 00:00:00        3.72                 15
    ##  5        5 February 2017  2017-02-28 00:00:00        1.45                 15
    ##  6        6 March    2017  2017-03-30 00:00:00        1.71                 15
    ##  7        7 April    2017  2017-04-01 00:00:00        1.82                 15
    ##  8        8 April    2017  2017-04-20 00:00:00        2.37                 15
    ##  9        9 May      2017  2017-05-10 00:00:00        2.64                 15
    ## 10       10 May      2017  2017-05-26 00:00:00        2.78                 15
    ## # ℹ 109 more rows
    ## # ℹ 8 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, homes_powered <dbl>, source <chr>

load and clean Gwynnda trash wheel dataset

``` r
gwynnda_trash_wheel = read_excel("data/202409 Trash Wheel Collection Data.xlsx", sheet = "Gwynnda Trash Wheel", skip = 1) |>
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

The total weight collected by prof trash wheel is 246.74 tons.

Total cigarette butts collected by Gwynnda in June 2022

``` r
tot_butts_june_2022=
   trash_wheel_data |> 
    filter(source == "Gwynnda Trash Wheel", month =="June", year == "2022") |> 
  summarize(total_weight = sum(weight_tons, na.rm = TRUE))

tot_butts_june_2022
```

    ## # A tibble: 1 × 1
    ##   total_weight
    ##          <dbl>
    ## 1         40.5

The total cigarette butts collected by Gwynnda in June 2022 is 40.53
tons.

## Problem 3

load the dataset and did some cleaning

``` r
bakers_df = read_csv("./data/bakers.csv", na = c("NA", ".", "", "N/A")) |> 
  janitor::clean_names() |>

 mutate(
    series = as.numeric(series))
```

    ## Rows: 120 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker Name, Baker Occupation, Hometown
    ## dbl (2): Series, Baker Age
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bakes_df = read_csv("./data/bakes.csv", na = c("NA", ".", "", "N/A")) |> 
  janitor::clean_names() |>
  mutate(
    series = as.numeric(series),
    episode = as.numeric(episode))
```

    ## Rows: 548 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker, Signature Bake, Show Stopper
    ## dbl (2): Series, Episode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
results_df = read_csv("./data/results.csv", na = c("NA", ".", "", "N/A"), skip = 2) |> 
  janitor::clean_names() |>
  
  mutate(series = as.numeric(series),
         episode = as.numeric(episode))
```

    ## Rows: 1136 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): baker, result
    ## dbl (3): series, episode, technical
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

use `anti_join()` to compare two datasets

``` r
anti_join(bakes_df, results_df, by = c("series", "episode", "baker"))
```

    ## # A tibble: 8 × 5
    ##   series episode baker    signature_bake                            show_stopper
    ##    <dbl>   <dbl> <chr>    <chr>                                     <chr>       
    ## 1      2       1 "\"Jo\"" Chocolate Orange CupcakesOrange and Card… Chocolate a…
    ## 2      2       2 "\"Jo\"" Caramelised Onion, Gruyere and Thyme Qui… Raspberry a…
    ## 3      2       3 "\"Jo\"" Stromboli flavored with Mozzarella, Ham,… Unknown     
    ## 4      2       4 "\"Jo\"" Lavender Biscuits                         Blueberry M…
    ## 5      2       5 "\"Jo\"" Salmon and Asparagus Pie                  Apple and R…
    ## 6      2       6 "\"Jo\"" Rum and Raisin Baked Cheesecake           Limoncello …
    ## 7      2       7 "\"Jo\"" Raspberry & Strawberry Mousse Cake        Pain Aux Ra…
    ## 8      2       8 "\"Jo\"" Raspberry and Blueberry Mille Feuille     Mini Victor…

``` r
anti_join(results_df, bakes_df, by = c("series", "episode", "baker"))
```

    ## # A tibble: 596 × 5
    ##    series episode baker    technical result
    ##     <dbl>   <dbl> <chr>        <dbl> <chr> 
    ##  1      1       2 Lea             NA <NA>  
    ##  2      1       2 Mark            NA <NA>  
    ##  3      1       3 Annetha         NA <NA>  
    ##  4      1       3 Lea             NA <NA>  
    ##  5      1       3 Louise          NA <NA>  
    ##  6      1       3 Mark            NA <NA>  
    ##  7      1       4 Annetha         NA <NA>  
    ##  8      1       4 Jonathan        NA <NA>  
    ##  9      1       4 Lea             NA <NA>  
    ## 10      1       4 Louise          NA <NA>  
    ## # ℹ 586 more rows

merge all three
