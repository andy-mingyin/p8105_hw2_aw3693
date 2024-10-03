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

## Problem 1

### creates the the dataframe

``` r
nyc_trans_df = 
  read_csv(
    "./data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",  col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c"))  |>
  janitor::clean_names()  
```

### clean the dataframe

``` r
clean_nyc_trans_df = nyc_trans_df |> 
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) |> 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))

trans_row = nrow(clean_nyc_trans_df)
trans_col = ncol(clean_nyc_trans_df)
```

This dataset contains information about subway entrances and exits in
the NYC Transit system, with key variables including the line, station
name, station latitude and longitude, routes served, whether entry is
allowed at a specific entrance, the presence of vending machines,
entrance type, and ADA (Americans with Disabilities Act) compliance.

I selected only relevant columns, such as line, station name, station
latitude/longitude, routes, entry, vending, entrance type, and ADA
compliance. I standardized column names using `janitor::clean_names()`
to ensure consistency. I converted the entry variable from character
format YES or NO to a logical variable TRUE or FALSE to make it easier
to work with by using `ifelse()`.

After the data cleaning process, it is now tidy.

After cleaning, the dataset contains 1868rows and 20 columns.

### creates distinct stations dataframe

``` r
distinct_stations = clean_nyc_trans_df |> 
  distinct(station_name, line) |> 
  count()
```

There are 465 distinct stations.

### ADA compliant stations dataframe

``` r
ada_compliant_stations = clean_nyc_trans_df |> 
  filter(ada == TRUE) |> 
  distinct(station_name, line) |> 
  count()
```

There are 84 ADA compliant stations.

#### proportion of no vending entry

``` r
no_vending_entry_proportion =  
  clean_nyc_trans_df |> 
  filter(vending == "NO") |> 
  pull(entry) |> 
  mean()
```

The proportion of stations without vending that allow entry is
0.3770492.

### Reformat data so that route number and route name are distinct variables.

``` r
A_route = clean_nyc_trans_df |>
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") |> 
  filter(route == "A") |> 
  select(station_name, line) |> 
  distinct() 


A_ada= clean_nyc_trans_df |>
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") |> 
  filter(route == "A" & ada == TRUE) |> 
  select(station_name, line) |> 
  distinct()
```

There are 60 distinct stations serve the A train.

Of the 60 stations that serve the A train, 17 are ADA compliant

## Problem 2

### load and clean mr trash wheel dataset.

``` r
mr_trash_wheel = read_excel("data/202409 Trash Wheel Collection Data.xlsx", sheet = "Mr. Trash Wheel", skip = 1, na = c(".", "NA", "")) |>
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

### load and clean prof trash wheel dataset

``` r
prof_trash_wheel = read_excel("data/202409 Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel", skip = 1, na = c(".", "NA", "")) |>
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

### load and clean Gwynnda trash wheel dataset

``` r
gwynnda_trash_wheel = read_excel("data/202409 Trash Wheel Collection Data.xlsx", sheet = "Gwynnda Trash Wheel", skip = 1,na = c(".", "NA", "")) |>
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

### combine three dataset

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

The combined dataset includes observations from three different Trash
Wheels: Mr. Trash Wheel, Professor Trash Wheel, and Gwynnda. After
importing and cleaning the data from three separate Excel sheets, we
added an additional variable, `source`, to distinguish between the
sources. The dataset contains 1033 rows and 17columns.

The key variables are,:`date`, `weight_tons`,`volume_cubic_yards`,
`plastic_bottles`, `polystyrene`,`cigarette_butts`, `glass_bottles` ,
`plastic_bags`, `wrappers`, `glass_bottles`, `plastic_bags`, `wrappers`
and `sports_balls`. The `weight_tons` shows that the weight of the trash
collected on that specific date. Rest of the key varaibles represent the
amount of the kinds of trash that are collected on the specific date.

### total weight collected by prof trash wheel

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

### Total cigarette butts collected by Gwynnda in June 2022

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

### load the dataset and did some cleaning

``` r
bakers_df = read_csv("./data/bakers.csv", na = c("NA", ".", "", "N/A")) |> 
  janitor::clean_names() |>
  mutate(
    series = as.numeric(series), 
    baker_name = str_to_lower(str_trim(baker_name)), 
     baker =  word(baker_name, 1)
  )
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
    episode = as.numeric(episode), 
    baker = str_to_lower(str_trim(baker)),  
    baker = ifelse(baker == '"jo"', "joanne", baker)  
  )
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
  mutate(
    series = as.numeric(series),
    episode = as.numeric(episode), 
    baker = str_to_lower(str_trim(baker))  
  )
```

    ## Rows: 1136 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): baker, result
    ## dbl (3): series, episode, technical
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bakes_df
```

    ## # A tibble: 548 × 5
    ##    series episode baker     signature_bake                          show_stopper
    ##     <dbl>   <dbl> <chr>     <chr>                                   <chr>       
    ##  1      1       1 annetha   "Light Jamaican Black Cakewith Strawbe… Red, White …
    ##  2      1       1 david     "Chocolate Orange Cake"                 Black Fores…
    ##  3      1       1 edd       "Caramel Cinnamon and Banana Cake"      <NA>        
    ##  4      1       1 jasminder "Fresh Mango and Passion Fruit Humming… <NA>        
    ##  5      1       1 jonathan  "Carrot Cake with Lime and Cream Chees… Three Tiere…
    ##  6      1       1 lea       "Cranberry and Pistachio Cakewith Oran… Raspberries…
    ##  7      1       1 louise    "Carrot and Orange Cake"                Never Fail …
    ##  8      1       1 mark      "Sticky Marmalade Tea Loaf"             Heart-shape…
    ##  9      1       1 miranda   "Triple Layered Brownie Meringue Cake\… Three Tiere…
    ## 10      1       1 ruth      "Three Tiered Lemon Drizzle Cakewith F… Classic Cho…
    ## # ℹ 538 more rows

``` r
results_df
```

    ## # A tibble: 1,136 × 5
    ##    series episode baker     technical result
    ##     <dbl>   <dbl> <chr>         <dbl> <chr> 
    ##  1      1       1 annetha           2 IN    
    ##  2      1       1 david             3 IN    
    ##  3      1       1 edd               1 IN    
    ##  4      1       1 jasminder        NA IN    
    ##  5      1       1 jonathan          9 IN    
    ##  6      1       1 louise           NA IN    
    ##  7      1       1 miranda           8 IN    
    ##  8      1       1 ruth             NA IN    
    ##  9      1       1 lea              10 OUT   
    ## 10      1       1 mark             NA OUT   
    ## # ℹ 1,126 more rows

``` r
bakers_df
```

    ## # A tibble: 120 × 6
    ##    baker_name       series baker_age baker_occupation             hometown baker
    ##    <chr>             <dbl>     <dbl> <chr>                        <chr>    <chr>
    ##  1 ali imdad             4        25 Charity worker               Saltley… ali  
    ##  2 alice fevronia       10        28 Geography teacher            Essex    alice
    ##  3 alvin magallanes      6        37 Nurse                        Brackne… alvin
    ##  4 amelia lebruin       10        24 Fashion designer             Halifax  amel…
    ##  5 andrew smyth          7        25 Aerospace engineer           Derby /… andr…
    ##  6 annetha mills         1        30 Midwife                      Essex    anne…
    ##  7 antony amourdoux      9        30 Banker                       London   anto…
    ##  8 beca lyne-pirkis      4        31 Military Wives' Choir Singer Aldersh… beca 
    ##  9 ben frazer            2        31 Graphic Designer             Northam… ben  
    ## 10 benjamina ebuehi      7        23 Teaching assistant           South L… benj…
    ## # ℹ 110 more rows

I standardize column names by using `janitor::clean_names()`. The
`series` and`episode` columns are converted to numeric to ensure correct
data types for furture analysis. There are first names and last name for
bakers in baker_df, but there are only first names in the other 2
dataframes. Then, I created another column to that only contains first
name so that it will be easier to merge datasets. I found out that Jo is
actually Joanne in bakes_df, since in the original bakes_df, there is a
baker “Jo”, but there is no “Jo” in the result_df. Then, I checked that
“Jo” and Joanne ’s episode and series match in two different data
frames. Then, I change “Jo” to Joanne in the bakes_df.

### use `anti_join()` to compare three datasets

``` r
anti_join(bakes_df, results_df, by = c("series", "episode", "baker"))
```

    ## # A tibble: 0 × 5
    ## # ℹ 5 variables: series <dbl>, episode <dbl>, baker <chr>,
    ## #   signature_bake <chr>, show_stopper <chr>

``` r
anti_join(results_df, bakes_df, by = c("series", "episode", "baker"))
```

    ## # A tibble: 588 × 5
    ##    series episode baker    technical result
    ##     <dbl>   <dbl> <chr>        <dbl> <chr> 
    ##  1      1       2 lea             NA <NA>  
    ##  2      1       2 mark            NA <NA>  
    ##  3      1       3 annetha         NA <NA>  
    ##  4      1       3 lea             NA <NA>  
    ##  5      1       3 louise          NA <NA>  
    ##  6      1       3 mark            NA <NA>  
    ##  7      1       4 annetha         NA <NA>  
    ##  8      1       4 jonathan        NA <NA>  
    ##  9      1       4 lea             NA <NA>  
    ## 10      1       4 louise          NA <NA>  
    ## # ℹ 578 more rows

``` r
anti_join(bakers_df, bakes_df, by = c("baker", "series"))
```

    ## # A tibble: 26 × 6
    ##    baker_name          series baker_age baker_occupation          hometown baker
    ##    <chr>                <dbl>     <dbl> <chr>                     <chr>    <chr>
    ##  1 alice fevronia          10        28 Geography teacher         Essex    alice
    ##  2 amelia lebruin          10        24 Fashion designer          Halifax  amel…
    ##  3 antony amourdoux         9        30 Banker                    London   anto…
    ##  4 briony williams          9        33 Full-time parent          Bristol  brio…
    ##  5 dan beasley-harling      9        36 Full-time parent          London   dan  
    ##  6 dan chambers            10        32 Support worker            Rotherh… dan  
    ##  7 david atherton          10        36 International health adv… Whitby   david
    ##  8 helena garcia           10        40 Online project manager    Leeds    hele…
    ##  9 henry bird              10        20 Student                   Durham   henry
    ## 10 imelda mccarron          9        33 Countryside recreation o… County … imel…
    ## # ℹ 16 more rows

``` r
anti_join(bakes_df, bakers_df, by = c("baker", "series"))
```

    ## # A tibble: 8 × 5
    ##   series episode baker  signature_bake                              show_stopper
    ##    <dbl>   <dbl> <chr>  <chr>                                       <chr>       
    ## 1      2       1 joanne Chocolate Orange CupcakesOrange and Cardam… Chocolate a…
    ## 2      2       2 joanne Caramelised Onion, Gruyere and Thyme Quiche Raspberry a…
    ## 3      2       3 joanne Stromboli flavored with Mozzarella, Ham, a… Unknown     
    ## 4      2       4 joanne Lavender Biscuits                           Blueberry M…
    ## 5      2       5 joanne Salmon and Asparagus Pie                    Apple and R…
    ## 6      2       6 joanne Rum and Raisin Baked Cheesecake             Limoncello …
    ## 7      2       7 joanne Raspberry & Strawberry Mousse Cake          Pain Aux Ra…
    ## 8      2       8 joanne Raspberry and Blueberry Mille Feuille       Mini Victor…

``` r
anti_join(results_df, bakers_df, by = c("series", "baker"))
```

    ## # A tibble: 8 × 5
    ##   series episode baker  technical result    
    ##    <dbl>   <dbl> <chr>      <dbl> <chr>     
    ## 1      2       1 joanne        11 IN        
    ## 2      2       2 joanne        10 IN        
    ## 3      2       3 joanne         1 IN        
    ## 4      2       4 joanne         8 IN        
    ## 5      2       5 joanne         6 IN        
    ## 6      2       6 joanne         1 STAR BAKER
    ## 7      2       7 joanne         3 IN        
    ## 8      2       8 joanne         1 WINNER

``` r
anti_join(bakers_df, results_df, by = c("series","baker"))
```

    ## # A tibble: 1 × 6
    ##   baker_name  series baker_age baker_occupation hometown     baker
    ##   <chr>        <dbl>     <dbl> <chr>            <chr>        <chr>
    ## 1 jo wheatley      2        41 Housewife        Ongar, Essex jo

### merge all three

``` r
final_df = results_df |>
  left_join(bakes_df, by = c("series", "episode", "baker")) |>
  left_join(bakers_df, by = c("series", "baker")) 
final_df
```

    ## # A tibble: 1,136 × 11
    ##    series episode baker  technical result signature_bake show_stopper baker_name
    ##     <dbl>   <dbl> <chr>      <dbl> <chr>  <chr>          <chr>        <chr>     
    ##  1      1       1 annet…         2 IN     "Light Jamaic… Red, White … annetha m…
    ##  2      1       1 david          3 IN     "Chocolate Or… Black Fores… david cha…
    ##  3      1       1 edd            1 IN     "Caramel Cinn… <NA>         edd kimber
    ##  4      1       1 jasmi…        NA IN     "Fresh Mango … <NA>         jasminder…
    ##  5      1       1 jonat…         9 IN     "Carrot Cake … Three Tiere… jonathan …
    ##  6      1       1 louise        NA IN     "Carrot and O… Never Fail … louise br…
    ##  7      1       1 miran…         8 IN     "Triple Layer… Three Tiere… miranda b…
    ##  8      1       1 ruth          NA IN     "Three Tiered… Classic Cho… ruth clem…
    ##  9      1       1 lea           10 OUT    "Cranberry an… Raspberries… lea harris
    ## 10      1       1 mark          NA OUT    "Sticky Marma… Heart-shape… mark whit…
    ## # ℹ 1,126 more rows
    ## # ℹ 3 more variables: baker_age <dbl>, baker_occupation <chr>, hometown <chr>

### export as csv file

``` r
write.csv(final_df, file = "./data/final_df.csv")
```

### star bakers and winners from series 5 to 10

``` r
star_bakers = final_df |> 
  filter(series >= 5 & series <= 10) |> 
  filter(result == "STAR BAKER" | result == "WINNER") |> 
  select(series, episode, baker, result)

star_bakers
```

    ## # A tibble: 60 × 4
    ##    series episode baker   result    
    ##     <dbl>   <dbl> <chr>   <chr>     
    ##  1      5       1 nancy   STAR BAKER
    ##  2      5       2 richard STAR BAKER
    ##  3      5       3 luis    STAR BAKER
    ##  4      5       4 richard STAR BAKER
    ##  5      5       5 kate    STAR BAKER
    ##  6      5       6 chetna  STAR BAKER
    ##  7      5       7 richard STAR BAKER
    ##  8      5       8 richard STAR BAKER
    ##  9      5       9 richard STAR BAKER
    ## 10      5      10 nancy   WINNER    
    ## # ℹ 50 more rows

Findings: From series 5 to 10, the all the winners are from episode 10.

### load and clean the viewership data

``` r
viewers_df = read_csv("./data/viewers.csv", na = c("NA", ".", "", "N/A")) |> 
  janitor::clean_names()
```

    ## Rows: 10 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): Episode, Series 1, Series 2, Series 3, Series 4, Series 5, Series ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tidy_viewers_df = viewers_df |> 
  pivot_longer(
    cols = starts_with("series"),  
    names_to = "season",  
    values_to = "viewers",  
     names_prefix = "series_" 
  ) |> 
  mutate(
    season = as.numeric(season) 
  )

head(tidy_viewers_df, 10)
```

    ## # A tibble: 10 × 3
    ##    episode season viewers
    ##      <dbl>  <dbl>   <dbl>
    ##  1       1      1    2.24
    ##  2       1      2    3.1 
    ##  3       1      3    3.85
    ##  4       1      4    6.6 
    ##  5       1      5    8.51
    ##  6       1      6   11.6 
    ##  7       1      7   13.6 
    ##  8       1      8    9.46
    ##  9       1      9    9.55
    ## 10       1     10    9.62

### find the avg viewers in series 1

``` r
avg_viewers_season_1 <- tidy_viewers_df |> 
  filter(season == 1) |>  
  summarise(avg_viewers = mean(viewers, na.rm = TRUE))
  avg_viewers_season_1
```

    ## # A tibble: 1 × 1
    ##   avg_viewers
    ##         <dbl>
    ## 1        2.77

### find the avg viewers in series 5

``` r
avg_viewers_season_5 <- tidy_viewers_df |> 
  filter(season == 5) |>  
  summarise(avg_viewers = mean(viewers, na.rm = TRUE))
  avg_viewers_season_5
```

    ## # A tibble: 1 × 1
    ##   avg_viewers
    ##         <dbl>
    ## 1        10.0

The average viewership in Season 1 is 2.77 The average viewership in
Season 5 is 10.0393
