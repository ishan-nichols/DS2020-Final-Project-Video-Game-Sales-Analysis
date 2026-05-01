---
title: "My Project"
output:
  html_document:
    keep_md: true
---



## Video Game Sales Analysis

Mason Gliege, Ewan Farnum, Ishan Nichols

# Cleaning

First, we read the dataset and stored it as a dataframe. 

The data had some null sales numbers, so we replaced them with zeros. 

There were also null values for critic scores. Since there is no way to estimate scores on games using the existing data, we removed all rows with no score.

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.1     ✔ stringr   1.6.0
## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.1.0     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
df <- read.csv("video_games_raw.csv")

str(df)
```

```
## 'data.frame':	64016 obs. of  14 variables:
##  $ img         : chr  "/games/boxart/full_6510540AmericaFrontccc.jpg" "/games/boxart/full_5563178AmericaFrontccc.jpg" "/games/boxart/827563ccc.jpg" "/games/boxart/full_9218923AmericaFrontccc.jpg" ...
##  $ title       : chr  "Grand Theft Auto V" "Grand Theft Auto V" "Grand Theft Auto: Vice City" "Grand Theft Auto V" ...
##  $ console     : chr  "PS3" "PS4" "PS2" "X360" ...
##  $ genre       : chr  "Action" "Action" "Action" "Action" ...
##  $ publisher   : chr  "Rockstar Games" "Rockstar Games" "Rockstar Games" "Rockstar Games" ...
##  $ developer   : chr  "Rockstar North" "Rockstar North" "Rockstar North" "Rockstar North" ...
##  $ critic_score: num  9.4 9.7 9.6 NA 8.1 8.7 8.8 9.8 8.4 8 ...
##  $ total_sales : num  20.3 19.4 16.1 15.9 15.1 ...
##  $ na_sales    : num  6.37 6.06 8.41 9.06 6.18 9.07 9.76 5.26 8.27 4.99 ...
##  $ jp_sales    : num  0.99 0.6 0.47 0.06 0.41 0.13 0.11 0.21 0.07 0.65 ...
##  $ pal_sales   : num  9.85 9.71 5.49 5.33 6.05 4.29 3.73 6.21 4.32 5.88 ...
##  $ other_sales : num  3.12 3.02 1.78 1.42 2.44 1.33 1.14 2.26 1.2 2.28 ...
##  $ release_date: chr  "17-09-2013" "18-11-2014" "28-10-2002" "17-09-2013" ...
##  $ last_update : chr  "" "03-01-2018" "" "" ...
```

``` r
View(df)

df_clean <- df %>% replace_na(list(jp_sales = 0, na_sales = 0, pal_sales = 0, other_sales = 0)) %>% drop_na(critic_score) 
```

### Next, we split the games into different categories by sales.

Small Games: under 100k sales
Medium Games: between 100k and 1 million sales
Large Games: between 1 million and 10 million sales
Very_Large_Games: over 10 million sales


``` r
# Filter for games with less than 100k sales
small_games <- df_clean %>% 
  filter(total_sales < 0.1)

# Filter for games between 100k and 1 million sales
medium_games <- df_clean %>% 
  filter(total_sales >= 0.1 & total_sales < 1)

# Filter for games between 1 million and 10 million sales
large_games <- df_clean %>% 
  filter(total_sales >= 1 & total_sales < 10)

# Filter for games with 10 million or more sales
very_large_games <- df_clean %>% 
  filter(total_sales >= 10)
```


