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

There were also null values for critic scores. Since there is no way to estimate scores on games using the existing data, we removed all rows with no score. Additionally, there were null values in total sales so those were also removed

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.2.0     ✔ readr     2.2.0
## ✔ forcats   1.0.1     ✔ stringr   1.6.0
## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.1     
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
df_clean <- df %>%
 mutate(
    jp_sales = replace_na(jp_sales, 0),
    na_sales = replace_na(na_sales, 0),
    pal_sales = replace_na(pal_sales, 0),
    other_sales = replace_na(other_sales, 0)
  ) %>%
  drop_na(critic_score, total_sales)
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

### Top 10 Best Selling Games

``` r
top_games <- df_clean %>%
  arrange(desc(total_sales)) %>%
  select(title, developer,  console, release_date, total_sales) %>%
  head(10)

top_games
```

```
##                             title      developer console release_date
## 1              Grand Theft Auto V Rockstar North     PS3   17-09-2013
## 2              Grand Theft Auto V Rockstar North     PS4   18-11-2014
## 3     Grand Theft Auto: Vice City Rockstar North     PS2   28-10-2002
## 4       Call of Duty: Black Ops 3       Treyarch     PS4   06-11-2015
## 5  Call of Duty: Modern Warfare 3  Infinity Ward    X360   08-11-2011
## 6         Call of Duty: Black Ops       Treyarch    X360   09-11-2010
## 7           Red Dead Redemption 2 Rockstar Games     PS4   26-10-2018
## 8      Call of Duty: Black Ops II       Treyarch    X360   13-11-2012
## 9      Call of Duty: Black Ops II       Treyarch     PS3   13-11-2012
## 10 Call of Duty: Modern Warfare 2  Infinity Ward    X360   10-11-2009
##    total_sales
## 1        20.32
## 2        19.39
## 3        16.15
## 4        15.09
## 5        14.82
## 6        14.74
## 7        13.94
## 8        13.86
## 9        13.80
## 10       13.53
```

### Critic Score vs Sales

``` r
ggplot(df_clean, aes(x = critic_score, y = total_sales)) +
  geom_point(alpha =0.3)+
   labs(
    title = "Critic Score vs. Total Sales",
    x = "Critic Score",
    y = "Total Sales (millions)"
  ) 
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Sales by Genre

``` r
genre_summary <-df_clean %>%
  group_by(genre) %>%
  summarise(
    avg_sales = mean(total_sales),
    total_sales = sum(total_sales),
    count = n()
  ) %>%
  arrange(desc(avg_sales))

ggplot(genre_summary, aes(x = reorder(genre, avg_sales), y = avg_sales, fill = avg_sales))+
  geom_col()+
  coord_flip()+
  labs(
    title = "Average Sales by Genre",
    x = "Genre",
    y = "Average Total Sales (millions)"
  ) 
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Sales by Console

``` r
console_summary <- df_clean %>%
  group_by(console) %>%
  summarise(
    total_sales = sum(total_sales),
    avg_sales = mean(total_sales),
    count = n()
  )%>%
  filter(count >= 20) %>%
  arrange(desc(total_sales))

ggplot(console_summary, aes(x = reorder(console, total_sales), y = total_sales, fill = avg_sales)) +
  geom_col() +
  coord_flip() +
   labs(
    title = "Consoles by Total Sales",
    x = "Console",
    y = "Total Sales (millions)"
  ) 
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->





