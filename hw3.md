hw3
================
Yue Chen
10/6/2020

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## Warning: package 'tibble' was built under R version 3.6.2

    ## Warning: package 'purrr' was built under R version 3.6.2

    ## ── Conflicts ───────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggridges)
library(patchwork)
```

    ## Warning: package 'patchwork' was built under R version 3.6.2

``` r
library(ggplot2)
```

## Problem 1

``` r
library(p8105.datasets)
data("instacart")
```

The dataset contains 1384617 rows and 15 columns. Observations are the
level of items in orders by user. There are user / order variables -
user ID, order ID, order day, and order hour. There are also item
variables - name, aisle, department, and some numeric codes.

How many aisles are there, and which aisles are the most items ordered
from?

``` r
instacart %>%
  count(aisle) %>%
  arrange(desc(n))
```

    ## # A tibble: 134 x 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

There are 134 aisles. The most ordered aisles are fresh vegetables,
fresh fruits, and packaged vegetable fruits.

Make a plot that shows the number of items ordered in each aisle,
limiting this to aisles with more than 10000 items ordered. Arrange
aisles sensibly, and organize your plot so others can read it.

``` r
instacart %>%
  count(aisle) %>%
  filter(n > 10000) %>%
  mutate(
    aisle = factor(aisle),
    aisle = fct_reorder(aisle, n)
  ) %>%
  ggplot(aes(x = aisle, y = n)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

![](hw3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Make a table showing the three most popular items in each of the aisles
“baking ingredients”, “dog food care”, and “packaged vegetables
fruits”. Include the number of times each item is ordered in your
table.

``` r
instacart %>%
  filter(aisle %in% c("baking ingredient", "dog food care", "packaged vegetables fruits")) %>%
  group_by(aisle) %>%
  count(product_name) %>%
  mutate(rank = min_rank(desc(n))) %>%
  filter(rank < 4) %>%
  arrange(aisle, rank) %>%
  knitr::kable()
```

| aisle                      | product\_name                                 |    n | rank |
| :------------------------- | :-------------------------------------------- | ---: | ---: |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |

Make a table showing the mean hour of the day at which Pink Lady Apples
and Coffee Ice Cream are ordered on each day of the week; format this
table for human readers.

``` r
instacart %>%
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>%
  summarize(mean_hour = mean(order_hour_of_day)) %>%
  pivot_wider(
    names_from = order_dow,
    values_from = mean_hour
  ) %>%
  knitr::kable()
```

| product\_name    |        0 |        1 |        2 |        3 |        4 |        5 |        6 |
| :--------------- | -------: | -------: | -------: | -------: | -------: | -------: | -------: |
| Coffee Ice Cream | 13.77419 | 14.31579 | 15.38095 | 15.31818 | 15.21739 | 12.26316 | 13.83333 |
| Pink Lady Apples | 13.44118 | 11.36000 | 11.70213 | 14.25000 | 11.55172 | 12.78431 | 11.93750 |

## Problem 2

``` r
accel_data = read_csv("./accel_data.csv") %>%
  janitor::clean_names() %>%
  mutate(
    weekday = recode(day, 
                     "Monday" = "Weekday", 
                     "Tuesday" = "Weekday", 
                     "Wednesday" = "Weekday", 
                     "Thursday" = "Weekday", 
                     "Friday" = "Weekday", 
                     "Saturday" = "Weekend",
                     "Sunday" = "Weekend")) %>%
  pivot_longer(
    activity_1:activity_1440,
    names_to = "activity",
    names_prefix = "activity_", 
    values_to = "count"
  ) %>%
  mutate(
    week = as.integer(week),
    day_id = as.integer(day_id),
    day = as.factor(day), 
    activity = as.integer(activity),
    count = as.numeric(count),
    weekday = as.logical(weekday)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

In the resulting dataset, there are 50400 observations and the following
variables: week, day\_id, day, weekday, activity, count.

Using your tidied dataset, aggregate accross minutes to create a total
activity variable for each day, and create a table showing these totals.

``` r
accel_data %>%
  group_by(day_id, day, weekday) %>%
  summarise(
    daily_count = sum(count)
  ) %>%
  knitr::kable()
```

| day\_id | day       | weekday | daily\_count |
| ------: | :-------- | :------ | -----------: |
|       1 | Friday    | NA      |    480542.62 |
|       2 | Monday    | NA      |     78828.07 |
|       3 | Saturday  | NA      |    376254.00 |
|       4 | Sunday    | NA      |    631105.00 |
|       5 | Thursday  | NA      |    355923.64 |
|       6 | Tuesday   | NA      |    307094.24 |
|       7 | Wednesday | NA      |    340115.01 |
|       8 | Friday    | NA      |    568839.00 |
|       9 | Monday    | NA      |    295431.00 |
|      10 | Saturday  | NA      |    607175.00 |
|      11 | Sunday    | NA      |    422018.00 |
|      12 | Thursday  | NA      |    474048.00 |
|      13 | Tuesday   | NA      |    423245.00 |
|      14 | Wednesday | NA      |    440962.00 |
|      15 | Friday    | NA      |    467420.00 |
|      16 | Monday    | NA      |    685910.00 |
|      17 | Saturday  | NA      |    382928.00 |
|      18 | Sunday    | NA      |    467052.00 |
|      19 | Thursday  | NA      |    371230.00 |
|      20 | Tuesday   | NA      |    381507.00 |
|      21 | Wednesday | NA      |    468869.00 |
|      22 | Friday    | NA      |    154049.00 |
|      23 | Monday    | NA      |    409450.00 |
|      24 | Saturday  | NA      |      1440.00 |
|      25 | Sunday    | NA      |    260617.00 |
|      26 | Thursday  | NA      |    340291.00 |
|      27 | Tuesday   | NA      |    319568.00 |
|      28 | Wednesday | NA      |    434460.00 |
|      29 | Friday    | NA      |    620860.00 |
|      30 | Monday    | NA      |    389080.00 |
|      31 | Saturday  | NA      |      1440.00 |
|      32 | Sunday    | NA      |    138421.00 |
|      33 | Thursday  | NA      |    549658.00 |
|      34 | Tuesday   | NA      |    367824.00 |
|      35 | Wednesday | NA      |    445366.00 |

It is hard to see a trend from the table.
