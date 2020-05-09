R Notebook
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0.9000     ✓ purrr   0.3.3     
    ## ✓ tibble  3.0.1          ✓ dplyr   0.8.4     
    ## ✓ tidyr   1.0.2          ✓ stringr 1.4.0     
    ## ✓ readr   1.3.1          ✓ forcats 0.4.0

    ## Warning: package 'tibble' was built under R version 3.6.2

    ## ── Conflicts ────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(pokedex)
```

``` r
pokemon %>% 
  select(identifier, type_1, type_2)
```

    ## # A tibble: 807 x 3
    ##    identifier type_1 type_2
    ##    <chr>      <chr>  <chr> 
    ##  1 bulbasaur  grass  poison
    ##  2 ivysaur    grass  poison
    ##  3 venusaur   grass  poison
    ##  4 charmander fire   <NA>  
    ##  5 charmeleon fire   <NA>  
    ##  6 charizard  fire   flying
    ##  7 squirtle   water  <NA>  
    ##  8 wartortle  water  <NA>  
    ##  9 blastoise  water  <NA>  
    ## 10 caterpie   bug    <NA>  
    ## # … with 797 more rows

# How many pokemon?

``` r
pokemon %>% 
  summarise(count = n())
```

    ## # A tibble: 1 x 1
    ##   count
    ##   <int>
    ## 1   807

## How many by type?

``` r
pokemon %>%
  mutate(dual_type = case_when(is.na(type_2) ~ TRUE,
                               TRUE ~ FALSE)) %>%
  group_by(dual_type) %>%
  summarise(count = n())
```

    ## # A tibble: 2 x 2
    ##   dual_type count
    ##   <lgl>     <int>
    ## 1 FALSE       405
    ## 2 TRUE        402

``` r
pokemon %>%
  group_by(type_1) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = type_1, y = count)) +
  geom_col() +
  labs(title = "Pokemon by primary type")
```

![](pokedex-intro_files/figure-gfm/primary%20type-1.png)<!-- -->

``` r
pokemon %>% 
  filter(!is.na(type_2)) %>% 
  group_by(type_2) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = type_2, y = count)) +
  geom_col() +
  labs(title = "Pokemon by secondary type",
       caption = "For Pokemon with dual type")
```

![](pokedex-intro_files/figure-gfm/secondary%20type-1.png)<!-- -->

``` r
pokemon %>% 
  select(identifier, type_1, type_2) %>% 
  pivot_longer(-identifier, names_to = "slot", values_to = "type") %>% 
  group_by(type) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(type)) %>% 
  ggplot(aes(x = type, y = count)) +
  geom_col() +
  labs(title = "Pokemon by either type",
       caption = "This will count a dual type pokemon twice,\nonce for each type")
```

![](pokedex-intro_files/figure-gfm/either%20type-1.png)<!-- -->

Type order doesn’t actually matter

``` r
pokemon %>%
  filter((type_1 == "ghost" & type_2 == "fire") |
           (type_1 == "fire" & type_2 == "ghost")) %>% 
  select(identifier, type_1, type_2)
```

    ## # A tibble: 4 x 3
    ##   identifier  type_1 type_2
    ##   <chr>       <chr>  <chr> 
    ## 1 litwick     ghost  fire  
    ## 2 lampent     ghost  fire  
    ## 3 chandelure  ghost  fire  
    ## 4 blacephalon fire   ghost

``` r
pokemon %>%
  mutate(
    type_1_ordered = case_when(is.na(type_2) ~ type_1,
                               type_1 < type_2 ~ type_1,
                               TRUE ~ type_2),
    type_2_ordered = case_when(type_1 > type_2 ~ type_1,
                               TRUE ~ type_2)
  ) -> pokemon
```

``` r
pokemon %>%
  mutate(type_combined = case_when(
    !is.na(type_2_ordered) ~ paste(type_1_ordered, type_2_ordered),
    is.na(type_2_ordered) ~ type_1_ordered
  )) %>%
  group_by(type_combined) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(type_combined = as_factor(type_combined)) %>%
  ggplot(aes(x = type_combined, y = count)) +
  geom_col() +
  labs(title = "Count of Pokemon by dual type",
       caption = "Ordered by count")
```

![](pokedex-intro_files/figure-gfm/count%20dual%20type-1.png)<!-- -->

``` r
pokemon %>% 
  group_by(type_1_ordered, type_2_ordered) %>% 
  filter(!is.na(type_2_ordered)) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = type_1_ordered, y = type_2_ordered, size = count)) +
  geom_point() +
  labs(title = "Coincidence of a particular dual type")
```

![](pokedex-intro_files/figure-gfm/covariance%20by%20type-1.png)<!-- -->

``` r
expand(pokedex$types %>%
         filter(id < 10001),
       pokedex$types %>%
         filter(id < 10001))
```

    ## # A tibble: 18 x 4
    ##       id identifier generation_id damage_class_id
    ##    <dbl> <chr>              <dbl>           <dbl>
    ##  1     1 normal                 1               2
    ##  2     2 fighting               1               2
    ##  3     3 flying                 1               2
    ##  4     4 poison                 1               2
    ##  5     5 ground                 1               2
    ##  6     6 rock                   1               2
    ##  7     7 bug                    1               2
    ##  8     8 ghost                  1               2
    ##  9     9 steel                  2               2
    ## 10    10 fire                   1               3
    ## 11    11 water                  1               3
    ## 12    12 grass                  1               3
    ## 13    13 electric               1               3
    ## 14    14 psychic                1               3
    ## 15    15 ice                    1               3
    ## 16    16 dragon                 1               3
    ## 17    17 dark                   2               3
    ## 18    18 fairy                  6              NA
