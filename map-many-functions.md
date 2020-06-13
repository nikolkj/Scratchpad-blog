---
output: 
  html_document:
    keep_md: yes
---

# Bodging: 'map()' many functions 

Let me get you in on a little secret: `purrr::map*()` functions are __*weird*__.

Generally, I am a firm`*apply()` family camp member, but *new year, new me?*

Here is my general impression of `purrr::map*()` ...

* Mapping a single field on to function is tame and straight forward.
* Things get spicy when you map  *many* fields into a function.
* Finally, doing those things with nested data-frames is just a convoluted mess. 

But, what if you want to take things up a notch and map various fields onto *many* functions in a single pass?
What if the output of those functions have different lengths and object types? 

I found some how-to's [here](https://www.brodrigues.co/blog/2018-01-19-mapping_functions_with_any_cols/) and [here](https://stackoverflow.com/questions/47415072/pass-multiple-functions-to-purrrmap/) but they make my head spin. 

I think things can be a little more intuitive if you deviate from the standard tidyverse map-workflow and leverage the under-appreciated "exposition pipe" operator (`%$%`). As an aside, I was introduced to `%$%` by the [thewoodpeckr blog](https://thewoodpeckr.wordpress.com/2020/02/10/upping-your-pipe-game/).

Effectively, `%$%` allows you to pass data-frame/tibble fields as vectors with tidyverse-style referencing. It is especially useful when using older or more *academic* packages, whose functions weren't built to accept tidyverse's unique referencing style.\
See previous thewoodpeckr link more information & examples.

## The good stuff 

**But first, some house-keeping.**


```r
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))

# IMPORTANT NOTE: 'magrittr' needs to be explicitly imported.
# ... It is technically included as part of the 'tidyverse' mega-package
# ... but for whatever reason only the '%>%' is included as part of that load.
# ... The explicit import will give you access to the more interesting operators like '%<>%', '%$%', etc. 

set.seed(124)
```


<br>Now, let's say you have a simple tibble called "temp" with two fields denoting a classification and a measure.</br>

```r
temp = tibble(
  class_var = sample(
    x = c("a", "b", "c"),
    size = 100,
    replace = TRUE
  ),
  num_var = rnorm(n = 100, mean = 100, sd = 20)
)

# Let's have a small preview ...
temp
```

```
## # A tibble: 100 x 2
##    class_var num_var
##    <chr>       <dbl>
##  1 a            90.2
##  2 c            93.2
##  3 b            58.4
##  4 c           139. 
##  5 a           107. 
##  6 b            93.6
##  7 a           105. 
##  8 b            74.8
##  9 a           108. 
## 10 c           126. 
## # … with 90 more rows
```

<br>Great, now we can group by the classification field, "class_var", and make a nested tibble.</br>
Our non-grouping data will pushed into a sub-tibble called "data". 

```r
temp = temp %>% 
  group_by(class_var) %>% 
  nest() 

# Now, our tibble looks something like this ...
# A good-'ol list of lists type of construction 

temp
```

```
## # A tibble: 3 x 2
## # Groups:   class_var [3]
##   class_var data             
##   <chr>     <list>           
## 1 a         <tibble [33 × 1]>
## 2 c         <tibble [29 × 1]>
## 3 b         <tibble [38 × 1]>
```

```r
temp$data[[1]] # Let's peal the onion
```

```
## # A tibble: 33 x 1
##    num_var
##      <dbl>
##  1    90.2
##  2   107. 
##  3   105. 
##  4   108. 
##  5   109. 
##  6    78.7
##  7    82.0
##  8    95.8
##  9   131. 
## 10   102. 
## # … with 23 more rows
```

### Tidyverse way

Let's say you now want to compute some summary statistics over the "num_var" measure; the standard way would look something like this:


```r
temp %>%
  mutate(
    stat1 = map_dbl(.x = data, .f = ~{mean(.x$num_var)}),
    stat2 = map_dbl(.x = data, .f = ~{sd(.x$num_var)}),
    stat3 = map_dbl(.x = data, .f = ~{median(.x$num_var)}),
    stat4 = map_dbl(.x = data, .f = ~{IQR(.x$num_var)})
    )
```

```
## # A tibble: 3 x 6
## # Groups:   class_var [3]
##   class_var data              stat1 stat2 stat3 stat4
##   <chr>     <list>            <dbl> <dbl> <dbl> <dbl>
## 1 a         <tibble [33 × 1]>  103.  16.7 105.   20.8
## 2 c         <tibble [29 × 1]>  105.  20.7 104.   31.3
## 3 b         <tibble [38 × 1]>  100.  20.0  99.5  22.5
```

You can call me a heathen, but I don't think that function notation is intuitive at all - sacrificing general readability for less keystrokes. 
Sure, it might be fine once you're used to coding in that `map*()`-style, but other R user will struggle if they don't specialize in the purrr package.

### [Bodge way](https://www.youtube.com/watch?v=lIFE7h3m40U)

Alternatively, you could follow the hacky workflow that goes a little like this: `df %$% list(func1, func2 ... funcN)`. Here we take each nested tibble, referenced as "df", and pipe in selected field-vectors as the function arguments using `%$%`. This renders the output of each function is a list item, where the function returns are not constrained by length or object-type.  

Going back to our previous example:


```r
temp2 = temp %>%
  mutate(var_stats = map(
    .x = data,
    .f = function(df) {
      df %$%
        list(
          "stat1" = mean(num_var),
          "stat2" = sd(num_var),
          "stat3" = median(num_var),
          "stat4" = IQR(num_var)
        )
    }
  ))

temp2 # Peak 
```

```
## # A tibble: 3 x 3
## # Groups:   class_var [3]
##   class_var data              var_stats       
##   <chr>     <list>            <list>          
## 1 a         <tibble [33 × 1]> <named list [4]>
## 2 c         <tibble [29 × 1]> <named list [4]>
## 3 b         <tibble [38 × 1]> <named list [4]>
```

```r
temp2$var_stats[[1]] # n'Peal
```

```
## $stat1
## [1] 102.5266
## 
## $stat2
## [1] 16.67112
## 
## $stat3
## [1] 105.2789
## 
## $stat4
## [1] 20.80416
```

<br>If the output dimensions are consistent between our functions and the output list is named, we can use `dplyr::bind_col()` to repackage the output as another set of nested tibbles.</br>

```r
temp2 = temp %>%
  mutate(var_stats = map(
    .x = data,
    .f = function(df) {
      df %$%
        list(
          "stat1" = mean(num_var),
          "stat2" = sd(num_var),
          "stat3" = median(num_var),
          "stat4" = IQR(num_var)
        ) %>%
        bind_cols()
    }
  ))

temp2 # Peak 
```

```
## # A tibble: 3 x 3
## # Groups:   class_var [3]
##   class_var data              var_stats       
##   <chr>     <list>            <list>          
## 1 a         <tibble [33 × 1]> <tibble [1 × 4]>
## 2 c         <tibble [29 × 1]> <tibble [1 × 4]>
## 3 b         <tibble [38 × 1]> <tibble [1 × 4]>
```

```r
temp2$var_stats[[1]] # n'Peal
```

```
## # A tibble: 1 x 4
##   stat1 stat2 stat3 stat4
##   <dbl> <dbl> <dbl> <dbl>
## 1  103.  16.7  105.  20.8
```

<br> With a little extra effort, we can even replicate the "Tidyverse way" output while maximizing readability.</br>

```r
temp2 = temp2 %>%
  select(class_var, data) %>%
  bind_cols(., bind_rows(temp2$var_stats))

temp2 # How about that ...
```

```
## # A tibble: 3 x 6
## # Groups:   class_var [3]
##   class_var data              stat1 stat2 stat3 stat4
##   <chr>     <list>            <dbl> <dbl> <dbl> <dbl>
## 1 a         <tibble [33 × 1]>  103.  16.7 105.   20.8
## 2 c         <tibble [29 × 1]>  105.  20.7 104.   31.3
## 3 b         <tibble [38 × 1]>  100.  20.0  99.5  22.5
```


### Bodge with many inputs & many functions

Bodging is nice because it lends itself to an expanded scope.\
It's not hard to accommodate to mapping many functions with varying inputs without have to mess with `pmap()` or other niche weirdness. 

Here, we'll replicate to the previous example but our tibble will have multiple "num_var" fields. Additionally, we will be mapping to two separate user-defined functions with different argument requirements.   


```r
# A bigger [temp]
temp = tibble(class_var = sample(x = c("a","b","c"), size = 100, replace= TRUE), 
              num_var1 = rnorm(n = 100, mean = 100, sd = 20),
              num_var2 = rnorm(n = 100, mean = 50, sd = 10),
              num_var3 = rnorm(n = 100, mean = 30, sd = 5)
              )

temp # Peak
```

```
## # A tibble: 100 x 4
##    class_var num_var1 num_var2 num_var3
##    <chr>        <dbl>    <dbl>    <dbl>
##  1 c             71.8     57.3     28.1
##  2 b            102.      36.2     27.3
##  3 a             64.5     29.5     38.0
##  4 b             95.8     52.4     21.8
##  5 c             93.9     43.5     32.7
##  6 a             87.8     33.3     31.7
##  7 c             97.2     50.8     23.5
##  8 b            102.      39.3     23.1
##  9 a            115.      54.8     30.5
## 10 a            145.      63.7     28.9
## # … with 90 more rows
```

```r
# Some useless functions
foo1 = function(a,b,c) {
  d = a - sqrt(b) + c
  return(d)
}

foo2 = function(a,b){
  c = a + 2*b
  return(c)
}
```

<br>Again, the name of the game is readability and explicitly defining what goes where without any implied purrr-semantics.</br>

```r
temp2 = temp %>% 
  group_by(class_var) %>% 
  nest() %>% 
  mutate(foo_out = map(
    .x = data,
    .f = function(df) {
      df %$% 
        list("foo1_results" = foo1(a = num_var1, b = num_var2, c = num_var3), 
             "foo2_results" = foo2(a = num_var1, b = num_var3)
             ) %>% 
        bind_cols()
        
    }
  ))

temp2 # Peak
```

```
## # A tibble: 3 x 3
## # Groups:   class_var [3]
##   class_var data              foo_out          
##   <chr>     <list>            <list>           
## 1 c         <tibble [31 × 3]> <tibble [31 × 2]>
## 2 b         <tibble [29 × 3]> <tibble [29 × 2]>
## 3 a         <tibble [40 × 3]> <tibble [40 × 2]>
```

```r
temp2$foo_out[[1]] # n'Peal
```

```
## # A tibble: 31 x 2
##    foo1_results foo2_results
##           <dbl>        <dbl>
##  1         92.4         128.
##  2        120.          159.
##  3        114.          144.
##  4        140.          184.
##  5         92.4         129.
##  6        117.          152.
##  7        116.          163.
##  8        123.          159.
##  9        118.          157.
## 10        164.          205.
## # … with 21 more rows
```

### Some closing notes
Is this all massive work-around because I don't get `map()` beyond the basics?\
Yeah, pretty much - but it goes a long way to reducing the [tidy-models](https://www.tidymodels.org/) learning curve since you live and breath nested tibbles in that framework. Really hoping you found this helpful.

If you are going to use this approach, here are some things to note:

* You are limited to the `map()` function since the output needs to be a list object.\
  + `map_chr()`, `map_lgl()`, `map_dbl()`, etc. are dead to you. 
  
* Again, `%$%` is essential, trying to use standard `%>%` operator will just throw errors. 

* Your `map()` output list must be named if you want to use `dplyr::bind_cols()`, etc. to simplify returned objects. 


<br></br>
<br></br>
<br></br>
<br></br>
<br></br>
