mda-Milestone-2
================

``` r
#loads needed packages to execute the code in this file and supresses startup messages
suppressPackageStartupMessages(library(datateachr)) #allows for access to the datasets in this file.
suppressPackageStartupMessages(library(tidyverse)) #loads packages needed for data visualization (ggplot2), and data manipulation (dplyr) that were used in this file.
suppressPackageStartupMessages(library(psych)) #loads package needed to perform geometric mean in a simple and easy way
```

# Task 1

### Task 1.1

##### Research Questions:

1.  What correlation exists (if any) between the symmetry of a tumor and
    texture of a tumor?\*
2.  Is there a correlation between tumor diagnosis and tumor
    smoothness?\*\*
3.  Is there a correlation between the area and perimeter of a tumor in
    regards to tumor diagnosis?\*
4.  Is there a correlation between the tumor’s fractal dimension and
    tumor diagnosis?\*\*

Changes from last milestone:

\*Wording of question was changed to ask about correlation instead or
relationship between variables

\*\*Research Question was changed to be less broad

### Task 1.2

#### Research Question 1

###### Summarizing (option 1)

``` r
#selects cancer_sample dataset
cancer_sample %>%
  
#groups data based on the diagnosis variable
  group_by(diagnosis) %>%
  
#provides mean, range, median, and count for symmetry_mean
  summarise(symmetry_geometric_mean = geometric.mean(symmetry_mean, na.rm = TRUE), symmetry_range = paste("(", min(symmetry_mean, na.rm=T), "-", max(symmetry_mean,na.rm=T), ")"), symmetry_median = median(symmetry_mean, na.rm = TRUE), symmetry_count = n())
```

    ## # A tibble: 2 × 5
    ##   diagnosis symmetry_geometric_… symmetry_range   symmetry_median symmetry_count
    ##   <chr>                    <dbl> <chr>                      <dbl>          <int>
    ## 1 B                        0.172 ( 0.106 - 0.274…           0.171            357
    ## 2 M                        0.191 ( 0.1308 - 0.30…           0.190            212

This task would help by allowing for a comparison of the mean values
between the two variables in questions. However, only one variable was
summarized here since the question only asked for one. Therefore, an
additional summarising steo would have to be done to compare these
summary statistics between the two

###### Graphing (option 7)

``` r
#selects cancer_sample dataset and produces a graph with symmetry_mean on the x-axis and texture_mean on the y-axis.
cancer_sample %>%
  ggplot(aes(symmetry_mean,texture_mean)) +
  
#plots the points for the above two variables with an alpha of 0.2 to allow for visualization of overlapping data points. A colour aesthetic was also used to show the diagnosis of each point.
  geom_point(alpha = 0.2) +
 
#sets the origin of the graph to 0,0 and ensures all points are included in the plot.
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(cancer_sample$symmetry_mean)+0.05)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(cancer_sample$texture_mean)+5)) +
  
#adds a linear trend line
  stat_smooth(method = 'lm', aes(colour = "red"), se = FALSE, fullrange=TRUE) +
  
#removes legend produced from trend line 
  theme(legend.position="none")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](mda-Milestone-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This task helps to see a potential correlation between the two variables
in question. The addition of the linear trend line helps to visualize
this potential correlation

#### Research Question 2

###### Summarizing (option 1)

``` r
#selects cancer_sample dataset
cancer_sample %>%
  
#groups data based on the diagnosis variable
  group_by(diagnosis) %>%
   
#provides mean, range, median, and count for smoothness_mean
  summarise(overall_smoothness_mean = mean(smoothness_mean, na.rm = TRUE), smoothness_range = paste("(", min(smoothness_mean, na.rm=T), "-", max(smoothness_mean,na.rm=T), ")"), smoothness_median = median(smoothness_mean, na.rm = TRUE), smoothness_count = n())
```

    ## # A tibble: 2 × 5
    ##   diagnosis overall_smoothne… smoothness_range smoothness_medi… smoothness_count
    ##   <chr>                 <dbl> <chr>                       <dbl>            <int>
    ## 1 B                    0.0925 ( 0.05263 - 0.1…           0.0908              357
    ## 2 M                    0.103  ( 0.07371 - 0.1…           0.102               212

This may be able to help answer the research question as it provides
summary statistics for this particular variable for the two possible
tumor diagnoses. so you can see potential differences in values. A graph
would work better in representing this data.

###### Graphing (option 7)

``` r
#selects cancer_sample dataset and produces a graph with diagnosis on the x-axis and smoothness_mean on the y-axis.
cancer_sample %>%
  ggplot(aes(diagnosis,smoothness_mean)) +
  
#produces a boxplot and makes the width of the boxplot smaller.
  geom_boxplot(width = 0.5) +
  
#adds the individuals data points, makes the points' transparency lower and smaller, and shifts the points horizontally while keeping the vertical position the same.
  geom_jitter(width = 0.2, size = 0.3, alpha = 0.2) +
  
#makes the y-axis start ar origin, and makes sure all points are within the graph  
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(cancer_sample$smoothness_mean)+0.03)) 
```

![](mda-Milestone-2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

This graph does help answer the research question as it shows that tumor
smoothness is quite similar as seen by the partial over lap of the bar
graph

#### Research Question 3

###### Summarizing (option 3)

``` r
#selects cancer_sample dataset
cancer_sample %>%
  
#groups data based on the diagnosis variable
  group_by(diagnosis) %>%
  
#produces an additional variable with 3 possible categories as the observations based on area_mean
  mutate(relative_tumor_size = case_when(area_mean < 650 ~ "small",
                                 area_mean < 1000 ~ "medium",
                                 TRUE ~ "large")) %>%
  
#only provides a tibble with the desired varibles
  select(ID,diagnosis,area_mean,relative_tumor_size,radius_mean:perimeter_mean,smoothness_mean:fractal_dimension_mean)
```

    ## # A tibble: 569 × 13
    ## # Groups:   diagnosis [2]
    ##          ID diagnosis area_mean relative_tumor_size radius_mean texture_mean
    ##       <dbl> <chr>         <dbl> <chr>                     <dbl>        <dbl>
    ##  1   842302 M             1001  large                      18.0         10.4
    ##  2   842517 M             1326  large                      20.6         17.8
    ##  3 84300903 M             1203  large                      19.7         21.2
    ##  4 84348301 M              386. small                      11.4         20.4
    ##  5 84358402 M             1297  large                      20.3         14.3
    ##  6   843786 M              477. small                      12.4         15.7
    ##  7   844359 M             1040  large                      18.2         20.0
    ##  8 84458202 M              578. small                      13.7         20.8
    ##  9   844981 M              520. small                      13           21.8
    ## 10 84501001 M              476. small                      12.5         24.0
    ## # … with 559 more rows, and 7 more variables: perimeter_mean <dbl>,
    ## #   smoothness_mean <dbl>, compactness_mean <dbl>, concavity_mean <dbl>,
    ## #   concave_points_mean <dbl>, symmetry_mean <dbl>,
    ## #   fractal_dimension_mean <dbl>

This did not help answer the research question. It just added an easier
way to differentiate between different sized tumors. A better way do to
this would to then count each category of relative\_tumor\_size within
each group and compare.

###### Graphing (option 6)

``` r
#selects cancer_sample dataset and produces a graph with area_mean on the x-axis and perimeter_mean on the y-axis.
cancer_sample %>%
  ggplot(aes(area_mean,perimeter_mean)) +
  
#plots the points for the above two variables with an alpha of 0.2 to allow for visualization of overlapping data points. A colour aesthetic was also used to show the diagnosis of each tumor.
  geom_point(aes(colour = diagnosis), alpha = 0.2) +

#makes axes logarithmic
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  
#adds log ticks for better analysis
  annotation_logticks() +
  
#adds logarithmic trend line
  stat_smooth(method="lm",formula=y~log(x), colour="black")
```

![](mda-Milestone-2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

This graph did help answer the research questions as can see a clear
relationship between the two variables in question. It can also be seen
that there appears to be a separation in diagnoses in relation to the
area/perimeter. Benign tumors appear to have a smaller perimeter and
area in comparison to malignant tumors.

#### Research Question 4

###### Summarizing (option 2)

``` r
#selects cancer_sample dataset
cancer_sample %>%
  
#groups data based on the diagnosis variable
  group_by(diagnosis) %>%
  
#provides a counts of the tumors grouped by diagnosis
  summarise(total_number_of_tumors = n())
```

    ## # A tibble: 2 × 2
    ##   diagnosis total_number_of_tumors
    ##   <chr>                      <int>
    ## 1 B                            357
    ## 2 M                            212

No this did not help answer the research question as it only provided
information on the number of tumors in each of the two diagnoses.

###### Graphing (option 7) density

``` r
#selects cancer_sample dataset and produces a graph with fractal_dimension_mean on the x_axis.
cancer_sample %>%
  ggplot(aes(fractal_dimension_mean)) +
  
#produces a density graph containing two lines that represent the two different types of diagnoses with the area below each line filled. Alpha was used to add transparency to the the colour filled area.
  geom_density(aes(fill = diagnosis), alpha = 0.3) +
  
#set origin of graph to 0,0
  expand_limits(x = 0, y = 0) +
  
#relablled y-axis from y, to density.
  ylab(label = "density")
```

![](mda-Milestone-2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Yes, this helped answer the research question as it showed lots of
overlap between the two diagnoses in regards to the fractal dimensions
of the tumors. There appears to be no significant difference in tumor
fractal dimension when comparing between the two tumor diagnoses

### Task 1.3

Based in the operations in task 1.2, all of my questions are close to be
answered. They are just missing some statistics to be able to suggest
whether the results produced are significant. The aspects that remain
unclear are r-values, and P-tests which would allow for the
determination of statistically significant results. I believe my
research questions are good and can be answered with the data provided.
I find the results from question 3 and 4 are the most interesting. The
graphs produced so far show a potential correlation in question 2, and
the comparison of variables in both of these questions to the tumor
diagnoses are interesting.

# Task 2

### Task 2.1

``` r
#selects cancer_sample dataset
cancer_sample %>%
  
#produces a tibble of the first 8 variables in this dataset
  select(ID:compactness_mean)
```

    ## # A tibble: 569 × 8
    ##          ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ##       <dbl> <chr>           <dbl>        <dbl>          <dbl>     <dbl>
    ##  1   842302 M                18.0         10.4          123.      1001 
    ##  2   842517 M                20.6         17.8          133.      1326 
    ##  3 84300903 M                19.7         21.2          130       1203 
    ##  4 84348301 M                11.4         20.4           77.6      386.
    ##  5 84358402 M                20.3         14.3          135.      1297 
    ##  6   843786 M                12.4         15.7           82.6      477.
    ##  7   844359 M                18.2         20.0          120.      1040 
    ##  8 84458202 M                13.7         20.8           90.2      578.
    ##  9   844981 M                13           21.8           87.5      520.
    ## 10 84501001 M                12.5         24.0           84.0      476.
    ## # … with 559 more rows, and 2 more variables: smoothness_mean <dbl>,
    ## #   compactness_mean <dbl>

My data is untidy as can be seen by each column not representing a
single variable, but rather a value (ie. a tumor measurement in the form
of a mean, se, or worst vlue), and each row represents multiple
observation (ie. each row represents a single tumor, but provides 30
observations for that one tumor). The below table shows 8 variables from
my dataset and as you can see, each row contains multiple observations
(ie. the means of 6 different tumor measurements for a single tumor).
Columns headers for 3 to 8 are values, not variable names. Instead of
having this wide version of the data. A longer version would make it
more tidy as it would gather multiple non-variable columns into a single
variable (ie. measurements), and now every row provides a single
observation.

-   Variables 1 & 2: These column are considered tidy because the
    columns are a variable and each cell provides a value. Additionally,
    each row provides an observation
-   Variables 3-8: These columns are untidy as the columns do not
    represent variables, but rather values. Within each column, each row
    does provide a single observations, but when looking across these
    rows you can see there are 6 values within each observation.

### Task 2.2

##### untidy(Original):

``` r
cancer_sample
```

    ## # A tibble: 569 × 32
    ##          ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ##       <dbl> <chr>           <dbl>        <dbl>          <dbl>     <dbl>
    ##  1   842302 M                18.0         10.4          123.      1001 
    ##  2   842517 M                20.6         17.8          133.      1326 
    ##  3 84300903 M                19.7         21.2          130       1203 
    ##  4 84348301 M                11.4         20.4           77.6      386.
    ##  5 84358402 M                20.3         14.3          135.      1297 
    ##  6   843786 M                12.4         15.7           82.6      477.
    ##  7   844359 M                18.2         20.0          120.      1040 
    ##  8 84458202 M                13.7         20.8           90.2      578.
    ##  9   844981 M                13           21.8           87.5      520.
    ## 10 84501001 M                12.5         24.0           84.0      476.
    ## # … with 559 more rows, and 26 more variables: smoothness_mean <dbl>,
    ## #   compactness_mean <dbl>, concavity_mean <dbl>, concave_points_mean <dbl>,
    ## #   symmetry_mean <dbl>, fractal_dimension_mean <dbl>, radius_se <dbl>,
    ## #   texture_se <dbl>, perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>, …

##### tidy:

``` r
(tidy_cancer_sample <- cancer_sample%>%
  pivot_longer(cols = c(-ID, -diagnosis), names_to = "measurements", values_to = "values"))
```

    ## # A tibble: 17,070 × 4
    ##        ID diagnosis measurements              values
    ##     <dbl> <chr>     <chr>                      <dbl>
    ##  1 842302 M         radius_mean              18.0   
    ##  2 842302 M         texture_mean             10.4   
    ##  3 842302 M         perimeter_mean          123.    
    ##  4 842302 M         area_mean              1001     
    ##  5 842302 M         smoothness_mean           0.118 
    ##  6 842302 M         compactness_mean          0.278 
    ##  7 842302 M         concavity_mean            0.300 
    ##  8 842302 M         concave_points_mean       0.147 
    ##  9 842302 M         symmetry_mean             0.242 
    ## 10 842302 M         fractal_dimension_mean    0.0787
    ## # … with 17,060 more rows

##### Back to untidy:

``` r
tidy_cancer_sample %>%
  pivot_wider(names_from = measurements, values_from = values)
```

    ## # A tibble: 569 × 32
    ##          ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ##       <dbl> <chr>           <dbl>        <dbl>          <dbl>     <dbl>
    ##  1   842302 M                18.0         10.4          123.      1001 
    ##  2   842517 M                20.6         17.8          133.      1326 
    ##  3 84300903 M                19.7         21.2          130       1203 
    ##  4 84348301 M                11.4         20.4           77.6      386.
    ##  5 84358402 M                20.3         14.3          135.      1297 
    ##  6   843786 M                12.4         15.7           82.6      477.
    ##  7   844359 M                18.2         20.0          120.      1040 
    ##  8 84458202 M                13.7         20.8           90.2      578.
    ##  9   844981 M                13           21.8           87.5      520.
    ## 10 84501001 M                12.5         24.0           84.0      476.
    ## # … with 559 more rows, and 26 more variables: smoothness_mean <dbl>,
    ## #   compactness_mean <dbl>, concavity_mean <dbl>, concave_points_mean <dbl>,
    ## #   symmetry_mean <dbl>, fractal_dimension_mean <dbl>, radius_se <dbl>,
    ## #   texture_se <dbl>, perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>, …

My original table was presented in an untidy manner as seen by each row
having multiple observations, and columns after variable 2 being
non-variables. Therefore, to make this table tidy, I utilized the
pivot\_longer function to remove the non-variable columns and instead
have a variable for measurements, and a variable for values. By doing
this it made each observation represent a single value

### Task 2.3

I have selected the following two research questions to continue with in
Milestone 3:

1.  Is there a correlation between the area and perimeter of a tumor in
    regards to tumor diagnosis?
2.  Is there a correlation between the tumor’s fractal dimension and
    tumor diagnosis?

I’ve chosen these two questions because they show the most interesting
information and I can produce graphs that can provide a great visual on
potential correlations.

``` r
#selects cancer_sample dataset and assigns the following code to desired_table
desired_table <- cancer_sample %>%
  
#selects for the variables of interest
  select(ID, diagnosis,perimeter_mean,fractal_dimension_mean,area_mean) %>%  
  
##produces an additional variable with 3 possible categories as the observations based on area_mean
  mutate(relative_tumor_size = case_when(area_mean < 650 ~ "small",
                                 area_mean < 1000 ~ "medium",
                                 TRUE ~ "large")) %>%
#arranges the observations based on both diagnosis, then relative_tumor_size
  arrange(diagnosis, relative_tumor_size) 
  
#selects desired_table dataset and assigns the following code to extra_table
extra_table <- desired_table %>%
  
#only provides samples with a relative_tumor_size of small and large  
filter(relative_tumor_size == c("small", "large")) %>%
  
#groups data based on the diagnosis variable and relative_tumor_size   
  group_by(diagnosis, relative_tumor_size) %>%
  
#provides total mean of the area_mean for each tumor diagnosis, and provides a count of how many small and large tumors are present in each tumor diagnosis
  summarise(total_area_mean = mean(area_mean, na.rm = TRUE), relative_tumor_size_count = n()) %>%
  
#reorganizes the table produced in the above code to make more sense (ie. relative_tumor_size and relative_tumor_size_count are beside each other now)
  select(diagnosis, total_area_mean, relative_tumor_size, relative_tumor_size_count)
```

    ## Warning in relative_tumor_size == c("small", "large"): longer object length is
    ## not a multiple of shorter object length

    ## `summarise()` has grouped output by 'diagnosis'. You can override using the `.groups` argument.

``` r
#prints the two tibbles made in this code chunk
print(desired_table)
```

    ## # A tibble: 569 × 6
    ##          ID diagnosis perimeter_mean fractal_dimension_mean area_mean relative_tumor_…
    ##       <dbl> <chr>              <dbl>                  <dbl>     <dbl> <chr>           
    ##  1   861598 B                   95.8                 0.0635      652. medium          
    ##  2   861648 B                   94.6                 0.0587      663. medium          
    ##  3   866458 B                   99.6                 0.0647      674. medium          
    ##  4   867387 B                  102                   0.0572      762. medium          
    ##  5 86973701 B                   97.8                 0.0649      690. medium          
    ##  6  8711216 B                  108.                  0.0527      880. medium          
    ##  7  8712291 B                   95.5                 0.0527      690. medium          
    ##  8  8712853 B                   96.2                 0.0565      686. medium          
    ##  9  8810436 B                   98.2                 0.0553      726. medium          
    ## 10 88147102 B                   97.4                 0.0591      684. medium          
    ## # … with 559 more rows

``` r
print(extra_table)
```

    ## # A tibble: 3 × 4
    ## # Groups:   diagnosis [2]
    ##   diagnosis total_area_mean relative_tumor_size relative_tumor_size_count
    ##   <chr>               <dbl> <chr>                                   <int>
    ## 1 B                    435. small                                     161
    ## 2 M                   1278. large                                      46
    ## 3 M                    555. small                                      21

# Additional Comments

For Task 2.3 where we had to use 4-8 functions on out dataset to make it
in a format that is useful for answering out two research questions,
after performing three functions I got my desired tibble so I assigned
that desired tibble as desired\_table. To show that I can use more
functions, I continued to manipulate the desired\_table and assigned
that code to extra\_table. In extra\_table I just wanted to get a count
of the small and large tumors only, and added the total area mean as an
additional piece of information to compare between the two tumor
diagnoses.

For commits and pushes that were made for this milestone, I used a new
branch, but when going to merch the two branches on github it was not
letting me. It stated I had a conflict error and it did not allow me to
correct this conflict. I noticed this started happening in the first
milestone when I removed the figure folder and then just reknit it to
bring it back. So for this milestone I had to to push the assignment
directly to the main branch since I could figure out how to correct that
error.
