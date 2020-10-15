
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Transition probabilities of COVID-19 levels of incidence in Toronto’s neighbourhoods

<!-- badges: start -->

<!-- badges: end -->

Antonio Paez (McMaster University)

Note that I am conducting this research project *live* and the
repository will be updated to reflect progress until the final paper.

## Abstract

The objective of this analysis is to investigate the spatio-temporal
variations of incidence of COVID-19 in the neighbourhoods in Toronto.
The neighbourhoods are relatively small, ad-hoc geographic areas used
for administrative purposes in Toronto, and therefore provide a
relatively high resolution spatial perspective of the process of
diffusion of COVID-19 in Toronto. Use of spatial Markov Chains can help
to track the transition probabilities of the neighbourhoods by level of
incidence as a spatially conditioned process. The key hypothesis is that
during lockdown, with a reduction in

## Keywords

COVID-19  
Spatial analysis  
Spatial Markov Chains  
Contagion  
Toronto  
Reproducible research

## Background

I have been invited to submit a paper for a special issue of Scienze
Regionali (the journal of AISRe - Italian Association of Regional
Sciences) on the topic of COVID-19. My initial idea was to do some work
using spatial statistics to detect clusters of incidence. Eric Delmelle
and his colleagues have done some interesting work using [scan
statistics](https://www.sciencedirect.com/science/article/abs/pii/S0143622820303039).
I thought that there was an interesting effect of scale: research using
scan statistics has happened at relatively large scales (e.g., [counties
in the
US](https://www.sciencedirect.com/science/article/pii/S1877584520300320);
[municipalities in
Brazil](https://www.medrxiv.org/content/10.1101/2020.06.14.20131102v1)).

After finding the data with cases in Toronto in the Toronto Open Data
Portal, I thought that it might be more interesting to try something at
a smaller scale-higher resolution. The data for Toronto (which has been
one of the hotspots in Ontario) is for individual cases and
time-stamped. Geocoding is at the level of the “neighbourhood”, an
ad-hoc subdivision in Toronto, much smaller than a municipality. Instead
of scan statistics for detecting hotspots, I thought to use spatial
Markov Chains to examine the transition probabilities of the level of
incidence in the neighborhoods, in particular from the lockdown.

Spatial Markov Chains were proposed by Rey
[2001](https://doi.org/10.1111/j.1538-4632.2001.tb00444.x) for the
analysis of economic growth and convergence, but can be used for any
process of growth and/or convergence that is spatially conditioned (see
for instance Schettini et al.,
[2011](https://doi.org/10.1177/0160017611403141) for an application to
efficiency in manufacturing in Brazil). COVID-19, as a contagious
disease, is an obvious example of a process of growth that is spatially
conditioned. There is circumstantial evidence that lockdown policies
reduced the contagion process in Spain (Paez et al., [2020]()), although
at a relatively large level of aggregation (provinces). Therefore, it
would be interesting to investigate the evolution of the incidence of
COVID-19 as a spatially conditioned process.

Conceptually, this is what the process would look like:

1.  Initial cases are imported (they do not generate internally). The
    starting point of the cases is not necessarily random. Since they
    are associated with travel, they will depend to some extent on
    socio-economic and demographic factors (initial cases were imported
    from China, the US, or Europe - traveling might indicate relatively
    high socio-economic status, and/or ethnic links to those regions).
    From the purpose of the current analysis, we will just assume that
    the location of the initial cases is a historical contingency.

2.  As the number of cases grows public health measures are instituted.
    I will think of two broad classes of public health measures: travel
    bans, meant to reduce the risk of importing new cases, and lock-down
    measure, meant to reduce the risk of community transmission.

An examination of imported cases (related to travel) will be informative
with respect to the effectiveness of travel bans. Examination of the
transition probabilities will be informative with respect to the
effectiveness of lockdown measures.

## Preliminaries

I downloaded the following files from Toronto [Open Data
Portal](https://open.toronto.ca/) (all files were retrieved on October
9, 2020):

1.  `COVID 19 cases.csv` (search term: “covid”)

2.  `wellbeing-toronto-population-total-2011-2016-and-age-groups-2016.xlsx`
    (search term: “wellbeing demographics”)

3.  `Neighbourhoods.shp` (search term: “neighbourhoods boundaries”)

I begin by reading the files. These are the packages that I will use:

``` r
library(lubridate)
library(readr)
library(readxl)
library(sf)
library(spdep)
library(tidyverse)
```

Read csv file with information about COVID-19 and convert dates to date
format:

``` r
covid19_cases <- read_csv("COVID19 cases.csv", 
                          col_types = cols(
                            `_id` = col_double(),
                            Assigned_ID = col_double(),
                            `Outbreak Associated` = col_character(),
                            `Age Group` = col_character(),
                            `Neighbourhood Name` = col_character(),
                            FSA = col_character(),
                            `Source of Infection` = col_character(),
                            Classification = col_character(),
                            `Episode Date` = col_character(),
                            `Reported Date` = col_character(),
                            `Client Gender` = col_character(),
                            Outcome = col_character(),
                            `Currently Hospitalized` = col_character(),
                            `Currently in ICU` = col_character(),
                            `Currently Intubated` = col_character(),
                            `Ever Hospitalized` = col_character(),
                            `Ever in ICU` = col_character(),
                            `Ever Intubated` = col_character())) %>%
  mutate(`Episode Date` = parse_date_time(`Episode Date`, orders = "mdy"),
         `Reported Date` = parse_date_time(`Reported Date`, orders = "mdy"))
```

Notice that there are two different dates in this file: `Episode Date`
and `Reported Date`. These are defined as follows:

  - `Episode Date`: The episode date is a derived variable that best
    estimates when the disease was acquired, and refers to the earliest
    available date from: symptom onset (the first day that COVID-19
    symptoms occurred), laboratory specimen collection date, or reported
    date.

  - `Reported Date`: The date on which the case was reported to Toronto
    Public Health.

There is expected to be an occasional lag between the episode and the
report.

The Source of Infection is a variable that describes “The most likely
way that sporadic cases acquired their COVID-19 infection. Only the most
likely exposure for each case is reported. Exposures that occurred up to
14 days before symptoms start are potential acquisition sources.” The
sources of infaction are:

  - Travel: Travel outside of Ontario.

  - Close contact with a case: Was in close contact with a confirmed or
    probable COVID-19 case (e.g., reside in the same household).

  - Institutional setting: Institutional settings includes, but not
    limited to: long-term care homes, acute care hospitals, complex case
    hospitals, special care facilities, retirement homes, rehabilitation
    hospitals.

  - Healthcare setting: Healthcare settings includes, but not limited
    to: family physician, dentist, ophthalmologist, sports doctor.

  - Community: Cases with no reported travel outside of Ontario, no
    known close contact with a COVID-19 case, and no reported infection
    acquired in an institutional or healthcare setting.

  - Pending Information on source of infection pending.

  - Unknown/missing Source of infection not identified or missing.

  - N/A: Outbreak-associated cases.

How many cases have been associated with each source of infection?

``` r
table(covid19_cases$`Source of Infection`)
#> 
#>             Close contact                 Community                Healthcare 
#>                      7510                      3045                      1256 
#>             Institutional N/A - Outbreak associated                   Pending 
#>                       366                      6474                        40 
#>                    Travel           Unknown/Missing 
#>                       913                      1538
```

Despite travel not being the most common source of infection, close
contact with those who traveled is the major source of infection. This
is followed by “Outbreak associated”. Need to find more precisely what
that means.

Read xlsx file with demographic information and rename columns for
neighbourhood ID and total population:

``` r
toronto_population <- read_excel("wellbeing-toronto-population-total-2011-2016-and-age-groups-2016.xlsx", 
                                 sheet = "016 Pop_TotalChange") %>% 
  rename(NeighbourhoodID = HoodID,
         total_population = Pop2016)
```

Summary of total population:

``` r
toronto_population %>% 
  select(total_population) %>% 
  summary()
#>  total_population
#>  Min.   : 6577   
#>  1st Qu.:12020   
#>  Median :16750   
#>  Mean   :19511   
#>  3rd Qu.:23855   
#>  Max.   :65913
```

Read shape file and rename columns:

``` r
neighbourhoods <- st_read("Neighbourhoods.shp") %>%
  rename(`_id` = FIELD_1,
          AREA_ID = FIELD_2,
          AREA_ATTR_ID = FIELD_3,
          PARENT_AREA_ID = FIELD_4,
          AREA_SHORT_CODE = FIELD_5,
          AREA_LONG_CODE = FIELD_6,
          AREA_NAME = FIELD_7,
          AREA_DESC = FIELD_8,
          X = FIELD_9,
          Y = FIELD_10,
          LONGITUDE = FIELD_11,
          LATITUDE = FIELD_12,
          OBJECTID = FIELD_13,
          Shape__Area = FIELD_14,
          Shape__Length = FIELD_15)
#> Reading layer `Neighbourhoods' from data source `C:\Antonio\Cases 2020\COVID-19-Toronto-Spatial\Neighbourhoods.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 140 features and 15 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -79.63926 ymin: 43.581 xmax: -79.11545 ymax: 43.85546
#> geographic CRS: WGS 84
```

The keys to joining the demographic data to the neighbourhood boundaries
are the NeighbourhoodID (in the demographic data frame) and
AREA\_SHORT\_CODE (in the boundaries data frame). Convert those columns
to character:

``` r
toronto_population <- toronto_population %>%
  mutate(NeighbourhoodID = as.character(NeighbourhoodID))

neighbourhoods <- neighbourhoods %>%
  mutate(AREA_SHORT_CODE = as.character(AREA_SHORT_CODE))
```

It is now possible to join the demographic data to the boundaries:

``` r
 neighbourhoods <- neighbourhoods %>%
  left_join(toronto_population, 
            by = c("AREA_SHORT_CODE" = "NeighbourhoodID"))
```

Plot population by neighbourhood:

``` r
ggplot(data = neighbourhoods) +
  geom_sf(aes(fill = total_population)) + 
  scale_fill_distiller(name = "Total Population",
                       palette = "YlOrRd", 
                       direction = 1) +
  theme_minimal()
```

![](README_files/figure-gfm/plot-population-1.png)<!-- -->

Some neighbourhood names are coded differently in the files `COVID19
cases.csv` and
`wellbeing-toronto-population-total-2011-2016-and-age-groups-2016.xlsx`
(I discovered this only later when joining the cases by neighbourhood to
the population). I will fix this here instead of there:

``` r
covid19_cases <- covid19_cases %>%
  mutate(`Neighbourhood Name` = case_when(`Neighbourhood Name` == "Briar Hill - Belgravia" ~ "Briar Hill-Belgravia",
                                          `Neighbourhood Name` == "Danforth-East York" ~ "Danforth East York",
                                          TRUE ~ `Neighbourhood Name`))
```

## Exploratory data analysis: temporal trends

To calculate the number of cases by episode date, we summarize the data
frame as follows:

``` r
covid19_cases_by_episode <- covid19_cases %>%
  group_by(`Episode Date`) %>%
  summarize(new_cases = n(), 
            .groups = "drop") %>%
  rename(Date = `Episode Date`)
```

To calculate the number of cases by reported date, we summarize the data
frame as follows:

``` r
covid19_cases_by_report <- covid19_cases %>%
  group_by(`Reported Date`) %>%
  summarize(new_cases = n(), 
            .groups = "drop") %>%
  rename(Date = `Reported Date`)
```

The cumulative number of new cases (by episode and reported dates) can
be calculated using the function `lag()`:

``` r
covid19_cases_by_episode <- covid19_cases_by_episode %>%
  mutate(cumulative_cases = cumsum(new_cases),
         type = "Episode")

covid19_cases_by_report <- covid19_cases_by_report %>%
  mutate(cumulative_cases = cumsum(new_cases),
         type = "Reported")
```

Bind these two data frames for visualization:

``` r
covid19_cases_by <- rbind(covid19_cases_by_episode, 
                          covid19_cases_by_report)
```

Plot the number of new cases:

``` r
ggplot(data = covid19_cases_by, 
       aes(x = Date, 
           y = new_cases, 
           fill = type)) +
  geom_col(position = "dodge") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90))
```

![](README_files/figure-gfm/plot-cases-1.png)<!-- -->

We see the peak of the first wave of the pandemic in Toronto in the
winter-spring 2020, and the early stages of the second wave at the end
of the summer, beginning of the fall.

Plot the cumulative number of cases by date:

``` r
ggplot(data = covid19_cases_by, 
       aes(x = Date, 
           y = cumulative_cases, 
           fill = type)) +
  geom_col(position = "dodge") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90))
```

![](README_files/figure-gfm/plot-cumulative-toronto-1.png)<!-- -->

The lag between the episode and report was relatively small in the
summer, but started increasing again at the end of the summer and
beginning of the fall.

What has been the average delay between episode and report over time?

``` r
covid19_cases %>% 
  transmute(Date = `Reported Date`, 
            delay = as.numeric(difftime(`Reported Date`, 
                                        `Episode Date`),
                               units = "days")) %>%
  group_by(Date) %>%
  summarize(mean_delay = mean(delay),
            .groups = "drop") %>%
  ggplot(aes(x = Date, y = mean_delay)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = y ~ x) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The mean delay has remained relatively constant between 5 and 7 days
throughout the pandemic.

The cumulative incidence can be calculated by dividing the cumulative
cases by the total population (this is in cases per million people):

``` r
covid19_cases_by <- covid19_cases_by %>%
  mutate(cumulative_incidence = cumulative_cases/(sum(toronto_population$total_population, na.rm = TRUE)/1000000))
```

The distribution of the cumulative incidence is as follows:

``` r
summary(covid19_cases_by$cumulative_incidence)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    0.366 1707.076 4937.818 3907.586 5704.776 7739.868
```

For context, according to
[worldometer](https://www.worldometers.info/coronavirus/) the total
cases per million population in various countries is (as of October 10,
2020):

  - USA: 23,960 cass per million

  - Brazil: 23,908 cases per million

  - Spain: 19,041 cases per million

  - Italy: 5,783 cases per million

  - Canada: 4,762 cases per million

What is the temporal distribution of cases by source of infection? To
explore this we need to summarize the data frame now by date and source
of infection. I’ll do this only by date of episode:

``` r
covid19_cases_by_source <- covid19_cases %>%
  group_by(`Episode Date`, 
           `Source of Infection`) %>%
  summarize(new_cases = n(),
            .groups = "drop") %>%
  rename(Date = `Episode Date`)
```

Now the cumulative sums need to be calculated separately for each source
of infection. To do this I pivot the table wider:

``` r
covid19_cases_by_source_wide <- covid19_cases_by_source %>%
  pivot_wider(names_from = `Source of Infection`,
              names_prefix = "new_",
              values_from = new_cases,
              values_fill = 0) %>%
  rename(new_Close = `new_Close contact`,
         new_Outbreak = `new_N/A - Outbreak associated`,
         new_Missing = `new_Unknown/Missing`)
```

The wider table makes it convenient to calculate the cumulative number
of cases by source of infection:

``` r
covid19_cases_by_source_wide <- covid19_cases_by_source_wide %>%
  mutate(cumulative_Travel = cumsum(new_Travel),
         cumulative_Community = cumsum(new_Community),
         cumulative_Close = cumsum(new_Close),
         cumulative_Healthcare = cumsum(new_Healthcare),
         cumulative_Outbreak = cumsum(new_Outbreak),
         cumulative_Institutional = cumsum(new_Institutional),
         cumulative_Pending = cumsum(new_Pending),
         cumulative_Missing = cumsum(new_Missing))
```

For visualization, I prefer to pivot to long again so that I can use
aesthetics and facets:

``` r
covid19_cases_by_source <- covid19_cases_by_source_wide %>%
  pivot_longer(!Date,
               names_to = c(".value", 
                            "Source"),
               names_sep = "_")
```

``` r
ggplot(data = covid19_cases_by_source, aes(x = Date, 
                        y = cumulative, 
                        fill = Source)) +
  geom_col(position = "stack") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90))
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Exploratory data analysis: spatial trends

To explore the spatial trends it is first necessary to join the
neighborhood covid and population data. The data frame with demographic
information includes a breakdown of population by age group but I will
only use total population (although it might be interesting to look at
incidence by age group at some point). I will begin by summarizing the
cases by date of *episode* (which is the earliest known date that
someone would have been contagious) and neighbourhood:

``` r
covid19_cases_by_neighborhood <- covid19_cases %>%
  rename(Date = `Episode Date`) %>%
  group_by(Date,
           `Neighbourhood Name`) %>%
  summarize(cases = n(),
            .groups = "drop")
```

This gives the cases per date per neighbourhood. To calculate the
cumulative number of cases per neighbourhood I will pivot the table
wider:

``` r
covid19_cases_by_neighborhood_wide <- covid19_cases_by_neighborhood %>%
  pivot_wider(names_from = `Neighbourhood Name`,
              values_from = cases,
              names_prefix = "cases_",
              values_fill = 0)
```

And then calculate the cumulative sum:

``` r
covid19_cases_by_neighborhood_wide <- covid19_cases_by_neighborhood_wide %>%
  mutate(across(starts_with("cases_"),
                cumsum, 
                .names = "cumulative.{.col}"))
```

Now pivot longer again to join the population statistics:

``` r
covid19_cases_by_neighborhood <- covid19_cases_by_neighborhood_wide %>%
  pivot_longer(!Date,
               names_to = c(".value", 
                            "Neighbourhood"),
               names_sep = "_") %>%
  rename(cumulative_cases = cumulative.cases)
```

Sanity check: what is the number of neighborhoods by date?

``` r
covid19_cases_by_neighborhood %>% 
  group_by(Date) %>% 
  summarize(neighbourhoods = n(),
            .groups = "drop") %>%
  summary()
#>       Date                     neighbourhoods
#>  Min.   :2020-01-21 00:00:00   Min.   :141   
#>  1st Qu.:2020-04-15 12:00:00   1st Qu.:141   
#>  Median :2020-06-12 00:00:00   Median :141   
#>  Mean   :2020-06-11 13:17:55   Mean   :141   
#>  3rd Qu.:2020-08-08 12:00:00   3rd Qu.:141   
#>  Max.   :2020-10-05 00:00:00   Max.   :141
```

Nice, so all dates have all neighborhoods. Notice that this table is
actually *longer* than the original `covid19_cases`, and that is because
the original table did not have records for neighborhoods when there
were zero cases, so we are now just making those zeros explicit.

Join the population values to the table. The key for this join is the
`Neighbourhood` on both data frames:

``` r
covid19_cases_by_neighborhood <- covid19_cases_by_neighborhood %>%
  left_join(toronto_population %>% 
              select(NeighbourhoodID,
                     Neighbourhood,
                     total_population),
            by = "Neighbourhood")
```

At this point, the data frame includes some NA values: those are cases
not associated with a neighbourhood.

Calculate the cumulative incidence (in cases per million):

``` r
covid19_cases_by_neighborhood <- covid19_cases_by_neighborhood %>%
  mutate(cumulative_incidence = cumulative_cases/(total_population/1000000))
```

What was the spatial distribution of the incidence at various dates? The
state of emergency in Ontario was declared on March 17, 2020:

``` r
neighbourhoods %>% 
  select(AREA_SHORT_CODE) %>%
  left_join(covid19_cases_by_neighborhood %>% 
              filter(Date == as.Date("2020-03-17")),
            by = c("AREA_SHORT_CODE" = "NeighbourhoodID")) %>%
  ggplot() + 
  geom_sf(aes(fill = cumulative_incidence)) +
  scale_fill_distiller(name = "Incidence at March 17",
                       palette = "YlOrRd", 
                       direction = 1) +
  theme_minimal()
```

![](README_files/figure-gfm/cumulative-incidence-by-neighbourhood-March-17-1.png)<!-- -->

May 19 was when the province moved to Stage 1 of the reopening plan:

``` r
neighbourhoods %>% 
  select(AREA_SHORT_CODE) %>%
  left_join(covid19_cases_by_neighborhood %>% 
              filter(Date == as.Date("2020-05-19")),
            by = c("AREA_SHORT_CODE" = "NeighbourhoodID")) %>%
  ggplot() + 
  geom_sf(aes(fill = cumulative_incidence)) +
  scale_fill_distiller(name = "Incidence at May 19",
                       palette = "YlOrRd", 
                       direction = 1) +
  theme_minimal()
```

![](README_files/figure-gfm/cumulative-incidence-by-neighbourhood-May-19-1.png)<!-- -->

Are the spatial patterns random? We can test the hypothesis of spatial
independence by means of Moran’s \(I\) coefficient. To implement this I
need to create a set of spatial weights:

``` r
neighbourhoods.listw <- poly2nb(as(neighbourhoods, "Spatial")) %>%
  nb2listw()
```

Testing for spatial independence at March 17:

``` r
incidence <- neighbourhoods %>% 
  select(AREA_SHORT_CODE) %>%
  left_join(covid19_cases_by_neighborhood %>% 
              filter(Date == as.Date("2020-03-17")),
            by = c("AREA_SHORT_CODE" = "NeighbourhoodID")) %>%
  select(cumulative_incidence) %>%
  st_drop_geometry()

moran.test(incidence$cumulative_incidence, 
           neighbourhoods.listw)
#> 
#>  Moran I test under randomisation
#> 
#> data:  incidence$cumulative_incidence  
#> weights: neighbourhoods.listw    
#> 
#> Moran I statistic standard deviate = 7.4351, p-value = 5.226e-14
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.354259608      -0.007194245       0.002363397
```

Testing for spatial independence at May 19:

``` r
incidence <- neighbourhoods %>% 
  select(AREA_SHORT_CODE) %>%
  left_join(covid19_cases_by_neighborhood %>% 
              filter(Date == as.Date("2020-05-19")),
            by = c("AREA_SHORT_CODE" = "NeighbourhoodID")) %>%
  select(cumulative_incidence) %>%
  st_drop_geometry()

moran.test(incidence$cumulative_incidence, 
           neighbourhoods.listw)
#> 
#>  Moran I test under randomisation
#> 
#> data:  incidence$cumulative_incidence  
#> weights: neighbourhoods.listw    
#> 
#> Moran I statistic standard deviate = 7.7697, p-value = 3.935e-15
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.370403404      -0.007194245       0.002361867
```

We conclude (at a very high level of confidence since \(p < 0.0001\))
that at these two dates the pattern was likely not random. Moreover, we
see that what at first glance look like the hot spots at each date are
in different regions of the city.

## Space-time dynamics

Now, how do I organize the data to calculate Moran’s I for all dates?
And then I will need to calculate the spatial lagged incidence.

Pivot cases by neighborhood wide but now the columns will be dates:

``` r
covid19_cases_by_neighborhood_wide <- covid19_cases_by_neighborhood %>% 
  select(Date, NeighbourhoodID, cumulative_cases) %>%
  pivot_wider(id_cols = NeighbourhoodID, names_from = Date, values_from = cumulative_cases)
```

Join to the `sf` object (it is *very* important that the neighbourhoods
in the wide table are sorted in the same order as the neighbourhoods in
the `sf` object since the `sf` object was used to create the adjacency
matrix; if not sorted in the same order, the values will not correspond
to the right spatial adjacency patterns):

``` r
neighbourhoods <- neighbourhoods %>%
  left_join(covid19_cases_by_neighborhood_wide, 
            by = c("AREA_SHORT_CODE" = "NeighbourhoodID"))
```

Calculate Moran’s I using `lapply()`:

``` r
# Select columns to be used in the calculation of Moran's I test
mi <- neighbourhoods %>% 
  select(starts_with("2020")) %>% 
  st_drop_geometry() 

# Retrieve dates
episode_dates = colnames(mi)

# Use lapply to calculate the test; the value will be a list
mi <- mi %>%
  lapply(moran.test, listw = neighbourhoods.listw)
```

Once that tests have been computed, extract the main results from the
list for visualization:

``` r
# Initialize data frame to store results of test
mi_test <- data.frame(mi[[1]][[3]])

# Extract results of test and place in data frame
for(i in 2:length(mi)){
  mi_test <- cbind(mi_test, mi[[i]][[3]])
}

# Convert to data frame including the dates
mi_test <- t(mi_test) %>%
  as.data.frame() %>%
  transmute(Date = ymd(episode_dates), I = `Moran I statistic`, Expectation, Variance)
```

Calculate the 95% interval of confidence:

``` r
mi_test <- mi_test %>%
  mutate(CI_low = Expectation - 1.96 * sqrt(Variance/nrow(neighbourhoods)),
         CI_high = Expectation + 1.96 * sqrt(Variance/nrow(neighbourhoods)))
```

Plot resuls of the test with expectation and interval of confidence:

``` r
ggplot(data = mi_test, 
       aes(x = Date)) + 
  geom_point(aes(y = I)) +
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high), 
              fill = "red", 
              alpha = 0.2) + 
  geom_line(aes(y = Expectation), 
            color = "red") 
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Except for the very early dates, the pattern has consistently been
outside of the 95% confidence interval, that is, we can comfortably
reject the null hypothesis of spatial independence. As we saw in the
examples before, the pattern suggests positive autocorrelation, with
geographical concentrations of high incidence values, as would be
expected of a process of contagion.

To further study the space-time dynamics, I need to calculate the
spatially lagged variable. Use `across()`?

``` r

junk <- neighbourhoods %>% st_drop_geometry() %>%
  transmute(across(starts_with("2020-"), ~lag.listw(neighbourhoods.listw, .x), .names = "(.col)_lag"))
```

``` r
lagged_vars <- lag.listw(neighbourhoods.listw, 
                         neighbourhoods %>% 
                           st_drop_geometry() %>%
                           select(which(colnames(neighbourhoods) == episode_dates[1])) %>%
                           pull())
for(i in 2:length(episode_dates)){
  lagged_vars <- cbind(lagged_vars,
                       lag.listw(neighbourhoods.listw, 
                         neighbourhoods %>% 
                           st_drop_geometry() %>%
                           select(which(colnames(neighbourhoods) == episode_dates[i])) %>%
                           pull()))
}

lagged_vars <- data.frame(lagged_vars)

colnames(lagged_vars) <- episode_dates
```

My next problem is organizing all the variables and lags to calculate
the transition probabilities.
