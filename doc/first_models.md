---
created: 2020-12-09T11:53:01-0500
modified: 2020-12-10T00:07:47-0500
title: Initial Results from Twitter Weather Vulnerability Project
date: \today
---

# Overview

We have all the data processed, and have begun running initial exploratory models.

# Data
For each of these models, we use vader as the sentiment metric, and do not exclude weather terms.

We include three weather variables in each model:
  * Temperature (sometimes calculated as Heat Index, sometimes just raw temp)
  * Precipitation (in mm at the hour of the tweet)
  * Solar Radiation (essentially how sunny it was at the time of the tweet)

Then, we run separate models to explore how these weather variables interact with four different on-the-ground factors:
  * Income at the census block group
  * Local impervious land cover (indicative of pavement, concrete, urbanization generally)
  * Local tree cover
  * Race at the census block group

We also control for the following fixed effects: year, day of week, day of year, time of day, county, and state*month

# Models

We run regression models with continuous segments for temp, precip, and sunshine.  For our interaction variables, we sometimes discretize them into categories, and sometimes use them as continuous variables.

We initially were fitting the regressions using `sklearn` in python.  However, this led to the problem of not having 




# Issues:

* We are getting different results for models fit with sklearn in python versus fixest in R.  Not sure why.  The python models show what we hypothesize: inequality affects vulnerability at the extremes.  The R models seem to show that inequality is consistent across census groups.jjkk
* The heat index creates a weird histogram.  It looks like there are cutoffs in the algorithm that make odd effects (at 40F/4.4C).  Maybe it is more meant to be applied to only high temperatures?  Could look into WBGT.

```
[ins] r$> heat.index(4.5, rh = 50, temperature.metric = 'celsius')
[1] 2

[ins] r$> heat.index(4.4, rh = 50, temperature.metric = 'celsius')
[1] 4
```

* The error bars with the fixef/survey approach are suprisingly wide, and consistent (they dont vary with the number of observations).  I'm not sure what to think about this.




