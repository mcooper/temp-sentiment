---
created: 2020-12-09T11:53:01-0500
modified: 2020-12-11T20:04:24-0500
title: Initial Results from Twitter Weather Vulnerability Project
date: \today
---

# First Model

The initial models took the following form, with income as a log-transformed continuous variable. (Splines for precipitation and sunshine were also included, but for simplicity's sake I'm only including heat-index temperature in this example).

```R
vader ~ 
        #Spline term with knots at -10, 0, 5, 10, 15, 20, 25, 30, 35
        income_percap * temp.hi + 
        I(income_percap*pmax(temp.hi + 10, 0)) + I(income_percap*pmax(temp.hi - 0, 0)) +
        I(income_percap*pmax(temp.hi - 5, 0)) + I(income_percap*pmax(temp.hi - 10, 0)) +
        I(income_percap*pmax(temp.hi - 15, 0)) + I(income_percap*pmax(temp.hi - 20, 0)) +
        I(income_percap*pmax(temp.hi - 25, 0)) + I(income_percap*pmax(temp.hi - 30, 0)) +
        I(income_percap*pmax(temp.hi - 35, 0)) 

        #Fixed effects
        | dow + doy + tod + fips + year + statemonth
```

And this function yielded the following results:

![Initial results - no standard errors](/home/mattcoop/temp-sentiment/res/temp.hi-income-no-ref-segments-noSE_Noweather.png){width=75%}

![Initial results - with standard errors](/home/mattcoop/temp-sentiment/res/temp.hi-income-no-ref-segments-SE_Noweather.png){width=75%}

These results look pretty good, and if we look at income at the 5th, 50th, and 95th percentiles, they are significantly different.

\pagebreak

# Second Model
These results were different from what I got when working with python, and in investigating why, and I realized our model was missing a bunch of parameters for the effects of temperature independent of incomes.  So I ran new models with splines for the effect of temperature alone (without income), as well as splines for temperature interacting with income.  So:

```R
vader ~ 
        #Spline term interacting with income
        income_percap * temp.hi + 
        I(income_percap*pmax(temp.hi + 10, 0)) + I(income_percap*pmax(temp.hi - 0, 0)) +
        I(income_percap*pmax(temp.hi - 5, 0)) + I(income_percap*pmax(temp.hi - 10, 0)) + 
        I(income_percap*pmax(temp.hi - 15, 0)) + I(income_percap*pmax(temp.hi - 20, 0)) +
        I(income_percap*pmax(temp.hi - 25, 0)) + I(income_percap*pmax(temp.hi - 30, 0)) +
        I(income_percap*pmax(temp.hi - 35, 0)) + 
        
        #Effects of temperature independent of income
        temp.hi + I(pmax(temp.hi + 10, 0)) + I(pmax(temp.hi - 0, 0)) + 
        I(pmax(temp.hi - 5, 0)) + I(pmax(temp.hi - 10, 0)) + 
        I(pmax(temp.hi - 15, 0)) + I(pmax(temp.hi - 20, 0)) + 
        I(pmax(temp.hi - 25, 0)) + I(pmax(temp.hi - 30, 0)) + 
        I(pmax(temp.hi - 35, 0)) 

        #Fixed Effects
        | dow + doy + tod + fips + year + statemonth

```
This yielded results with more stark effects of heatwaves on sentiment inequality, and also had a better AIC.

![Second results - no standard errors](/home/mattcoop/temp-sentiment/res/temp.hi-income-ref-segments-noSE_Noweather.png){width=75%}

![Second results - with standard errors](/home/mattcoop/temp-sentiment/res/temp.hi-income-ref-segments-SE_Noweather.png){width=75%}

So these look like better results by the shape of the curves, and by AIC.  But the standard errors are so massive that if we include standard errors, you cant even really see the effect!

\pagebreak

# Standard Error Issues

So our second model both supports our hypothesis more and has a better fit.  But the standard errors are huge.  The difference in mean sentiment from Saturday to Monday is 0.01, so an error range of ~0.2 is massive!  I am not sure how to interpret these errors, but I don't think they are showing a 95% confidence interval.

I still dont understand:

1. Why the error sizes for the first model are the same size across all temperature ranges, but butterfly-shaped for the second model.
2. Why the "centerpoint" of the second model's errors is centered around 10 degrees, rather than 20, where we have the most observations.
3. How the svycontrasts function is even calculating these errors, or how these standard errors are even valid without an entire variance-covariance matrix that includes the fixed effects.

Possible Interpretations:

1. The errors are incorrect, and a different approach (for example a bootstrap) would yield much tighter error sizes.
2. This is correct, because sentiment is high-variance and the effects of temperature are marginal.  So we just need to interpret our findings in light of this massive uncertainty.

Potential Next Steps:

1. Run a small bootstrap (n=100 would take 48 hours and cost $200 in credits), see if 95% of the models fall within a tighter range.
2. Go back to python and try to debug the issues in calculating the variance-covariance matrix.
3. Fit models with fewer parameters (fewer knots, and just fit `income_percap*temp.hi` like in the first model).  It seems the errors sizes seem to increase as we are combining more parameters.

# Issue with Heat Index

There also seems to be an issue with the heat index algorithm.  There are weird cutoffs in the algorithm that cause bunching in the histogram. For example, at rh=50, temperatures of 4.4 and 4.5 produce nonsensical heat index temperatures:

```R
[ins] r$> heat.index(4.5, rh = 50, temperature.metric = 'celsius')
[1] 2

[ins] r$> heat.index(4.4, rh = 50, temperature.metric = 'celsius')
[1] 4
```

I do think we should stick to something like the heat index, because inequalities in sentiment are much more apparent with that metric compared to just raw temperature.  So I will calculate the Wet Bulb Globe Temperature, which also captures interactions between RH and temperature (as well as wind and sunshine), and interacts continuously with temperature, without cutoffs in the algorithm.  Hopefully this will give us the same results in terms of heatwaves exacerbating inequalities in sentiment, but provide a more continuous distribution of temperatures.

