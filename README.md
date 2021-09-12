# Estimate the Impact of Advertising
A marketing mix model to estimate the impact of advertising on consumers’ brand quality attitudes

## Description
-This is a group project from the *Analytics in Marketing* course at Rady School of Management, UC San Diego
-The goal was to code a marketing mix model to estimate the impact of advertising on consumers’ brand quality attitudes as in Du, Joo, Wilbur (2018).
-Built four regression models with increasing levels of controls using felm.

## Dataset
-Organized data collected from surveys

## Methods
-Log-transformed ad metrics, competitor ad metrics, and brand attitude metrics using the log of (x+1).
-Created 13 lag variables for each brand attitude metric and 5 lags for each advertising spend metric (both own brand and competitor).
-Regressed quality on ad metrics (5 lags of each of the 3 ad metrics, 5 lags of each of the competitor ad metrics, 13 lags of each of the brand attitude metrics), with week fixed effects and brand fixed effects being projected out. 
-Added weights option into the model, weighted by the number of respondents of the brand quality survey.
-Replaced the brand fixed effects with brand-quarter fixed effects to control the unobserved effects of brand-level variables such as budgetary changes that tend to happen quarterly.
-Replaced the week fixed effects with industry-week fixed effects to control the unobserved effects of industry-level variables such as seasonal fluctuations in industry demand. 
-Reviewed the coefficient estimated by each model and their standard deviation and findings. 

## Conclusion
-Perceived quality tends to increase with the brand's own traditional advertising in national and local media; however digital advertising doesn't seem to have significant effects on the brand quality.
-We observed that adjusted R-square statistics increases as we add control variables to the model, and that the estimates of advertising parameters become more consistent with more control variables in the model. 


