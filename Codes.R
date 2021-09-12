## SECTION 1: Prepare the data

### 1. Load randomized_data_for_hw.csv using read.csv.

library(tidyverse)
dat <- read_csv("~/mgta451/randomized_data_for_hw.csv")

### 2. Here are the variables available in the dataset.
### 3. Log-transform ad metrics, competitor ad metrics, and brand attitude metrics using the log of (x+1).


dat$nat <- log(dat$nat+1)
dat$loc <- log(dat$loc+1)
dat$dig <- log(dat$dig+1)
dat$compnat <- log(dat$compnat + 1)
dat$comploc <- log(dat$comploc + 1)
dat$compdig <- log(dat$compdig + 1)
dat$qua <- log(dat$qua + 1)
dat$val <- log(dat$val + 1)
dat$sat <- log(dat$sat + 1)


### 4. Create 13 lag variables for each brand attitude metric.


dat<- dat %>% group_by(Brand) %>%
  mutate(lag1qua = lag(qua, n=1),
         lag2qua = lag(qua, n=2),
         lag3qua = lag(qua, n=3),
         lag4qua = lag(qua, n=4),
         lag5qua = lag(qua, n=5),
         lag6qua = lag(qua, n=6),
         lag7qua = lag(qua, n=7),
         lag8qua = lag(qua, n=8),
         lag9qua = lag(qua, n=9),
         lag10qua = lag(qua, n=10),
         lag11qua = lag(qua, n=11),
         lag12qua = lag(qua, n=12),
         lag13qua = lag(qua, n=13),
         lag1val = lag(val, n=1),
         lag2val = lag(val, n=2),
         lag3val = lag(val, n=3),
         lag4val = lag(val, n=4),
         lag5val = lag(val, n=5),
         lag6val = lag(val, n=6),
         lag7val = lag(val, n=7),
         lag8val = lag(val, n=8),
         lag9val = lag(val, n=9),
         lag10val = lag(val, n=10),
         lag11val = lag(val, n=11),
         lag12val = lag(val, n=12),
         lag13val = lag(val, n=13),
         lag1sat = lag(sat, n=1),
         lag2sat = lag(sat, n=2),
         lag3sat = lag(sat, n=3),
         lag4sat = lag(sat, n=4),
         lag5sat = lag(sat, n=5),
         lag6sat = lag(sat, n=6),
         lag7sat = lag(sat, n=7),
         lag8sat = lag(sat, n=8),
         lag9sat = lag(sat, n=9),
         lag10sat = lag(sat, n=10),
         lag11sat = lag(sat, n=11),
         lag12sat = lag(sat, n=12),
         lag13sat = lag(sat, n=13))

### 5. Create 5 lags for each advertising spend metric (both own brand and competitor).

dat <- dat %>%
  group_by(Brand) %>%
  mutate(lag1nat = lag(nat, n=1),
         lag2nat = lag(nat, n=2),
         lag3nat = lag(nat, n=3),
         lag4nat = lag(nat, n=4),
         lag5nat = lag(nat, n=5),
         lag1loc = lag(loc, n=1),
         lag2loc = lag(loc, n=2),
         lag3loc = lag(loc, n=3),
         lag4loc = lag(loc, n=4),
         lag5loc = lag(loc, n=5),
         lag1dig = lag(dig, n=1),
         lag2dig = lag(dig, n=2),
         lag3dig = lag(dig, n=3),
         lag4dig = lag(dig, n=4),
         lag5dig = lag(dig, n=5),
         lag1compnat = lag(compnat, n=1),
         lag2compnat = lag(compnat, n=2),
         lag3compnat = lag(compnat, n=3),
         lag4compnat = lag(compnat, n=4),
         lag5compnat = lag(compnat, n=5),
         lag1comploc = lag(comploc, n=1),
         lag2comploc = lag(comploc, n=2),
         lag3comploc = lag(comploc, n=3),
         lag4comploc = lag(comploc, n=4),
         lag5comploc = lag(comploc, n=5),
         lag1compdig = lag(compdig, n=1),
         lag2compdig = lag(compdig, n=2),
         lag3compdig = lag(compdig, n=3),
         lag4compdig = lag(compdig, n=4),
         lag5compdig = lag(compdig, n=5))


### 6. Drop the first 13 observations for each brand, since the lagged brand attitude metrics contain NAs during the first 13 weeks of the data.

dat <- dat %>%
  group_by(Brand) %>%
  slice(14:n())

## SECTION 2: Build Regressions

#### Get the dataframe from part A

library(tidyverse)
dat <- read_csv("~/mgta451/randomized_data_for_hw.csv")
dat$nat <- log(dat$nat+1)
dat$loc <- log(dat$loc+1)
dat$dig <- log(dat$dig+1)
dat$compnat <- log(dat$compnat + 1)
dat$comploc <- log(dat$comploc + 1)
dat$compdig <- log(dat$compdig + 1)
dat$qua <- log(dat$qua + 1)
dat$val <- log(dat$val + 1)
dat$sat <- log(dat$sat + 1)
dat<- dat %>% group_by(Brand) %>%
  mutate(lag1qua = lag(qua, n=1),
         lag2qua = lag(qua, n=2),
         lag3qua = lag(qua, n=3),
         lag4qua = lag(qua, n=4),
         lag5qua = lag(qua, n=5),
         lag6qua = lag(qua, n=6),
         lag7qua = lag(qua, n=7),
         lag8qua = lag(qua, n=8),
         lag9qua = lag(qua, n=9),
         lag10qua = lag(qua, n=10),
         lag11qua = lag(qua, n=11),
         lag12qua = lag(qua, n=12),
         lag13qua = lag(qua, n=13),
         lag1val = lag(val, n=1),
         lag2val = lag(val, n=2),
         lag3val = lag(val, n=3),
         lag4val = lag(val, n=4),
         lag5val = lag(val, n=5),
         lag6val = lag(val, n=6),
         lag7val = lag(val, n=7),
         lag8val = lag(val, n=8),
         lag9val = lag(val, n=9),
         lag10val = lag(val, n=10),
         lag11val = lag(val, n=11),
         lag12val = lag(val, n=12),
         lag13val = lag(val, n=13),
         lag1sat = lag(sat, n=1),
         lag2sat = lag(sat, n=2),
         lag3sat = lag(sat, n=3),
         lag4sat = lag(sat, n=4),
         lag5sat = lag(sat, n=5),
         lag6sat = lag(sat, n=6),
         lag7sat = lag(sat, n=7),
         lag8sat = lag(sat, n=8),
         lag9sat = lag(sat, n=9),
         lag10sat = lag(sat, n=10),
         lag11sat = lag(sat, n=11),
         lag12sat = lag(sat, n=12),
         lag13sat = lag(sat, n=13))
dat <- dat %>%
  group_by(Brand) %>%
  mutate(lag1nat = lag(nat, n=1),
         lag2nat = lag(nat, n=2),
         lag3nat = lag(nat, n=3),
         lag4nat = lag(nat, n=4),
         lag5nat = lag(nat, n=5),
         lag1loc = lag(loc, n=1),
         lag2loc = lag(loc, n=2),
         lag3loc = lag(loc, n=3),
         lag4loc = lag(loc, n=4),
         lag5loc = lag(loc, n=5),
         lag1dig = lag(dig, n=1),
         lag2dig = lag(dig, n=2),
         lag3dig = lag(dig, n=3),
         lag4dig = lag(dig, n=4),
         lag5dig = lag(dig, n=5),
         lag1compnat = lag(compnat, n=1),
         lag2compnat = lag(compnat, n=2),
         lag3compnat = lag(compnat, n=3),
         lag4compnat = lag(compnat, n=4),
         lag5compnat = lag(compnat, n=5),
         lag1comploc = lag(comploc, n=1),
         lag2comploc = lag(comploc, n=2),
         lag3comploc = lag(comploc, n=3),
         lag4comploc = lag(comploc, n=4),
         lag5comploc = lag(comploc, n=5),
         lag1compdig = lag(compdig, n=1),
         lag2compdig = lag(compdig, n=2),
         lag3compdig = lag(compdig, n=3),
         lag4compdig = lag(compdig, n=4),
         lag5compdig = lag(compdig, n=5))
dat <- dat %>%
  group_by(Brand) %>%
  slice(14:n())


### 1. Regress quality on each of the 3 ad metrics, including 5 lags of each of the 3 ad metrics, 5 lags of each of the competitor ad metrics, 13 lags of each of the brand attitude metrics, week fixed effects and brand fixed effects. Use the lm command.

lm1 <- lm(qua~ nat + loc + dig + compnat + comploc + compdig + nat + loc + dig + 
            lag1qua + lag2qua + lag3qua + lag4qua + lag5qua + lag6qua + lag7qua + 
            lag8qua + lag9qua + lag10qua + lag11qua + lag12qua + lag13qua + 
            lag1val + lag2val + lag3val + lag4val + lag5val + lag6val + lag7val + 
            lag8val + lag9val + lag10val + lag11val + lag12val + lag13val + lag1sat + 
            lag2sat + lag3sat + lag4sat + lag5sat + lag6sat + lag7sat + lag8sat + 
            lag9sat + lag10sat + lag11sat + lag12sat + lag13sat + lag1nat + lag2nat + 
            lag3nat + lag4nat + lag5nat + lag1loc + lag2loc + lag3loc + lag4loc + 
            lag5loc + lag1dig + lag2dig + lag3dig + lag4dig + lag5dig + lag1compnat + 
            lag2compnat + lag3compnat + lag4compnat + lag5compnat + lag1comploc + 
            lag2comploc + lag3comploc + lag4comploc + lag5comploc + lag1compdig + 
            lag2compdig + lag3compdig + lag4compdig + lag5compdig + as.factor(Brand) + 
            as.factor(time_period), data = dat)

### 2. Using the same regression equation in step 1, use the felm command from the lfe package. Y

lm2 <- felm(qua~ nat + loc + dig + compnat + comploc + compdig + lag1qua + 
              lag2qua + lag3qua + lag4qua + lag5qua + lag6qua + lag7qua + lag8qua + 
              lag9qua + lag10qua + lag11qua + lag12qua + lag13qua + lag1val + 
              lag2val + lag3val + lag4val + lag5val + lag6val + lag7val + lag8val + 
              lag9val + lag10val + lag11val + lag12val + lag13val + lag1sat + lag2sat + 
              lag3sat + lag4sat + lag5sat + lag6sat + lag7sat + lag8sat + lag9sat + 
              lag10sat + lag11sat + lag12sat + lag13sat + lag1nat + lag2nat + lag3nat + 
              lag4nat + lag5nat + lag1loc + lag2loc + lag3loc + lag4loc + lag5loc + 
              lag1dig + lag2dig + lag3dig + lag4dig + lag5dig + lag1compnat + 
              lag2compnat + lag3compnat + lag4compnat + lag5compnat + lag1comploc + 
              lag2comploc + lag3comploc + lag4comploc + lag5comploc + lag1compdig + 
              lag2compdig + lag3compdig + lag4compdig + lag5compdig | as.factor(Brand) + 
              as.factor(time_period), data = dat)

### 3. Now on the regression ran in #2, weight the data by qua_vol. How do the results change?

lm3 <- felm(qua~ nat + loc + dig + compnat + comploc + compdig + lag1qua + lag2qua + 
              lag3qua + lag4qua + lag5qua + lag6qua + lag7qua + lag8qua + lag9qua + 
              lag10qua + lag11qua + lag12qua + lag13qua + lag1val + lag2val + lag3val + 
              lag4val + lag5val + lag6val + lag7val + lag8val + lag9val + lag10val + 
              lag11val + lag12val + lag13val + lag1sat + lag2sat + lag3sat + lag4sat + 
              lag5sat + lag6sat + lag7sat + lag8sat + lag9sat + lag10sat + lag11sat + 
              lag12sat + lag13sat + lag1nat + lag2nat + lag3nat + lag4nat + lag5nat + 
              lag1loc + lag2loc + lag3loc + lag4loc + lag5loc + lag1dig + lag2dig + 
              lag3dig + lag4dig + lag5dig + lag1compnat + lag2compnat + lag3compnat + 
              lag4compnat + lag5compnat + lag1comploc + lag2comploc + lag3comploc + 
              lag4comploc + lag5comploc + lag1compdig + lag2compdig + lag3compdig + 
              lag4compdig + lag5compdig | as.factor(Brand) + as.factor(time_period), 
            weights = dat$qua_vol, data = dat)

### 4. Now on the regression ran in #3, replace the brand fixed effects with brand-quarter fixed effects. Notice how the results change.

lm4 <- felm(qua~ nat + loc + dig + compnat + comploc + compdig + lag1qua + 
              lag2qua + lag3qua + lag4qua + lag5qua + lag6qua + lag7qua + lag8qua + 
              lag9qua + lag10qua + lag11qua + lag12qua + lag13qua + lag1val + 
              lag2val + lag3val + lag4val + lag5val + lag6val + lag7val + lag8val + 
              lag9val + lag10val + lag11val + lag12val + lag13val + lag1sat + 
              lag2sat + lag3sat + lag4sat + lag5sat + lag6sat + lag7sat + lag8sat + 
              lag9sat + lag10sat + lag11sat + lag12sat + lag13sat + lag1nat + lag2nat + 
              lag3nat + lag4nat + lag5nat + lag1loc + lag2loc + lag3loc + lag4loc + 
              lag5loc + lag1dig + lag2dig + lag3dig + lag4dig + lag5dig + lag1compnat + 
              lag2compnat + lag3compnat + lag4compnat + lag5compnat + lag1comploc + 
              lag2comploc + lag3comploc + lag4comploc + lag5comploc + lag1compdig + 
              lag2compdig + lag3compdig + lag4compdig + lag5compdig | 
              as.factor(Brand)*as.factor(yrqtr) + as.factor(time_period), 
            weights = dat$qua_vol, data = dat)

### 5. Now on the regression ran in #4, replace the week fixed effects with industry-week fixed effects. Notice how the results change.

lm5 <- felm(qua~ nat + loc + dig + compnat + comploc + compdig + lag1qua + lag2qua + 
              lag3qua + lag4qua + lag5qua + lag6qua + lag7qua + lag8qua + lag9qua + 
              lag10qua + lag11qua + lag12qua + lag13qua + lag1val + lag2val + lag3val + 
              lag4val + lag5val + lag6val + lag7val + lag8val + lag9val + lag10val + 
              lag11val + lag12val + lag13val + lag1sat + lag2sat + lag3sat + lag4sat + 
              lag5sat + lag6sat + lag7sat + lag8sat + lag9sat + lag10sat + lag11sat + 
              lag12sat + lag13sat + lag1nat + lag2nat + lag3nat + lag4nat + lag5nat + 
              lag1loc + lag2loc + lag3loc + lag4loc + lag5loc + lag1dig + lag2dig + 
              lag3dig + lag4dig + lag5dig + lag1compnat + lag2compnat + lag3compnat + 
              lag4compnat + lag5compnat + lag1comploc + lag2comploc + lag3comploc + 
              lag4comploc + lag5comploc + lag1compdig + lag2compdig + lag3compdig + 
              lag4compdig + lag5compdig | as.factor(Brand)*as.factor(yrqtr) + 
              as.factor(time_period)*as.factor(Industry), weights = dat$qua_vol, data = dat)

### 6. Create a pretty table that shows your advertising coefficient estimates for #2, 3, 4, and 5 side by side. 

library(stargazer)
stargazer(lm2, lm3, lm4, lm5,type = 'text', digits=10, digits.extra=10,
          omit=c("lag1qua","lag2qua","lag3qua","lag4qua","lag5qua","lag6qua","lag7qua",
                 "lag8qua","lag9qua","lag10qua","lag11qua","lag12qua","lag13qua","lag1val",
                 "lag2val","lag3val","lag4val","lag5val","lag6val","lag7val","lag8val","lag9val",
                 "lag10val","lag11val","lag12val","lag13val","lag1sat","lag2sat","lag3sat",
                 "lag4sat","lag5sat","lag6sat","lag7sat","lag8sat","lag9sat","lag10sat","lag11sat",
                 "lag12sat","lag13sat"))

