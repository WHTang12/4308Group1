# This code lists the data cleaning (NA handling, minor date and variable filtering, ) 
# and processing (making our target variable)
# and some minor EDA done (plotting of inflation over time)
# and stationarity test
# In hindsight, we should have just used fbi package to import the transformed data, to make this code file more tidy
# Note: We only filter for our official start year 1985 in each model script, not here

# Required packages
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("aTSA")

# Import packages
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(aTSA) # this one is for stationarity test, much more easier and detailed than the other 2 packages: tseries and urca(?)

# Keep row of required transformations aside first
transformations <- read_csv("../data/2025-09-MD.csv", n_max = 1)

# Importing actual data
df <- read_csv("../data/2025-09-MD.csv")[-1, ]
coreCPI <- read_csv("../data/CPILFESL.csv") 

glimpse(df)  

# 1. Convert date into datetime
# 2. Create year and month columns using the date column
# 3. Then filter for observations after 1980
df1 <- df %>%
  mutate(sasdate = mdy(sasdate),
         year = year(sasdate),
         month = month(sasdate)) %>%
  relocate(sasdate, year, month, .before = 1) %>% # this one is so they are the first few columns
  filter(year >= 1979) # putting 1979 so we can do x months lag later on

##### Merge coreCPI with FRED-MD data #####
sum(is.na(coreCPI)) # no NA's in coreCPI series

coreCPI <- coreCPI %>%
  mutate(observation_date = ymd(observation_date)) %>%
  filter(year(observation_date) >= 1979)

# Checking first whether any rows will get omitted before merger
coreCPI %>%
  anti_join(df1, by = c("observation_date" = "sasdate")) # only september 2025, no issue

# Then, do the merge
df1 <- df1 %>%
  left_join(coreCPI, by = c("sasdate" = "observation_date"))

##### Checking for/dealing with NA's #####
colSums(is.na(df1)) # check NAs for each column 

# Some variables only have 1-2 NA's, and one variable (ACOGNO) has 146 NA's.
# 1. ACOGNO only started from 1992, hence the NA's, so we will just omit the variable
df1 <- df1 %>%
  select(-ACOGNO)

# 2. Focusing on the row-columns with 1-2 NA's
print(df1 %>% filter(if_any(everything(), is.na)) %>%
        select(sasdate, which(colSums(is.na(.)) > 0)),
      width = Inf)
# a: April 2020, Commercial paper stopped, so we omit the two variables associated with it
df1 <- df1 %>%
  select(-CP3Mx, -COMPAPFFx)
# b: Missing data for latest available month (August 2025), maybe we omit latest month for now
df1 <- df1 %>%
  filter(!(year == 2025 & month == 8))

colSums(is.na(df1)) # Now left with 1 NA in S&P div yield for July 2025


##### Computing inflation rate using CoreCPI (this will be done in each models' code file too) #####
df1 <- df1 %>%
  mutate(log_coreCPI = log(CPILFESL),
         log_lagcoreCPI = log(lag(CPILFESL)),
         inflationRate = 100 * (log_coreCPI - log_lagcoreCPI))

# Plot of trend of inflation rate over the years, just to see whether data has any issues
df1 %>%
  filter(year >= 1970) %>%
  ggplot(aes(x = sasdate, y = inflationRate)) +
  geom_line() +
  labs(
    title = "Core CPI Inflation Rate (Monthly, 1980–Present)",
    x = "Date",
    y = "Inflation Rate (%)"
  ) +
  theme_minimal()

##### Transformations according to Fred-MD #####
transformations <- transformations[, -1]
transformations[1, ] <- as.list(as.numeric(transformations[1, ]))

# Remove the variables we omitted from df1 first
transformations <- transformations %>%
  select(-ACOGNO, -CP3Mx, -COMPAPFFx)

transformation1 <- names(transformations)[as.numeric(transformations[1, ]) == 1]
transformation2 <- names(transformations)[as.numeric(transformations[1, ]) == 2]
transformation3 <- names(transformations)[as.numeric(transformations[1, ]) == 3]
transformation4 <- names(transformations)[as.numeric(transformations[1, ]) == 4]
transformation5 <- names(transformations)[as.numeric(transformations[1, ]) == 5]
transformation6 <- names(transformations)[as.numeric(transformations[1, ]) == 6]
transformation7 <- names(transformations)[as.numeric(transformations[1, ]) == 7]

# Apply transformations
df2 <- df1 %>%
  mutate(
    across(all_of(transformation1), ~ .),                                                   # no transformation required
    across(all_of(transformation2), ~ . - lag(.)),                                          # first difference
    across(all_of(transformation3), ~ (. - lag(.)) - (lag(.) - lag(., 2))),                # second difference
    across(all_of(transformation4), ~ log(.)),                                             # log
    across(all_of(transformation5), ~ log(.) - log(lag(.))),                               # log first difference
    across(all_of(transformation6), ~ (log(.) - log(lag(.))) - (log(lag(.)) - log(lag(., 2)))), # log second difference
    across(all_of(transformation7), ~ ((./lag(.) - 1) - (lag(.)/lag(.,2) - 1)))            # idk what's this called
  )

### --- Stationarity test --- ###
df_test <- df2 %>% 
  filter(year >= 1985) %>% # only do in the years of concern
  select(-sasdate, -year, -month)

# Run ADF test. Each function call does all three types of tests:
# 1. No drift and trend
# 2. Drift but no trend
# 3. Both drift and trend
adf_results <- list()
for(col in names(df_test)) {
  adf_results[[col]] <- adf.test(df_test[[col]], output = F)
}

# No drift and trend, identify non-stationary
non_stationary_vars_type1 <- c()
for(col in names(df_test)) {
  type1_pvals <- adf_results[[col]]$type1[, "p.value"]
  if(all(type1_pvals > 0.05)) {
    non_stationary_vars_type1 <- c(non_stationary_vars_type1, col)
  }
}
non_stationary_vars_type1

# Both drift and trend, identify non-stationary
non_stationary_vars_type3 <- c()
for(col in names(df_test)) {
  type3_pvals <- adf_results[[col]]$type3[, "p.value"]
  if(all(type3_pvals > 0.05)) {
    non_stationary_vars_type3 <- c(non_stationary_vars_type3, col)
  }
}
non_stationary_vars_type3

# We remove these variables due to non-stationarity 
# Some are near-stationary so I didn't exclude them
df2 <- df2 %>%
  select(-PERMIT, -PERMITNE, -PERMITMW, -PERMITS, -PERMITW)

# save file
# write.csv(df2, "../data/cleaned_fred2.csv", row.names = FALSE)
