# L07 - strings ----
# Stat 301-1

## load packages ----

library(tidyverse)
library(forcats)

## notes ----
mpg |> 
  mutate(
    manufacturer = factor(manufacturer),
    cyl = factor(cyl, 
                 levels = c(8, 6, 5, 4), 
                 labels = c("Very High", "High", "Low", "Very Low"),
                 ordered = TRUE) |> fct_rev()
    ) |> 
  count(cyl)

### Exercises ----

### Exercise 1 ----

gss_clean <- gss_cat |> 
  mutate(
    marital = factor(marital,
                     levels = c("Married", "Never married", "Separated", 
                                "Divorced", "Widowed", "No answer"))
  )

gss_cat |> 
  ggplot(aes(x = marital)) +
  geom_bar()

gss_clean |> 
  ggplot(aes(x = marital)) +
  geom_bar()

### Exercise 2----

# Prove this was successful by making a simple bar chart.

gss_clean |> 
  mutate(
    marital = recode(marital, "Never married" = "single"),
    marital = tolower(marital)) |> 
  ggplot(aes(x = marital)) +
  geom_bar()


### Exercise 3----

gss_clean <- gss_clean |> 
  mutate(
    race = factor(race, levels = c("Not applicable",
                           "Black",
                           "White",
                           "Hispanic",
                           "Asian",
                           "American Indian")
    )
  )

levels(gss_clean$race)

### Exercise 4----

ex_04 <- c("a", "b", "c", "d")
  

ex_04 <- factor(ex_04, levels = c("a", "b", "cd", "d")) 

levels(ex_04)

ex_04

# see qmd for explanation

### Exercise 5----

# Create a bar chart to explore the distribution of `rincome` (reported income). 
# What makes the default bar chart hard to understand? Improve the bar chart.

gss_clean |> 
  ggplot(aes(x = rincome)) +
  geom_bar()

# there are levels like "no answer" and "Don't know" and "Not Applicable"
# in the same section as numerical values for income. 

rincome_reordered <- c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999",
                       "$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999",
                       "$10000 - 14999", "$15000 - 19999", "$20000 - 24999", "$25000 or more",
                       "No answer", "Don't know", "Refused", "Not applicable")

gss_clean |> 
  mutate(
    rincome = factor(rincome, 
                     levels = rincome_reordered)
  ) |> 
  ggplot(aes(x = ordered(rincome))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  labs(
    title = "Reported Income in `gss_cat` Dataset",
    subtitle = "Most with a reported income made more than 25,000 dollars anually",
    x = "Reported Income (in USD)",
    y = "Count"
  )


### Exercise 6----
# The variable `year` is currently coded as an integer. Demonstrate how to convert this variable to a `factor`.

gss_clean$year <- as.integer(gss_clean$year)

is.integer(gss_clean$year)


### Exercise 7----

rincome_collapsed <- c("Low", "Low", "Low", "Low",
                       "Medium", "Medium", "Medium", "Medium",
                       "High", "High", "High", "High",
                       "No answer", "Don't know", "Refused", "Not applicable")

gss_clean |> 
  mutate(
    rincome = factor(rincome, 
      levels = c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999",
                  "$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999",
                  "$10000 - 14999", "$15000 - 19999", "$20000 - 24999", "$25000 or more",
                  "No answer", "Don't know", "Refused", "Not applicable"),
      labels = rincome_collapsed
    )
  ) |> 
ggplot(aes(x = ordered(rincome))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  labs(
    title = "Reported Income in `gss_cat` Dataset",
    subtitle = "Most with a reported income made more than 25,000 dollars anually",
    x = "Reported Income (in USD)",
    y = "Count"
  )


