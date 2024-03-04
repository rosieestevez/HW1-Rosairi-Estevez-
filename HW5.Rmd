---
title: "Lab4/HW5"
output: html_document
date: "2024-02-29"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Study Group = Chris and Isamari

## KNN Estimation

-   take an unclassified observation
-   look for classified observations near it
-   guess that it is like its neighbors

```{r}
library(ggplot2)
library(tidyverse)

```

```{r}
require(class)
```

```{r}
require(caret)
```

## AGE, INCOME, EDUCATION LEVEL

```{r}
brfss22$Age_midpt <- fct_recode(brfss22$X_AGEG5YR, "21" = "Age 18 to 24",
                                "27" = "Age 25 to 29", "32" = "Age 30 to 34",
                                "37" = "Age 35 to 39", "42" = "Age 40 to 44",
                                "47" = "Age 45 to 49", "52" = "Age 50 to 54",
                                "57" = "Age 55 to 59", "62" = "Age 60 to 64",
                                "67" = "Age 65 to 69", "72" = "Age 70 to 74",
                                "77" = "Age 75 to 79", "82" = "Age 80 or older",
                                NULL = "Dont know/Refused/Missing")
```

```{r}
brfss22$Age_midpt <- as.numeric(levels(brfss22$Age_midpt))[brfss22$Age_midpt]
```

```{r}
ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) & !is.na(brfss22$MENTHLTH) 
brfss_ACE <- subset(brfss22, select_ACE)
```

```{r}
summary(brfss_ACE)
```

```{r}
brfss22$income_midpoint <- fct_recode(brfss22$INCOME3, "7500" = "Household income less than $10,000",
                                      "12500" = "Less than $15,000 ($10,000 to less than $15,000)",
                                      "17500" = "Less than $20,000 ($15,000 to less than $20,000) ", 
                                      "22500" = "Less than $25,000 ($20,000 to less than $25,000) ",
                                      "30000" = "Less than $35,000 ($25,000 to less than $35,000) ",
                                      "42500" = "Less than $50,000 ($35,000 to less than $50,000) ",
                                      "62500" = "Less than $75,000 ($50,000 to less than $75,000)",
                                      "87500" = "Less than $100,000 ($75,000 to less than $100,000)",
                                      "125000" = "Less than $150,000 ($100,000 to less than $150,000)",
                                      "175000" = "Less than $200,000 ($150,000 to less than $200,000)",
                                      "210000" = "$200,000 or more",
                                      NULL = "Dont know/Not sure",
                                      NULL = "Refused")
```

```{r}

brfss22$income_midpoint <-
as.numeric(levels(brfss22$income_midpoint))[brfss22$income_midpoint]
```

```{r}
brfss22$Educ_number <- fct_recode(brfss22$EDUCA, 
                                  "0" = "Never attended school or only kindergarten", 
                                  "4.5" = "Grades 1 through 8 (Elementary)",
                                  "10" = "Grades 9 through 11 (Some high school)",
                                  "12" = "Grade 12 or GED (High school graduate)",
                    "14" = "College 1 year to 3 years (Some college or technical school)",
                    "16" = "College 4 years or more (College graduate)",
                    NULL = "Refused" )
```

```{r}
brfss22$Educ_number <- as.numeric(levels(brfss22$Educ_number))[brfss22$Educ_number]
```

```{r}
summary(brfss_ACE$ACEPUNCH)
```

```{r}
summary(brfss_ACE$ACESWEAR)
```

```{r}
summary(brfss_ACE$ACEDRUGS)
```

```{r}
summary(brfss_ACE)
```

## ACE PUNCH, SWEAR, and DRUGS

```{r}
brfss_ACE$ACESWEAR_recode <- fct_recode(brfss_ACE$ACESWEAR,
                                        "0" = "Adverse Childhood Exper, never: How often did a parent or adult in your home ever swear at you, insult you, or put you down",
                                "0.5" = "once",
                                "1" = "more than once",
                                NULL = "dont know not sure",
                                NULL = "refused"
)
brfss_ACE$ACESWEAR_recode <- as.numeric(levels(brfss_ACE$ACESWEAR_recode))[brfss_ACE$ACESWEAR_recode]


summary(brfss_ACE$ACESWEAR_recode)
```

```{r}
brfss_ACE$ACEPUNCH_recode <- fct_recode(brfss_ACE$ACEPUNCH, 
                                        "0" = "Adverse Childhood Exper, never: How often did your parents or adults in your home ever slap, hit, kick, punch or beat each other up",
                                "0.5" = "once", 
                                "1" = "more than once",
                                NULL = "dont know not sure",
                                NULL = "refused"
)

brfss_ACE$ACEPUNCH_recode <- as.numeric(levels(brfss_ACE$ACEPUNCH_recode))[brfss_ACE$ACEPUNCH_recode]
```

```{r}
brfss_ACE$ACEDRUGS_recode <- fct_recode(brfss_ACE$ACEDRUGS,
                                        "0" = "Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications",
                                "0.5" = "No",
                                NULL = "dont know not sure",
                                NULL = "refused"
)
brfss_ACE$ACEDRUGS_recode <- as.numeric(levels(brfss_ACE$ACEDRUGS_recode))[brfss_ACE$ACEDRUGS_recode]

summary(brfss_ACE$ACEDRUGS_recode)
```

```{r}
brfss_ACE$MENTHLTH_recode <- cut(brfss_ACE$MENTHLTH, breaks = c(-1,0,1,5,10,15,31))
summary(brfss_ACE$MENTHLTH_recode)

```

```{r}
standardize_varb_to01 <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
}
```

```{r}
X1 <- standardize_varb_to01(brfss_ACE$Age_midpt)
X2 <- standardize_varb_to01(brfss_ACE$Educ_number)
X3 <- standardize_varb_to01(brfss_ACE$income_midpoint)
X4 <- brfss_ACE$ACEPUNCH_recode
X5 <- brfss_ACE$ACESWEAR_recode
X6 <- brfss_ACE$ACEDRUGS_recode
Y <- brfss_ACE$MENTHLTH_recode

nonmissingobs <- complete.cases(Y,X1,X2,X3,X4,X5,X6)


```

```{r}
X1 <- subset(X1, nonmissingobs)
X2 <- subset(X2, nonmissingobs)
X3 <- subset(X3, nonmissingobs)
X4 <- subset(X4, nonmissingobs)
X5 <- subset(X5, nonmissingobs)
X6 <- subset(X6, nonmissingobs)
dat_use <- data.frame(X1,X2,X3,X4,X5,X6)
Y <-subset(Y, nonmissingobs)
```

```{r}
set.seed(123456)
NN_obs <- length(Y)
select1 <- (runif(NN_obs) < 0.6)
train_data <- subset(dat_use, select1)
test_data <- subset(dat_use,select1)
cl_data <- Y[select1]
```

```{r}
true_data <- Y[!select1]
```

```{r}
for (indx in seq(1, 9, by= 2)) {
 pred_y <- knn3Train(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
 num_correct_labels <- sum(pred_y == true_data)
 correct_rate <- num_correct_labels/length(true_data)
 print(c(indx,correct_rate))
}
```

```{r}
library(ggplot2)
```

```{r}
p_menhlth_educ <- ggplot(data = brfss_ACE,
                         mapping = aes(x = Educ_number,
                                       y = MENTHLTH,
                                       color = ACESWEAR_recode,
                                       fill = ACESWEAR_recode))
```

```{r}
p_menhlth_educ + geom_point() + 
  geom_smooth(method = "loess") +
  scale_x_log10()
```

```{r}
p_menhlth_income <- ggplot(data = brfss_ACE,
                          mapping = aes(x = income_midpoint,
                                        y = MENTHLTH))


```

```{r}
p_menhlth_income + geom_line(aes(group=ACEDRUGS_recode))
```

```{r}
p_income_mh <- ggplot(data = brfss_ACE,
                      mapping = aes(x =income_midpoint,
                                    y = MENTHLTH))
```

```{r}
p_income_mh + geom_smooth()
```

```{r}
p_hist_mh <- ggplot(data = brfss_ACE,
                    mapping = aes(x = MENTHLTH, fill = EDUCA, color = EDUCA))
```

```{r}
p_hist_mh + geom_density(alpha = 0.2)
```

```{r}
p_hist_mh + geom_freqpoly(bins = 5)
```

```{r}
p_hist_mhage <- ggplot(data = brfss_ACE,
                    mapping = aes(x = MENTHLTH, fill = Age_midpt, color = Age_midpt))
```

```{r}
p_hist_mhage + geom_density(alpha = 0.2)
```

```{r}
p_hist_mhage + geom_freqpoly(bins = 5)
```
