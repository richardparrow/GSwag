# libraries
library(tidyverse)
library(lubridate)


# loading
load("trainPolished.RData")
load("testPolished.RData")

combi = rbind(train, test)


# date things
combi = combi %>%
  mutate(date = date %>% ymd(),
         year = date %>% year(),
         week = date %>% week(),
         wday = date %>% wday())



############################


# re-split in train and test
isTrain = 1:nrow(train)
rm(train, test)
gc()

train = combi[isTrain, ]
test = combi[-isTrain, ]

# save objects as .RData
save(train, file = "trainFeatEng.RData")
save(test, file = "testFeatEng.RData")

