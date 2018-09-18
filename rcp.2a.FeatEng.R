# libraries
library(tidyverse)
library(lubridate)
library(urltools)


# loading
load("trainPolished.RData")
load("testPolished.RData")

combi = rbind(train, test)


# date things
combi = combi %>%
  mutate(date = date %>% ymd(),
         year = date %>% year(),
         week = date %>% week(),
         wday = date %>% wday(week_start = getOption("lubridate.week.start", 1))) # così parte dal Lunedì


# .com nel dominio
combi = combi %>%
  mutate(dotCom = networkDomain %>% str_detect(".com"),
         dotNet = networkDomain %>% str_detect(".net"),
         dotComNet = ifelse(dotCom == 0, ifelse(dotNet == 0, "None", "dotNet"), "dotCom") %>% as.factor()) 


# altre robbe nel dominio
domainSplit = lapply(combi[, "networkDomain"], function(x) suffix_extract(domain(x)))

combi = cbind(combi, domainSplit$networkDomain) %>% 
  as.tibble() %>%
  select(-host) %>%
  replace_na(list(subdomain = "Not Available",
                  domain = "Not Available",
                  suffix = "Not Available")) %>%
  mutate(subdomain = subdomain %>% as.factor(),
         domain = domain %>% as.factor(),
         suffix = suffix %>% str_remove_all("com.|net.") %>% as.factor())




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

