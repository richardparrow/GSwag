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


# .com nel dominio - per ora commentato perché suffix dovrebbe fare tutto
#combi = combi %>%
#  mutate(dotCom = networkDomain %>% str_detect(".com"),
#         dotNet = networkDomain %>% str_detect(".net"),
#         dotComNet = ifelse(dotCom == 0, ifelse(dotNet == 0, "None", "dotNet"), "dotCom") %>% as.factor()) 


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


# tengo solo i suffissi che hanno mean(target) != 0
suffixMean = combi[1:nrow(train), ] %>%
  group_by(suffix) %>%
  summarise(transactionMean = transactionRevenue %>% mean())

suffixOther = as.character(suffixMean$suffix[which(suffixMean$transactionMean == 0)])

combi = combi %>%
  mutate(suffix = suffix %>% fct_collapse(other = suffixOther))


# hist e pageviews dominano, provo a fare un po' di shit
group_mean <- function(x, group) ave(x, group, FUN = function(x) mean(x, na.rm = TRUE))

combi = combi %>%
  mutate(hits.mean.visitNumber = group_mean(hits, visitNumber),
         hits.mean.browser = group_mean(hits, browser),
         hits.mean.operatingSystem = group_mean(hits, operatingSystem),
         hits.mean.subContinent = group_mean(hits, subContinent),
         hits.mean.country = group_mean(hits, country),
         hits.mean.city = group_mean(hits, city),
         hits.mean.metro = group_mean(hits, metro),
         hits.mean.suffix = group_mean(hits, suffix),
         
         pageviews.mean.visitNumber = group_mean(pageviews, visitNumber),
         pageviews.mean.browser = group_mean(pageviews, browser),
         pageviews.mean.operatingSystem = group_mean(pageviews, operatingSystem),
         pageviews.mean.subContinent = group_mean(pageviews, subContinent),
         pageviews.mean.country = group_mean(pageviews, country),
         pageviews.mean.city = group_mean(pageviews, city),
         pageviews.mean.metro = group_mean(pageviews, metro),
         pageviews.mean.suffix = group_mean(pageviews, suffix))



# grabbo le ore dall'orario di visita
combi = combi %>%
  mutate(visitStartHour = visitStartTime %>% as.POSIXct(., origin = "1970-01-01") %>% hour())




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

