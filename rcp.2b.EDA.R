# libraries
library(ggplot2)


# data
load("trainPolished.RData")


# elemento tempo - settimane
train %>% 
  group_by(week) %>% 
  summarise(mean = transactionRevenue %>% mean()) %>%
  ggplot() + aes(x = week, y = mean) + geom_point() + geom_smooth(method = "loess")

# giorni della settimana
train %>% 
  group_by(wday) %>% 
  summarise(mean = transactionRevenue %>% mean()) %>%
  ggplot() + aes(x = wday, y = mean) + geom_point() + geom_smooth(method = "lm")

# Sembra che un minimo di trend temporale ci sia e più che altro si cala nel week-end.


# .com - per ora commentato perché suffix dovrebbe fare tutto
#train %>% 
#  group_by(week, dotCom) %>% 
#  summarise(mean = transactionRevenue %>% mean()) %>%
#  ggplot() + aes(x = week, y = mean, colour = dotCom, fill = dotCom) + geom_point() + geom_smooth(method = "loess", alpha = .2)

# .net
#train %>% 
#  group_by(week, dotNet) %>% 
#  summarise(mean = transactionRevenue %>% mean()) %>%
#  ggplot() + aes(x = week, y = mean, colour = dotNet, fill = dotNet) + geom_point() + geom_smooth(method = "loess", alpha = .2)

# .com & .net ecc.
train %>% 
  group_by(week, dotSomething) %>% 
  summarise(mean = transactionRevenue %>% mean()) %>%
  ggplot() + aes(x = week, y = mean, colour = dotSomething, fill = dotSomething) + geom_point() + geom_smooth(method = "loess", alpha = .2)

# Tra domainNetwork .com vs altri c'è molta separazione.


# suffix
train %>%
  mutate(suffixLump = suffix %>% fct_lump(n = 9)) %>%
  group_by(week, suffixLump) %>%
  summarise(mean = transactionRevenue %>% mean()) %>%
  ggplot() + aes(x = week, y = mean, colour = suffixLump, fill = suffixLump) + geom_point() + geom_smooth(method = "loess", alpha = .2)

# suffissi diversi hanno trend molto diversi, potrebbe esserci una connessione interessante.

# suffixes che hanno ritorno più alto
# il problema è che qui coi boplot non si vede una fava perché quasi tutte le transazioni hanno valore 0
suffixMean = combi[1:nrow(train), ] %>%
  group_by(suffix) %>%
  summarise(transactionMean = transactionRevenue %>% mean())

suffixMean[order(suffixMean$transactionMean, decreasing = T)[1:20], ]

suffixNonzero = suffixMean[which(suffixMean$transactionMean != 0), ]

ggplot(suffixNonzero) + aes(x = suffix, y = transactionMean, fill = transactionMean) + geom_bar(stat = "identity") + coord_flip()



