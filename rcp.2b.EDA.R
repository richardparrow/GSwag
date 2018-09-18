# libraries
library(ggplot2)


# data
load("trainPolished.RData")


# temporal element
ggplot(train) + aes(x = date, y = transactionRevenue, fill = continent) + 
                geom_point(alpha = .4) +
                geom_smooth(method = "lm")
                                                              