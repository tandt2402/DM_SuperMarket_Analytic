# install.packages("tidyr", repos = "http://cran.us.r-project.org")
# install.packages("arules", repos = "http://cran.us.r-project.org")
# install.packages("arulesViz", repos = "http://cran.us.r-project.org")
# install.packages("methods", repos = "http://cran.us.r-project.org")

library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)

ordr_pr <- read_csv("./input/order_products__prior.csv")
prods <- read_csv("./input/products.csv")

# get the shopping baskets
order_baskets <- ordr_pr %>%
  inner_join(prods, by = "product_id") %>%
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

# compute transactions
transactions <- as(order_baskets$basket, "transactions")

item_frequencies <- itemFrequency(transactions, type = "a")
support <- 0.02
freq_items <- sort(item_frequencies, decreasing = F)
freq_items <- freq_items[freq_items > support * length(transactions)]

par(mar = c(2, 10, 2, 2));
options(scipen = 5)
barplot(freq_items, horiz = T, las = 1, main = "Frequent Items", cex.names = .8, xlim = c(0, 500000))
mtext(paste("support:", support), padj = .8)
abline(v = support * length(transactions), col = "red")

support <- 0.008
itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp = support, minlen = 2), control = list(verbose = FALSE))

par(mar = c(5, 18, 2, 2) + .1)
sets_order_supp <- DATAFRAME(sort(itemsets, by = "support", decreasing = F))
barplot(sets_order_supp$support, names.arg = sets_order_supp$items, xlim = c(0, 0.02), horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
mtext(paste("support:", support), padj = .8)

rules1 <- apriori(transactions, parameter = list(supp = 0.00001, conf = 0.6, maxlen = 3), control = list(verbose = FALSE))
summary(quality(rules1))
inspect(sort(rules1, by = "lift")[1:10])
inspect(sort(rules1, by = "confidence")[1:10])

rules2 <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.4, maxlen = 3), control = list(verbose = FALSE))
summary(quality(rules2))
inspect(sort(rules2, by = "lift")[1:10])
inspect(sort(rules2, by = "confidence")[1:10])

rules3 <- apriori(transactions, parameter = list(supp = 0.005, conf = 0.1, maxlen = 3), control = list(verbose = FALSE))
summary(quality(rules3))
inspect(sort(rules3, by = "lift")[1:10])
inspect(sort(rules3, by = "confidence")[1:10])
