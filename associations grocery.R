library(arules)
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)

itemFrequencyPlot(groceries,topN=20)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=2,maxlen=3))

library(arulesViz)
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")

groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

library(arulesViz)
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
inspect(groceries_rules[1:4])

groceries_rules <- sort(groceries_rules,by="lift")

inspect(groceries_rules[1:4])

