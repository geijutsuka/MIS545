install.packages("arules")
install.packages("arulesViz", dependencies = TRUE)
install.packages("igraph")
install.packages("visNetwork")
install.packages("plyr")
library(arules)
library(arulesViz)
library(igraph)
library(visNetwork)
library(plyr)

#check data:
head(filteredOutcomes)
adoptionRecord <- subset(filteredOutcomes, select = c('animal_type','age_years','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(adoptionRecord)

#NOT WORKING:
result <- apriori(adoptionRecord, parameter = list(sup = 0.35, conf = 0.8, target = "rules"),
                  appearance = list(default = 'lhs', rhs = c('outcome_type=Adoption', 'outcome_type=notAdopted'))
)
result
result <- sort(result, decreasing = TRUE, by = "support")
inspect(result[1:5])
top_five <- sort(result, decreasing = TRUE, by = "support")[1:2]
plot(top_five, shading="lift", control=list(main = "Two-key plot"))