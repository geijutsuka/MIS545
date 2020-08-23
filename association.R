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

categoricalOutcomes <- subset(filteredOutcomes, select = c('animal_type','age_range','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(categoricalOutcomes)

assoc_result <- apriori(categoricalOutcomes, parameter = list(sup=0.005, conf=0.5, target = "rules"),
                      appearance = list(default = 'lhs', rhs = c('outcome_type=Adoption', 'outcome_type=notAdopted'))
)

assoc_result
inspect(assoc_result[1:10])
assoc_result <- sort(assoc_result, decreasing = TRUE, by = "support")
top_ten_support <- sort(assoc_result, decreasing = TRUE, by = "support")[1:10]
plot(top_ten_support, shading="lift", control=list(main = "Two-key plot"))

# Adoption result
adoptResult <- apriori(categoricalOutcomes, parameter = list(sup=0.005, conf=0.5, target = "rules"),
                     appearance = list(default = 'lhs', rhs = 'outcome_type=Adoption'))
adoptResult <- sort(adoptResult, decreasing = TRUE, by = "confidence")
inspect(adoptResult[1:5])

# No adoption result
noadoptResult <- apriori(categoricalOutcomes, parameter = list(sup=0.005, conf=0.5, target = "rules"), 
                     appearance = list(default = 'lhs', rhs = 'outcome_type=notAdopted'))
noadoptResult <- sort(noadoptResult, decreasing = TRUE, by = "confidence")
inspect(noadoptResult[1:5])

# Back to top 10 results
plot(top_ten_support, method = "paracoord", shading = "support")

ig <- plot(top_ten_support, method = "graph")
ig_df <- get.data.frame(ig, what = "both")
nodes <- data.frame(id=ig_df$vertices$name,
                    value = ig_df$vertices$support,
                    title = ifelse(ig_df$vertices$label=="", ig_df$vertices$name, ig_df$vertices$label), ig_df$vertices)
edges <- ig_df$edges
network <- visNetwork(nodes, edges) %>%
  visOptions(manipulation = TRUE) %>%  
  visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%  
  visInteraction(navigationButtons = TRUE)  
network



