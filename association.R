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

assoc_result <- apriori(categoricalOutcomes, parameter = list(sup=0.1, conf=0.5, target = "rules", minlen = 2),
                      appearance = list(default = 'lhs', rhs = c('outcome_type=Adoption', 'outcome_type=notAdopted'))
)

assoc_result
# Output: 37 rules
plot(assoc_result, shading="lift", control=list(main = "Two-key plot"))

inspect(assoc_result[1:5])
top_five_support <- sort(assoc_result, decreasing = TRUE, by = "support")[1:5]
inspect(top_five_support)
top_five_confidence <- sort(assoc_result, decreasing = TRUE, by = "confidence")[1:5]
inspect(top_five_confidence)
plot(top_five_support, shading="lift", control=list(main = "Two-key plot sorted by support"))
plot(top_five_confidence, shading="lift", control=list(main = "Two-key plot sorted by confidence"))

# Adoption result
adoptResult <- apriori(categoricalOutcomes, parameter = list(sup=0.1, conf=0.5, target = "rules"),
                     appearance = list(default = 'lhs', rhs = 'outcome_type=Adoption'))
adoptResult <- sort(adoptResult, decreasing = TRUE, by = "confidence")
inspect(adoptResult[1:5])

# No adoption result
noadoptResult <- apriori(categoricalOutcomes, parameter = list(sup=0.1, conf=0.5, target = "rules"), 
                     appearance = list(default = 'lhs', rhs = 'outcome_type=notAdopted'))
noadoptResult <- sort(noadoptResult, decreasing = TRUE, by = "confidence")
inspect(noadoptResult[1:5])

# Back to top 5 results
plot(top_five_support, method = "paracoord", shading = "support", control=list(main = "Parallel coordinates for top five support rules"))

ig_sup <- plot(top_five_support, method = "graph", control=list(main = "Graph for top five support rules"))
# Data sorted by support is cleaner graph

ig_sup_df <- get.data.frame(ig_sup, what = "both")
nodes <- data.frame(id=ig_sup_df$vertices$name,
                    value = ig_sup_df$vertices$support,
                    title = ifelse(ig_sup_df$vertices$label=="", ig_sup_df$vertices$name, ig_sup_df$vertices$label), ig_sup_df$vertices)
edges <- ig_sup_df$edges
network_sup <- visNetwork(nodes, edges) %>%
  visOptions(manipulation = TRUE) %>%  
  visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%  
  visInteraction(navigationButtons = TRUE)  
network_sup



