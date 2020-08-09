#Run preprocessing.R before running this file
head(filteredOutcomes)

#Format name to be "None" or "Named"
class(filteredOutcomes$name)
filteredOutcomes$name <- as.character(filteredOutcomes$name)
class(filteredOutcomes$name)
filteredOutcomes$name[!is.na(filteredOutcomes$name)] <- "Named"
filteredOutcomes$name[is.na(filteredOutcomes$name)] <- "None"
#Turn back into a factor and check summary
filteredOutcomes$name <- as.factor(filteredOutcomes$name)
summary(filteredOutcomes$name)
