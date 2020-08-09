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

#Format outcome_type to either be "adopted" or "notadopted"
class(filteredOutcomes$outcome_type)
filteredOutcomes$outcome_type <- as.character(filteredOutcomes$outcome_type)
filteredOutcomes$outcome_type[filteredOutcomes$outcome_type != "Adoption"] <- "notAdopted"

filteredOutcomes$outcome_type <- as.factor(filteredOutcomes$outcome_type)
levels(filteredOutcomes$outcome_type)
head(filteredOutcomes)
