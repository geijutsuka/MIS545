shelterOutcomes <- read.csv('aac_shelter_outcomes.csv', stringsAsFactors = TRUE, header = TRUE, na.strings=c(""," ","NA"))
nrow(shelterOutcomes)
nrow(shelterOutcomes[complete.cases(shelterOutcomes),])
head(shelterOutcomes)
#mark Rto-adopt as an adoption
shelterOutcomes$outcome_type[shelterOutcomes$outcome_type == "Rto-Adopt"] <- "Adoption"
summary(shelterOutcomes$outcome_type)
#remove rows with "return to owner" in outcome_type
noReturns <- subset(shelterOutcomes, outcome_type != "Return to Owner")
nrow(noReturns)
#remove rows with "Unknown" for sex_upon_outcome
noSexorReturn <- subset(noReturns, sex_upon_outcome != "Unknown")
nrow(noSexorReturn)
summary(noSexorReturn)
#remove the columns monthyear and animal_id
filteredOutcomes <- subset(noSexorReturn, select = -c(monthyear, animal_id))
head(filteredOutcomes)


narrowedOutcomes <- subset(filteredOutcomes, select = c('animal_type','breed','color', 'date_of_birth', 'datetime', 'sex_upon_outcome','outcome_type'))
head(narrowedOutcomes)

narrowedOutcomes$age <- as.Date(as.character(narrowedOutcomes$datetime), format="%Y-%m-%d")-
                      as.Date(as.character(narrowedOutcomes$date_of_birth), format="%Y-%m-%d")

narrowedOutcomes <- narrowedOutcomes[c(1,2,3,4,5,8,6,7)]

narrowedOutcomes$datetime <- as.Date(as.character(narrowedOutcomes$datetime), format="%Y-%m-%d")
narrowedOutcomes$date_of_birth <- as.Date(as.character(narrowedOutcomes$date_of_birth), format="%Y-%m-%d")

head(narrowedOutcomes)
narrowedOutcomes

