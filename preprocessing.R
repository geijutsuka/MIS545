#import data and set NA for blanks and nulls:
shelterOutcomes <- read.csv('aac_shelter_outcomes.csv', stringsAsFactors = TRUE, header = TRUE, na.strings=c(""," ","NA"))
nrow(shelterOutcomes)
head(shelterOutcomes)

#See complete cases:
#nrow(shelterOutcomes[complete.cases(shelterOutcomes),])

#Simplify based on outcome_type data
summary(shelterOutcomes$outcome_type)
#Mark Rto-Adopt as an Adoption
shelterOutcomes$outcome_type[shelterOutcomes$outcome_type == "Rto-Adopt"] <- "Adoption"
#Check to see if Rto-Adopt values are gone 
summary(shelterOutcomes$outcome_type)
#Remove rows with "return to owner" and "NA" in outcome_type
shelterOutcomes <- subset(shelterOutcomes, outcome_type != "Return to Owner")
shelterOutcomes <- subset(shelterOutcomes, outcome_type != "NA")
summary(shelterOutcomes$outcome_type)

#Remove rows with "Unknown" and NULL in sex_upon_outcome
shelterOutcomes <- subset(shelterOutcomes, sex_upon_outcome != "Unknown")
shelterOutcomes <- subset(shelterOutcomes, sex_upon_outcome != "NULL")
summary(shelterOutcomes$sex_upon_outcome)

#Make sure removed data is minimal (less than 50% of 78256)
nrow(shelterOutcomes)

#Remove the unneeded columns monthyear, animal_id, age_upon_outcome (age will be calculated later in transformation)
filteredOutcomes <- subset(shelterOutcomes, select = -c(monthyear, animal_id, age_upon_outcome, outcome_subtype))
head(filteredOutcomes)
