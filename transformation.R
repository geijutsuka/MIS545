#Run preprocessing.R before running this file
head(filteredOutcomes)

#Calculate the age from the difference of datetime of the outcome and DOB and format the dates
filteredOutcomes$age_days <- as.Date(as.character(filteredOutcomes$datetime), format="%Y-%m-%d")-
  as.Date(as.character(filteredOutcomes$date_of_birth), format="%Y-%m-%d")
filteredOutcomes$datetime <- as.Date(as.character(filteredOutcomes$datetime), format="%Y-%m-%d")
filteredOutcomes$date_of_birth <- as.Date(as.character(filteredOutcomes$date_of_birth), format="%Y-%m-%d")
head(filteredOutcomes)
#Make the age numeric, get rid of negative ages, and divide the amount of days into years
filteredOutcomes$age_days <- as.integer(filteredOutcomes$age_days)
min(filteredOutcomes$age_days)
filteredOutcomes <- filteredOutcomes[filteredOutcomes$age_days > 0, ]
#break into years:
filteredOutcomes$age_years <- filteredOutcomes$age_days/365
#break into weeks: filteredOutcomes$age_weeks <- filteredOutcomes$age_days/7
filteredOutcomes$age_years <- round(filteredOutcomes$age_years, digits = 0)
filteredOutcomes$age_days <- NULL
filteredOutcomes$age_years <- as.integer(filteredOutcomes$age_years)
head(filteredOutcomes)

#Reorganize the columns to move age and sex from the end and place outcome type last
filteredOutcomes <- filteredOutcomes[c(1,2,3,4,5,9,6,8,7)]

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

