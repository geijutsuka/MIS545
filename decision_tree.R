install.packages("C50")
install.packages("tidyverse")
library(C50)
library(tidyverse)
head(filteredOutcomes)
nodatesOutcomes <- subset(filteredOutcomes, select = c('animal_type','breed','color', 'age','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(nodatesOutcomes)

nodatesOutcomes$sex_upon_outcome <- as.character(nodatesOutcomes$sex_upon_outcome)

nodatesOutcomes$animal_type <- as.factor(nodatesOutcomes$animal_type)
nodatesOutcomes$breed <- as.factor(nodatesOutcomes$breed)
nodatesOutcomes$color <- as.factor(nodatesOutcomes$color)
nodatesOutcomes$age <- as.factor(nodatesOutcomes$age)
nodatesOutcomes$sex_upon_outcome <- as.factor(nodatesOutcomes$sex_upon_outcome)
nodatesOutcomes$outcome_type <- as.factor(nodatesOutcomes$outcome_type)
nrow(nodatesOutcomes)
head(nodatesOutcomes)
levels(nodatesOutcomes$sex_upon_outcome)

summary(nodatesOutcomes$sex_upon_outcome)

seq_len(5)
sample(seq_len(5), 3)
sample_size <- floor(0.8 * nrow(nodatesOutcomes))
training_index <- sample(seq_len(nrow(nodatesOutcomes)), size = sample_size)
train <- nodatesOutcomes[training_index,]
test <- nodatesOutcomes[-training_index,]
predictors <- c('animal_type','age','sex_upon_outcome')
model <- C5.0(x = train[,predictors], y = train$outcome_type)
summary(model)
plot(model)
