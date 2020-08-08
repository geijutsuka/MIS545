install.packages("C50")
library(C50)
head(narrowedOutcomes)
nodateOutcomes <- subset(narrowedOutcomes, select = c('animal_type','breed','color', 'age','sex_upon_outcome','outcome_type'))

nodateOutcomes$animal_type <- as.factor(nodateOutcomes$animal_type)
nodateOutcomes$breed <- as.factor(nodateOutcomes$breed)
nodateOutcomes$color <- as.factor(nodateOutcomes$color)
nodateOutcomes$age <- as.factor(nodateOutcomes$age)
nodateOutcomes$sex_upon_outcome <- as.factor(nodateOutcomes$sex_upon_outcome)
nodateOutcomes$outcome_type <- as.factor(nodateOutcomes$outcome_type)
nrow(nodateOutcomes)
head(nodateOutcomes)

seq_len(5)
sample(seq_len(5), 3)
sample_size <- floor(0.8 * nrow(nodateOutcomes))
training_index <- sample(seq_len(nrow(nodateOutcomes)), size = sample_size)
train <- nodateOutcomes[training_index,]
test <- nodateOutcomes[-training_index,]
predictors <- c('animal_type','age','sex_upon_outcome')
model <- C5.0(x = train[,predictors], y = train$nodateOutcomes)

