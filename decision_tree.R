install.packages("C50")
install.packages("tidyverse")
install.packages("pROC")
library(C50)
library(tidyverse)
library(pROC)

head(filteredOutcomes)
nodatesOutcomes <- subset(filteredOutcomes, select = c('animal_type','breed','color','age_years','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(nodatesOutcomes)

nodatesOutcomes$sex_upon_outcome <- as.character(nodatesOutcomes$sex_upon_outcome)

nodatesOutcomes$animal_type <- as.factor(nodatesOutcomes$animal_type)
nodatesOutcomes$breed <- as.factor(nodatesOutcomes$breed)
nodatesOutcomes$color <- as.factor(nodatesOutcomes$color)
nodatesOutcomes$age_years <- as.integer(nodatesOutcomes$age_years)
nodatesOutcomes$name <- as.factor(nodatesOutcomes$name)
nodatesOutcomes$sex_upon_outcome <- as.factor(nodatesOutcomes$sex_upon_outcome)
nodatesOutcomes$outcome_type <- as.factor(nodatesOutcomes$outcome_type)
nrow(nodatesOutcomes)
head(nodatesOutcomes)
levels(nodatesOutcomes$sex_upon_outcome)
levels(nodatesOutcomes$outcome_type)
summary(nodatesOutcomes$sex_upon_outcome)

seq_len(5)
sample(seq_len(5), 3)
sample_size <- floor(0.8 * nrow(nodatesOutcomes))
training_index <- sample(seq_len(nrow(nodatesOutcomes)), size = sample_size)
train <- nodatesOutcomes[training_index,]
test <- nodatesOutcomes[-training_index,]
predictors <- c('animal_type','age_years','name','sex_upon_outcome')
# predictors <- names(nodatesOutcomes)[-7]
predictors
model <- C5.0(x = train[, predictors], y = train$outcome_type)
summary(model)
plot(model)
pred <- predict(model, newdata = test)
evaluation <- cbind(test, pred)
head(evaluation)
evaluation$correct <- ifelse(evaluation$outcome_type == evaluation$pred, 1,0)
head(evaluation)
sum(evaluation$correct)/nrow(evaluation)
#output: 0.7984138 (80% correct)

table(evaluation$outcome_type, evaluation$pred)

TPR <- sum(evaluation$pred == 'Adoption' & evaluation$outcome_type == 'Adoption')/sum(evaluation$outcome_type == 'Adoption')
TNR <- sum(evaluation$pred == 'notAdopted' & evaluation$outcome_type == 'notAdopted')/sum(evaluation$outcome_type == 'notAdopted')
FPR <- 1 - TNR
FNR <- 1 - TPR
tree_precision <- sum(evaluation$outcome_type == 'Adoption' & evaluation$pred == 'Adoption')/sum(evaluation$pred == 'Adoption')
tree_precision
# Output: 0.7745
tree_recall <- sum(evaluation$outcome_type == 'Adoption' & evaluation$pred == 'Adoption')/sum(evaluation$outcome_type == 'Adoption')
tree_recall
# Output: 0.9644
Fscore <- 2 * tree_precision * tree_recall / (tree_precision + tree_recall)
Fscore
# Output: 0.8591

reg <- glm(outcome_type ~ . , data = train, family = binomial())
summary(reg)

evaluation <- test
evaluation$prob <- predict(reg, newdata = evaluation, type = "response")

count_adoptions <- nrow(subset(nodatesOutcomes, nodatesOutcomes$outcome_type == "Adoption"))
baseline <- count_adoptions / nrow(nodatesOutcomes)
baseline
g <- roc(evaluation$outcome_type ~ evaluation$prob, data = evaluation)
plot(g)
g
