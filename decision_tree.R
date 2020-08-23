install.packages("C50")
install.packages("tidyverse")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(C50)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(pROC)

head(filteredOutcomes)
narrowedOutcomes <- subset(filteredOutcomes, select = c('animal_type','age_years','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(narrowedOutcomes)

narrowedOutcomes$sex_upon_outcome <- as.character(narrowedOutcomes$sex_upon_outcome)

narrowedOutcomes$animal_type <- as.factor(narrowedOutcomes$animal_type)
narrowedOutcomes$breed <- as.factor(narrowedOutcomes$breed)
narrowedOutcomes$color <- as.factor(narrowedOutcomes$color)
#narrowedOutcomes$age_years <- as.integer(narrowedOutcomes$age_years)
narrowedOutcomes$name <- as.factor(narrowedOutcomes$name)
narrowedOutcomes$sex_upon_outcome <- as.factor(narrowedOutcomes$sex_upon_outcome)
narrowedOutcomes$outcome_type <- as.factor(narrowedOutcomes$outcome_type)
nrow(narrowedOutcomes)
head(narrowedOutcomes)
levels(narrowedOutcomes$sex_upon_outcome)
levels(narrowedOutcomes$outcome_type)
summary(narrowedOutcomes$sex_upon_outcome)

seq_len(5)
sample(seq_len(5), 3)
dt_sample_size <- floor(0.8 * nrow(narrowedOutcomes))
dt_training_index <- sample(seq_len(nrow(narrowedOutcomes)), size = dt_sample_size)
dt_train <- narrowedOutcomes[dt_training_index,]
dt_test <- narrowedOutcomes[-dt_training_index,]
dt_predictors <- c('animal_type','age_years','name','sex_upon_outcome')
# Plot default decision tree
dt_model <- C5.0(x = dt_train[, dt_predictors], y = dt_train$outcome_type)
summary(dt_model)
plot(dt_model)
# Plot more readable rpart plot
binary.model <- rpart(outcome_type ~ ., data = dt_train)
rpart.plot(binary.model, type = 5, extra = "auto", fallen.leaves = TRUE)

# Evaluate the effectiveness:
pred <- predict(dt_model, newdata = dt_test)
dt_evaluation <- cbind(dt_test, pred)
head(dt_evaluation)
dt_evaluation$correct <- ifelse(dt_evaluation$outcome_type == dt_evaluation$pred, 1,0)
head(dt_evaluation)
sum(dt_evaluation$correct)/nrow(dt_evaluation)
#output: 0.8137 (81% correct)

table("Reference"=dt_evaluation$outcome_type, "Prediction"=dt_evaluation$pred)

dt_TPR <- sum(dt_evaluation$pred == 'Adoption' & dt_evaluation$outcome_type == 'Adoption')/sum(dt_evaluation$outcome_type == 'Adoption')
dt_TNR <- sum(dt_evaluation$pred == 'notAdopted' & dt_evaluation$outcome_type == 'notAdopted')/sum(dt_evaluation$outcome_type == 'notAdopted')
dt_FPR <- 1 - dt_TNR
dt_FNR <- 1 - dt_TPR
tree_precision <- sum(dt_evaluation$outcome_type == 'Adoption' & dt_evaluation$pred == 'Adoption')/sum(dt_evaluation$pred == 'Adoption')
tree_precision
# Output: 0.7798
tree_recall <- sum(dt_evaluation$outcome_type == 'Adoption' & dt_evaluation$pred == 'Adoption')/sum(dt_evaluation$outcome_type == 'Adoption')
tree_recall
# Output: 0.9447
Fscore <- 2 * tree_precision * tree_recall / (tree_precision + tree_recall)
Fscore
# Output: 0.8531

reg <- glm(outcome_type ~ . , data = dt_train, family = binomial())
summary(reg)

dt_evaluation <- dt_test
dt_evaluation$prob <- predict(reg, newdata = dt_evaluation, type = "response")

count_adoptions <- nrow(subset(narrowedOutcomes, narrowedOutcomes$outcome_type == "Adoption"))
dt_baseline <- count_adoptions / nrow(narrowedOutcomes)
dt_baseline
g <- roc(dt_evaluation$outcome_type ~ dt_evaluation$prob, data = dt_evaluation)
plot(g)
g
# AUC: 0.842