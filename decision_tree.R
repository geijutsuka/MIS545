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
nodatesOutcomes <- subset(filteredOutcomes, select = c('animal_type','breed','color','age_years','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(nodatesOutcomes)

nodatesOutcomes$sex_upon_outcome <- as.character(nodatesOutcomes$sex_upon_outcome)

nodatesOutcomes$animal_type <- as.factor(nodatesOutcomes$animal_type)
nodatesOutcomes$breed <- as.factor(nodatesOutcomes$breed)
nodatesOutcomes$color <- as.factor(nodatesOutcomes$color)
#nodatesOutcomes$age_years <- as.integer(nodatesOutcomes$age_years)
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
dt_sample_size <- floor(0.8 * nrow(nodatesOutcomes))
dt_training_index <- sample(seq_len(nrow(nodatesOutcomes)), size = dt_sample_size)
dt_train <- nodatesOutcomes[dt_training_index,]
dt_test <- nodatesOutcomes[-dt_training_index,]
dt_predictors <- c('animal_type','age_years','name','sex_upon_outcome')
# dt_predictors <- names(nodatesOutcomes)[-7]
dt_predictors
dt_model <- C5.0(x = dt_train[, dt_predictors], y = dt_train$outcome_type)
summary(dt_model)
plot(dt_model)
#rpart.plot(dt_model)
pred <- predict(dt_model, newdata = dt_test)
dt_evaluation <- cbind(dt_test, pred)
head(dt_evaluation)
dt_evaluation$correct <- ifelse(dt_evaluation$outcome_type == dt_evaluation$pred, 1,0)
head(dt_evaluation)
sum(dt_evaluation$correct)/nrow(dt_evaluation)
#output: 0.7984138 (80% correct)

table(dt_evaluation$outcome_type, dt_evaluation$pred)

dt_TPR <- sum(dt_evaluation$pred == 'Adoption' & dt_evaluation$outcome_type == 'Adoption')/sum(dt_evaluation$outcome_type == 'Adoption')
dt_TNR <- sum(dt_evaluation$pred == 'notAdopted' & dt_evaluation$outcome_type == 'notAdopted')/sum(dt_evaluation$outcome_type == 'notAdopted')
dt_FPR <- 1 - dt_TNR
dt_FNR <- 1 - dt_TPR
tree_precision <- sum(dt_evaluation$outcome_type == 'Adoption' & dt_evaluation$pred == 'Adoption')/sum(dt_evaluation$pred == 'Adoption')
tree_precision
# Output: 0.7791
tree_recall <- sum(dt_evaluation$outcome_type == 'Adoption' & dt_evaluation$pred == 'Adoption')/sum(dt_evaluation$outcome_type == 'Adoption')
tree_recall
# Output: 0.9427
Fscore <- 2 * tree_precision * tree_recall / (tree_precision + tree_recall)
Fscore
# Output: 0.8531

reg <- glm(outcome_type ~ . , data = dt_train, family = binomial())
summary(reg)

dt_evaluation <- dt_test
dt_evaluation$prob <- predict(reg, newdata = dt_evaluation, type = "response")

count_adoptions <- nrow(subset(nodatesOutcomes, nodatesOutcomes$outcome_type == "Adoption"))
dt_baseline <- count_adoptions / nrow(nodatesOutcomes)
dt_baseline
g <- roc(dt_evaluation$outcome_type ~ dt_evaluation$prob, data = dt_evaluation)
plot(g)
g
