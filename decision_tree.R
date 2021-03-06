install.packages("C50")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)

narrowedOutcomes <- subset(filteredOutcomes, select = c('animal_type','age_range','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(narrowedOutcomes)

narrowedOutcomes$animal_type <- as.factor(narrowedOutcomes$animal_type)
narrowedOutcomes$name <- as.factor(narrowedOutcomes$name)
narrowedOutcomes$sex_upon_outcome <- as.factor(narrowedOutcomes$sex_upon_outcome)
narrowedOutcomes$outcome_type <- as.factor(narrowedOutcomes$outcome_type)

levels(narrowedOutcomes$sex_upon_outcome)
levels(narrowedOutcomes$outcome_type)

# Sample and training data creation
seq_len(5)
sample(seq_len(5), 3)
dt_sample_size <- floor(0.8 * nrow(narrowedOutcomes))
dt_training_index <- sample(seq_len(nrow(narrowedOutcomes)), size = dt_sample_size)
dt_train <- narrowedOutcomes[dt_training_index,]
dt_test <- narrowedOutcomes[-dt_training_index,]
dt_predictors <- c('animal_type','age_range','name','sex_upon_outcome')

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
#output: 0.8102 (81% correct)

table("Reference"=dt_evaluation$outcome_type, "Prediction"=dt_evaluation$pred)

dt_TPR <- sum(dt_evaluation$pred == 'Adoption' & dt_evaluation$outcome_type == 'Adoption')/sum(dt_evaluation$outcome_type == 'Adoption')
dt_TNR <- sum(dt_evaluation$pred == 'notAdopted' & dt_evaluation$outcome_type == 'notAdopted')/sum(dt_evaluation$outcome_type == 'notAdopted')
dt_FPR <- 1 - dt_TNR
dt_FNR <- 1 - dt_TPR
tree_precision <- sum(dt_evaluation$outcome_type == 'Adoption' & dt_evaluation$pred == 'Adoption')/sum(dt_evaluation$pred == 'Adoption')
tree_precision
# Output: 0.7743
tree_recall <- sum(dt_evaluation$outcome_type == 'Adoption' & dt_evaluation$pred == 'Adoption')/sum(dt_evaluation$outcome_type == 'Adoption')
tree_recall
# Output: 0.9456
Fscore <- 2 * tree_precision * tree_recall / (tree_precision + tree_recall)
Fscore
# Output: 0.8514

reg <- glm(outcome_type ~ . , data = dt_train, family = binomial())
summary(reg)

dt_evaluation <- dt_test
dt_evaluation$prob <- predict(reg, newdata = dt_evaluation, type = "response")

count_adoptions <- nrow(subset(narrowedOutcomes, narrowedOutcomes$outcome_type == "Adoption"))
dt_baseline <- count_adoptions / nrow(narrowedOutcomes)
dt_baseline
# Output: 0.5785
g <- roc(dt_evaluation$outcome_type ~ dt_evaluation$prob, data = dt_evaluation)
plot(g)
g
# Output:
# AUC: 0.8405
# Data: dt_evaluation$prob in 6638 controls (dt_evaluation$outcome_type Adoption) < 4836 cases (dt_evaluation$outcome_type notAdopted).