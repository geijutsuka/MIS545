install.packages("caTools")
install.packages("neuralnet")
install.packages("plyr")
library(caTools)
library(neuralnet)
library(plyr)

#leaving out dates and breed due to too many variations
simplifiedOutcomes <- subset(filteredOutcomes, select = c('animal_type','age_years','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(simplifiedOutcomes)

#Make all the inputs into integers (age_years already integer)
integerOutcomes <- simplifiedOutcomes
integerOutcomes$animal_type <- as.integer(integerOutcomes$animal_type)
integerOutcomes$name <- as.integer(integerOutcomes$name)
integerOutcomes$sex_upon_outcome <- as.integer(integerOutcomes$sex_upon_outcome)

#Turn outcome type into a 0 or 1 (0 for notAdpoted, 1 for Adoption) plyr needed
integerOutcomes$outcome_type <- revalue(integerOutcomes$outcome_type, c("Adoption"=1))
integerOutcomes$outcome_type <- revalue(integerOutcomes$outcome_type, c("notAdopted"=0))
integerOutcomes$outcome_type <- as.character(integerOutcomes$outcome_type)
summary(integerOutcomes)
head(integerOutcomes)

#Create vector of column max and min values
maxs <- apply(integerOutcomes[,1:4], 2, max)
mins <- apply(integerOutcomes[,1:4], 2, min)

scaled.data <- as.data.frame(scale(integerOutcomes[,1:4], 
                                   center = mins,
                                   scale = maxs - mins))
print(head(scaled.data, 2))
Adopted = as.integer(integerOutcomes$outcome_type)
Adopted
data = cbind(Adopted, scaled.data)

set.seed(101)
split = sample.split(data$Adopted, SplitRatio = 0.70)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
feats <- names(scaled.data)
f <- paste(feats, collapse =' + ')
f <- paste('Adopted ~', f)
f <- as.formula(f)

nn <- neuralnet(f, train, hidden = c(3), stepmax = 1e10, linear.output = FALSE)
predicted.nn.values <- compute(nn, test[1:4])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result, round, digits = 0)
table(test$Adopted, predicted.nn.values$net.result)
plot(nn)
summary(nn)

# Evaluate the effectiveness:
nn_predict <- predicted.nn.values$net.result
nn_evalu <- cbind(test, nn_predict)
head(nn_evalu)
nn_evalu$correct <- ifelse(nn_evalu$Adopted == nn_evalu$nn_predict, 1,0)
head(nn_evalu)
sum(nn_evalu$correct)/nrow(nn_evalu)
# Output: 0.5155 (52% correct)

nn_TPR <- sum(nn_evalu$nn_predict == 1 & nn_evalu$Adopted == 1)/sum(nn_evalu$Adopted == 1)
nn_TPR
nn_TNR <- sum(nn_evalu$nn_predict == 0 & nn_evalu$Adopted == 0)/sum(nn_evalu$Adopted == 0)
nn_FPR <- 1 - nn_TNR
nn_FNR <- 1 - nn_TPR
nn_precision <- sum(nn_evalu$Adopted == 1 & nn_evalu$nn_predict == 1)/sum(nn_evalu$nn_predict == 1)
nn_precision
# Output: 0.9963
nn_recall <- sum(nn_evalu$Adopted == 1 & nn_evalu$nn_predict == 1)/sum(nn_evalu$Adopted == 1)
nn_recall
# Output: 0.1632
nn_Fscore <- 2 * nn_precision * nn_recall / (nn_precision + nn_recall)
nn_Fscore
# Output: 0.2805

# NOT WORKING CORRECTLY:
nn_reg <- glm(outcome_type ~ . , data = simplifiedOutcomes, family = binomial())
summary(nn_reg)

nn_evalu$prob <- predict(nn_reg, newdata = test, type = "response")

adoption_total <- nrow(subset(simplifiedOutcomes, simplifiedOutcomes$outcome_type == "Adoption"))
adoption_total
nn_baseline <- adoption_total / nrow(simplifiedOutcomes)
nn_baseline
# Output: 0.5785

nn_rocplot <- roc(nn_evalu$Adopted ~ nn_evalu$prob, data = nn_evalu)
plot(nn_rocplot)
nn_rocplot
