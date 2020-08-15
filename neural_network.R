install.packages("caTools")
install.packages("neuralnet")
install.packages("tidyverse")

simplifiedOutcomes <- subset(filteredOutcomes, select = c('animal_type','age','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(simplifiedOutcomes)
str(simplifiedOutcomes)

simplifiedOutcomes$sex_upon_outcome <- as.character(simplifiedOutcomes$sex_upon_outcome)
simplifiedOutcomes$sex_upon_outcome <- as.factor(simplifiedOutcomes$sex_upon_outcome)

summary(simplifiedOutcomes)

nrow(simplifiedOutcomes)

library(tidyverse)

levels(simplifiedOutcomes$animal_type)
levels(simplifiedOutcomes$name)
levels(simplifiedOutcomes$sex_upon_outcome)
levels(simplifiedOutcomes$outcome_type)

numericOutcomes <- simplifiedOutcomes
#Make all the inputs numeric (age already numeric)
numericOutcomes$animal_type <- as.numeric(numericOutcomes$animal_type)
numericOutcomes$name <- as.numeric(numericOutcomes$name)
numericOutcomes$sex_upon_outcome <- as.numeric(numericOutcomes$sex_upon_outcome)
#Turn outcome type into a 0 or 1 (0 for notAdpoted, 1 for Adoption)
library(plyr)
numericOutcomes$outcome_type <- revalue(numericOutcomes$outcome_type, c("Adoption"=1))
numericOutcomes$outcome_type <- revalue(numericOutcomes$outcome_type, c("notAdopted"=0))
numericOutcomes$outcome_type <- as.character(numericOutcomes$outcome_type)

summary(numericOutcomes)
head(numericOutcomes)

#Create vector of column max and min values
maxs <- apply(numericOutcomes[,1:4], 2, max)
mins <- apply(numericOutcomes[,1:4], 2, min)
scaled.data <- as.data.frame(scale(numericOutcomes[,1:4], 
                                   center = mins,
                                   scale = maxs - mins))
print(head(scaled.data, 2))
Adopted = as.numeric(numericOutcomes$outcome_type)
Adopted
data = cbind(Adopted, scaled.data)

library(caTools)
nrow(numericOutcomes)
set.seed(1001)
split = sample.split(data$Adopted, SplitRatio = 0.70)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
feats <- names(scaled.data)
f <- paste(feats, collapse =' + ')
f <- paste('Adopted ~', f)
f <- as.formula(f)
library(neuralnet)
nn <- neuralnet(f, train, hidden = c(3,3), stepmax = 1e7, linear.output = FALSE)
predicted.nn.values <- compute(nn, test[1:4])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result, round, digits = 0)
table(test$Adopted, predicted.nn.values$net.result)
plot(nn)
