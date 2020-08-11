install.packages("caTools")
install.packages("neuralnet")
install.packages("tidyverse")

slimmedOutcomes <- subset(filteredOutcomes, select = c('animal_type', 'age','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(slimmedOutcomes)
str(slimmedOutcomes)

slimmedOutcomes$sex_upon_outcome <- as.character(slimmedOutcomes$sex_upon_outcome)
slimmedOutcomes$sex_upon_outcome <- as.factor(slimmedOutcomes$sex_upon_outcome)

summary(slimmedOutcomes)

slimmedOutcomes$age <- as.numeric(slimmedOutcomes$age)
min(slimmedOutcomes$age)

slimmedOutcomes <- slimmedOutcomes[slimmedOutcomes$age > 0, ]

slimmedOutcomes$age <- slimmedOutcomes$age/365

nrow(slimmedOutcomes)

library(tidyverse)

levels(slimmedOutcomes$animal_type)
levels(slimmedOutcomes$age)
levels(slimmedOutcomes$sex_upon_outcome)
levels(slimmedOutcomes$outcome_type)

numericOutcomes <- slimmedOutcomes
#Make all the inputs numeric
numericOutcomes$animal_type <- as.numeric(numericOutcomes$animal_type)
numericOutcomes$sex_upon_outcome <- as.numeric(numericOutcomes$sex_upon_outcome)

#Turn outcome type into a 0 or 1 (0 for notadpoted, 1 for adoption)
library(plyr)
numericOutcomes$outcome_type <- revalue(numericOutcomes$outcome_type, c("Adoption"=1))
numericOutcomes$outcome_type <- revalue(numericOutcomes$outcome_type, c("notAdopted"=0))
numericOutcomes$outcome_type <- as.character(numericOutcomes$outcome_type)

summary(numericOutcomes)
head(numericOutcomes)

#Create vector of column max and min values
maxs <- apply(numericOutcomes[,1:3], 2, max)
mins <- apply(numericOutcomes[,1:3], 2, min)
scaled.data <- as.data.frame(scale(numericOutcomes[,1:3], 
                                   center = mins,
                                   scale = maxs - mins))
print(head(scaled.data, 2))
Adopted = as.numeric(numericOutcomes$outcome_type)
Adopted
data = cbind(Adopted, scaled.data)

library(caTools)
set.seed(101)
split = sample.split(data$Adopted, SplitRatio = 0.70)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
feats <- names(scaled.data)
f <- paste(feats, collapse =' + ')
f <- paste('Adopted ~', f)
f <- as.formula(f)
library(neuralnet)
nn <- neuralnet(f,train,hidden = c(10,10,10), linear.output = FALSE)
predicted.nn.values <- compute(nn, test[1:3])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result, round, digits = 0)
table(test$Adopted, predicted.nn.values$net.result)
plot(nn)