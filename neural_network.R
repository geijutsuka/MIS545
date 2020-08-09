install.packages("caTools")
install.packages("neuralnet")

nodatesOutcomes <- subset(filteredOutcomes, select = c('animal_type','color', 'age','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(nodatesOutcomes)
str(nodatesOutcomes)

nodatesOutcomes$sex_upon_outcome <- as.character(nodatesOutcomes$sex_upon_outcome)
nodatesOutcomes$sex_upon_outcome <- as.factor(nodatesOutcomes$sex_upon_outcome)

levels(nodatesOutcomes$sex_upon_outcome)

maxs <- apply(nodatesOutcomes[,1:4], 2, max)
mins <- apply(nodatesOutcomes[,1:4], 2, min)
scaled.data <- as.data.frame(scale(nodatesOutcomes[,1:4], center = mins,
                                   scale = maxs - mins))
print(head(scaled.data, 2))
Adopted = as.numeric(nodatesOutcomes$outcome_type)-1
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
predicted.nn.values <- compute(nn, test[2:18])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result, round, digits = 0)
table(test$Private, predicted.nn.values$net.result)
plot(nn)