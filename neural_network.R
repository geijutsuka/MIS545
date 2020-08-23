install.packages("caTools")
install.packages("neuralnet")
install.packages("plyr")
library(caTools)
library(neuralnet)
library(plyr)

#leaving out dates and breed due to too many variations
simplifiedOutcomes <- subset(filteredOutcomes, select = c('animal_type','age_years','name','sex_upon_outcome','outcome_type'), stringsAsFactors = TRUE)
head(simplifiedOutcomes)
str(simplifiedOutcomes)
summary(simplifiedOutcomes)
nrow(simplifiedOutcomes)

levels(simplifiedOutcomes$animal_type)
levels(simplifiedOutcomes$name)
levels(simplifiedOutcomes$sex_upon_outcome)
levels(simplifiedOutcomes$outcome_type)

integerOutcomes <- simplifiedOutcomes
#Make all the inputs into integers (age_years already integer)
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

nrow(integerOutcomes)
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
