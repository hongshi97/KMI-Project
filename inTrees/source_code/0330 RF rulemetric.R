# install.packages("inTrees")
# install.packages("RRF")

library(inTrees)
library(RRF)
data(iris)
X <- iris[,1:(ncol(iris)-1)]
target <- iris[,"Species"]

X$Sepal.Length <- ifelse(X$Sepal.Length >= mean(X$Sepal.Length),'Long','Short')
X$Sepal.Length <- as.factor(X$Sepal.Length)

X$Sepal.Width <- ifelse(X$Sepal.Width >= mean(X$Sepal.Width),'Big','Small')
X$Sepal.Width <- as.factor(X$Sepal.Width)


rf <- RRF(X,as.factor(target),ntree=100) # build an ordinary RF
treeList <- RF2List(rf)
ruleExec <- extractRules(treeList,X, digits = 4) # transform to R-executable rules
ruleExec <- unique(ruleExec)
ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules
ruleMetric

# getFreqPattern 함수 써보기 
freqPattern <- getFreqPattern(ruleMetric, minsup = 0.01, minconf = 0.5, minlen = 2, maxlen = 6)
freqPatternMetric <- getRuleMetric(freqPattern, X, target)
freqPatternMetric

# pruneRule 함수 써보기
pruneRule(ruleMetric,X,target, maxDecay = 0.01, typeDecay = 2)

# selectRuleRRF 함수 써보기
selectRuleRRF(ruleMetric, X, target)


ruleMetric <- data.frame(ruleMetric)
ruleMetric