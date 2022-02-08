library(inTrees)
library(RRF)
data(iris)
X <- iris[,1:(ncol(iris)-1)]
target <- iris[,"Species"]

rf <- RRF(X,as.factor(target),ntree=100) # build an ordinary RF
treeList <- RF2List(rf)
ruleExec <- extractRules(treeList,X, digits = 4) # transform to R-executable rules
ruleExec <- unique(ruleExec)
ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules
head(ruleMetric)

# pruneRule 함수 써보기
temp_rules <- pruneRule(ruleMetric,X,target, maxDecay = 0.01, typeDecay = 2)
temp_rules

# pruneRule한 결과를 getFreqPattern 함수에 넣어보기 
freqPattern <- getFreqPattern(temp_rules, minsup = 0.01, minconf = 0.5, minlen = 2, maxlen = 6)
freqPatternMetric <- getRuleMetric(freqPattern, X, target)
head(freqPatternMetric)

# pruneRule과 getFreqPattern 함수를 거친 결과를 selectRuleRRF 함수에 넣어보기
selectRuleRRF(freqPatternMetric, X, target)


ruleMetric <- data.frame(ruleMetric)
ruleMetric