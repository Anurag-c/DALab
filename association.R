library(arules)
str(titanic.raw)

rules <- apriori(titanic.raw)
inspect(rules)

rules <- apriori(titanic.raw, parameter = list(minlen = 2, supp = 0.005, conf = 0.8), appearance = list(rhs = c("Survived=No", "Survived=Yes"), default = "lhs"), control = list(verbose = F))
inspect(rules)
summary(rules)
length(rules)

rules.sorted <- sort(rules, by = "lift")
inspect(rules.sorted)

rules1 <- apriori(titanic.raw, parameter = list(minlen = 2, supp = 0.005, conf = 0.8), appearance = list(rhs = c("Survived=No"), default = "lhs"), control = list(verbose = F))
inspect(rules1)

rules2 <- apriori(titanic.raw, parameter = list(minlen = 2, supp = 0.005, conf = 0.8), appearance = list(rhs = c("Survived=Yes"), default = "lhs"), control = list(verbose = F))
inspect(rules2)

rules3 <- union(rules1, rules2)
inspect(rules3)

rules4 <- intersect(rules, rules1)
inspect(rules4)

areEqual <- setequal(rules, rules1)
areEqual

matchsets <- match(rules, rules1)
matchsets