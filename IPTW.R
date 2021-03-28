#IPTW(inverse probability treatment weighting):
#packages needed:
library(ipw)
library(survey)

weight <- ipwpoint(exposure = can_1, family = "binomial", link = "logit",
                   numerator =~ 1,
                   denominator =~ failed_0+peer_can_0+antisocial_0+family_drug_use+sex+can_0,
                   trunc = 0.01, data = as.data.frame(currentDataset))
currentDataset$.ipw0 = weight$weights.trunc

weight <- ipwpoint(exposure = can_2, family = "binomial", link = "logit",
                   numerator =~ can_1,
                   denominator =~ can_1+failed_0+peer_can_0+antisocial_0+family_drug_use+sex+can_0+failed_1+peer_can_1+antisocial_1,
                   trunc = 0.01, data = as.data.frame(currentDataset))
currentDataset$.ipw1 = weight$weights.trunc

weight <- ipwpoint(exposure = can_3, family = "binomial", link = "logit",
                   numerator =~ can_1+can_2,
                   denominator =~ can_1+can_2+failed_0+peer_can_0+antisocial_0+family_drug_use+sex+can_0+failed_1+peer_can_1+antisocial_1+failed_2+peer_can_2+antisocial_2,
                   trunc = 0.01, data = as.data.frame(currentDataset))
currentDataset$.ipw2 = weight$weights.trunc

currentDataset$.final_weight <- currentDataset$.ipw0*currentDataset$.ipw1*currentDataset$.ipw2

currentDataset$cumulative <- currentDataset$can_1+currentDataset$can_2+currentDataset$can_3
clus <- svydesign(id =~ 1, weights =~ .final_weight, data = currentDataset)
res <- svyglm(illicit ~ cumulative, design = clus,family = binomial)
summary(res)
cbind(coef(res), confint(res, level = 0.95))
exp(cbind(OR = coef(res), confint(res, level = 0.95)))
