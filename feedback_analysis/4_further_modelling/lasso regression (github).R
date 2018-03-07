
library(stm)
library(glmnet)
library(car)
library(boot)
library(Matrix)
library(data.table)
library(quanteda)



# setwd('C:/Users/user/Desktop/')
# load('.RData')
combined_data <- read.csv('combined_data.csv')

# REGRESJE LINEARNE

# w1 <- "q9 ~ factor(ods)+factor(NHSEnglandRegionCode)+factor(CCGCode)+pacjenci_wszyscy_zestaw_imd+okres+sr_imd+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V410+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+0"
# 2, 14, 16, 33:92



f1 <- as.formula("q9.numeric_x ~ okres+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+sr_imd+pacjenci_wszyscy_zestaw_imd+CCGCode+NHSEnglandRegionCode+ods+0")
f2 <- as.formula("q10.numeric_x ~ okres+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+sr_imd+pacjenci_wszyscy_zestaw_imd+CCGCode+NHSEnglandRegionCode+ods+0")
f3 <- as.formula("q11.numeric_x ~ okres+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+sr_imd+pacjenci_wszyscy_zestaw_imd+CCGCode+NHSEnglandRegionCode+ods+0")
f4 <- as.formula("q12.numeric_x ~ okres+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+sr_imd+pacjenci_wszyscy_zestaw_imd+CCGCode+NHSEnglandRegionCode+ods+0")
f5 <- as.formula("q13.numeric_x ~ okres+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+sr_imd+pacjenci_wszyscy_zestaw_imd+CCGCode+NHSEnglandRegionCode+ods+0")
f6 <- as.formula("q14.numeric_x ~ okres+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+sr_imd+pacjenci_wszyscy_zestaw_imd+CCGCode+NHSEnglandRegionCode+ods+0")

# memory.limit(size = 50000)

m1 <- glm(f1, family = gaussian, combined_data)
m2 <- glm(f2, family = gaussian, combined_data)
m3 <- glm(f3, family = gaussian, combined_data)
m4 <- glm(f4, family = gaussian, combined_data)
m5 <- glm(f5, family = gaussian, combined_data)
m6 <- glm(f6, family = gaussian, combined_data)



# summary(m1)
# ?summary.glm
# wsad <- dane.pod.regresje(modelLDA.t30)
# wynikq9  <- glm(w1, family = gaussian, wsad)
# wynikq10 <- glm(w2, family = gaussian, wsad)
# wynikq11 <- glm(w3, family = gaussian, wsad)
# wynikq12 <- glm(w4, family = gaussian, wsad)
# wynikq13 <- glm(w5, family = gaussian, wsad)
# wynikq14 <- glm(w6, family = gaussian, wsad)
# 
# cverr.q9 <- cv.glm(wsad,wynikq9,K=5)
# cverr.q10 <- cv.glm(wsad,wynikq10,K=5)
# cverr.q11 <- cv.glm(wsad,wynikq11,K=5)
# cverr.q12 <- cv.glm(wsad,wynikq12,K=5)
# cverr.q13 <- cv.glm(wsad,wynikq13,K=5)
# cverr.q14 <- cv.glm(wsad,wynikq14,K=5)
# 
# #prediction error (unadjusted)
# cverr.q9$delta[1]
# cverr.q10$delta[1]
# cverr.q11$delta[1]
# cverr.q12$delta[1]
# cverr.q13$delta[1]
# cverr.q14$delta[1]
# 

# tabela1 <- as.data.frame(compareCoefs(reg.t30$q09,reg.t30$q10,reg.t30$q11,reg.t30$q12,reg.t30$q13,reg.t30$q14, digits = 1))
# names(tabela1) <- c("Model 1", 'SE 1',"Model 2", 'SE 2',"Model 3", 'SE 3',"Model 4", 'SE 4',"Model 5", 'SE 5',"Model 6", 'SE 6')
# t.lista <- c('topic 1', 'topic 2', 'topic 3', 'topic 4', 'topic 5','topic 6',
#             'topic 7','topic 8','topic 9','topic 10',
#             'topic 11', 'topic 12', 'topic 13', 'topic 14', 'topic 15','topic 16',
#             'topic 17','topic 18','topic 19','topic 20',
#             'topic 21', 'topic 22', 'topic 23', 'topic 24', 'topic 25','topic 26',
#             'topic 27','topic 28','topic 29','topic 30')
# row.names(tabela1) <- t.lista

# #TABELA 2: Lasso
# 
# 
# meta.lasso <- meta[,4:9]
# 
# lasso.q9 <- topicLasso(q9.numeric ~ 1, data=meta.lasso, stmobj = modelLDA.t30, seed=1,nfolds=5)
# lasso.q10 <- topicLasso(q10.numeric ~ 1, data=meta.lasso, stmobj = modelLDA.t30, seed=1,nfolds=5)
# lasso.q11 <- topicLasso(q11.numeric ~ 1, data=meta.lasso, stmobj = modelLDA.t30, seed=1,nfolds=5)
# lasso.q12 <- topicLasso(q12.numeric ~ 1, data=meta.lasso, stmobj = modelLDA.t30, seed=1,nfolds=5)
# lasso.q13 <- topicLasso(q13.numeric ~ 1, data=meta.lasso, stmobj = modelLDA.t30, seed=1,nfolds=5)
# lasso.q14 <- topicLasso(q14.numeric ~ 1, data=meta.lasso, stmobj = modelLDA.t30, seed=1,nfolds=5)
# sd(meta.lasso$q14.numeric)
# 
# # lista coef-ów z lasso
# View(as.list(coef(lasso.q13)))
# 
# lasso.q9$cvm
# lasso.q10$cvm
# lasso.q11$cvm
# lasso.q12$cvm
# lasso.q13$cvm
# lasso.q14$cvm


