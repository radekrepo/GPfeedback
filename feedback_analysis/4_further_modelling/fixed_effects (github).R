library(plm)
library(lmtest)
library(texreg)
library(tidyr)
library(dplyr)
library(plyr)
library(splines)

# combined_data.csv file is the source of data for fixed-effects modelling

#SUM TOPICS TO 3 CLUSTERS
# The clusters were determined for 57 topics with the help of community 
# detection algorithm for network models embedded in gephi software



poz <- c('V30','V52','V21','V5','V36','V53',
         'V10','V18','V28','V15','V2','V55',
         'V26','V39','V49','V38','V25','V43',
         'V6','V41','V12','V16','V44','V24')
neg <- c('V1','V46','V57','V32','V17','V51',
         'V54','V20','V40','V3','V4','V56',
         'V37','V27','V7','V8','V22','V47',
         'V9','V29','V14','V33','V50','V42',
         'V11','V45','V34')
neut <- c('V23','V31','V35','V48','V19','V13')

# sumka <- function(input) {
#   temp = combined_data[input[1]]
#   for (i in 2:length(input)) {
#     temp <- temp + combined_data[input[i]]
#   }
#   return(temp)
# }
# 
# klastr_poz <- sumka(poz)
# klastr_neg <- sumka(neg)
# klastr_neut <- sumka(neut)
# 
# combined_data['pos_topics'] <- klastr_poz
# combined_data['neg_topics'] <- klastr_neg
# combined_data['neut_topics'] <- klastr_neut

#monthly + CCG aggregate
#TWORZENIE PANEL DATA
# fe_data <- combined_data %>% group_by(CCGCode, okres) %>% summarise_each(funs(mean))
# fe_data2 <- fe_data02[c(1,2,c(10:15),c(34:95))]
# write.csv(fe_data2, file = 'panel_data.csv')

fe_data <- read.csv('panel_data.csv')

# FIXED EFFECTS CLUSTERED ON GP ODS CODES
fixed_effects <- plm( q14.numeric_x ~ pos_topics + neg_topics + avg_imd + patients_all_imd, 
                      data = fe_data, 
                      index = c("CCGCode", "month"), 
                      model = "within", 
                      effect = "individual")

# model1_se <- coeftest(fixed_effects, vcov = pvcovHC(fixed_effects, method = "arellano", type = "HC3"))
# print(model1_se)

summary(fixed_effects)
plmtest(fixed_effects, effect="individual")


# TIME FIXED EFFECTS
time_effects <- plm( q14.numeric_x ~ pos_topics + neg_topics + avg_imd + patients_all_imd, 
                      data = fe_data, 
                      index = c("CCGCode", "month"), 
                      model = "within", 
                      effect = "time")

# model1_se <- coeftest(time_effects, vcov = pvcovHC(time_effects, method = "arellano", type = "HC3"))
# print(model1_se)

summary(time_effects)
plmtest(time_effects, effect="time")


# TWO WAYS FIXED EFFECTS
twoway_effects1 <- plm(q9.numeric_x ~ pos_topics + neg_topics + avg_imd + patients_all_imd, 
                      data = fe_data, 
                      index = c("CCGCode", "month"), 
                      model = "within", 
                      effect = "twoways")

model_se_1 <- coeftest(twoway_effects1, vcov = pvcovHC(twoway_effects1, method = "arellano", type = "HC3"))
print(model1_se)

summary(twoway_effects)

# screenreg(list(twoway_effects1, twoway_effects2, twoway_effects3, twoway_effects4, twoway_effects5, twoway_effects6),
#           custom.model.names = c("Model 1", "Model 2",'Model 3','Model 4','Model 5','Model 6'))




