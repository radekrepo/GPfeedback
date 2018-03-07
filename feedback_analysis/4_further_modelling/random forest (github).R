

#COMPUTING RANDOM FOREST MODELS WITH THE DATA. 
#The 'regdata' object can be produced with code from 'LDA topic model (github).R' file


#**Random forests**
# load(".RData") # function for loading objects from other R script files
library(randomForest)
library(ggplot2)
library(xlsx)
set.seed(1)
# ?randomForest

q9 <- regdata[,2]
q10 <- regdata[,3]
q11 <- regdata[,4]
q12 <- regdata[,5]
q13 <- regdata[,6]
q14 <- regdata[,7]
x <- regdata[,8:64]

#modelowanie random forest

rf.q9 <-  randomForest(x, y=factor(q9))
rf.q10 <-  randomForest(x, y=factor(q10))
rf.q11 <-  randomForest(x, y=factor(q11))
rf.q12 <-  randomForest(x, y=factor(q12))
rf.q13 <-  randomForest(x, y=factor(q13))
rf.q14 <-  randomForest(x, y=factor(q14))



tematy <- c('1. distress phone booking', '2. good doctors', '3. bad facilities', '4. incompetent', '5. great vaccine help', '6. satisfied',
            "7. can't choose doctor",'8. bad opinions','9. appointment impossible','10. decent practice',
            '11. hard appointments', '12. saying thanks', '13. poor mental care', 
            '14. booking roulette', '15. improved','16. give dignity',
            "17. don't listen",'18. long-term experience','19. male relative went','20. paperwork issue',
            '21. big changes', '22. disappointing', '23. son treated', 
            '24. [no meaning]', '25. great GP','26. respectful',
            '27. distressing','28. effective help','29. no appointment','30. long-term condition',
            '31. blood test', '32. long wait times', '33. long wait', 
            '34. [meaning not certain]', '35. hospital referral','36. parking problem',
            '37. not helpful','38. friendly','39. professional','40. repeat prescription',
            '41. the best ever', '42. impossible appointment', '43. recommend',
            '44. long-term happy', '45. time delay','46. poor experience',
            '47. walk-in help','48. diabetes check','49. impressive','50. out of hours care',
            '51. pros and cons', '52. empathy', '53. demand pressure',
            '54. upset!', '55. really recommend','56. lack common sense',
            '57. [meaning not certain]')

#accuracy, precision, recall and f1 scores
mac <- rf.q14$confusion
accuracy <- sum(diag(mac)) / sum(mac[,1:5]) #(TP+TN)/(TP+TN+FP+FN)
precision <-  diag(mac) / rowSums(mac[,1:5]) # TP/(TP+FP)
recall <- diag(mac) / colSums(mac[,1:5]) # TP/(TP+FN)
f1score <- 2*precision*accuracy/(precision+accuracy) # 2*PREC*RECLL/(PREC+RECLL)

q14.perf <- c()

q14.perf$precision <- as.vector(precision)
q14.perf$recall <- as.vector(recall)
q14.perf$f1score <- as.vector(f1score)

accuracies$q14 <- accuracy

# write.xlsx(rf.q14$confusion[,1:5], "14conf.xlsx")



varImpPlot(rf.q9, main = "Top 30 topics predicting response to a statement \n'Are you able to get through to the surgery by telephone?'") 
varImpPlot(rf.q10, main = "Top 30 topics predicting response to a statement \n'Are you able to get an appointment when you want one?'") 
varImpPlot(rf.q11, main = "Top 30 topics predicting response to a statement \n'Do the staff treat you with dignity and respect?'") 
varImpPlot(rf.q12, main = "Top 30 topics predicting response to a statement \n'Does the surgery involve you in decisions \nabout your care and treatment?'") 
varImpPlot(rf.q13, main = "Top 30 topics predicting response to a statement \n'How likely are you to recommend this GP surgery to friends \nand family if they needed similar care or treatment?'") 
varImpPlot(rf.q14, main = "Top 30 topics predicting response to a statement \n'This GP practice provides accurate and up to date \ninformation on services and opening hours'") 



punkty1 <- c(rf.q9$importance)
punkty2 <- c(rf.q10$importance)
punkty3 <- c(rf.q11$importance)
punkty4 <- c(rf.q12$importance)
punkty5 <- c(rf.q13$importance)
punkty6 <- c(rf.q14$importance)

df1.1 <- data.frame(punkty1,tematy)
df1.2 <- data.frame(punkty2,tematy)
df1.3 <- data.frame(punkty3,tematy)
df1.4 <- data.frame(punkty4,tematy)
df1.5 <- data.frame(punkty5,tematy)
df1.6 <- data.frame(punkty6,tematy)
names(df1.5)
#sortowanie wg. wartości
df1.1 <- df1.1[order(df1.1[,1],decreasing = TRUE),]
df1.2 <- df1.2[order(df1.2[,1],decreasing = TRUE),]
df1.3 <- df1.3[order(df1.3[,1],decreasing = TRUE),]
df1.4 <- df1.4[order(df1.4[,1],decreasing = TRUE),]
df1.5 <- df1.5[order(df1.5[,1],decreasing = TRUE),]
df1.6 <- df1.6[order(df1.6[,1],decreasing = TRUE),]

# MeanDecreaseGini
df2.1 <- transform(df1.1, tematy = reorder(tematy, punkty1))
df2.2 <- transform(df1.2, tematy = reorder(tematy, punkty2))
df2.3 <- transform(df1.3, tematy = reorder(tematy, punkty3))
df2.4 <- transform(df1.4, tematy = reorder(tematy, punkty4))
df2.5 <- transform(df1.5, tematy = reorder(tematy, punkty5))
df2.6 <- transform(df1.6, tematy = reorder(tematy, punkty6))

# graf m1 “Are you able to get through to the surgery by telephone?”
p1 <- ggplot(df2.1[1:30,], aes(tematy,punkty1)) + geom_col() + coord_flip() + ylab("topic importance") + xlab("topics") + ggtitle("PHONE ACCESS EASE")
# graf m2 “Are you able to get an appointment when you want one?”
p2 <- ggplot(df2.2[1:30,], aes(tematy,punkty2)) + geom_col() + coord_flip() + ylab("topic importance") + xlab("topics") + ggtitle("APPOINTMENT EASE")
# graf m3 “Do the staff treat you with dignity and respect?”
p3 <- ggplot(df2.3[1:30,], aes(tematy,punkty3)) + geom_col() + coord_flip() + ylab("topic importance") + xlab("topics") + ggtitle("GIVEN DIGNITY AND RESPECT")
# graf m4 “Does the surgery involve you in decisions about your care and treatment?”
p4 <- ggplot(df2.4[1:30,], aes(tematy,punkty4)) + geom_col() + coord_flip() + ylab("topic importance") + xlab("topics") + ggtitle("INVOLVED IN CARE DECISIONS")
# graf m5 “How likely are you to recommend this GP surgery to friends and family if they needed similar care or treatment?”
p5 <- ggplot(df2.5[1:30,], aes(tematy,punkty5)) + geom_col() + coord_flip() + ylab("topic importance") + xlab("topics") + ggtitle("LIKELY TO RECOMMEND")
# graf m6 “This GP practice provides accurate and up to date information on services and opening hours”
p6 <- ggplot(df2.6[1:30,], aes(tematy,punkty6)) + geom_col() + coord_flip() + ylab("topic importance") + xlab("topics") + ggtitle("UP-TO-DATE GP INFORMATION")

multiplot(p1,p2,p3,p4,p5,p6, cols=2)


#funkcja multiplot
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
