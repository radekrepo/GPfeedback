
#implements LDA topic model

library('stm')
setwd("c:/Users/user/Desktop/")


input.csv <- read.csv('gp_revs_upto12Jan2017.csv')
selekcja <- input.csv[c(1,3,4,9:14)]
selekcja <- selekcja[complete.cases(selekcja), ]

komentarze <- as.vector(selekcja$content)

prep <- textProcessor(selekcja$content, metadata=selekcja, 
                      lowercase=TRUE, removestopwords=TRUE, removenumbers=TRUE, 
                      removepunctuation=TRUE, stem=TRUE, wordLengths=c(3,Inf), 
                      sparselevel=1, language="en", 
                      verbose=TRUE, onlycharacter= TRUE, striphtml=TRUE,
                      customstopwords=NULL) 

#'regdata' object is used for further modelling
regdata <- selekcja[-prep$docs.removed,c(1,4:9)]

plotRemoved(prep$documents, lower.thresh = 10)

prep2 <- prepDocuments(prep$documents, prep$vocab, prep$meta, lower.thresh=10, upper.thresh = 100000)


docs <- prep2$documents
vocab <- prep2$vocab
meta <-prep2$meta

modelLDA <- stm(documents = docs, vocab = vocab, 57, data=NULL,
                init.type=c("LDA"), seed=1, 
                max.em.its=500, emtol=1e-5,
                verbose=TRUE, reportevery=5,   
                LDAbeta=TRUE, interactions=TRUE, 
                ngroups=1, model=NULL, 
                gamma.prior=c("Pooled", "L1"), sigma.prior=0,
                kappa.prior=c("L1"), control=list())


thetas <- data.frame(modelLDA$theta)
regdata['V1'] <- thetas[,1]
regdata['V2'] <- thetas[,2]
regdata['V3'] <- thetas[,3]
regdata['V4'] <- thetas[,4]
regdata['V5'] <- thetas[,5]
regdata['V6'] <- thetas[,6]
regdata['V7'] <- thetas[,7]
regdata['V8'] <- thetas[,8]
regdata['V9'] <- thetas[,9]
regdata['V10'] <- thetas[,10]
regdata['V11'] <- thetas[,11]
regdata['V12'] <- thetas[,12]
regdata['V13'] <- thetas[,13]
regdata['V14'] <- thetas[,14]
regdata['V15'] <- thetas[,15]
regdata['V16'] <- thetas[,16]
regdata['V17'] <- thetas[,17]
regdata['V18'] <- thetas[,18]
regdata['V19'] <- thetas[,19]
regdata['V20'] <- thetas[,20]
regdata['V21'] <- thetas[,21]
regdata['V22'] <- thetas[,22]
regdata['V23'] <- thetas[,23]
regdata['V24'] <- thetas[,24]
regdata['V25'] <- thetas[,25]
regdata['V26'] <- thetas[,26]
regdata['V27'] <- thetas[,27]
regdata['V28'] <- thetas[,28]
regdata['V29'] <- thetas[,29]
regdata['V30'] <- thetas[,30]
regdata['V31'] <- thetas[,31]
regdata['V32'] <- thetas[,32]
regdata['V33'] <- thetas[,33]
regdata['V34'] <- thetas[,34]
regdata['V35'] <- thetas[,35]
regdata['V36'] <- thetas[,36]
regdata['V37'] <- thetas[,37]
regdata['V38'] <- thetas[,38]
regdata['V39'] <- thetas[,39]
regdata['V40'] <- thetas[,40]
regdata['V41'] <- thetas[,41]
regdata['V42'] <- thetas[,42]
regdata['V43'] <- thetas[,43]
regdata['V44'] <- thetas[,44]
regdata['V45'] <- thetas[,45]
regdata['V46'] <- thetas[,46]
regdata['V47'] <- thetas[,47]
regdata['V48'] <- thetas[,48]
regdata['V49'] <- thetas[,49]
regdata['V50'] <- thetas[,50]
regdata['V51'] <- thetas[,51]
regdata['V52'] <- thetas[,52]
regdata['V53'] <- thetas[,53]
regdata['V54'] <- thetas[,54]
regdata['V55'] <- thetas[,55]
regdata['V56'] <- thetas[,56]
regdata['V57'] <- thetas[,57]


# POSTPROCESSING
labelTopics(LDAbezNaN.k70.groups1.seed1)

dev.new(width=5, height=8)
plot.STM(LDAbezNaN.k50.groups1.seed1, type= 'summary')
plot.STM(LDA.k60.groups1.seed1, type= 'perspectives', topics = c(31,60))

# ?stm
dev.off()

#finds representative statement on a topic
kom1 <- komentarze[-prep$docs.removed]
thought <- findThoughts(LDAbezNaN.k60.groups1.seed1,
                texts = kom1, topics = 52, n=10)
for(i in c(1:10)){
  print(thought$docs[[1]][[i]])
  print(' ')
}

#finds topics containing specific words
# ?findTopic
#word clouds
cloud(LDA.k60.groups1.seed1, topic =55, max.words = 30)

#dobór optymalnego modelu LDA
# TO MOŻE TROCHĘ POTRWAĆ!
set.seed(1)
optym.k60.lda.seed1 <- manyTopics(docs, vocab, c(60))

#topic correlations
mod.out.corr <- topicCorr(LDAbezNaN.k60.groups1.seed1)
plot.topicCorr(mod.out.corr)

#estimateEffect musi mieć 0 Nan-ów i tyle samo dokumentów co model


bez.pustych.meta <- selekcja[4:9]
bez.pustych.meta <- bez.pustych.meta[-prep$docs.removed,] # usuwanie usuniętych komentarzy
prep.test <- estimateEffect(~ q9.numeric + q10.numeric + q11.numeric + q12.numeric + q13.numeric + q14.numeric, LDAbezNaN.k60.groups1.seed1, bez.pustych.meta)


# graphic visualisation of estimateEffect function's outcomes
plot.estimateEffect(prep.test, "q9.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(4,15,31,45,38), method="continuous", ylim=c(0.01,0.04),
                    main = 'Are you able to get through to the surgery by telephone?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q9.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(26,44,50,58,59), method="continuous", ylim=c(0.01,0.04),
                    main = 'Are you able to get through to the surgery by telephone?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q10.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(51,49,30,17,15), method="continuous", ylim=c(0.01,0.04),
                    main = 'Are you able to get an appointment when you want one?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q10.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(40,41,26,27,1), method="continuous", ylim=c(0.01,0.04),
                    main = 'Are you able to get an appointment when you want one?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q11.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(34,39,10,6,14), method="continuous", ylim=c(0.01,0.04),
                    main = 'Do the staff treat you with dignity and respect?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q11.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(9,41,44,40,36), method="continuous", ylim=c(0.01,0.04),
                    main = 'Do the staff treat you with dignity and respect?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q12.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(40,60,33,20,35), method="continuous", ylim=c(0.00,0.02),
                    main = 'Does the surgery involve you in decisions about your\n care and treatment?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q12.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(12,21,36,41,50), method="continuous", ylim=c(0.00,0.02),
                    main = 'Does the surgery involve you in decisions about your\n care and treatment?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q13.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(56,57,48,22,6), method="continuous", ylim=c(0.00,0.04),
                    main = 'How likely are you to recommend this GP surgery to friends\n and family if they needed similar care or treatment?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q13.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(49,55,50,58, 24), method="continuous", ylim=c(0.00,0.04),
                    main = 'How likely are you to recommend this GP surgery to friends\n and family if they needed similar care or treatment?',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q14.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(24,49,9,28,31), method="continuous", ylim=c(0.01,0.025),
                    main = 'This GP practice provides accurate and up to date\n information on services and opening hours',
                    xlab = "Star ratings")
plot.estimateEffect(prep.test, "q14.numeric", model=LDAbezNaN.k60.groups1.seed1, 
                    topics=c(26,22,10,12,1), method="continuous", ylim=c(0.01,0.025),
                    main = 'This GP practice provides accurate and up to date\n information on services and opening hours',
                    xlab = "Star ratings")
par(mfrow=c(1,1))

dev.off()

#extension 'stm' library
library(stmCorrViz)

stmCorrViz(LDAbezNaN.k60.groups1.seed1, "LDAbezNaN.k60viz.html", 
           documents_raw=kom1, 
           documents_matrix=prep$documents)


# 
# library(stmBrowser)
# # ?stmBrowser
# data(poliblog5k)
# 
# #Create date
# dec312007 <- as.numeric(as.Date("2007-12-31"))
# poliblog5k.meta$date <- as.Date(dec312007+poliblog5k.meta$day,
#                                 origin="1970-01-01")
# out <- prepDocuments(poliblog5k.docs, poliblog5k.voc, poliblog5k.meta)
# stm.out <- stm(out$documents, out$vocab, K=10,
#                prevalence=~rating + date,
#                data=out$meta,
#                max.em.its=1) #generally run models
# #longer than this.
# # library(stmBrowser)
# # setwd(tempdir())
# stmBrowser(stm.out, data=out$meta, c("rating", "date"),
#            text="text")
# # #Remove files
# # unlink("stm-visualization", recursive=TRUE)
