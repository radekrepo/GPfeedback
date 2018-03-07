
library(stm)
library(igraph)


# THE CODE IN THIS FILE WILL TAKE A WHILE TO RUN 
# (several days using a single 4GB RAM core of a laptop with Windows)

# prepare data for LDA modelling
setwd("c:/Users/user/Desktop/")

#import data
input <- read.csv("all_gp_comments4.csv")
feedback <- input[c(1:3,7:13)]

names(feedback)

#preprocess data
dataprep <- textProcessor(feedback$content, metadata=feedback, 
                      lowercase=TRUE, removestopwords=TRUE, removenumbers=TRUE, 
                      removepunctuation=TRUE, stem=TRUE, wordLengths=c(3,Inf), 
                      sparselevel=1, language="en", 
                      verbose=TRUE, onlycharacter= TRUE, striphtml=TRUE,
                      customstopwords=NULL) 
dataprep2 <- prepDocuments(dataprep$documents, dataprep$vocab, dataprep$meta, lower.thresh=10, upper.thresh = 100000)


#run LDA models and compare them according to their semantic coherence and exclusivity scores
# CALCULATIONS TAKE A WHILE FOR ALL MODELS

comp3_4 <- searchK(dataprep2$documents, dataprep2$vocab, c(3:4), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp5_6 <- searchK(dataprep2$documents, dataprep2$vocab, c(5:6), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp7_8 <- searchK(dataprep2$documents, dataprep2$vocab, c(7:8), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp9_10 <- searchK(dataprep2$documents, dataprep2$vocab, c(9:10), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp11_12 <- searchK(dataprep2$documents, dataprep2$vocab, c(11:12), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp13_14 <- searchK(dataprep2$documents, dataprep2$vocab, c(13:14), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp15_16 <- searchK(dataprep2$documents, dataprep2$vocab, c(15:16), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp17_18 <- searchK(dataprep2$documents, dataprep2$vocab, c(17:18), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp19_20 <- searchK(dataprep2$documents, dataprep2$vocab, c(19:20), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp21_22 <- searchK(dataprep2$documents, dataprep2$vocab, c(21:22), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp23_24 <- searchK(dataprep2$documents, dataprep2$vocab, c(23:24), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp25_26 <- searchK(dataprep2$documents, dataprep2$vocab, c(25:26), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp27_28 <- searchK(dataprep2$documents, dataprep2$vocab, c(27:28), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp29_30 <- searchK(dataprep2$documents, dataprep2$vocab, c(29:30), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp31_32 <- searchK(dataprep2$documents, dataprep2$vocab, c(31:32), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp33_34 <- searchK(dataprep2$documents, dataprep2$vocab, c(33:34), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp35_36 <- searchK(dataprep2$documents, dataprep2$vocab, c(35:36), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp37_38 <- searchK(dataprep2$documents, dataprep2$vocab, c(37:38), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp39_40 <- searchK(dataprep2$documents, dataprep2$vocab, c(39:40), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp41_42 <- searchK(dataprep2$documents, dataprep2$vocab, c(41:42), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp43_44 <- searchK(dataprep2$documents, dataprep2$vocab, c(43:44), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp45_46 <- searchK(dataprep2$documents, dataprep2$vocab, c(45:46), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp47_48 <- searchK(dataprep2$documents, dataprep2$vocab, c(47:48), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp49_50 <- searchK(dataprep2$documents, dataprep2$vocab, c(49:50), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp51_52 <- searchK(dataprep2$documents, dataprep2$vocab, c(51:52), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp53_54 <- searchK(dataprep2$documents, dataprep2$vocab, c(53:54), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp55_56 <- searchK(dataprep2$documents, dataprep2$vocab, c(55:56), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp57_58 <- searchK(dataprep2$documents, dataprep2$vocab, c(57:58), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp59_60 <- searchK(dataprep2$documents, dataprep2$vocab, c(59:60), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp61_62 <- searchK(dataprep2$documents, dataprep2$vocab, c(61:62), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp63_64 <- searchK(dataprep2$documents, dataprep2$vocab, c(63:64), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp65_66 <- searchK(dataprep2$documents, dataprep2$vocab, c(65:66), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp67_68 <- searchK(dataprep2$documents, dataprep2$vocab, c(67:68), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp69_70 <- searchK(dataprep2$documents, dataprep2$vocab, c(69:70), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp71_72 <- searchK(dataprep2$documents, dataprep2$vocab, c(71:72), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp73_74 <- searchK(dataprep2$documents, dataprep2$vocab, c(73:74), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp75_76 <- searchK(dataprep2$documents, dataprep2$vocab, c(75:76), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp77_78 <- searchK(dataprep2$documents, dataprep2$vocab, c(77:78), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp79_80 <- searchK(dataprep2$documents, dataprep2$vocab, c(79:80), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp81_82 <- searchK(dataprep2$documents, dataprep2$vocab, c(81:82), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp83_84 <- searchK(dataprep2$documents, dataprep2$vocab, c(83:84), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp85_86 <- searchK(dataprep2$documents, dataprep2$vocab, c(85:86), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp87_88 <- searchK(dataprep2$documents, dataprep2$vocab, c(87:88), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp89_90 <- searchK(dataprep2$documents, dataprep2$vocab, c(89:90), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

comp91_92 <- searchK(dataprep2$documents, dataprep2$vocab, c(91:92), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp93_94 <- searchK(dataprep2$documents, dataprep2$vocab, c(93:94), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp95_96 <- searchK(dataprep2$documents, dataprep2$vocab, c(95:96), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp97_98 <- searchK(dataprep2$documents, dataprep2$vocab, c(97:98), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)
comp99_100 <- searchK(dataprep2$documents, dataprep2$vocab, c(99:100), init.type='LDA', data=prep2$meta,
                     heldout.seed=1)

mm <- c(comp3_4,comp5_6,comp7_8,comp9_10,
        comp11_12,comp13_14,comp15_16,comp17_18,comp19_20,
        comp21_22,comp23_24,comp25_26,comp27_28,comp29_30,
        comp31_32,comp33_34,comp35_36,comp37_38,comp39_40,
        comp41_42,comp43_44,comp45_46,comp47_48,comp49_50,
        comp51,comp52,comp53,comp54,comp55,comp56,comp57,comp58,comp59,comp60,
        comp61_62,comp63_64,comp65_66,comp67_68,comp69_70,
        comp71_72,comp73_74,comp75_76,comp77_78,comp79_80,
        comp81_82,comp83_84,comp85_86,comp87_88,comp89_90,
        comp91_92,comp93_94,comp95_96,comp97_98,comp99_100)

# retrieving semantic coherence and exclusivity scores for all models
semcoh <- c()
exclus <- c()
K <- c()
for (x in 1:length(mm)){
  if (x %% 2 != 0){
    tmp <- mm[[x]]
    for (xx in 1:length(tmp[1])){
      K <- c(K,tmp[1][[xx]])
      exclus <- c(exclus,tmp[6][[xx]])
      semcoh <- c(semcoh,tmp[7][[xx]])
    }
  }
}

# normalize semantic coherence and exclusivity scores (mean = 0, sd = 1)
semcoh <- scale(semcoh)
semcoh <- c(NaN,NaN,semcoh)
exclus <- scale(exclus)
exclus <- c(NaN,NaN,exclus)
avg_norm_score <- (semcoh + exclus)/2
K <- c(NaN,NaN,K)

#compare exclusivity and semantic coherence scores for LDA models
zipper <- as.data.frame(K)
zipper$semcoh <- semcoh
zipper$exclus <- exclus
zipper$avg_norm_score <- avg_norm_score
# View(zipper)

rm(x,xx,mm,tmp) # remove redundant objects from global environment

# plot normalised semantic coherence and exclusivity scores. Export the resulting graph

# Plot the bar chart.
plot(exclus,type = "l",col = "red", xlab = "number of generated topics", 
     ylab = "normalised score values", ylim = c(-4.5,3), lwd=2.5,
     main = "Semantic coherence and exclusivity scores for calculated topic models",
     bg = 'transparent')
lines(semcoh, type = "l", col = "blue", lwd=2.5)
lines(avg_norm_score, type = 'l', col = 'green', lwd=2.5)
legend(58,-1.2,c('Exclusivity','Semantic coherence', 'average normalized score'),
       col=c('red','blue','green'),lwd=c(2.5,2.5,2.5))

dev.copy(jpeg,'mod3-mod100.jpg',width = 900, height = 480) # save a copy of the plot

dev.off()
dev.off()


############
############
############
############

# compute chosen LDA model. In this case, model with 20 topics

docs <- dataprep2$documents
vocab <- dataprep2$vocab
meta <-dataprep2$meta
chosen_k <- 20

modelLDA <- stm(documents = docs, vocab = vocab, chosen_k, data=NULL,
                init.type=c("LDA"), seed=1, #instead of 'LDA' you can use 'Spectral' which allows faster computation on larger datasets
                max.em.its=500, emtol=1e-5,
                verbose=TRUE, reportevery=5,   
                LDAbeta=TRUE, interactions=TRUE, 
                ngroups=1, model=NULL, #argument 'ngroups' may speed up model computation but makes R use more RAM
                gamma.prior=c("Pooled", "L1"), sigma.prior=0,
                kappa.prior=c("L1"), control=list())

# Community detection in the optimal LDA topic model

tt <- topicCorr(modelLDA) #calculate correlations between topics (then use only positive correlations for community detection)
#convert correlation matrix into a graph object (suitable for processing with igraph package)
m_gr <- graph_from_adjacency_matrix(tt$poscor,mode='upper', weighted = TRUE) 
optimal_clusters <- cluster_optimal(m_gr)
print(membership(optimal_clusters))

# identify clusters of neutral topics. Neutral topics need to be unused later on
# to avoid multicollinearity problem in prediction of Likert-scale satisfaction.
# (predictions are done later on, using Python)
# In this study, topics are considered neutral when they cluster words used for
# expressing individuals' personal circumstances rather than ideas related to the
# GP service experience.

labelTopics(modelLDA)
dev.new(width=5, height=8)
plot.STM(modelLDA, type= 'summary') # plot popularities of topics in reviews
# plot.STM(modelLDA, type= 'perspectives', topics = c(5,2)) # compare two topics
dev.off()

# ALTERNATIVE CLUSTERING TECHNIQUE - hierarchical clustering, enabling manual choice of clusters to be used later
# hierarchical_clusters <- cluster_louvain(m_gr)
# three_clusters <- cut_at(hierarchical_clusters, no = 3)

# In this study, topic 2 with most common words: "need, see, time, one, problem, can, make" 
# was excluded. It was the only topic in its cluster.
topics_to_ignore <- c(2)

# sum topic probabilities in documents according to clusters they belong to,
# in order to simplify semantic information from reviews.

optimal_clusters <- membership(optimal_clusters)
dt <- as.data.frame(modelLDA$theta)



col_count <- length(optimal_clusters)
for (x in min(optimal_clusters):max(optimal_clusters)){
  tm <- which(optimal_clusters %in% c(x))
  cl <- data.frame(rep(0,length(dt[[1]])))
  for (xx in tm){
    if (xx %in% topics_to_ignore == FALSE){
      cl <- cl + dt[[xx]]
    }
  }
  if (sum(cl) != 0){
    col_count <- col_count + 1
    dt[col_count] <- cl[1]
    names(dt)[col_count] <- paste('cluster',toString(x),sep='')
    print('')
  }
  else {
    print(paste('empty cluster', toString(x), "was ignored", sep = " "))
  }
}

x <- length(optimal_clusters) + 1
dt <- dt[x:length(dt)]

# remove missing reviews from 'input'. Several reviews may get deleted in 
# preparation for LDA modelling because the only words included in the review 
# were pruned out by data pre-processor, resulting in empty reviews

kom1 <- feedback[-dataprep$docs.removed,]

#add datetime
dt$datetime <- kom1$datetime

#add review ID
dt$my.ref.num <- kom1$my.ref.num

#add GP ID
dt$nhs_gp_num <- kom1$nhs_gp_num

#add Likert-scale responses
dt$q9 <- kom1$q9.numeric
dt$q10 <- kom1$q10.numeric
dt$q11 <- kom1$q11.numeric
dt$q12 <- kom1$q12.numeric
dt$q13 <- kom1$q13.numeric
dt$q14 <- kom1$q14.numeric

#add text content of review (for reference)
dt$content <- kom1$content

# reorder columns 
dt <- dt[,c("my.ref.num", "nhs_gp_num", "datetime", 'content', "q9", "q10", "q11", "q12",
            "q13", "q14", "cluster1", "cluster3", "cluster4", "cluster5", "cluster6")]

# Export document-topic matrix with summed topic-document probabilities data
# into a file in .csv format. Further data processing is carried out in Python

write.csv(file="r_output.csv", x=dt, row.names = FALSE)

rm(kom1,dt,feedback,meta,relations,zipper,chosen_k,avg_norm_score,
   col_count,docs,exclus,K,m_gr,optimal_clusters,semcoh,tm,topics_to_ignore,
   tt,vocab,x,xx,cl)
