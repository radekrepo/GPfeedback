#run LDA models and compare them according to their semantic coherence and exclusivity scores
# CALCULATIONS TAKE A WHILE FOR ALL MODELS. They were computed in chunks speed up calculations
# and to avoid computer crashing.

library('stm')
setwd("c:/Users/user/Desktop/")


input.csv <- read.csv('gp_kom_do12sty2017.csv')
selekcja <- input.csv[c(1,3,4,9:14)]
selekcja <- selekcja[complete.cases(selekcja), ]

komentarze <- as.vector(selekcja$content)


prep <- textProcessor(selekcja$content, metadata=selekcja, 
                      lowercase=TRUE, removestopwords=TRUE, removenumbers=TRUE, 
                      removepunctuation=TRUE, stem=TRUE, wordLengths=c(3,Inf), 
                      sparselevel=1, language="en", 
                      verbose=TRUE, onlycharacter= TRUE, striphtml=TRUE,
                      customstopwords=NULL) 

plotRemoved(prep$documents, lower.thresh = 10)

dataprep2 <- prepDocuments(prep$documents, prep$vocab, prep$meta, lower.thresh=10, upper.thresh = 100000)






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