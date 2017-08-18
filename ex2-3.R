#HWK2
#Exercise 2
setwd("C:/Users/ststest/Dropbox/TextAnalysis Grimmer/Day2/HWK2")

uni<-read.csv('dtm_1.csv', ,header=T,stringsAsFactors = F)
tri<-read.csv('dtm_2.csv', ,header=T,stringsAsFactors = F)

#Get rates
# # UNIGRAMS
unid<-uni[,-1]
for (i in 1:nrow(unid)){
  s<- sum(unid[i,])
  unid[i,]<- unid[i,]/s
}
#add id back on
unid<-cbind(uni$Speaker, unid)
colnames(unid)[1]<- c("Speaker")

#This should do the same faster
#a<-uni[,-1]
#z<-apply(a,1,function(x) x/sum(x))

# # TRIGRAMS
trid<-tri[,-1]
for (j in 1:nrow(trid)){
  s<- sum(trid[j,])
  trid[j,]<- trid[j,]/s
}
trid<-cbind(tri$Speaker, trid)
colnames(trid)[1]<- c("Speaker")

#No of docs by author
shelbyN<-table(unid$Speaker)[2]
sessionsN<-table(unid$Speaker)[1]

#means and variances of unigrams
mushuni <- colMeans(unid[unid$Speaker=="shelby",-1])
museuni<- colMeans(unid[unid$Speaker=="sessions",-1])
varshuni<- apply(unid[unid$Speaker=="shelby",-1], 2, var)
varseuni<- apply(unid[unid$Speaker=="sessions",-1], 2, var)

#means and variances of trigrams
mushtri <- colMeans(trid[trid$Speaker=="shelby",-1])
musetri<- colMeans(trid[trid$Speaker=="sessions",-1])
varshtri<- apply(trid[trid$Speaker=="shelby",-1], 2, var)
varsetri<- apply(trid[trid$Speaker=="sessions",-1], 2, var)



# #INDEPENDENT LINEAR DESCRIMINANT
#Weights
ldWeightsU<- (mushuni - museuni) / (varshuni + varseuni)
ldWeightsT<- (mushtri - musetri) / (varshtri + varsetri)
sldWeightsU<- sort(ldWeightsU)
sldWeightsT<- sort(ldWeightsT)

#10 most discriminating words for each guy
LDmostSessionsU<- head(sldWeightsU, 10)
names(LDmostSessionsU)
LDmostShelbyU<- tail(sldWeightsU, 10)
names(LDmostShelbyU)
LDmostSessionsT<- head(sldWeightsT, 10) 
LDmostShelbyT<- tail(sldWeightsT, 10)

#Computing discrimination statistic p
linear_disc<-as.matrix(unid[,-1])%*%ldWeightsU


# # STANDARDIZED MEAN DIFFERENCE
#unigrams
numeratoru<- mushuni - museuni
denominatoru<- sqrt((varshuni/shelbyN) + (varseuni/sessionsN))
sdiffu<-numeratoru / denominatoru

#trigrams
numeratort<- mushtri - musetri
denominatort<- sqrt((varshtri/shelbyN) + (varsetri/sessionsN))
sdifft<-numeratort / denominatort

sdWeightsU<- sort(sdiffu)
sdWeightsT<- sort(sdifft)

#10 most discriminating words for each guy
SDmostSessionsU<- head(sdWeightsU,10)
SDmostShelbyU<- tail(sdWeightsU, 10)
SDmostSessionsT<- head(sdWeightsT, 10)
SDmostShelbyT<- tail(sdWeightsT, 10)


# # STANDARDIZED LOG ODDS
#unigrams
totShelbyU<-sum(uni[uni$Speaker=="shelby",-1])
piShelbyU<-(colSums(uni[uni$Speaker=="shelby",-1]) + 1)/(totShelbyU + (ncol(uni)-1))
totSessionsU<- sum(uni[uni$Speaker=="sessions",-1]) 
piSessionsU<- (colSums(uni[uni$Speaker=="sessions", -1]) + 1)/(totSessionsU + (ncol(uni)-1))

#log odds rati and variance
lorU<- log(piShelbyU/ (1- piShelbyU)) - log(piSessionsU/ (1-piSessionsU))
varlorU<- (1/(colSums(uni[uni$Speaker=="shelby",-1]) + 1)) + (1/(colSums(uni[uni$Speaker=="sessions",-1]) + 1)) 
stdlorU<- lorU/ sqrt(varlorU)


#trigrams
totShelbyT<-sum(tri[tri$Speaker=="shelby",-1])
piShelbyT<-(colSums(tri[tri$Speaker=="shelby",-1]) + 1)/(totShelbyT + (ncol(tri)-1))
totSessionsT<- sum(tri[tri$Speaker=="sessions",-1]) 
piSessionsT<- (colSums(tri[tri$Speaker=="sessions", -1]) + 1)/(totSessionsT + (ncol(tri)-1))

#log odds rati and variance
lorT<- log(piShelbyT/ (1- piShelbyT)) - log(piSessionsT/ (1-piSessionsT))
varlorT<- (1/(colSums(tri[tri$Speaker=="shelby",-1]) + 1)) + (1/(colSums(tri[tri$Speaker=="sessions",-1]) + 1)) 
stdlorT<- lorT/ sqrt(varlorT)

#sorting
sloWU<- sort(stdlorU)
sloWT<- sort(stdlorT)

sloSessionsMostU<- head(sloWU,10)
sloShelbyMostU<- tail(sloWU, 10)
sloSessionsMostT<- head(sloWT, 10)
sloShelbyMostT<- tail(sloWT, 10)


# # # PLOTTING DISCRIMINATING WORDS FOR UNIGRAMS
par(mfrow=c(3,1))
###Plotting standardized mean diff
LDmostdescU<-c(LDmostSessionsU,LDmostShelbyU)
plot(LDmostdescU,1:20,pch='',xlab='Linear Weight', yaxt='n', ylab='',xlim=c(-290,1385),
     main='20 most discriminanting words in linear discriminant analysis')
text(LDmostdescU,1:20,labels = names(LDmostdescU), cex=.8)

###Plotting standardized mean diff
SDmostdescU<-c(SDmostSessionsU,SDmostShelbyU)
plot(SDmostdescU,1:20,pch='',xlab='SD mean Weight', yaxt='n', ylab='',
     main='20 most discriminanting words in Std. mean difference analysis')
text(SDmostdescU,1:20,labels = names(SDmostdescU), cex=.8)

###Plotting standardized mean diff
slomostdescU<-c(sloSessionsMostU,sloShelbyMostU)
plot(slomostdescU,1:20,pch='',xlab='Standardized log odds Weight', yaxt='n', ylab='',
     main='20 most discriminanting words in Std. Log Odds Ratio')
text(slomostdescU,1:20,labels = names(slomostdescU), cex=.8)

# # # PLOTTING DISCRIMINATING WORDS FOR TRIGRAMS
par(mfrow=c(3,1))
###Plotting standardized mean diff
LDmostdescT<-c(LDmostSessionsT,LDmostShelbyT)
plot(LDmostdescT,1:20,pch='',xlab='Linear Weight', yaxt='n', ylab='',xlim=c(-100,500),
     main='20 most discriminanting words in linear discriminant analysis')
text(LDmostdescT,1:20,labels = names(LDmostdescT), cex=.8)

###Plotting standardized mean diff
SDmostdescT<-c(SDmostSessionsT,SDmostShelbyT)
plot(SDmostdescT,1:20,pch='',xlab='SD mean Weight', yaxt='n', ylab='',xlim=c(-60,70),
     main='20 most discriminanting words in Std. mean difference analysis')
text(SDmostdescT,1:20,labels = names(SDmostdescT), cex=.8)

###Plotting standardized mean diff
slomostdescT<-c(sloSessionsMostT,sloShelbyMostT)
plot(slomostdescT,1:20,pch='',xlab='Standardized log odds Weight', yaxt='n', ylab='',xlim=c(-22,6),
     main='20 most discriminanting words in Std. Log Odds Ratio')
text(slomostdescT,1:20,labels = names(slomostdescT), cex=.8)

#Comments
#1) There is considerable variability in discrimination methods. For instance 'news' is the most Sessions-type word in the standard mean difference approach, and is close to it in the standard log odds ratio approac, but doesn't show up in the linear discriminant method.
#2) That said, there are a lot of things that make sense. Shelby is from alabama, so several of the most Shelby-type trigrams include alabama
#3) There might be other things interesting here, but the only thing I know from these gentlemen was that Shelby wasinvolved in a sex scandal in the 90s or so



### Exercise 3
set.seed(654)
samp_shelby<-sample(1:shelbyN,100)
samp_sessions<-sample((shelbyN+1):dim(tri)[1],100)
samp<-c(samp_shelby,samp_sessions)
tr<-tri[samp,-1]

#Euclidian Distance
edist<- matrix(nrow=200, ncol=200)

euc_dist<-function(x,y){
  return(sqrt(sum((x-y)^2)))
}

for (i in 1:dim(tr)[1]){
  for (j in 1:dim(tr)[1]){
    if(is.na(edist[i,j])==T){
      a<-euc_dist(tr[i,],tr[j,])
      edist[i,j]<-a 
      edist[j,i]<-a
  }}
}

write.csv(edist,'euclid.csv')

#Most different are 54 and 41
which(edist==max(edist),arr.ind = T)[,1]


#Euclidian with tf-idf
#edist_tfidf<- matrix(nrow=200, ncol=200)

calcIDF<- function(x){
  return(log(200/length(which(x>0))))
}

idf<- apply(tr, 2, calcIDF)
tr_idf<- as.matrix(t(apply(tr, 1, function(x) x*idf)))

edist_tfidf<- as.matrix(dist(tr_idf, method="euclidean"))
diag(edist_tfidf)<- NA
write.csv(edist_tfidf,'edist_tfidf.csv')

#Most different are ALSO 54 and 41
which(edist_tfidf==max(edist_tfidf, na.rm=T),arr.ind = T)[,1]

#which(edist_tfidf==min(edist_tfidf, na.rm=T),arr.ind = T)


#Cosine similarity
cosin<-matrix(nrow=200, ncol=200)
cosineSim<- function(x, y){
  return( sum(x*y)/ sqrt( sum(x^2)* sum(y^2)))
}

for (i in 1:nrow(tr)){
  for (j in 1:nrow(tr)){
    a<-cosineSim(tr[i,], tr[j,])
    cosin[i, j]<- a
    cosin[j, i]<- a
  }
}
write.csv(cosin, "cosinsim.csv")
diag(cosin)<-NA

#Several have smallest and highest cosine similarity
which(cosin==1,arr.ind = T)
which(cosin==0,arr.ind = T)


#Cosine similarity with tf-idf
cosin_tfidf<-matrix(nrow=200, ncol=200)
for (i in 1:nrow(tr_idf)){
  for (j in 1:nrow(tr_idf)){
    a <- cosineSim(tr_idf[i,], tr_idf[j,])
    cosin_tfidf[i, j]<- a
    cosin_tfidf[j, i]<- a
  }
}
diag(cosin_tfidf)<- NA
write.csv(cosin_tfidf, "csTFIDF.csv")

#Same as above
dim(which(cosin_tfidf==1,arr.ind = T))
dim(which(cosin_tfidf==0,arr.ind = T))


# # Normalize the rows of the trigram dtm
nmd_tr<-tr
for (i in 1:nrow(nmd_tr)){
  nmd_tr[i,]<- nmd_tr[i,]/sum(nmd_tr[i,])
}
sigma = 90
gauss<- exp(-(as.matrix(dist(nmd_tr)))/sigma)
diag(gauss)<- NA
write.csv(gauss, "gaussian.csv")


#which(gauss==max(gauss, na.rm=T),arr.ind = T)
#which(gauss==min(gauss, na.rm=T),arr.ind = T)



# # Gaussian with tf-idf
idf<- apply(nmd_tr, 2, calcIDF)
normed_idf<- as.matrix(t(apply(nmd_tr, 1, function(x) x*idf)))
gaussNorm<- exp(-(as.matrix(dist(normed_idf)))/sigma)
write.csv(gaussNorm, "gaussNorm.csv")

#54 and 36 as the closest docs
#which(gaussNorm==max(gaussNorm, na.rm=T),arr.ind = T)
#which(gaussNorm==min(gaussNorm, na.rm=T),arr.ind = T)
