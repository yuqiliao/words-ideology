###Assignment 3
###Yuqi Liao
###802642182
###April 13, 2015

getwd()
setwd("/Users/apple/Desktop/MSPP/2nd-Semester Course/PPOL 670-Introduction to Data Science/Assignment_3_Words&Ideology")

#1.download the congression data (using code from Sood)
#https://gist.github.com/soodoku/85d79275c5880f67b4cf
#output:capitolwords113.csv

#2.preprocess the data (using code from Sood)
#https://github.com/soodoku/Text-as-Data
#output:cleanedwords.csv

#3.download the ideology score data
#http://voteview.com/dwnomin.htm
#SL01113D21_PRES_12.DTA
#HL01113D21_PRES_12.DTA

#4.merge the two datasets (the congression data with ideology score data) using a "bridge data", the Charles Stewart's data.

library(foreign)
capitolwords = read.csv("cleanedwords.csv", header = TRUE)
senate <- read.dta("SL01113D21_PRES_12.DTA")
house <- read.dta("HL01113D21_PRES_12.DTA")

###process senate and house data
senate <- senate[!(senate$cong!=113),]
house <- house[!(house$cong!=113),]
bridge <- read.csv("results_plus_hand_entry.csv", header = TRUE)

###merge them all
total <- rbind(senate, house)
merged1 <- merge(total, bridge, by="idno")
merged2 <- merge(merged1, capitolwords, by="bioguide_id")
write.csv(merged2, "mergeddataset.csv")
###output:mergeddataset.csv


#5.come up with lists of bigrams and trigrams for replicants and democratics respectively

merged <- read.csv("mergeddataset.csv", header = TRUE)

Sys.setenv(JAVA_HOME = '/Library/Java//Home')
Sys.setenv(LD_LIBRARY_PATH = '$LD_LIBRARY_PATH:$JAVA_HOME/lib')
install.packages('rJava', type='source')
library(rJava)
options(java.parameters = "-Xmx8000m")
library(RWeka)

###seperate the merged dataset by D and R
d <- subset(merged, speaker_party=='D')
r <- subset(merged, speaker_party=='R')

###create a new variable with the speaking column only.
speakingD<- d$speaking
speakingR<- r$speaking

###bigrams & trigrams
bigrams_unsorted_d <- NGramTokenizer(speakingD, Weka_control(min = 2, max = 2))
trigrams_unsorted_d <- NGramTokenizer(speakingD, Weka_control(min = 3, max = 3))

bigrams_unsorted_r <- NGramTokenizer(speakingR, Weka_control(min = 2, max = 2))
trigrams_unsorted_r <- NGramTokenizer(speakingR, Weka_control(min = 3, max = 3))

bigrams_d <- sort(table(bigrams_unsorted_d),decreasing=T)
trigrams_d <- sort(table(trigrams_unsorted_d),decreasing=T)

bigrams_r <- sort(table(bigrams_unsorted_r),decreasing=T)
trigrams_r <- sort(table(trigrams_unsorted_r),decreasing=T)

write.csv(bigrams_d, "bigrams_d.csv")
write.csv(trigrams_d, "trigrams_d.csv")

write.csv(bigrams_r, "bigrams_r.csv")
write.csv(trigrams_r, "trigrams_r.csv")

###merge data again for chi-square test (to find out the most statistically significant bigrams and trigrams)

bigramsD <- read.csv("bigrams_d.csv", header = TRUE)
bigramsR <- read.csv("bigrams_r.csv", header = TRUE)

trigramsD <- read.csv("trigrams_d.csv", header = TRUE)
trigramsR <- read.csv("trigrams_r.csv", header = TRUE)

mergedbigram <- merge(bigramsD, bigramsR, by="bigram")
mergedtrigram <- merge(trigramsD, trigramsR, by="trigram")

write.csv(mergedbigram, "mergedbigram.csv")
write.csv(mergedtrigram, "mergedtrigram.csv")

###Chi-square test
###Calcuate the total of each column in excel, and re-read the dataset into r
#bigramD:3108783
#bigramR:2754980
#bigramT:5863763
mergedbigramnew <- read.csv("mergedbigram.csv", header = TRUE)
View(mergedbigramnew)
bigram_d_freq <- mergedbigramnew[,3]
bigram_r_freq <- mergedbigramnew[,4]
bigram_t_freq <- mergedbigramnew[,5]

bigram_d_freq_not <- 5863763-bigram_d_freq
bigram_r_freq_not <- 5863763-bigram_r_freq

chisq <- function() {
  numerator = (bigram_r_freq*bigram_r_freq_not-bigram_d_freq*bigram_d_freq_not)^2
  denominator = (bigram_r_freq+bigram_d_freq)*(bigram_r_freq+bigram_r_freq_not)*(bigram_d_freq+bigram_d_freq_not)*(bigram_r_freq_not+bigram_d_freq_not)
  return (numerator/denominator)
}

mergedbigramwithchi <- cbind(mergedbigramnew,chisq())
mergedbigramwithchi_sort <- mergedbigramwithchi[order(chisq(), decreasing = TRUE),] 


###Keep only the top 500 observations for bigram
mergedbigramwithchi_sort_top500 <- head(mergedbigramwithchi_sort, 500)
View(mergedbigramwithchi_sort_top500)
write.csv(mergedbigramwithchi_sort_top500,"mergedbigramwithchi_sort_top500.csv")

###For trigram, do everything again.

#trigramD:660907
#trigramR:566441
#trigramT:1227348
mergedtrigramnew <- read.csv("mergedtrigram.csv", header = TRUE)
View(mergedtrigramnew)

trigram_d_freq <- mergedtrigramnew[,3]
trigram_r_freq <- mergedtrigramnew[,4]
trigram_t_freq <- mergedtrigramnew[,5]

trigram_d_freq_not <- 1227348-trigram_d_freq
trigram_r_freq_not <- 1227348-trigram_r_freq


chisq <- function() {
  numerator = (trigram_r_freq*trigram_r_freq_not-trigram_d_freq*trigram_d_freq_not)^2
  denominator = (trigram_r_freq+trigram_d_freq)*(trigram_r_freq+trigram_r_freq_not)*(trigram_d_freq+trigram_d_freq_not)*(trigram_r_freq_not+trigram_d_freq_not)
  return (numerator/denominator)
}

mergedtrigramwithchi <- cbind(mergedtrigramnew,chisq())
mergedtrigramwithchi_sort <- mergedtrigramwithchi[order(chisq(), decreasing = TRUE),] 

###Keep only the top 500 observations for trigram
mergedtrigramwithchi_sort_top500 <- head(mergedtrigramwithchi_sort, 500)
View(mergedtrigramwithchi_sort_top500)
write.csv(mergedtrigramwithchi_sort_top500,"mergedtrigramwithchi_sort_top500.csv")


#6.Come up with the top 100 republican and democrat bigrams and trigrams in a text file.
###procees by excel
###output:bigram democrat100.csv; bigram republican100.csv; trigram democrat100.csv; trigram republican100.csv
###output(combined txt file):Top 100 republican and democrat bigrams and trigrams.txt


#7.modeling
###turn the mergedbigram dataset from long data into wide data
a <- mergedbigramwithchi_sort_top500
ta <- t(a)
write.csv(ta,"ta.csv")

###Used excel to merge the ta dataset with the dataset that has the ideology score
###output:mergeddataset_new_clean.csv
mergeddataset_new_clean <- read.csv("mergeddataset_new_clean.csv", header = TRUE)
###come up with the function that read through the speaking cells and assignmet values 1 or 0 to the 500 bigram dummy vairables.
View(mergeddataset_new_clean)


###The original code to looping through all the rows runs non-stop(too large)
speaking <- mergeddataset_new_clean$speaking
for (i in 1:nrow(mergeddataset_new_clean)) {
  for (j in 5:ncol(mergeddataset_new_clean)) { 
    if (grepl(colnames(mergeddataset_new_clean)[j],toString(speaking[i]),ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE) == TRUE) 
    { mergeddataset_new_clean[i,j] <- 1}
    else 
    { mergeddataset_new_clean[i,j] <- 0 }
  }  
}

#####zeromatrix <- matrix(0,nrow=nrow(mergeddataset_new_clean),ncol=500)

###Then, try randomly select 5000 rows, which is around 1/10 to all the rows, to save time
mergeddataset_new_clean_sample <- mergeddataset_new_clean[sample(nrow(mergeddataset_new_clean), 5000), ]
speaking <- mergeddataset_new_clean_sample$speaking

nrow <- nrow(mergeddataset_new_clean_sample)
ncol <- ncol(mergeddataset_new_clean_sample)

for (i in 1:5000) {
  for (j in 5:504) { 
    if (grepl(colnames(mergeddataset_new_clean_sample)[j],toString(speaking[i]),ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE) == TRUE) 
    { mergeddataset_new_clean_sample[i,j] <- 1}
    else 
    { mergeddataset_new_clean_sample[i,j] <- 0 }
  }  
}

View(mergeddataset_new_clean_sample)
write.csv(mergeddataset_new_clean_sample,"mergeddataset_new_clean_sample.csv")



###OLS model
mergedbigramwithchi_sort_top500 <- read.csv("mergedbigramwithchi_sort_top500.csv", header = TRUE)
View(mergedbigramwithchi_sort_top500)
fit <- lm(dwnom1 ~ .-dwnom1 -Name.in.Bridge.dataset -speaker_party -speaking, data = mergeddataset_new_clean_sample)
summary(fit)


###Ridge model
library(glmnet)
library(ISLR)

grid <- 10^seq(10,-2,length=100)
x <- model.matrix(dwnom1~.-speaking -Name.in.Bridge.dataset, data=mergeddataset_new_clean_sample)[,-1]
y <- mergeddataset_new_clean_sample$dwnom1

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,]

######split samples into a training set and a test set #
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

######fit a ridge regression model on the training set
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

######check large lambda and lambda=0
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

######cross-validation using cv.glmnet()
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
#########the value of λ that results in the smallest cross-validation error is 5.51076

ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2)

######refit our ridge regression model on the full data set
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]




###Lasso
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

######perform cross-validation and compute the associated test error
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mean((lasso.pred-y.test)^2)

######
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef