
library(rpart)
library(rpart.plot)
library(corrplot)
library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)

imdb <- read.csv("movie_metadata.csv")

View(imdb)
str(imdb)
head(imdb)
summary(imdb)
names(imdb)
dim(imdb)
#finding null values in each column
colSums(sapply(imdb,is.na))

#Removing rows having null values in gross and budget
imdb <- imdb[!is.na(imdb$gross), ]
imdb <- imdb[!is.na(imdb$budget), ]
#Keeping movies only from USA
imdb <- imdb[(imdb$country == "USA"), ]

# checking for duplicate rows and removing
sum(duplicated(imdb))
imdb <- imdb[!duplicated(imdb), ]


sum(sapply(imdb, is.na))
#total null values in dataset now
colSums(sapply(imdb,is.na))
summary(imdb)

# Removing genres and aspect_ratio variables,plot_keywords and movie_imdb_link
imdb <- subset(imdb, select = -c(genres))
imdb <- subset(imdb, select = -c(aspect_ratio))
imdb <- subset(imdb, select = -c(movie_imdb_link))
imdb <- subset(imdb, select = -c(plot_keywords))


imdb <- imdb[!is.na(imdb$facenumber_in_poster), ]
imdb <- imdb[!is.na(imdb$actor_3_facebook_likes), ]
imdb <- imdb[!is.na(imdb$num_critic_for_reviews), ]
colSums(sapply(imdb, is.na))

#checking for zero values
colSums(imdb == 0)
#removing variables having large number of zeroes
#imdb <- subset(imdb, select = -c(director_facebook_likes))
imdb <- subset(imdb, select = -c(movie_facebook_likes))
imdb <- subset(imdb, select = -c(facenumber_in_poster))

dim(imdb)

colSums(imdb == 0)

summary(imdb)

boxplot(imdb$gross~ imdb$title_year, xlab = "Year", ylab = "Gorss Revenue")
ggplot(imdb, aes(title_year))+ geom_bar()

#content-rating has many factors with same kind of rating
summary(imdb$content_rating)

#removing spaces from the content rating (21 values)
imdb <- imdb[!(imdb$content_rating %in% ""),]

#clubbing all same kind of ratings together !
imdb$content_rating[imdb$content_rating == 'M']   <- 'PG' 
imdb$content_rating[imdb$content_rating == 'GP']  <- 'PG' 
imdb$content_rating[imdb$content_rating == 'X']   <- 'NC-17'

#adding unknown content_ratings to restricted 
imdb$content_rating[imdb$content_rating == 'Approved']  <- 'R' 
imdb$content_rating[imdb$content_rating == 'Not Rated'] <- 'R' 
imdb$content_rating[imdb$content_rating == 'Passed']    <- 'R' 
imdb$content_rating[imdb$content_rating == 'Unrated']   <- 'R' 
imdb$content_rating <- factor(imdb$content_rating)

summary(imdb$content_rating)

#keeping only movies after 1980
imdb <- imdb[imdb$title_year >= 1980,]

#creating return on investment ratio
imdb$roi <- (imdb$gross-imdb$budget)/imdb$budget
plot(imdb$roi~imdb$budget,ylim = c(-1.5,20),ylab="ROI",xlim=c(0,50000000),xlab="Budget $")

#removing outliers having very high ROI
imdb <- imdb[imdb$roi <= 20,]

View(imdb)

ggcorr(imdb, label = TRUE, label_round = 2, label_size = 3, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

#for removing high collinearity > 0.70
imdb$other_actors_facebook_likes <- imdb$actor_2_facebook_likes + imdb$actor_3_facebook_likes
imdb <- subset(imdb,select = -c(cast_total_facebook_likes, 
                                actor_2_facebook_likes,actor_3_facebook_likes))
imdb <- subset(imdb,select = -c(num_user_for_reviews))

ggcorr(imdb, label = TRUE, label_round = 2, label_size = 3, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

#Defining ROI_class
imdb$roi_class <- imdb$roi>= 0
imdb$roi_class <- factor(imdb$roi_class, levels = c(TRUE,FALSE), 
                         labels = c("Profitable", 
                                    "Non-profitable"))

summary(imdb)
head(imdb)
names(imdb)
imselect <- imdb[,c(4,7,15,16,20,21)]

set.seed(10)
train.index <- sample(row.names(imselect), dim(imselect)[1]*0.6)
valid.index <- setdiff(row.names(imselect), train.index)
train <- imselect[train.index, ]
valid <- imselect[valid.index, ]


#DECISION-TREE
str(train)
names(train)


cv.ct <- rpart(roi_class ~ ., data = train, cp= 0.0001, method="class",
               minsplit=9, maxdepth=15) 
#prp(cv.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0) 
printcp(cv.ct)
pruned.ct <- prune(cv.ct, cp=0.00563910)
options(scipen=999)
prp(pruned.ct,type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0)
library(forecast)

pred.train <- predict(pruned.ct,train[,-6],type = "class")
confusionMatrix(pred.train, as.factor(train$roi_class))

pred.valid <- predict(pruned.ct,valid[,-6],type = "class")
confusionMatrix(pred.valid, as.factor(valid$roi_class))

#NEURALNETWORK
names(imdb)
IMDB <- imdb
head(IMDB)
IMDB$roi_class <- factor(IMDB$roi_class, levels = c("Profitable","Non-profitable"), labels = c("1","0"))
head(IMDB)
summary(IMDB)

imdbnn <- IMDB[ ,c(4,7,15,16,20,21)]
str(imdbnn)

#converting content rating in dummy variable
library(dummies)
content_rating_cat <- dummy(imdbnn$content_rating,sep="_")
content_rating_cat <- as.data.frame(content_rating_cat)
head(content_rating_cat)
imdbnn <- cbind.data.frame(imdbnn[,-3], content_rating_cat)

#normalizing data in 0-1
str(imdbnn)
f <- function(x) (x - min(x))/ (max(x) - min(x))
norm <- as.data.frame(apply(imdbnn[,c(1,2,3,4)],2,f))

names(imdbnn)
imdbnn<- cbind.data.frame(norm, imdbnn[ ,-c(1,2,3,4)])
head(imdbnn)
str(imdbnn)

names(imdbnn)
names(imdbnn)[7]<- "content_rating_NC17"
names(imdbnn)[9]<- "content_rating_PG13"
names(imdbnn)
set.seed(10)
train.index <- sample(row.names(imdbnn), dim(imdbnn)[1]*0.6)
valid.index <- setdiff(row.names(imdbnn), train.index)
train <- imdbnn[train.index, ]
valid <- imdbnn[valid.index, ]
str(train)
names(train)
library(neuralnet)

nn <- neuralnet(roi_class ~ duration + actor_1_facebook_likes + budget + other_actors_facebook_likes
                + content_rating_G + content_rating_NC17 + content_rating_PG  + content_rating_PG13 + content_rating_R
                , data = train[ ,c(1:10)], linear.output = F, err.fct = "ce", hidden = 5)

nn$weights
prediction(nn)
plot(nn, rep="best")
library(forecast)
predict.train <- compute(nn, train[,-5])
predict.train$net.result
predicted.class = apply(predict.train$net.result,1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class =="0", "0", "1")), train$roi_class)

predict.valid <- compute(nn, valid[,-5])
predict.valid$net.result
predicted.class = apply(predict.valid$net.result,1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class =="0", "0", "1")), valid$roi_class)

