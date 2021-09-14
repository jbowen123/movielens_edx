# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

#if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#install.packages("latexpdf")
library(latexpdf)
tinytex::install_tinytex()
#install.packages("tinytex")
library(tinytex)
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(dplyr)
library(vctrs)
library(forcats)
library(ggplot2)
library(hms)
library(stringr)
#install.packages("viridis")
library(viridis)
library(tidyselect)
#install.packages("corrplot")
library(corrplot)
#install.packages("GGally")
library(GGally)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("e1071")
library(e1071)
#install.packages("rmarkdown")
library(rmarkdown)
library(knitr)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
# title = as.character(title),
# genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#####################################################################
###################################################################
##### a review of the database for Movielens project###############



##show a few statistics about the edx database
## first up is counts by rating
##create a database that strips out movie titles and genres descriptions so that the database is smaller and more efficient
edxsmall <- edx[,c(1,2,3,4)]
edxsmall$userId <- as.factor(edxsmall$userId)

##show counts of unique records

unique_count <- edx %>% summarize(n_users= n_distinct(userId), n_movies=n_distinct(movieId), n_genres=n_distinct(genres), n_timestamp=n_distinct(timestamp))
unique_count

###find the mean, median and skewness of the edx rating
meanedxrating <- mean(edxsmall$rating)
medianedxrating <- median(edxsmall$rating)
skewnessedsrating <- skewness(edxsmall$rating)

measures <- tibble(measure=c("mean", "median", "skewness"), value=c(meanedxrating, medianedxrating, skewnessedsrating) )
measures

##look at the distribution of the average movie Rating by a) movie and b) userId
avgmovieratingmovieId <- edxsmall %>% group_by(movieId) %>% summarise(avg_rating = mean(rating))
avgmovieratinguserId <- edxsmall %>% group_by(userId) %>% filter(n()>=100) %>% summarise(avg_rating=mean(rating))
distribavgratingmovieId <- ggplot(avgmovieratingmovieId, aes(x=avg_rating))+geom_histogram(bins=50)+geom_vline(data=avgmovieratingmovieId, aes(xintercept=mean(avg_rating)), col="red")+geom_vline(data=avgmovieratingmovieId, aes(xintercept=median(avg_rating)), col="blue")+ggtitle("distribution of average movie rating by movieId; blue line= median, red line= average of average rating by movieId")
distribavgratinguserId <- ggplot(avgmovieratinguserId, aes(x=avg_rating))+geom_histogram(bins=50)+geom_vline(data=avgmovieratinguserId, aes(xintercept=mean(avg_rating)), col="red")+geom_vline(data=avgmovieratinguserId, aes(xintercept=median(avg_rating)), col="blue")+ggtitle("distribution of average movie rating by userId (n()>=100); blue line= median, red line= average of average rating by userId")

distribavgratingmovieId

distribavgratinguserId


## investigate the Genres variable
##count the genre descriptors
genre_descriptors <- edx[,c(6)] %>% distinct(genres) %>% mutate(count_descriptor=(str_count(genres,pattern="\\|")+1))

##find the most popular genres by rating count
favourite_genres <- edx[,c(3,6)] %>% group_by(genres) %>% summarise(count_ratings=n()) %>% left_join(genre_descriptors, by="genres") %>% arrange(desc(count_ratings)) %>%head(5)

favourite_genres


##find out if genre affects the user or ratings
## need userid, rating and an index which identifies the genres (and group the genres by the first word descriptor)
##first up, devise a useful index number for the genres (otherwise, visuals are almost impossible)
##start with genre_descriptors 

partoneindex <- genre_descriptors %>% mutate(first=word(genres, sep="\\|")) %>% arrange(first, count_descriptor)

uniquefirst <- partoneindex %>% distinct(first) %>% mutate(id=row_number())

##now create the index
index <- partoneindex %>% mutate(genre_index=row_number()) %>% left_join(uniquefirst, by="first") 

countid <- index %>% group_by(id)%>%summarise(n()) %>% arrange(id)
##index2 <- index %>% unite(col="idf", c("id","id2"), sep="")
indexfinal <- index[,c(1,4)]

##create a 2d histogram of rating by userId for each genre

genredata<- edx[,c(1,3,6)] %>% left_join(indexfinal, by="genres") %>% group_by(userId) %>% filter(n()>100) 

genredata2 <- genredata[,c(1,2,4)] %>% group_by(userId,genre_index) %>% summarise(count=n()) 

ggplot(genredata2, aes(x=genre_index,y=userId))+geom_bin2d(bins=120)+scale_fill_continuous(type="viridis")+theme_bw()+ggtitle("Genre (by rating count) by userId")

#now, see if the ratings which are above the edx mean have an impact on that 2d histogram
##select the count for all rating above the edx mean

meanrating <- mean(edx$rating)
genredata3 <- genredata[,c(1,2,4)] %>% filter(rating==5) %>% group_by(userId,genre_index) %>% summarise(count=n())

ggplot(genredata3, aes(x=genre_index,y=userId))+geom_bin2d(bins=120)+scale_fill_continuous(type="viridis")+theme_bw()+ggtitle("Genre (by rating count) by userId; rating=5")

##find out if the number of genre descriptors has any impact on ratings
highlyratedgenres <- edx[,c(3,6)] %>% filter(rating == 5)  %>% left_join(genre_descriptors, by="genres") %>% group_by(count_descriptor) %>% summarise(count=n())%>% arrange((count_descriptor))

highlyratedgenres

###look at how count of genre descriptors affects movie ratings...this has not been included in the report
##moviesandgenre <- edx[, c(2,3,6)] %>% filter(rating==5)%>% left_join(genre_descriptors, by="genres") %>% group_by(count_descriptor) %>% summarise(n())
##moviesandgenre

####look at the timestamp variable
##first, convert the timestamp to a POSIXct variable, then create the year, month, day, hours variables
## do further investigations for the movieId with ratings more than 1000 in a year (to try to keep the size of the database manageable)
##remove the title and the genres...replace genres with the index number for the genre (use )
small <- edx[,c(1,2,3,4,6)] %>% left_join(indexfinal, by="genres")
smaller <- small[,c(1,2,3,4,6)]
movietime <- smaller %>% mutate(time= as_datetime(timestamp), year=year(time), month=month(time), mday=mday(time), wday=wday(time), hour=hour(time)) 

## look for correlations with ratings (count, average rating) by year, month, mday, wday,hour
###these estimations were not used in the report...but were informative and therefore kept in the program
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
install.packages("Hmisc")
library(Hmisc)


yeardate <- movietime[,c(3,7)] %>% group_by(year) %>% summarise(count=n(), avgrating=mean(rating))

##unique year was estimated but not used in the report
uniqueyear <- yeardate %>% distinct(year)

shapiro_count<- shapiro.test(yeardate$count)
shapiro_avgrate <- shapiro.test(yeardate$avgrating)
shapiro_count
shapiro_avgrate
##corrplot(correlate_year, method="ellipse", sig.level=.01, insig="blank", diag=FALSE)+ggtitle("Correlation estimates between Year (from timestamp), average rating, count of rating")

##make a graph

yearplotcount <- ggplot(yeardate, aes(x=as.factor(year), y=count, label=round(count, digits=0)))+geom_point()+geom_segment(aes(x=as.factor(year), xend=as.factor(year), y=0, yend=count))+geom_text(size=4, vjust=-0.75)+xlab("Year")+ylab("Count of rating (in thousands)")+ggtitle("Count of rating by Year of rating")
yearplotavgrate <- ggplot(yeardate, aes(x=as.factor(year), y=avgrating, label=round(avgrating, digits=3)))+geom_point()+geom_segment(aes(x=as.factor(year), xend=as.factor(year), y=0, yend=avgrating))+geom_text(size=4, vjust=-0.75)+xlab("Year")+ylab("Average rating")+ggtitle("Average rating by Year of rating")

yearplotcount
yearplotavgrate

correlate_year <- cor(x=yeardate$year, y=yeardate$avgrating, method="pearson")
correlate_year

##ggplot(yeardate, aes(x=as.factor(year), y=avgrating, label=round(avgrating, digits=3)))+geom_point()+geom_segment(aes(x=as.factor(year), xend=as.factor(year), y=0, yend=avgrating))+geom_text(size=4, vjust=-0.75)+xlab("Year")+ylab("Average rating")+ggtitle("Average rating by Year of rating")


##make a 2d histogram of rating, counts of that rating, with year of the x axis (rating on the y axis, count of rating might not work)
yeardate2 <- movietime[,c(3,7)]

ggplot(yeardate2, aes(x=year, y=rating))+geom_bin2d()+scale_fill_viridis_c(option="magma")+theme_bw()+ggtitle("Count of rating score by year") #+scale_x_continuous(breaks=c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

## try the analysis of the 'release year' of the movies....does release year reveal anything useful
##first, you need to extract the year of release from the title

releaseyear <- edx %>% mutate(release_year = as.numeric(str_sub(title,-5,-2))) %>% left_join(indexfinal, by="genres")
releaseyeargrouped <- releaseyear[,c(3,7)] %>% group_by(release_year) %>% summarise(avg_rating=mean(rating), count=n())
justreleaseyear <- releaseyear[,c(5)] %>% distinct(title) %>% mutate(release_year = as.numeric(str_sub(title,-5,-2))) %>% group_by(release_year) %>% summarise(count=n()) 
ryearrelease <-ggplot(justreleaseyear, aes(x=as.factor(release_year), y=count))+geom_point()+geom_segment(aes(x=as.factor(release_year), xend=as.factor(release_year), y=0, yend=count))+xlab("Release Year")+ylab("Count of titles")+ggtitle("Count of titles by Release Year")+scale_x_discrete(breaks=c(1915, 1925, 1935, 1945, 1955, 1965, 1975, 1985, 1995, 2005, 2015))##+scale_y_continuous(labels="comma")
yearplotcountr <- ggplot(releaseyeargrouped, aes(x=as.factor(release_year), y=count))+geom_point()+geom_segment(aes(x=as.factor(release_year), xend=as.factor(release_year), y=0, yend=count))+xlab("Release Year")+ylab("Count of rating")+ggtitle("Count of rating by Release Year of rating")+scale_x_discrete(breaks=c(1915, 1925, 1935, 1945, 1955, 1965, 1975, 1985, 1995, 2005, 2015))##+scale_y_continuous(labels="comma")
yearplotavgrater <- ggplot(releaseyeargrouped, aes(x=as.factor(release_year), y=avg_rating, label=round(avg_rating, digits=3)))+geom_point()+geom_segment(aes(x=as.factor(release_year), xend=as.factor(release_year), y=0, yend=avg_rating))+xlab("Release Year")+ylab("Average rating")+ggtitle("Average rating by Release Year of rating")+scale_x_discrete(breaks=c(1915, 1925, 1935, 1945, 1955, 1965, 1975, 1985, 1995, 2005, 2015))


ryearrelease

yearplotcountr

yearplotavgrater
###look at months
## is there any difference in counts or rating by month?

months <- movietime[,c(3,8)] %>% group_by(month) %>% summarise(avg_rating=mean(rating), count=n())
monthplotsc <- ggplot(months, aes(x=as.factor(month), y=count))+geom_point()+geom_segment(aes(x=as.factor(month), xend=as.factor(month), y=0, yend=count))+xlab("Month")+ylab("Count of rating")+ggtitle("Count of rating by Month")
monthplotsr <- ggplot(months, aes(x=as.factor(month), y=avg_rating))+geom_point()+geom_segment(aes(x=as.factor(month), xend=as.factor(month), y=0, yend=avg_rating))+xlab("Month")+ylab("Average rating")+ggtitle("Average rating by Month")

monthplotsc
monthplotsr

##try the 2d histogram for average monthly ratings - the scale is better and there is the perceptible difference across months
monthplothist <- ggplot(months, aes(x=as.factor(month), y=avg_rating))+geom_bin2d(show.legend=FALSE)+scale_fill_viridis_c(option="inferno", direction=1)+theme_bw()+ggtitle("average rating by month") +scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+ylab("average rating by month")

monthplothist
##if there is an uptick in July for blockbusters and in November for Oscar winners, does this affect genres...are genres selections based on month?

genresbymonth <- movietime[,c(3,5,8)]  %>% group_by(month, genre_index) %>% summarise(count=n(), avgrating=mean(rating)) %>% filter(avgrating>=3.0) 
histgenresbymonth <- ggplot(genresbymonth, aes(x=month, y=genre_index))+geom_bin2d()+scale_fill_viridis_c(option="inferno", direction=1)+theme_bw()+ggtitle("count of genres by month, average rating >= 3.0") +scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+ylab("genre by index - alphabetical order")

histgenresbymonth


##what is going on with weeks, days and hours?  is there a simple way to get this done? yes.  mday
### the analysis of days, weeks and hours was not used in the report (dead end for my purposes, but informative)
mdays <- movietime[,c(3,9)] %>% group_by(mday) %>% summarise(avg_rating=mean(rating), count=n())
mdayplotsc <- ggplot(mdays, aes(x=as.factor(mday), y=count))+geom_point()+geom_segment(aes(x=as.factor(mday), xend=as.factor(mday), y=0, yend=count))+xlab("Day of Month")+ylab("Count of rating")+ggtitle("Count of rating by Day of Month")

mdayplotsc
#mdayplotsr <- ggplot(mdays, aes(x=as.factor(mday), y=avg_rating))+geom_point()+geom_segment(aes(x=as.factor(mday), xend=as.factor(mday), y=0, yend=avg_rating))+xlab("Day of Month")+ylab("Average rating")+ggtitle("Average rating Day by Month")
#mdayplotsr

histbymonday <- ggplot(mdays, aes(x=as.factor(mday), y=avg_rating), show.legend=FALSE)+geom_bin2d(show.legend=FALSE)+scale_fill_viridis_c(option="inferno", direction=1)+theme_bw()+ggtitle("average rating by day of month") +scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31))+ylab("day of month")
histbymonday



##go back to correlation tests....create a table with rating, count, by day and by hour...see what comes out


dayshours <- movietime[,c(3,10,11)] %>% group_by(wday,hour) %>% summarise(avg_rating=mean(rating), count=n())

shapiro.test(dayshours$count)
shapiro.test(dayshours$avg_rating)

corrMatwday <- cor(dayshours)
corrplot(corrMatwday, method="number", title="Correlation plot of days and hours, average rating, count of rating", mar=c(0,0, 1,0))

##try another way to test for covariance
ggscatter(dayshours,x="wday", y="avg_rating", add="reg.line", conf.int=TRUE, cor.coef=TRUE, cor.method="pearson", xlab="Week day", ylab="avg_rating")

ggscatter(dayshours,x="hour", y="avg_rating", add="reg.line", conf.int=TRUE, cor.coef=TRUE, cor.method="pearson", xlab="hour", ylab="avg_rating")

barcountbyhour <- ggplot(dayshours, aes(x=as.factor(hour), y=count))+geom_bar(stat="identity")+ggtitle("Count of ratings by hour of day")
barcountbyhour


####remove files no longer required, to free up more RAM
rm(avgmovieratingmovieId, avgmovieratinguserId, barcountbyhour, countid, dayshours,distribavgratingmovieId,
   distribavgratinguserId, favourite_genres, genre_descriptors, genredata, genredata2, genredata3, genresbymonth,
   highlyratedgenres, histbymonday, histgenresbymonth, index, justreleaseyear, mdayplotsc, mdays, measures,
   monthplothist, monthplotsc, monthplotsr, movietime, partoneindex, releaseyear, releaseyeargrouped, ryearrelease,
   shapiro_avgrate, shapiro_count, small, smaller, unique_count, uniquefirst, uniqueyear, yeardate, yeardate2,
   yearplotavgrater, yearplotcount, yearplotcountr, months)


###################### End of review of database#######################################
#########################################################################################
########################################################################################
###################### prediction models begin ########################################

####create the bins for the timestamp variable and the other time related variables
### the timestamp is rounded to the 'day' (which means, later on, timestamps do not line up exactly from origin or the manipulated bin version)
timeset <- edx %>% mutate(time= round_date(as_datetime(timestamp),"day")) 

##find the minimum date in order to create the time bins

min<- min(timeset$time)

timestamplist <- timeset[, 7] %>% distinct() %>% arrange(time) %>% mutate(bin=1)

### taking note of the Netflix Prize paper, bins were arranged to be approximately 10 weeks
bin_days <- 70

## make a database of bins and then fit it to the timestamplist
##step by step - because this is a new process
smalltimelist <- timestamplist %>% mutate(newt=as.numeric(time))
newt <- as.matrix(smalltimelist[,3], nrow=4633, ncol=1)
class(newt)

df <- matrix(1:75, nrow=75, ncol=1)

newdf <- as.data.frame(df) %>% mutate(bin=min +days(70*(V1-1)), binend=bin+days(70))

newdf1 <- newdf %>% mutate(binendd = as.numeric(binend))
newdf2 <- as.vector(newdf1[,4])

newdf3 <- newdf %>% mutate(binb=as.numeric(bin))
newdf4 <- as.vector(newdf3[,4])
class(newdf2)

length(newdf2)
##merge timestamplist on the binend of newdf...

categories <- data.frame(newt, bin=cut(newt, breaks=c(newdf4),labels=FALSE, include.lowest=TRUE, right=TRUE,dig.lab=8))
categories2 <- categories %>% mutate(thing=as.POSIXct(newt, origin="1970-01-01"))

##now, join the two tables categories2 and smalltimelist by newt
smalltimelist2 <- smalltimelist[,c(1,3)]
allnewt <- smalltimelist2 %>% left_join(categories2, by="newt") %>% mutate(timed =as.POSIXct(time, tryFormats=c("%Y-%m-%d")))

##now, try to join allnewt back into the timeset database by time

timesetbinned <- timeset %>% left_join(allnewt, by="time")
str(timesetbinned)
edxbinned <- timesetbinned[,c(1,2,3,4,6,7,9, 10, 11)]


##as an aside....count of rating by userId on a particular days is interesting...many movies, one day..too often the case
testthis <- edxbinned %>% filter(userId==13) %>% group_by(userId, time) %>% summarise(countid = n())
mean_count_per_day <- edxbinned %>% group_by(userId, time) %>% summarise(countd=n())
mean(mean_count_per_day$countd)
##create the frequency variable...the log of the number of ratings on a single day by one user

frequencyrate <- edxbinned %>% group_by(userId, time) %>% summarise(countd=n()) %>% mutate(logF=log(countd))

##scale the frequency variable to a z distribution
sdlogF <- sd(frequencyrate$logF)
meanlogF <- mean(frequencyrate$logF)
frequencyratescaled <- frequencyrate %>% mutate(scale_logF=(logF-meanlogF)/sdlogF)
minF<-min(frequencyrate$logF)
minF

edxbinned4 <- edxbinned %>%  left_join(frequencyratescaled[,c(1,2,5)], by=c("userId", "time")) %>% mutate(ratings=rating)

###scale the ratings variable to a z distribution
sdrating <- sd(edxbinned4$ratings)
meanrating <- mean(edxbinned4$ratings)

edxbinned3 <- edxbinned4 %>% mutate(rating=(ratings-meanrating)/sdrating)
##test for mean at zero
mrating<- mean(edxbinned3$rating)


##enough variables....create the train set and the test set
##reduce the size of the table
edxshort <- edxbinned3[,c(1,2,3,5,7,10)]


set.seed(755)
test_index <- createDataPartition(y=edxshort$rating, times=1, p=0.2, list=FALSE)

train_set <- edxshort[-test_index,]
test_set <- edxshort[test_index,]

test_set<- test_set %>% semi_join(train_set, by = "movieId") %>% semi_join(train_set, by = "userId")


###remove the large files that are no longer required
rm(edx, edxbinned, edxbinned3, edxbinned4, edxshort, edxsmall, frequencyrate, frequencyratescaled, mean_count_per_day, timeset, timesetbinned)

##now, begin the testing of the model

##create the baseline model ('naive model')

##create the RMSE function

RMSE <- function(true_ratings, predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

##create a results table
rmse_results <- tibble(method="just the average", RMSE=naive_rmse)
format(rmse_results, digits=5, nsmall=5)


###create the b_i for the movieId effect 
mu <- mean(train_set$rating)

movie_avgs <- train_set %>% group_by(movieId) %>% summarise(b_i = mean(rating-mu))

qplot(b_i, data=movie_avgs, bins=35, color=I("black"))+ ggtitle("b_i estimates for movie effect")+xlab("estimates of b_i")+ylab("count of b_i estimate")

##put in y_hat (u,i) = u_hat + b_hat(i)

predicted_ratings <- mu + test_set %>% left_join(movie_avgs, by=c("movieId")) %>% pull(b_i)

movie_effect_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect", RMSE=movie_effect_rmse)


#include bin effect and movie effects
bin_avgs <- train_set %>% left_join(movie_avgs, by ="movieId") %>% group_by(bin) %>% summarise(b_b = mean(rating-mu-b_i))

##train_set %>% group_by(bin) %>% filter(n() >=50)%>% summarise(b_b = mean(rating)) %>% ggplot(aes(b_b))+geom_histogram(bins=35, color="black")+ggtitle("b_b estimates for bin effect")
qplot(b_b, data=bin_avgs, bins=35, color=I("black"))+ ggtitle("b_b estimates for movie effect")+xlab("estimates of b_b")+ylab("count of b_b estimate")


##put in y_hat (u,i) = u_hat + b_hat(i)

predicted_ratings <- test_set %>% left_join(movie_avgs, by="movieId")%>%left_join(bin_avgs, by=c("bin")) %>% mutate(pred=mu+b_i+b_b)%>%pull(pred)

movie_and_bin_effect_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include bin and movie effect", RMSE=movie_and_bin_effect_rmse)


##movie effect plus bin effect plus user effect
##average rating for user u for those that have rated 100 or more movies
#train_set %>% group_by(userId) %>% filter(n() >=100)%>% summarise(b_u = mean(rating)) %>% ggplot(aes(b_u))+geom_histogram(bins=30, color="black")+ggtitle("b_u estimates for user effect")

user_avgs <- train_set %>% left_join(movie_avgs, by="movieId") %>% left_join(bin_avgs, by="bin") %>% group_by(userId) %>% summarise(b_u = mean(rating- mu - b_i - b_b))

qplot(b_u, data=user_avgs, bins=35, color=I("black"))+ ggtitle("b_u estimates for user effect")+xlab("estimates of b_u")+ylab("count of b_u estimate")


predicted_ratings <- test_set %>% left_join(movie_avgs, by="movieId") %>% left_join(bin_avgs, by="bin")%>%left_join(user_avgs, by='userId') %>%mutate(pred=mu+b_i+b_b+b_u)%>%pull(pred)
user_effect_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect + bin effect + user effect", RMSE=user_effect_rmse)

####add the effect of the frequency (overall number of ratings a user gave on a single day)
#train_set %>% group_by(scale_logF) %>% filter(n()>= 5) %>% summarise(b_F = mean(rating)) %>% ggplot(aes(b_F))+geom_histogram(bins=30, color="black")+ggtitle("b_F estimates for rating frequency effect")

freq_avgs <- train_set%>% left_join(movie_avgs, by="movieId") %>% left_join(bin_avgs, by="bin") %>% left_join(user_avgs, by="userId")%>% group_by(scale_logF) %>% summarise(b_F = mean(rating- mu - b_i - b_b - b_u))

qplot(b_F, data=freq_avgs, bins=35, color=I("black"))+ ggtitle("b_F estimates for frequency of rating effect")+xlab("estimates of b_F")+ylab("count of b_F estimate")


predicted_ratings <- test_set %>% left_join(movie_avgs, by="movieId") %>% left_join(bin_avgs, by="bin")%>%left_join(user_avgs, by='userId') %>% left_join(freq_avgs, by="scale_logF") %>%mutate(pred=mu+b_i+b_b+b_u+b_F)%>%pull(pred)
freq_effect_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect + bin effect + user effect+freq effect", RMSE=freq_effect_rmse)

##add the genre effect
##include a genre effect.  keep only categories of genre with more than 1000 ratings.  make the cor plot. run model.
#train_set %>% group_by(genres) %>% filter(n()>= 1000) %>% summarise(b_g = mean(rating)) %>% ggplot(aes(b_g))+geom_histogram(bins=30, color="black")+ggtitle("b_g estimates for genre effect")

genre_avgs <- train_set %>% left_join(movie_avgs, by="movieId") %>% left_join(bin_avgs, by="bin") %>% left_join(user_avgs, by="userId") %>% left_join(freq_avgs, by="scale_logF") %>% group_by(genres) %>% summarise(b_g = mean(rating- mu - b_i - b_u-b_b-b_F))

qplot(b_g, data=genre_avgs, bins=35, color=I("black"))+ ggtitle("b_g estimates for genres effect")+xlab("estimates of b_g")+ylab("count of b_g estimate")


predicted_ratings <- test_set %>% left_join(movie_avgs, by="movieId") %>% left_join(user_avgs, by='userId') %>% left_join(bin_avgs, by="bin")%>% left_join(freq_avgs, by="scale_logF")%>% left_join(genre_avgs, by="genres") %>% mutate(pred=mu+b_i+b_u + b_b+b_F+ b_g)%>%pull(pred)
genres_effect_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect + bin effect + user effect+freq effect+genre effect", RMSE=genres_effect_rmse)


##standardize the variables for input
## find lambda with standardized movie effect and user effect 

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)+ggtitle("find lambda for movie effect + user effect") 

##take the value of lambda from the plot and install into the lambda value for standardizing

lambda <- 5.0

## get movie_reg_avgs and user_reg_avgs
mu <- mean(train_set$rating)
movie_reg_avgs5 <- train_set %>% group_by(movieId) %>% summarise(b_i = sum(rating - mu)/(n()+lambda), n_i=n())

#tibble(original= movie_avgs$b_i, regularized=movie_reg_avgs$b_i, n=movie_reg_avgs$n_i) %>% ggplot(aes(original, regularized, size=sqrt(n)))+geom_point(shape=1, alpha=0.5)+ggtitle("Regularized movie effect")

bin_reg_avgs5 <- train_set %>% left_join(movie_reg_avgs5, by="movieId") %>% group_by(bin) %>% summarise(b_b = sum(rating - mu - b_i)/(n()+lambda), n_u=n())

user_reg_avgs5 <- train_set %>% left_join(movie_reg_avgs5, by="movieId") %>% left_join(bin_reg_avgs5, by="bin") %>% group_by(userId) %>% summarise(b_u = sum(rating - mu - b_i - b_b)/(n()+lambda), n_u=n())

#tibble(original= user_avgs$b_u, regularized=user_reg_avgs$b_u, n=user_reg_avgs$n_u) %>% ggplot(aes(original, regularized, size=sqrt(n)))+geom_point(shape=1, alpha=0.5)+ggtitle("Regularized user effect")

freq_reg_avgs5 <- train_set %>% left_join(movie_reg_avgs5, by="movieId") %>% left_join(user_reg_avgs5, by="userId") %>%left_join(bin_reg_avgs5, by="bin") %>% group_by(scale_logF) %>% summarise(b_F = sum(rating - mu - b_i -b_b -b_u)/(n()+lambda), n_g=n())

genre_reg_avgs5 <- train_set %>% left_join(movie_reg_avgs5, by="movieId") %>% left_join(user_reg_avgs5, by="userId") %>%left_join(bin_reg_avgs5, by="bin") %>% left_join(freq_reg_avgs5, by="scale_logF") %>% group_by(genres) %>% summarise(b_g = sum(rating - mu - b_i -b_b -b_u-b_F)/(n()+lambda), n_g=n())
#tibble(original= genre_avgs$b_g, regularized=genre_reg_avgs$b_g, n=genre_reg_avgs$n_g) %>% ggplot(aes(original, regularized, size=sqrt(n)))+geom_point(shape=1, alpha=0.5)+ggtitle("Regularized genres effect")


##find the new RMSE
#just movieId
#qplot(b_i, data=movie_reg_avgs, bins=15, color=I("black"))+ ggtitle("b_i estimates for regularized movie effect")+xlab("estimates of regularized b_i")+ylab("count of regularized b_i estimate")
predicted_ratings <- mu + test_set %>% left_join(movie_reg_avgs5, by='movieId') %>% pull(b_i)
reg_movie_effect_lambda5 <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect, regularize with lambda = 5", RMSE=reg_movie_effect_lambda5)


##movie id and bin effect

predicted_ratings <- test_set %>% left_join(movie_reg_avgs5, by="movieId") %>% left_join(bin_reg_avgs5, by='bin') %>%mutate(pred=mu+b_i+b_b)%>%pull(pred)
reg_movie_and_bin_lambda5 <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect + bin effect, regularize with lambda = 5", RMSE=reg_movie_and_bin_lambda5)


##movid id and user effect and bin effect

predicted_ratings <- test_set %>% left_join(movie_reg_avgs5, by="movieId") %>% left_join(user_reg_avgs5, by='userId') %>% left_join(bin_reg_avgs5, by="bin") %>% mutate(pred=mu+b_i+b_u + b_b)%>%pull(pred)
reg_movie__user_genres_lambda5 <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect + bin effect + user effect, regularize with lambda = 5", RMSE=reg_movie__user_genres_lambda5)

##movie id and user effect and frequency effect and bin effect
predicted_ratings <- test_set %>% left_join(movie_reg_avgs5, by="movieId") %>% left_join(user_reg_avgs5, by='userId') %>% left_join(bin_reg_avgs5, by="bin") %>% left_join(freq_reg_avgs5, by="scale_logF") %>% mutate(pred=mu+b_i+b_u + b_b +b_F)%>%pull(pred)

reg_movie__user_bin_freq_lambda5 <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect + user effect + bin effect+freq effect, regularize with lambda = 5", RMSE=reg_movie__user_bin_freq_lambda5)

##movie id, user effect, bin effect, frequency effect, genres effect

predicted_ratings <- test_set %>% left_join(movie_reg_avgs5, by="movieId") %>% left_join(user_reg_avgs5, by='userId') %>% left_join(bin_reg_avgs5, by="bin") %>% left_join(freq_reg_avgs5, by="scale_logF") %>% left_join (genre_reg_avgs5, by="genres") %>% mutate(pred=mu+b_i+b_u + b_b +b_F+b_g)%>%pull(pred)

reg_movie__user_bin_freq__genres_lambda5 <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- add_row(rmse_results, method="include movie effect + user effect + bin effect+freq effect+genres effect, regularize with lambda = 5", RMSE=reg_movie__user_bin_freq__genres_lambda5)


####
#####test the validation set

####first step is to create the bins for the validation set
validationtimeset <- validation %>% mutate(time= round_date(as_datetime(timestamp),"day")) 
validationbinned <- validationtimeset %>% left_join(allnewt, by="time")
validationbin <- validationbinned[,c(1,2,3,4,6,7,9, 10, 11)]
##create frequency variable
validfrequencyrate <- validationbin %>% group_by(userId, time) %>% summarise(countd=n()) %>% mutate(logF=log(countd))

##scale the frequency variable to a z distribution
validsdlogF <- sd(validfrequencyrate$logF)
validmeanlogF <- mean(validfrequencyrate$logF)
validfrequencyratescaled <- validfrequencyrate %>% mutate(scale_logF=(logF-meanlogF)/sdlogF)
minF<-min(validfrequencyrate$logF)
minF
mean_count_per_day <- validationbinned %>% group_by(userId, time) %>% summarise(countd=n())
mean(mean_count_per_day$countd)

validbinned4 <- validationbinned %>%  left_join(validfrequencyratescaled[,c(1,2,5)], by=c("userId", "time")) %>% mutate(ratings=rating)

###scale the ratings variable to a z distribution
validationsdrating <- sd(validbinned4$ratings)
validmeanrating <- mean(validbinned4$ratings, na.rm=TRUE)

validbinned3 <- validbinned4 %>% mutate(rating=(ratings-validmeanrating)/validationsdrating)
##test for mean at zero
validmrating<- mean(validbinned3$rating)
##reduce the size of the table
validationshort <- validbinned3[,c(1,2,3,4,5,6,7,9,12)]

str(validationshort)
###find the RMSE of the model#####for some reason the function does not work, so it is calculated

predicted_ratings <- validationshort %>% left_join(movie_reg_avgs5, by="movieId") %>% left_join(user_reg_avgs5, by='userId') %>% left_join(bin_reg_avgs5, by="bin") %>% left_join(freq_reg_avgs5, by="scale_logF") %>% left_join (genre_reg_avgs5, by="genres") %>% mutate(pred=mu+b_i+b_u + b_b +b_F+b_g)%>%pull(pred)
#test_set<- validationshort
#str(test_set)
pred_rating <- as.vector(predicted_ratings)
validation_rating <- as.vector(validationshort$rating)
calc<- as_tibble(cbind(pred_rating, validation_rating))%>% mutate(subtract=(validation_rating-pred_rating)^2)
RMSE<- sqrt(mean(calc$subtract, na.rm=TRUE))
RMSE


###Check again
findRMSE <- sqrt(mean((validation_rating-pred_rating)^2, na.rm=TRUE))
findRMSE

rmse_results <- add_row(rmse_results, method="validation set", RMSE=findRMSE)
