Listing_Oct <- read.csv("/Users/VickyWu/Desktop/R datasets/airbnb_listings_0618.csv")
str(Listing_Oct)

#host_response_rate, price, cleaning fee should be numerics instead of factors
Listing_Oct$host_response_rate <- as.numeric(as.character(Listing_Oct$host_response_rate))
Listing_Oct$price <- as.numeric(sub('$','',as.character(Listing_Oct$price),fixed=TRUE)) 
Listing_Oct$cleaning_fee <- as.numeric(sub('$','',as.character(Listing_Oct$cleaning_fee),fixed=TRUE)) 
str(Listing_Oct)

#Test NAs and replace NAs wtih means
is.na(Listing_Oct$listing_id) #no NAs
is.na(Listing_Oct$host_id) #no NAs
is.na(Listing_Oct$host_listings_count) #yes
#Other NAs in int/num variables can be examined from looking at str(Listing_Oct)
host_response_rate.mean <- mean(Listing_Oct$host_response_rate, na.rm=TRUE)
Listing_Oct$host_response_rate[is.na(Listing_Oct$host_response_rate)]=host_response_rate.mean
host_response_rate.mean
Listing_Oct$host_response_rate

listing_count.mean <- mean(Listing_Oct$host_listings_count, na.rm=TRUE)
Listing_Oct$host_listings_count[is.na(Listing_Oct$host_response_rate)]=listing_count.mean

accommodates.mean <- mean(Listing_Oct$accommodates, na.rm=TRUE)
Listing_Oct$accommodates[is.na(Listing_Oct$accommodates)]=accommodates.mean

bathrooms.mean <- mean(Listing_Oct$bathrooms, na.rm=TRUE)
Listing_Oct$bathrooms[is.na(Listing_Oct$bathrooms)]=bathrooms.mean

bedrooms.mean <- mean(Listing_Oct$bedrooms, na.rm=TRUE)
Listing_Oct$bedrooms[is.na(Listing_Oct$bedrooms)]=bedrooms.mean

beds.mean <- mean(Listing_Oct$beds, na.rm=TRUE)
Listing_Oct$beds[is.na(Listing_Oct$beds)]=beds.mean

price.mean <- mean(Listing_Oct$price, na.rm=TRUE)
Listing_Oct$price[is.na(Listing_Oct$price)]=price.mean

cleaning_fee.mean<-mean(Listing_Oct$cleaning_fee, na.rm=TRUE)
Listing_Oct$cleaning_fee[is.na(Listing_Oct$cleaning_fee)]=cleaning_fee.mean

reviews_scores_rating.mean<-mean(Listing_Oct$review_scores_rating, na.rm=TRUE)
Listing_Oct$review_scores_rating[is.na(Listing_Oct$review_scores_rating)]=reviews_scores_rating.mean

review_scores_accuracy.mean<-mean(Listing_Oct$review_scores_accuracy, na.rm=TRUE)
Listing_Oct$review_scores_accuracy[is.na(Listing_Oct$review_scores_accuracy)]=review_scores_accuracy.mean

review_scores_cleanliness.mean<-mean(Listing_Oct$review_scores_cleanliness, na.rm=TRUE)
Listing_Oct$review_scores_cleanliness[is.na(Listing_Oct$review_scores_cleanliness)]=review_scores_cleanliness.mean

review_scores_checkin.mean<-mean(Listing_Oct$review_scores_checkin, na.rm=TRUE)
Listing_Oct$review_scores_checkin[is.na(Listing_Oct$review_scores_checkin)]=review_scores_checkin.mean

review_scores_communication.mean<-mean(Listing_Oct$review_scores_communication, na.rm=TRUE)
Listing_Oct$review_scores_communication[is.na(Listing_Oct$review_scores_communication)]=review_scores_communication.mean

review_scores_location.mean<-mean(Listing_Oct$review_scores_location, na.rm=TRUE)
Listing_Oct$review_scores_location[is.na(Listing_Oct$review_scores_location)]=review_scores_location.mean

review_scores_value.mean<-mean(Listing_Oct$review_scores_value, na.rm=TRUE)
Listing_Oct$review_scores_value[is.na(Listing_Oct$review_scores_value)]=review_scores_value.mean

reviews_per_month.mean<-mean(Listing_Oct$reviews_per_month, na.rm=TRUE)
Listing_Oct$reviews_per_month[is.na(Listing_Oct$reviews_per_month)]=reviews_per_month.mean
reviews_per_month.mean
Listing_Oct$reviews_per_month

str(Listing_Oct)


##############EDA################## 
install.packages("tidyverse")
library(tidyverse)
list_1 <- as.data.frame(Listing_Oct)
summary(list_1)

############Univariate Analysis##################
#examine price 
boxplot(list_1$price)
plot(density(list_1$price), main="density spread of price") 
####positively skewed

#examine neighbourhood 
summary(list_1$neighbourhood_group_cleansed)
?plot()
plot_nbh <- plot(list_1$neighbourhood_group_cleansed, main="number of listings in each borough",
                 ylab="number of listings") 
text(plot_nbh, list_1$neighbourhood_group_cleansed, labels=summary(list_1$neighbourhood_group_cleansed))
####Brooklyn and Manhattan have way more listings than the other 3 boroughs

#examine property type
summary(list_1$property_type)
plot(list_1$property_type)

#examine room type
summary(list_1$room_type)
plot_rt<-plot(list_1$room_type)
plot_rt
text(plot_rt, list_1$room_type, labels=summary(list_1$room_type), locator(1))


#examine number of reviws
plot(list_1$number_of_reviews,ylab="number of reviews")
boxplot(list_1$number_of_reviews, main="number of reviews")
mean(list_1$number_of_reviews)
####number of reviews is positively skewed with a heavy tail to the right 

#examine review score rating
boxplot(list_1$review_scores_rating, main="review score ratings")
plot(density(list_1$review_scores_rating), main="density spread review score rating", xlab="review score")
####Two peaks at aurnd 95 and 99
####negatively skewed, with a heavy tail on the left

######overall review score ratings in each borough
by(list_1$review_scores_rating, list_1$neighbourhood_group_cleansed, mean)
#examine review per month
summary(list_1$reviews_per_month)
boxplot(list_1$reviews_per_month, main="number of reviews per month")
plot(density(list_1$reviews_per_month), main="density spread review per month", xlab="reviews per mmonth")
####postively skewed, with a tail on the right 
####most of the data fall between 0-5

#examine instant bookable
summary(list_1$instant_bookable)
inst_bookable <- filter(list_1, instant_bookable=="t")
not_inst_bookable <- filter(list_1, instant_bookable=="f")
####compare price for instant bookable and not instant bookable 
boxplot(inst_bookable$price,not_inst_bookable$price, xlab="instant bookable     not instant bookable", ylab="listng price", 
        main="Price comparison ~ instant bookable")
####not instant bookable listings have a slightly higher price than instant bookable ones

#############       Multivariate Analysis        ##############

#########price~neighborhood########
by(list_1$price, list_1$neighbourhood_group_cleansed, summary)
plot(by(list_1$price, list_1$neighbourhood_group_cleansed, mean),xlab="Bronx  Brooklyn  Manhattan  Queens  Staten Island", ylab="average listing price", 
     col=c("red", "black","green","black","black"), main="Average listing price by borough")
#### Manhattan has the highest average listing price and Bronx has the lowest average listing price
boxplot(list_1$price~list_1$neighbourhood_group_cleansed, notch=TRUE, col=c("red","grey","green","grey","grey"),
        main="listing price by borough")

#compare the average listing price in each borough
agg_neigh<-aggregate(list_1$price, list(list_1$neighbourhood_group_cleansed),mean)
agg_neigh

#ANOVA test to compare average lisitng price by neighborhood
library(car)
leveneTest(price ~ neighbourhood_group_cleansed, data = list_1)
#### p<0.0001, data variance of each borough is statistically significant

## Welch-one way ANOVA test
oneway.test(price ~ neighbourhood_group_cleansed , data=list_1, var.equal=FALSE)
####p<.05, shows differences between groups, but we dont know where the differences are

#when comparing means between each two boroughs
TukeyHSD(mod.aov)
# we cannot calim any difference in means between Queens~Bronx, Staten Island~Bronx, STaten Island~Brooklyn, Staten Island~Queens 

#model the variances before using ANOVA test
install.packages("dplyr")
library(nlme)
mod.gls <- gls(price~neighbourhood_group_cleansed, data=list_1,
               weights=varIdent(form= ~ 1 | neighbourhood_group_cleansed))
anova(mod.gls)

###########price~room type########
install.packages("sm")
library(sm)
sm.density.compare(list_1$price, list_1$room_type,
                   xlab="listing price")
price_legend <- factor(list_1$room_type, 
                       labels=c("Entire Home/Apt", "Private Room", "Shared Room"))
colfill <- c(2:(2+length(levels(price_legend))))
legend(locator(1), levels(price_legend), fill=colfill)
####as we can see from the graph, Entire Home/Apt has an obvious higher price then Private Room and Shared Room
####we want to use t-test to see if the average price of Priate Room and Shared Room is statistically different
library(dplyr)
Private_rooms <- filter(list_1, room_type=="Private room")
Shared_rooms <- filter(list_1, room_type=="Shared room")
Entire_rooms <- filter(list_1, room_type=="Entire home/apt")
library(car)
leveneTest(price~room_type, data=list_1) #variances of each room type are not equal 
t.test(Private_rooms$price, Shared_rooms$price, var.equal = FALSE)
#### p<.05, we are able to claim difference in the two means

##########price~accommodates#########
summary(list_1$accommodates)
str(list_1$accommodates)
library(car)
scatterplot(price~accommodates, data=list_1)
scatter.smooth(list_1$price~list_1$accommodates, ylab="listing price", xlab="accommodates")
####as seen from the line, as accomodates increase, listing price tends to increase 


###########correlation matrix for review score ratings#############
par(mfrow=c(1,1))
scores.matrix<- as.matrix(list_1[,24:30])
cov(scores.matrix)
install.packages("Hmisc")
library(Hmisc)
res_1 <- rcorr(scores.matrix)
res_1
res_1$r
res_1$P
###########structure correlation matrix###########
flattenCorrMatrix <- function(cormat, pmat) {
     ut <- upper.tri(cormat)
     data.frame(
          row = rownames(cormat)[row(cormat)[ut]],
          column = rownames(cormat)[col(cormat)[ut]],
          cor  =(cormat)[ut],
          p = pmat[ut]
     )
}
library(Hmisc)
res_2<-rcorr(scores.matrix)
flattenCorrMatrix(res_2$r, res_2$P)

#########Visulization######
install.packages("corrplot")
library(corrplot)
corrplot(cor(scores.matrix),
         method="ellipse", title="corrplot of review ratings")


#######################PCA for review score ratings####################
scores.matrix <- as.matrix(list_1[,24:30])
head(scores.matrix)
?prcomp()
pca <- prcomp(scores.matrix, scale=TRUE)
head(pca$x)
plot(pca$x[,1], pca$x[,2], main="PCA plot for review score ratings")

##plot the variances
pca$sdev
pca.var <- pca$sdev^2
pca.var.perc <- round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.perc,mian="Scree Plot", xlab="PCs", ylab="Percentage Variance")
dev.off()

##
library(ggplot2)
pca.data <- data.frame(X=pca$x[,1], Y=pca$x[,2])
pca.data.sample <- sample_n(pca.data,10)
pca.data.sample


############         Multiple Regression Model for review score ratings         ##############
m_1 <- lm(review_scores_rating~review_scores_accuracy+review_scores_cleanliness+review_scores_accuracy+review_scores_checkin
          +review_scores_communication+review_scores_location+review_scores_value
          +as.factor(listing_id)+as.factor(host_id)+as.factor(host_name)+as.factor(host_response_time)
          +as.factor(host_response_rate)+as.factor(host_is_superhost)+as.factor(host_listings_count)
          + as.factor(neighbourhood_group_cleansed) +as.factor(zipcode) +as.factor(latitude)+as.factor(longitude)
          +as.factor(property_type)+as.factor(room_type)+as.factor(accommodates)+as.factor(bathrooms)+as.factor(bedrooms)
          +as.factor(beds)+as.factor(bed_type)+as.factor(price)+as.factor(cleaning_fee)+as.factor(minimum_nights)+as.factor(availability_30)
          +as.factor(number_of_reviews)+as.factor(instant_bookable)
          + as.factor(cancellation_policy)+as.factor(instant_bookable)+as.factor(reviews_per_month), data=list_1)
summary(m_1)
Sys.setenv('R_MAX_VSIZE'=32000000000)
Sys.getenv('R_MAX_VSIZE')
####transform dependent variable to log()
m_3 <- lm(log(review_scores_rating)~review_scores_accuracy+review_scores_cleanliness+review_scores_accuracy+review_scores_checkin
          +review_scores_communication+review_scores_location+review_scores_value, data=list_1)
summary(m_3)

####all sex  review score are positively related to over all review score rating with statistical significance
####R^2 = 0.7355
####accuracy, cleanliness and value has the most positive impact on overall review scor ratings

#############         Check collinearity           ###########
library(car)
vif(m_1)
####no two variables show collinearity

############            HLM Model           ###########
library(lme4)
str(list_1)
summary(list_1)
ratings.hlm1 <- lmer(review_scores_rating~review_scores_accuracy+review_scores_cleanliness
                     +review_scores_accuracy+review_scores_checkin
                     +review_scores_communication+review_scores_location
                     +review_scores_value+
                          (1| host_id), 
                     data=list_1)
summary(ratings.hlm1)

#################Superhost##############
###Superhost crosstab########
tab_1 <-table(list_1$host_is_superhost, list_1$neighbourhood_group_cleansed)
tab_1 ##superhost numbers: BK>MHA>Q>Bronx>SI
round(prop.table(tab_1),2)
tab_2 <- table(list_1$host_is_superhost,list_1$room_type)
tab_2##superhost numbers: Entire apt/home>PR
round(prop.table(tab_2),2)
tab_3 <- table(list_1$host_is_superhost,list_1$instant_bookable)
tab_3##instant_bookable=T has more superhosts
tab_4 <- table(list_1$host_is_superhost,list_1$cancellation_policy)
tab_4
round(prop.table(tab_4),2)##strict with grace period has the most superhosts
xtabs(~host_is_superhost+neighbourhood_group_cleansed+room_type+instant_bookable+cancellation_policy,
      data=list_1)

############## logistic regression ##############
logistic_reg_2 <-glm(host_is_superhost~
                          host_listings_count+price+minimum_nights+availability_30
                     +number_of_reviews+review_scores_rating+reviews_per_month,
                     data=list_1, family="binomial")
summary(logistic_reg_2)
####availability has the strongest positive influence on superhost t/f
####price, number of reviews, review score ratings and reviews per month also show statistically significant positive influence


################      map listings in neigborhoods       ##############
install.packages("ggmap")
install.packages("ggplot2")
install.packages("RgoogleMaps")
install.packages("DT")
install.packages("tigris")
library(ggmap)
library(ggplot2)
library(RgoogleMaps)
library(DT)
library(tigris)
options(tigris_class="sf")
library(sf)
ny<- county_subdivisions("NY",county = c(005,047,061,081,085), cb=T)

ggplot(ny)+
     geom_sf()+
     theme_linedraw()+
     geom_point(data=Private_rooms, aes(x=longitude, y=latitude, fill="Private Room", colour="red"), col="red") +
     geom_point(data=Entire_rooms, aes(x=longitude, y=latitude, fill="Entire Home", colour="blue"), col="blue")+
     geom_point(data=Shared_rooms, aes(x=longitude, y=latitude, fill="Shared Room", colour= ("green")), col="green")+
     theme(panel.grid.major = element_line(colour="transparent"))+
     labs(title="NYC listing density ~ boroughs")

?geom_point()

list_1$neighbourhood_group_cleansed
