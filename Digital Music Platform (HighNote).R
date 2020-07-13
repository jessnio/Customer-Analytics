#Midterm Cust Analytics Jesslyn Antonio
library(dplyr)
#QUESTION 1
highnote <- read.csv(file.choose())
head(highnote)
highnote$adopter<-as.numeric(highnote$adopter)
#Adopter subsamples
library(dplyr)
adopter <- filter(highnote, highnote$adopter == 1)
View(adopter)
library(pastecs)
stat.desc(adopter)
#Non-adaptor subsamples
non_adopter <- filter(highnote, highnote$adopter == 0)
View(non_adopter)
stat.desc(non_adopter)

#QUESTION 2
library(ggplot2)
highnote$adopter<-as.character(highnote$adopter)

#Box plot for demographics
ggplot(highnote, aes(x=adopter, y=age)) +geom_boxplot(col=c('blue', 'orange') )
ggplot(highnote, aes(x=adopter, y=avg_friend_age)) +geom_boxplot(col=c('blue', 'darkgreen'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")
ggplot(highnote, aes(x=adopter, y=avg_friend_male)) +geom_boxplot(col=c('blue', 'darkgreen'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")

#Box plot for peer influence
ggplot(highnote, aes(x=adopter, y=friend_cnt)) +geom_boxplot(col=c('orange', 'darkgray'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red")
ggplot(highnote, aes(x=adopter, y=friend_country_cnt)) +geom_boxplot(col=c('orange', 'darkgray'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")
#Scatter plot for peer influence
ggplot(highnote, aes(x=friend_country_cnt, y=friend_cnt)) + geom_point(color = "purple")
#Box plot for user engagement
ggplot(highnote, aes(x=adopter, y=songsListened)) +geom_boxplot(col=c('darkblue', 'lightblue'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")
ggplot(highnote, aes(x=adopter, y=lovedTracks)) +geom_boxplot(col=c('darkblue', 'lightblue'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")



#QUESTION 3
highnote$adopter<-as.numeric(highnote$adopter)
#Create a new variable and call it subscriber_friend
highnote <- mutate(highnote, subscriber_friend=ifelse(subscriber_friend_cnt==0, 0, 1))

#Statistical significance of unmatched data
t.test(highnote$subscriber_friend, highnote$adopter)

#Calculate the mean for each covariate
cov <- c('age','male', 'friend_cnt',	'avg_friend_age', 'avg_friend_male',	
         'friend_country_cnt', 'songsListened',	'lovedTracks',
         'posts',	'playlists',	'shouts',	'tenure',	'good_country')

lapply(cov, function(v) {
  t.test(highnote[, v] ~ highnote$subscriber_friend)
})
#Run a generalized linear model for all variables
linear1 <- glm(subscriber_friend~age+male+friend_cnt+avg_friend_age+avg_friend_male+
         friend_country_cnt+songsListened+lovedTracks+posts+playlists+
         shouts+tenure+good_country,
       family = binomial(), data = highnote)
summary(linear1)

#Drop insignificant variables such as 'male', 'playlists', 'shouts', 'good_country'
linear2 <- glm(subscriber_friend~age+friend_cnt+avg_friend_age+avg_friend_male+
          friend_country_cnt+songsListened+lovedTracks+tenure+posts,
        family = binomial(), data = highnote)
summary(linear2)

#Calculate the propensity score for each person
prs_df <- data.frame(pr_score = predict(linear2, type = "response"),
                     subscriber_friend=linear1$model$subscriber_friend)
head(prs_df)

#After estimating the propensity score, create histograms of the estimated propensity scores 
labs <- paste("Subscriber friends:", c("Zero", "One or More"))
prs_df %>%
  mutate(subscriber_friend = ifelse(subscriber_friend == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "blue") +
  facet_wrap(~subscriber_friend) +
  xlab("Probability of having subscriber friends") +
  theme_bw()

#Eliminate missing values
highnote_nomiss <- highnote %>%  
  select(adopter,subscriber_friend_cnt, subscriber_friend,one_of(cov)) %>%
  na.omit()


#Use MatchIt 
#This package estimates the propensity score in the background
#and matches observations based on the method of choice (“nearest”)
install.packages("MatchIt")
library(MatchIt)
mod_match <- matchit(subscriber_friend ~ age+male+friend_cnt+avg_friend_age+avg_friend_male+
                       friend_country_cnt+songsListened+lovedTracks+posts+playlists+
                       shouts+tenure+good_country, 
                     method = "nearest", data = highnote_nomiss)
summary(mod_match)
plot(mod_match)

#Create a dataframe containing only the matched observations
dta_m <- match.data(mod_match)
dim(dta_m)

#Examining covariate balance in the matched sample and having visual inspection

fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$subscriber_friend <- as.factor(dta$subscriber_friend)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = subscriber_friend)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

install.packages("gridExtra")
library(gridExtra)

grid.arrange(
  fn_bal(dta_m, "age"),
  fn_bal(dta_m, "male"),
  fn_bal(dta_m, "friend_cnt"),
  fn_bal(dta_m, "avg_friend_age"),
  fn_bal(dta_m, "avg_friend_male"),
  fn_bal(dta_m, "friend_country_cnt"),
  fn_bal(dta_m, "songsListened"),
  fn_bal(dta_m, "lovedTracks"),
  fn_bal(dta_m, "posts"),
  fn_bal(dta_m, "playlists"),
  fn_bal(dta_m, "shouts"),
  fn_bal(dta_m, "tenure"),
  fn_bal(dta_m, "good_country"),
  nrow = 7, widths = c(1, 0.8)
)


#Difference of means
library(dplyr)
dta_m %>%
  group_by(subscriber_friend) %>%
  select(one_of(cov)) %>%
  summarise_all(funs(mean))

lapply(cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$subscriber_friend)
})

# Estimating treatment effects
t.test(dta_m$subscriber_friend ~ dta_m$adopter)
#Use OLS with covariates
ols <- lm(adopter~subscriber_friend, data = dta_m)
summary(ols)

#QUESTION 4
#Create a logistic regression model
summary(logistic <- glm(adopter~age+male+friend_cnt+avg_friend_age+avg_friend_male+
                           friend_country_cnt+songsListened+lovedTracks+posts+playlists+
                           shouts+tenure+good_country+subscriber_friend_cnt+subscriber_friend,
                         family = binomial(), data = highnote))
#Drop insignificant variables such as avg_friend_male, posts, and shouts
summary(logistic <- glm(adopter~age+male+friend_cnt+avg_friend_age+friend_country_cnt+
                           songsListened+lovedTracks+playlists+
                           tenure+good_country+subscriber_friend_cnt+subscriber_friend,
                         family = binomial(), data = highnote))

#Create correlations between all variables
correl <- subset(highnote, select=c(age, male, friend_cnt, avg_friend_age,
                             friend_country_cnt, songsListened, lovedTracks, playlists,
                             tenure, good_country, subscriber_friend_cnt, subscriber_friend))
correlresult <- cor(correl)
library('corrplot') 
corrplot(correlresult , method = "square", col = terrain.colors(100)) 
corrplot(correlresult , method = "number", col = terrain.colors(100) ) 

#Create the final logistic regression model
reg <- glm(adopter~age+male+songsListened+lovedTracks+playlists+
                   tenure+good_country+subscriber_friend_cnt+subscriber_friend,
                 family = binomial(), data = highnote)
summary(reg)


#The odds ratio for key variables
odds <- data.frame(exp(coef(reg)))
odds


