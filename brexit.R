
library(tidyverse)
options(digits = 3)
# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N<-1500

#Monte carlo simulation for verifying avg, se of sum
sum_x<-replicate(10000, {
X<-sample(c(1,0),N,replace=TRUE,prob=c(p,(1-p)))
sum(X)
})
mean(sum_x)
sd(sum_x)


#Monte carlo simulation for verifying avg, se of mean
mean_x<-replicate(10000, {
  X<-sample(c(1,0),N,replace=TRUE,prob=c(p,(1-p)))
  mean(X)
})
mean(mean_x)
sd(mean_x)

X_bar<-N*p
se_bar<-sqrt(N*p*(1-p))
X_hat<-(N*p)/N
se_hat<-sqrt((p*(1-p))/N)
exp_d<-2*p-1
se_d<-2*(sqrt((p*(1-p))/N))

head(brexit_polls)

brexit_polls<-brexit_polls %>% mutate(x_hat=(spread+1)/2)

mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

xhat1<-brexit_polls[1,'x_hat']
n1<-brexit_polls[1,'samplesize']
sehat1<-sqrt((xhat1*(1-xhat1))/n1)
conf1<-qnorm(0.975)*sehat1
lower1<-xhat1-conf1
upper1<-xhat1+conf1

between(0.5,lower1,upper1)
between(.52,lower1,upper1)
brexit_polls[1,]


june_polls<-brexit_polls %>% filter(enddate >= "2016-06-01")
june_polls<- june_polls %>% mutate(se_x_hat=sqrt((xhat1*(1-xhat1))/samplesize),se_hat=2*se_x_hat,
lower=spread-(qnorm(0.975)*se_hat),upper=spread+(qnorm(0.975)*se_hat),hit=(lower<=-0 & 0<=upper))
mean(june_polls$hit)


june_polls<-brexit_polls %>% filter(enddate >= "2016-06-01")
june_polls<- june_polls %>% mutate(se_x_hat=sqrt((xhat1*(1-xhat1))/samplesize),se_hat=2*se_x_hat,
lower=spread-(qnorm(0.975)*se_hat),upper=spread+(qnorm(0.975)*se_hat),hit=(lower>0 & 0<=upper))
mean(june_polls$hit)

june_polls<-brexit_polls %>% filter(enddate >= "2016-06-01")
june_polls<- june_polls %>% mutate(se_x_hat=sqrt((xhat1*(1-xhat1))/samplesize),se_hat=2*se_x_hat,
lower=spread-(qnorm(0.975)*se_hat),upper=spread+(qnorm(0.975)*se_hat),hit=(lower<=-0.038 & -0.038<=upper))
mean(june_polls$hit)

june_polls%>%group_by(pollster)%>%summarise(n=n(),hits=mean(hit))%>%arrange(desc(hits))

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
se_hat=2*sqrt((p_hat*(1-p_hat))/N),
lowerbound=spread-(qnorm(0.975)*se_hat),
upperbound=spread+(qnorm(0.975)*se_hat))

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

chi<-brexit_hit %>% group_by(poll_type,hit)%>%summarise(num=n())%>%spread(poll_type,num)
chi%>%select(-hit)%>% chisq.test()%>%.$p.value

odds_online<-(chi$Online[2]/sum(chi$Online))/(chi$Online[1]/sum(chi$Online))
odds_online
odds_tel<-(chi$Telephone[2]/sum(chi$Telephone))/(chi$Telephone[1]/sum(chi$Telephone))
odds_tel
odds_online/odds_tel

brexitplot<-brexit_polls %>% ggplot(aes(enddate,spread,col=poll_type))
brexitplot+geom_smooth(method = "loess",span=0.4)+geom_point(aes(col=poll_type))+geom_hline(yintercept = -.038)

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(enddate,proportion,col=vote))+geom_smooth(method="loess",span=0.3)

# Graph mode