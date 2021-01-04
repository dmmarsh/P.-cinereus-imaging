dat<-fullnessdataV2
#convert from character/number to factors
dat$salamander<-as.factor(dat$salamander)
dat$observer<-as.factor(dat$observer)
dat$date<-as.factor(dat$date)
dat$treatment<-as.factor(dat$treatment)
summary(dat)

library(lme4)
#model for all data
lme2 <- lmer(prop_full ~ flies.eaten + (1|salamander/date) + (1|observer), data=dat)
summary(lme2)

#remove fly number 28 and re-run
dat2<-subset(dat, salamander!="28")
lme3 <- lmer(prop_full ~ flies.eaten + (1|salamander/date) + (1|observer), data=dat2)
summary(lme3)

library(insight)
get_variance(lme2)
get_variance(lme3)

library(performance)
r2_nakagawa(lme2, by_group = FALSE)
r2_nakagawa(lme3, by_group = FALSE)

#average together 6 measurements for each salamander for plotting (Fig 5)
library(dplyr)
meandata<- dat %>%
  group_by(salamander, treatment) %>%
  summarize(meanflies = mean(flies.eaten), 
            meanfull = mean(prop_full), n = n())

library(ggplot2)
ggplot(meandata, aes(meanflies, meanfull, color=treatment)) +
  geom_point(size=2)+
  xlab("Mean number of flies consumed")+
  ylab("Gut fullness (food area/body area)") +
  scale_color_manual(labels=c("High food", "Medium food", "No food"), values = c("blue", "green", "red")) +
  theme_bw() + 
  theme(axis.title=element_text(size = 10))
 

