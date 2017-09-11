# Analysis on Smart cities in India and finding the co-relation b/w sex ratio and LiteracyRate,
#and which city is good for opening any offices (For managerial purpose).
# NAME: Rajat Tiwari
# EMAIL: rajattiwari3101@gmail.com
# COLLEGE / COMPANY: PES University (PESIT)

#1) Setting working directory
setwd("C:/Users/Rajat/Desktop")

#2) Read the data using read.csv
cities.df <- read.csv(paste("smartcities.csv", sep=""))
data <- read.csv(paste("smartcities.csv", sep=""))

#3) View the data frame in R
View(cities.df)

#4) Summary of the data
summary(cities.df) 

#5)mean,median and standard devition
library(psych)
describe

#6)Histogram for showing Total Population By state
library(ggplot2) # Data visualization
library(reshape2)
#total population

ggplot(data, aes(x=data$state_name, y=data$population_total, fill=data$state_name)) + ggtitle("Total population by States") + labs(x="States", y="Population") + 
  geom_bar(stat="identity") +
  guides(fill=FALSE) + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))

#7)Histogram showing total population of male and female
#male & female population by states
statemf <- aggregate( cbind(population_total, population_male, population_female) ~ state_name, data = data , FUN = sum )
statemf1 <- melt(statemf, id.vars = "state_name", measure.vars = c("population_male", "population_female"))
ggplot(statemf1, aes(x=statemf1$state_name, y=statemf1$value, fill=statemf1$variable)) + labs(x="States", y="Population") + 
  ggtitle("Male & Female population by states") +
  geom_bar(stat="identity", position=position_dodge())  + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) +
  scale_fill_discrete(name ="Male & Female", labels=c("Male Population", "Female Population"))

#8)Hisogram showing Differance in Male & Female Effective Literacy rate by states
data1 <- data
statemefc2t <- aggregate( cbind(data1$effective_literacy_rate_male, data1$effective_literacy_rate_female, data1$effective_literacy_rate_total) ~ state_name, data = data1 , FUN = sum )
statemefc2t$rt <- ((statemefc2t$V1 - statemefc2t$V2)/statemefc2t$V3)*100
ggplot(statemefc2t, aes(x=statemefc2t$state_name, y=statemefc2t$rt, fill=statemefc2t$state_name)) + labs(x="States", y="Literacy Rate") + 
  ggtitle("Differance in Male & Female Effective Literacy rate by states") +
  geom_bar(stat="identity", position=position_dodge())  + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) +
  guides(fill=FALSE)

#9)#How many male and female literates are in each state?
library(dplyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
df_literate<-df %>% 
  group_by(state_name) %>% 
  summarise(female = sum(literates_female),
            male = sum(literates_male))

mdata<-melt(df_literate, id = "state_name")

ggplot(aes(reorder(x = state_name, -value), y = value, group = variable), data = mdata)+
  geom_line(aes(color = variable))+
  labs(list(title = "Total Number of literates(Male Vs Female)", x = "State", y = "Total Number of Literates"))+
  scale_colour_solarized("blue")+
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))

#10)#Literacy Rate Comparison Male Vs Female
ggplot(aes(x = name_of_city, group = 1 ), data = df)+
  geom_line(aes(y = effective_literacy_rate_female, colour = "Female")) +
  geom_line(aes(y = effective_literacy_rate_male, colour = "Male")) +
  labs(list(title = "Literacy Rate Comparison(Male Vs Female)", x = "Cities", y = "Literacy Rate"))+
  scale_colour_solarized("blue")+
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))

#11)# Sex Ratio and Child Sex Ratio
ggplot(aes(x = name_of_city, group = 1 ), data = df)+
  geom_line(aes(y = sex_ratio, colour = "sex_ratio")) +
  geom_line(aes(y = child_sex_ratio, colour = "child_sex_ratio")) +
  labs(list(title = "Sex Ratio Vs Child Sex Ratio", x = "Cities", y = "Ratio"))+
  scale_colour_solarized("blue")+
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))

#12)#What are the top 10 most populated cities in India?
df %>% 
  group_by(name_of_city) %>% 
  summarise(total = sum(population_total)) %>% 
  arrange(desc(total)) %>% 
  top_n(n= 10, wt = total) %>% 
  ggplot(aes(reorder(x = name_of_city, -total), y = total))+
  geom_bar(aes(fill = name_of_city), stat = "identity")+
  labs(list(title = "Total Population Per City", x = "City", y = "Total Population"))+
  scale_colour_solarized("blue")+
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))

#13)bwplot for sex ratio for cities 
library(lattice)
bwplot(name_of_city ~ sex_ratio, data=cities.df ,horizontal=TRUE,xlab="SEX_ratio" , col=c("red","blue","Green","yellow"))

#14)bwplot for  sex_ratio for states
library(lattice)
bwplot(state_name~sex_ratio, data=cities.df ,horizontal=TRUE,xlab="SEX_ratio" , col=c("red","blue","Green","yellow"))

#15)bwplot for effective_literacy rate female
library(lattice)
bwplot(state_name~effective_literacy_rate_female, data=cities.df ,horizontal=TRUE,xlab="SEX_ratio" , col=c("red","blue","Green","yellow"))

#16)bwplot for child sex ratio
bwplot(state_name~child_sex_ratio, data=cities.df ,horizontal=TRUE,xlab="SEX_ratio" , col=c("red","blue","Green","yellow"))

#17)bwplot for effective literatcy rate total
bwplot(state_name~effective_literacy_rate_total, data=cities.df ,horizontal=TRUE,xlab="SEX_ratio" , col=c("red","blue","Green","yellow"))

#18)scatter plot for sex_ratio and child sex ratio
library(car)
scatterplotMatrix(formula = ~ sex_ratio + child_sex_ratio, cex=0.6,
                  data=cities.df,diagonal="histogram")
#19)scatter plot for sex_ratio and effective literatcy rate total
scatterplotMatrix(formula = ~ sex_ratio +effective_literacy_rate_total , cex=0.6,
                  data=cities.df,diagonal="histogram")
#20)scatter plot for sex_ratio and effective_literacy rate female
scatterplotMatrix(formula = ~ sex_ratio +effective_literacy_rate_female , cex=0.6,
                  data=cities.df,diagonal="histogram")

#21)corrgram
library(corrgram)
corrgram(cities.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Sex_ratio and effective literacy rate")

#22)covvariance matrix
cov(cities.df$sex_ratio,cities.df$effective_literacy_rate_female)

#23)regression model
fit <- lm(sex_ratio ~ effective_literacy_rate_female +effective_literacy_rate_total+child_sex_ratio, data=cities.df)
summary(fit)
fitted(fit)
