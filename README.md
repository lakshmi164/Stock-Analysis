library(readr)

cipla = read_csv("C:/Users/DEVI PRADEEP/Downloads/R/Quote-Equity-CIPLA-EQ-19-01-2024-to-19-01-2025.csv")
mankind = read_csv("C:/Users/DEVI PRADEEP/Downloads/R/Quote-Equity-MANKIND-EQ-19-01-2024-to-19-01-2025.csv")
cipla
mankind

#explore the structure 
str(cipla)
str(mankind)

#check for missing values
sum(is.na(cipla))
sum(is.na(mankind))

#change date to data type
cipla$Date = as.Date(cipla$Date,format = "%d-%b-%Y")
mankind$Date = as.Date(mankind$Date,format = "%d-%b-%Y")

library(dplyr)
#select necessary columns
cipla = cipla %>%
  select( Date,close,VOLUME)
mankind = mankind%>%
  select(Date,close,VOLUME)

#descriptive statistics
summary(cipla$close)
summary(mankind$close)

#standard deviation
sd(cipla$close)
sd(mankind$close)

#the standard deviation of mankind is higher than cipla means 
#Mankind’s closing prices are more volatile than Cipla’s.

#plot the closing prices
library(ggplot2)
ggplot()+
  geom_line(data = cipla,aes(x=Date,y=close,color="cipla"))+
  geom_line(data=mankind,aes(x=Date,y=close,color="mankind"))+
  labs(title="closing price of cipla and mankind",
       x="Date",
       y="Closing Price")
#closing price of mankind is more fluctuating, compared with closing price of cipla
#so return from  mankind is more compared with cipla even if the risk is higher.

#moving avg
library(zoo)
cipla = cipla %>%
  arrange(Date)%>%
  mutate(MA_20 = rollmean(close,20,fill = NA),
         MA_50 = rollmean(close,50,fill = NA))
mankind = mankind %>%
  arrange(Date)%>%
  mutate(MA_20 = rollmean(close,20,fill = NA),
         MA_50 = rollmean(close,50,fill = NA))
cipla
mankind

#plot the moving avg
ggplot()+
  geom_line(data = cipla,aes(x=Date,y=close,color="cipla"))+
  geom_line(data=mankind,aes(x=Date,y=close,color="mankind"))+
  geom_line(data = cipla,aes(x=Date,y=MA_20,color="cipla 20 day MA"))+
  geom_line(data=mankind,aes(x=Date,y=MA_20,color="mankind 20 day MA"))+
  geom_line(data = cipla,aes(x=Date,y=MA_50,color="cipla 50 day MA"))+
  geom_line(data=mankind,aes(x=Date,y=MA_50,color="mankind 50 day MA"))+
  labs(title="closing price of cipla and mankind",
       x="Date",
       y="Closing Price")

#the moving avg of cipla is more stable compared with mankind
#the moving avg of mankind is more fluctuating compared with cipla

#calculate the returns
cipla = cipla %>%
  mutate(Return = (close -lag(close))/lag(close))
cipla
mankind = mankind %>%
  mutate(Return = (close -lag(close))/lag(close))
mankind

sd_cipla = sd(cipla$Return,na.rm = T)
sd_mankind = sd(mankind$Return,na.rm = T)
sd_cipla
sd_mankind

#the standard deviation of mankind is higher than cipla means
#Mankind’s returns are more volatile than Cipla’s.

#plot the return 
ggplot()+
  geom_line(data = cipla , aes(x=Date,y=Return,colour = "cipla"))+
  geom_line(data=mankind,aes(x=Date,y=Return,colour = "mankind"))+
  labs(title="Return of cipla and mankind",
       x="Date",
       y="Return")+
  scale_color_manual(values = c("cipla"="blue","mankind"="red"))

#cumulative return
cipla = cipla %>%
  mutate(Return = ifelse(is.na(Return),0,Return),
         Cumulative_Return = cumprod(Return+1))
         
cipla
mankind = mankind %>%
  mutate(Return = ifelse(is.na(Return),0,Return),
         Cumulative_Return = cumprod(Return+1))
mankind

#plot the cumulative return
ggplot()+
  geom_line(data = cipla , aes(x=Date,y=Cumulative_Return,colour = "cipla"))+
  geom_line(data=mankind,aes(x=Date,y=Cumulative_Return,colour = "mankind"))+
  labs(title="Cumulative Return of cipla and mankind",
       x="Date",
       y="Cumulative Return")+
  scale_color_manual(values = c("cipla"="blue","mankind"="red"))

#the cumulative return of mankind is more compared with cipla

length(cipla$Return)
length(mankind$Return)

#correlation
cor(cipla$Return,mankind$Return,use = "complete.obs")

#the correlation between cipla and mankind is -0.02480909.
#This means that the returns of Cipla and Mankind are not correlated.

#investment growth for 650000 in cipla and mankind 
initial_investment =650000
cipla = cipla %>%
  mutate(investment = initial_investment*Cumulative_Return)
cipla
mankind = mankind %>%
  mutate(investment = initial_investment*Cumulative_Return)
mankind

#plot the investment growth
ggplot()+
  geom_line(data = cipla , aes(x=Date,y=investment,colour = "cipla"))+
  geom_line(data=mankind,aes(x=Date,y=investment,colour = "mankind"))+
  labs(title="Investment Growth of cipla and mankind",
       x="Date",
       y="Investment")+
  scale_color_manual(values = c("cipla"="blue","mankind"="red"))

#the investment growth of mankind is more compared with cipla
#so the return from mankind is more compared with cipla even if the risk is higher.

#Mankind is a better investment compared with Cipla.
#but people who are risk averse can go for cipla.it is more stable compared with mankind.



