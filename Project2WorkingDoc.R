library(ggplot2)
library(tidyverse)

diamonds<-read_csv("diamonds4.csv")

diamonds

head(diamonds)
(diamonds[!complete.cases(diamonds),])
(diamonds[complete.cases(diamonds),])

#No na's - nice

#To visual counts of different parameters

ggplot(diamonds, aes(x=cut))+
  geom_bar(fill="blue")

ggplot(diamonds, aes(x=clarity))+
  geom_bar(fill="red")

ggplot(diamonds, aes(x=color))+
  geom_bar(fill="green")

#Scatter plot of Price vs Carat with all parameters

ggplot(data = diamonds, aes(x = carat, y = price, color = color, size = clarity, shape = cut)) +
  geom_point()

#Broken down by indvudual parameters

ggplot(data = diamonds, aes(x = carat, y = price, color = cut) +
  geom_point()
  
ggplot(data = diamonds, aes(x = carat, y = price, color = cut) +
  geom_point()
  
ggplot(data = diamonds, aes(x = carat, y = price, color = clarity) +
  geom_point()
  

  
#Website Claims order of importance is: Cut, color, carat, clarity

#First: create new column "price/carat" to control for carat size as we explore how other attributes affect price
diamonds<-diamonds%>%
  mutate(PricePerCarat = price/carat)

#Need to reorder some factors 

diamonds<- diamonds%>%
  mutate(cut = cut%>%
           fct_relevel(c("Good","Very Good","Ideal","Astor Ideal")))

diamonds<- diamonds%>%
  mutate(clarity = clarity%>%
           fct_relevel(c("SI2","SI1","VS2","VS1", "VVS2", "VVS1", "IF", "FL")))

#Getgrouped means of proce per carat for each attribute

ClarityVSPricePerCarat<-diamonds%>%
  group_by(clarity)%>%
  summarize(meanPrice=mean(PricePerCarat))

ColorVSPricePerCarat<-diamonds%>%
  group_by(color)%>%
  summarize(meanPrice=mean(PricePerCarat))

CutVSPricePerCarat<-diamonds%>%
  group_by(cut)%>%
  summarize(meanPrice=mean(PricePerCarat))


#Plots of grouped means 

ggplot(ClarityVSPricePerCarat, aes(x=clarity, y=meanPrice))+
  geom_bar(stat="identity")

ggplot(ColorVSPricePerCarat, aes(x=color, y=meanPrice))+
  geom_bar(stat="identity")

ggplot(CutVSPricePerCarat, aes(x=cut, y=meanPrice))+
  geom_bar(stat="identity")


#Grouping all

EverythingVSPricePerCarat<-diamonds%>%
  group_by(clarity, cut, color)%>%
  summarize(meanPrice=mean(PricePerCarat))


#Still working on this 
  
ggplot(EverythingVSPricePerCarat,aes(x=cut, fill=meanPrice))+
  geom_bar(position = "fill")+
  facet_wrap(~clarity)

#looking at some outliers

diamonds%>%
  filter(cut == "Astor Ideal")

