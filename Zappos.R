```{r}
zappos<-read.csv("zappos.csv",stringsAsFactors = FALSE)
zappos$platform<-as.factor(zappos$platform)
zappos$site<-as.factor(zappos$site)
zappos$day<- as.Date(zappos$day, "%m/%d/%Y")
str(zappos)

summary(zappos)
levels(zappos$platform)
library(sjPlot)

numeric<-subset(zappos,select = c("visits","distinct_sessions","gross_sales","bounces","add_to_cart","product_page_views","search_page_views"))
sjt.corr(numeric)

zappos$platform[zappos$platform=='']<-'Unknown'
zappos$platform[zappos$platform=='SymbianOS']<-'Other'
zappos$month<-months(zappos$day)

table(zappos$new_customer)

tapply(zappos$orders,zappos$new_customer,sum)

tapply(zappos$add_to_cart,zappos$new_customer,sum)


library(ggplot2)

library(reshape)

zappos$diff<-zappos$add_to_cart-zappos$orders



cartdiff<-aggregate(zappos$diff ~ zappos$month,FUN=sum)

names(cartdiff)<-c("month","diff")

cartdiff

cartdiff$month<-as.factor(cartdiff$month)

cartdiff$month<-factor(cartdiff$month, levels=c("January","June","July","August","September","October","November","December"))

options(scipen=10000)
a<-ggplot(cartdiff, aes(x=cartdiff$month,y=cartdiff$diff),xlab("Di"))+ geom_bar(stat = "identity")
a

plot(cartdiff$diff, type = 'l', xlab = "Difference")
x<-subset(zappos,is.na(zappos$gross_sales)==F)

salesbyday<-aggregate( x$gross_sales ~ x$day, FUN = sum)


names(salesbyday)<-c("day","sales")


forecastdata<-subset(salesbyday,salesbyday$day>"2013-05-01")

write.csv(forecastdata,"forecastdata.csv",row.names = FALSE)

str(forecastdata)



p <- ggplot(forecastdata, aes(x=day, y=sales))
p + geom_line()



#Model to predict no of orders orders

library(caTools)





split<-sample.split(zappos$orders,SplitRatio = 0.6)


zaptrain<-subset(zappos,split=TRUE)
zaptest<-subset(zappos, split=FALSE)


regvisits<-lm(orders~ visits,data = zappos)
regproduct<-lm(orders ~ product_page_views, data = zappos)
regsearches<-lm(orders ~ search_page_views, data = zappos)

summary(regvisits)
summary(regproduct)
summary(regsearches)
str(zappos)

pred1<-predict(regvisits,newdata = zaptest)
sse1<-sum(regvisits$residuals^2)
sse1
pred2<-predict(regproduct,newdata = zaptest)
sse2<-sum(regproduct$residuals^2)
sse2
pred3<-predict(regsearches,newdata = zaptest)
sse3<-sum(regsearches$residuals^2)
sse3
pred1
```