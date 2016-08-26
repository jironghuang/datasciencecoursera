library("ggplot2")
library("dplyr")
library("reshape")
library("reshape2")
library("grid")

# library("nnet")
# library("boot")
# library("scatterplot3d")
# library(Rcmdr)

#load in data on index. Index ticker symbol can be assigned to index variable
#SnP 500 GSPC
#STI STI
#Hang Seng Index HSI
#FTSE FTSE
#Shanghai Composite SSEC
#Korean Index KS11
#Swiss Index SSMI
#Athen Index GD.AT
#Nikkei N225 N225

index = "GSPC"
stock = paste("http://real-chart.finance.yahoo.com/table.csv?s=%5E",index,"&a=00&b=1&c=1900&d=01&e=26&f=2017&g=d&ignore=.csv", sep = "")
# stock = paste("http://real-chart.finance.yahoo.com/table.csv?s=GD.AT","&a=00&b=1&c=1900&d=00&e=26&f=2016&g=d&ignore=.csv", sep = "")


yahoo.read <- function(url){
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,7)]    #7 is adj close
  df$Date <- as.Date(as.character(df$Date))
  return(df)}

stock_price  <- yahoo.read(stock)


#create a loop and lag x times
lag_days = 260

# stk_price = function(lag_days){
  for(i in 1:lag_days) { #lag_days = 260
    
    # as.data.frame(lag(stock_price,i))
    a = assign(paste("lag",i,sep=""),as.data.frame(lead(stock_price$Adj.Close,i)))
    names(a) = paste("lag",i)
    
    stock_price = cbind(stock_price,a)
    rm(list = ls()[grepl("lag", ls())]) #remove lags from each iteration, ls refers to list of dataframes
  }
  # return (stock_price)}
# stk_price(260)        #something wrong from creating a function

nrows = dim(a)[1]


#Create a % of 52 week high, 52 week low
#% 52 week high
max_52weeks = as.data.frame(apply(stock_price[,-1],1,max))

lower_52weekhigh = (stock_price$Adj.Close -  max_52weeks)/max_52weeks
names(lower_52weekhigh)[1] = "col1"

hist(lower_52weekhigh[,1])
ggplot(lower_52weekhigh,aes(col1)) + geom_histogram() + ggtitle("Frequency of Change from 52 Week High, STI")+
  xlab("Change from 52 Week High")+ylab("Counts")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title=element_text(face="bold", size=20))+ xlim(-0.6, 0)
    ggsave("C:/Users/Huang Jirong/Desktop/Index Analysis/lower_52weekhigh_hist.jpeg")

ggplot(lower_52weekhigh,aes(col1)) + geom_freqpoly() + ggtitle("Frequency of Change from 52 Week High, STI")+
  xlab("Change from 52 Week High")+ylab("Counts")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title=element_text(face="bold", size=20))+ xlim(-0.6, 0)
    ggsave("C:/Users/Huang Jirong/Desktop/Index Analysis/lower_52weekhigh_kernel density.jpeg")


#% off 52 week low
min_52weeks = as.data.frame(apply(stock_price[,-1],1,min))

higher_52weeklow = (stock_price$Adj.Close -  min_52weeks)/min_52weeks
names(higher_52weeklow )[1] = "col1"

hist(higher_52weeklow )

d= density(higher_52weeklow [-(nrows-260):-nrows,1], na.rm = FALSE)
plot(d)



#plot kernel density and histogram to see frequency

quantile(lower_52weekhigh[-(nrows-260):-nrows,1], c(0.025,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))


#Create a lag of 1,3,5 years ago
leadprice = as.data.frame(lag(stock_price$Adj.Close,260))
names(leadprice)[1] = "lead1yr"
leadprice$lead2yr = lag(stock_price$Adj.Close,520)
leadprice$lead3yr = lag(stock_price$Adj.Close,780)

#Compute returns
returns= as.data.frame((leadprice$lead1yr -  stock_price$Adj.Close)/stock_price$Adj.Close)
names(returns)[1] = "returns_1yr"
returns$returns_2yr = (leadprice$lead2yr -  stock_price$Adj.Close)/stock_price$Adj.Close
returns$returns_3yr = (leadprice$lead3yr -  stock_price$Adj.Close)/stock_price$Adj.Close


#Histogram on 1 year returns (creat a dummy for each 10 year period and use the stacked histogram code in http://docs.ggplot2.org/current/geom_histogram.html)
ggplot(returns,aes(returns_1yr)) + geom_histogram() + ggtitle("No. of 1 Year Returns")+xlab("No. of 1 Year Returns")+ylab("Counts")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+theme(plot.title=element_text(face="bold", size=20))
ggplot(returns,aes(returns_1yr)) + geom_freqpoly() + ggtitle("Frequency of 1 Year Returns")+xlab("Frequency of 1 Year Returns")+ylab("Frequency")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+theme(plot.title=element_text(face="bold", size=20))




#Look at returns-%52 week high scatterplot
dat = cbind(stock_price$Date,lower_52weekhigh,higher_52weeklow,returns)
names(dat) = c("Date","lower_52weekhigh","higher_52weeklow", "returns_1yr","returns_2yr","returns_3yr")

#Example using cars
# myPlot <- ggplot(cars, aes(speed, dist, color=as.integer(dt) ) ) +
#   geom_point() +scale_colour_gradient(low="blue", high="red",breaks=myBreaks) +
#   labs(color="Date")

myBreaks <- function(x){
  breaks <- c(min(x),median(x),max(x))
  breaks = quantile(x, c(0, 0.2,0.4,0.6,0.8,1))
  attr(breaks,"labels") <- as.Date(breaks, origin="1970-01-01")
  names(breaks) <- attr(breaks,"labels")
  return(breaks)
}


yr1_date = ggplot(dat,aes(Date,returns_1yr,colour=as.integer(Date))) + geom_point(alpha = 0.6) +
  scale_colour_gradientn(colours = rainbow(5),breaks=myBreaks) + ggtitle("1yr returns vs. Date")+xlab("Date")+ylab("Returns 1 year later")
yr1_date = yr1_date+geom_smooth(method = "lm", se = TRUE) + stat_smooth()
yr1_date+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title=element_text(face="bold", size=20)) + labs(color="date")




yr1_high = ggplot(dat,aes(lower_52weekhigh,returns_1yr,colour=as.integer(Date))) + geom_point(alpha = 0.6) +
  scale_colour_gradientn(colours = rainbow(5), breaks=myBreaks) + ggtitle("1yr returns vs. Change from 52 week high, STI")+xlab("Change from 52 week high")+ylab("Returns 1 year later")
yr1_high = yr1_high+geom_smooth(method = "lm", se = TRUE) + stat_smooth()
yr1_high = yr1_high +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title=element_text(face="bold", size=20)) + labs(color="date")
yr1_high
    ggsave("C:/Users/Huang Jirong/Desktop/Index Analysis/1yr returns vs. Change from 52 week high_STI.jpeg")
      yr1 = lm(dat$returns_1yr~ dat$lower_52weekhigh)
      yr1$coefficients[2]*(-0.03) + yr1$coefficients[1]
    

yr2_high = ggplot(dat,aes(lower_52weekhigh,returns_2yr,colour=as.integer(Date))) + geom_point(alpha = 0.6) +
  scale_colour_gradientn(colours = rainbow(7), breaks=myBreaks) + ggtitle("2yr returns vs. Change from 52 week high, STI")+xlab("Change from 52 week high")+ylab("Returns 2 year later")
yr2_high = yr2_high+geom_smooth(method = "lm", se = TRUE) + stat_smooth()
yr2_high = yr2_high +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title=element_text(face="bold", size=20)) + labs(color="date")
yr2_high
    ggsave("C:/Users/Huang Jirong/Desktop/Index Analysis/2yr returns vs. Change from 52 week high_STI.jpeg")
      yr2 = lm(dat$returns_2yr~ dat$lower_52weekhigh)
      yr2$coefficients[2]*(-0.03) + yr2$coefficients[1]

yr3_high = ggplot(dat,aes(lower_52weekhigh,returns_3yr,colour=as.integer(Date))) + geom_point(alpha = 0.6) +
  scale_colour_gradientn(colours = rainbow(7), breaks=myBreaks) + ggtitle("3yr returns vs. Change from 52 week high, STI")+xlab("Change from 52 week high")+ylab("Returns 3 year later")
yr3_high = yr3_high+geom_smooth(method = "lm", se = TRUE) + stat_smooth()
yr3_high = yr3_high +theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title=element_text(face="bold", size=20)) + labs(color="date")
yr3_high
    ggsave("C:/Users/Huang Jirong/Desktop/Index Analysis/3yr returns vs. Change from 52 week high_STI.jpeg")
      yr3 = lm(dat$returns_3yr~ dat$lower_52weekhigh)
      yr3$coefficients[2]*(-0.03) + yr3$coefficients[1]
      
#function to part the plots above
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)    
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(yr1_high, vp = vplayout(1, 1))
print(yr2_high, vp = vplayout(1, 2))
print(yr3_high, vp = vplayout(2, 1))

    

#Look at returns-%52 week low scatterplot
yr1_low = ggplot(dat,aes(higher_52weeklow,returns_1yr,colour=as.integer(Date))) + geom_point(alpha = 0.6) +
  scale_colour_gradientn(colours=c('red','green','blue'), breaks=myBreaks) + ggtitle("1yr returns vs. % off 52 week low")
yr1_low+geom_smooth(method = "lm", se = TRUE) + stat_smooth()

yr2_low = ggplot(dat,aes(higher_52weeklow,returns_2yr,colour=as.integer(Date))) + geom_point(alpha = 0.6) +
  scale_colour_gradientn(colours=c('red','green','blue'), breaks=myBreaks) + ggtitle("2yr returns vs. % off 52 week low")
yr2_low+geom_smooth(method = "lm", se = TRUE) + stat_smooth()

yr3_low = ggplot(dat,aes(higher_52weeklow,returns_3yr,colour=as.integer(Date))) + geom_point(alpha = 0.6) +
  scale_colour_gradientn(colours=c('red','green','blue'), breaks=myBreaks) + ggtitle("3yr returns vs. % off 52 week low")
yr3_low+geom_smooth(method = "lm", se = TRUE) + stat_smooth()


#Compute returns wiht a 52 week low vs high matrix

#Scatter3d
#Use ggplot2 version    


# s3d_1yr = scatterplot3d(lower_52weekhigh[-(nrows-260):-nrows,1],higher_52weeklow[-(nrows-260):-nrows,1],returns[-(nrows-260):-nrows,1])
# fit <- lm(returns[-(nrows-260):-nrows,1] ~ lower_52weekhigh[-(nrows-260):-nrows,1]+higher_52weeklow[-(nrows-260):-nrows,1])
# s3d_1yr$plane3d(fit)
#
# s3d_2yr = scatterplot3d(lower_52weekhigh[-(nrows-520):-nrows,1],higher_52weeklow[-(nrows-520):-nrows,1],returns[-(nrows-520):-nrows,2])
# fit <- lm(returns[-(nrows-520):-nrows,2] ~ lower_52weekhigh[-(nrows-520):-nrows,1]+higher_52weeklow[-(nrows-520):-nrows,1])
# s3d_2yr$plane3d(fit)