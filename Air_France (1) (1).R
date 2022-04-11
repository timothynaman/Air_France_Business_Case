#####################################################################
#MBAN 2 
#Team 11 - Diego Polar, Veera Thota, Timothy Naman, Tri Dung Dinh,Takahito Komori
#Marketing Strategy for Air France 
#####################################################################

library(readxl)

af <- read_excel("~/Desktop/Air France Case Spreadsheet Supplement.xls")

#rename column for easier coding experiences
names(af) <- c("pub_id", "pub_name", "keyword_id","keyword","match_type",
               "campaign","keyword_group","category","bid_strat","keyword_type",
               "status","se_bid","clicks","click_charges","avg_cpc","impressions",
               "engine_click_thru_perc","avg_pos","trans_conv_perc",
               "total_cost_per_trans","revenue","total_cost","total_booking")
############################
af <- af[!af$campaign == "Unassigned",]
af$bid_strat <- gsub("Position 1 -2 Target","Position 1-2 Target",af$bid_strat)
af$bid_strat <- gsub("Postiion 1-4 Bid Strategy","Position 1-4 Bid Strategy",af$bid_strat)

af$profit <- af$revenue - af$total_cost
af$profit_margin <- (af$profit/af$revenue)*100
af$cpb <- af$total_booking/af$total_cost
af$convers_rate <- (af$total_booking/af$clicks)*100
af$road <- (af$revenue/af$total_cost)


############################
#subsetting revenuve 
af <- as.data.frame(af)

af2 <- af[!af$revenue==0,]

af3 <- af[af$revenue==0,]


###
summary(af)

##subtotal by publisher

sub_pub<-aggregate(cbind(revenue, total_cost, total_booking, clicks) ~ pub_name, data = af, FUN=sum, na.rm=TRUE)

sub_pub$profit <- sub_pub$revenue - sub_pub$total_cost
sub_pub$profit_margin <- round((sub_pub$profit/sub_pub$revenue)*100, 2)
sub_pub$profit_per_booking <- sub_pub$profit/sub_pub$total_booking
sub_pub$cpc <- sub_pub$clicks/sub_pub$total_cost
##Average return on advertising 

sub_pub$roa<-sub_pub$revenue/sub_pub$total_cost

##Average cost per booking 
sub_pub$avg_cpb <- sub_pub$total_cost/sub_pub$total_booking
##Conversion rate 
sub_pub$convers_rate <- (sub_pub$total_booking/sub_pub$clicks)*100

##
#plot(x=af$clicks, y=af$total_booking)

##Subtotal by campaign 
sub_camp<-aggregate(cbind(revenue, total_cost, total_booking, clicks) ~ campaign, data = af, FUN=sum, na.rm=TRUE)

sub_camp$convers_rate <- (sub_camp$total_booking/sub_camp$clicks)*100

sub_camp$profit <- sub_camp$revenue - sub_camp$total_cost
sub_camp$profit_margin <- round((sub_camp$profit/sub_camp$revenue)*100, 2)


sub_camp$profit_per_booking <- sub_camp$profit/sub_camp$total_booking
sub_camp$cpc <- sub_camp$clicks/sub_camp$total_cost
##Average return on advertising 

sub_camp$roa<-sub_camp$revenue/sub_camp$total_cost

##Average cost per booking 
sub_pub$avg_cpb <- sub_pub$total_cost/sub_pub$total_booking
##Conversion rate 
sub_pub$convers_rate <- round((sub_pub$total_booking/sub_pub$clicks)*100, 2)
##Subtotal by campaign sub-group publisher
sub_camp_pub<-aggregate(cbind(revenue, total_cost, total_booking, clicks) ~ campaign + pub_name, data = af, FUN=sum, na.rm=TRUE)

sub_camp_pub$profit <- sub_camp_pub$revenue - sub_camp_pub$total_cost
sub_camp_pub$profit_margin <- round((sub_camp_pub$profit/sub_camp_pub$revenue)*100, 2)

##Subtotal by bid strategy
sub_bid_strat<-aggregate(cbind(revenue, profit, total_booking, clicks, total_cost) ~ bid_strat, data = af, FUN=sum, na.rm=TRUE)
sub_bid_strat$profit<- round(sub_bid_strat$profit,0)
sub_bid_strat$profit_margin <- (sub_bid_strat$profit/sub_bid_strat$revenue)*100
sub_bid_strat$convers_rate <- (sub_bid_strat$total_booking/sub_bid_strat$clicks)*100

sub_bid_strat$profit_per_booking <- sub_bid_strat$profit/sub_bid_strat$total_booking
sub_bid_strat$cpc <- sub_bid_strat$clicks/sub_bid_strat$total_cost
##Average return on advertising 

sub_bid_strat$roa<-sub_bid_strat$revenue/sub_bid_strat$total_cost

sum(sub_pub$total_booking)
sum(sub_bid_strat$total_booking)
##Subtotal by 

##Plotting
library(ggplot2)
#install.packages("tidyverse")
#installed.packages("gapminder")
#install.packages("scales")
library(tidyverse)
library(gapminder)
library(scales)
###Publisher & Profit Margin
ggplot(data = sub_pub, aes(x=pub_name,y=profit_margin,color=pub_name,fill=pub_name)) +
  geom_bar(stat="identity", width=0.4) + labs(x="Publisher") + labs(y="Profit Margin") + ggtitle("Profit Margin by Publisher") +geom_text(aes(label = profit_margin), vjust = -0.5, colour = "black") +theme(legend.position="none")


ggplot(data = sub_pub, aes(x=pub_name,y=total_booking,color=pub_name,fill=pub_name)) +
  geom_bar(stat="identity", width=0.4) + labs(x="Publisher") + labs(y="Total Bookings") + ggtitle("Total Bookings by Publisher") +geom_text(aes(label = total_booking), vjust = -0.5, colour = "black") +theme(legend.position="none")

ggplot(data = sub_pub, aes(reorder(x=pub_name, total_booking),y=total_booking,color=pub_name,fill=pub_name)) +
  geom_bar(stat="identity", width=0.4) + labs(x="Publisher") + labs(y="Total Bookings") + ggtitle("Total Bookings by Publisher") +geom_text(aes(label = total_booking), vjust = -0.5, colour = "black") +theme(legend.position="none")
##Pubs & Conversion Rate
ggplot(data = sub_pub, aes(reorder(x=pub_name, -convers_rate),y=convers_rate,color=pub_name,fill=pub_name)) +
  geom_bar(stat="identity", width=0.4) + labs(x="Publisher") + labs(y="Conversion Rate (%)") + ggtitle("Conversion Rate by Publisher") +geom_text(aes(label = convers_rate), vjust = -0.4, colour = "black", size=6) +theme(legend.position="none",text=element_text(size=24)) 

##Bid_strat & Profit 
ggplot(data = sub_bid_strat, aes(reorder(x=bid_strat, -profit),y=profit,color=bid_strat,fill=bid_strat)) +
  geom_bar(stat="identity", width=0.4) + labs(x="Bid Strategy") + labs(y="Profit") + ggtitle("Profit by Bid Strategy") +geom_text(aes(label = profit), vjust = -0.5, colour = "black", size=6) +theme(legend.position="none",text=element_text(size=24)) + scale_y_continuous(labels=dollar)


###Campaign and Profit Margin 
library(dplyr)
sub_camp_top<- top_n(sub_camp, 3, sub_camp$profit_margin)

sub_camp_bot<- top_n(sub_camp, -3, sub_camp$profit)

sub_camp_topbot <- rbind(sub_camp_top,sub_camp_bot)

##Top/Bot 3 campaign by PM
ggplot(data = sub_camp_topbot, aes(x=campaign,y=profit_margin,color=campaign,fill=campaign)) +
  geom_bar(stat="identity", width=0.4) + labs(x="Campaign") + labs(y="Profit Margin (%)") + ggtitle("Top/Bottom 3 Profit Margin by Campaign")+geom_text(aes(label = profit_margin), vjust = 4, colour = "black", size=6) +theme(legend.position="none",text=element_text(size=24))

ggplot(data = sub_camp_top, aes(x=campaign,y=profit_margin,color=campaign,fill=campaign)) +
  geom_bar(stat="identity", width=0.4) + labs(x="Campaign") + labs(y="Profit Margin(%)") + ggtitle("Top 3 Profit Margin by Campaign")+geom_text(aes(label = profit_margin), vjust = -0.5, colour = "black", size=6) +theme(legend.position="none",text=element_text(size=24))

ggplot(data = sub_pub, aes(x=pub_name,y=avg_cpb,color=pub_name,fill=pub_name)) +
  geom_bar(stat="identity", width = 0.5)




###Publihers and Return on Advertising 

ggplot(data = sub_pub, aes(x=pub_name,y=avg_cpb,color=pub_name,fill=pub_name)) +
  geom_bar(stat="identity", width = 0.5)

##Running Correl between variables 

af$convers_rate <- af$total_booking/af$clicks
af$gross_profit <- (af$revenue-af$total_cost)/af$revenue

#install.packages("ggpubr")
library("ggpubr")

#Correl se_bid & total_booking
ggscatter(af, x = "se_bid", y = "total_booking", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SE Bid", ylab = "Total Bookings")


corel_table<-cor(af2[,12:ncol(af2)])

##sub_camp_0
sub_camp2<-aggregate(cbind(revenue, total_cost, total_booking, clicks, impressions) ~ campaign, data = af3, FUN=sum, na.rm=TRUE)

##Regression 

sub_keyword<-aggregate(cbind(revenue, total_cost, total_booking, clicks, impressions) ~ keyword, data = af, FUN=sum, na.rm=TRUE)

sub_keyword$cpc <- sub_keyword$total_cost/sub_keyword$clicks




