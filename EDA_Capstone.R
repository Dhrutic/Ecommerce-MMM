### Load Libraries
library(e1071)
library(cowplot)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
library(ggthemes)
library(cowplot)
library(dplyr)
library(zoo)
library(DataCombine)
library(imputeTS)
library(MASS)
library(car)
library(DAAG)
library(caTools)
library(tidyr)
library(sqldf)


setwd("D:")
### Data Load
#import raw consumer electronics data
raw_data <- read.csv(file="ConsumerElectronics.csv",stringsAsFactors = FALSE)
media_investments_data <- read_xlsx("Media data and other information.xlsx", sheet = "Media Investment", skip = 2)
nps <- read_xlsx("Media data and other information.xlsx", sheet = "Monthly NPS Score", skip =2, col_names = FALSE)

top_three_category_data <-subset(raw_data, product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio"))
str(top_three_category_data)   #566267 obs. of  20 variables

#Re formatting nps data fram

monthlynps <- data.frame(
  month_sequence = c(1:12),
  Year = c(2015,2015,2015,2015,2015, 2015,2016,2016,2016,2016,2016,2016),
  Month = c(7,8,9,10,11,12,1,2,3,4,5,6),
  nps_score = c(54.6,60.0,46.9,44.4,47.0,45.8,47.1,50.3,49.0,51.8,47.3,50.5))


#load special sale days
### Creating a vector "date" to store the dates of Special Sale Calender as provided in the  'Media data and other information.xlsx'
special_sale_dates <- as.Date(c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28","2015-08-29","2015-08-30","2015-10-15","2015-10-16","2015-10-17",
                                "2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-11-11","2015-11-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26","2015-12-27",
                                "2015-12-28","2015-12-29","2015-12-30","2015-12-31","2016-01-01","2016-01-02","2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01",
                                "2016-02-02","2016-02-14","2016-02-15","2016-02-20","2016-02-21","2016-03-07","2016-03-08","2016-03-09","2016-05-25","2016-05-26","2016-05-27"))



#Data preparation
# subset data between June 2015 to July 2016


top_three_category_data$order_date <- as.Date(top_three_category_data$order_date);

filtered_data <-  dplyr::filter(top_three_category_data ,
                                top_three_category_data$order_date >=  as.Date("2015-07-01"),
                                top_three_category_data$order_date <=as.Date("2016-06-30"));


########################################Data cleaning ##############################################################

### Filter out the duplicate records based on columns "order_date", "order_id", "order_item_id" and "units"
nrow(filtered_data[duplicated(filtered_data[c(2,5,6,8)]),])
filtered_data <- filtered_data[!duplicated(filtered_data[c(2,5,6,8)]),]
sum(is.na(filtered_data$cust_id==T))
#Filtering out rows where cust_id=NA
filtered_data <- filtered_data[complete.cases(filtered_data[,13]), ]
### Filtering out the rows/order where Product_MRP equals to '0'
filtered_data <- subset(filtered_data, product_mrp != 0)


### Replacing the "gmv" with product_mrp*units wherever gmv=0 (assuming products were sold without any discount)
row_no <- which(filtered_data$gmv==0)
na_row_no<-which(is.na(filtered_data$gmv)==T)
for(i in 1:length(row_no)){
  filtered_data$gmv[row_no[i]] <- (filtered_data$product_mrp[row_no[i]])*(filtered_data$units[row_no[i]])
}
for(j in 1:length(na_row_no)){
  filtered_data$gmv[na_row_no[i]] <- (filtered_data$product_mrp[na_row_no[i]])*(filtered_data$units[na_row_no[i]])
}

# Checking for the "units" column in case of any outlier 
quantile(filtered_data$units, seq(0,1,.001))  #99.9% of orders/records have <=4 units
table(filtered_data$units)

# Capping the maximum order to 4 and filtering out the records having units>4 as outlier
filtered_data <- subset(filtered_data, units <= 4)

### Checking of the NA or any invalid valid values in "deliverybdays" column
table(filtered_data$deliverybdays)  #so many \\N values and some negative values also

# Assining 0 to the values where deliverycdays are "\\N" and negative values
filtered_data$deliverybdays[filtered_data$deliverybdays == "\\N" | filtered_data$deliverybdays<0]<- 0
filtered_data$deliverybdays <- as.numeric(filtered_data$deliverybdays)

### Checking of the NA or any invalid valid values in "deliverycdays" column
table(filtered_data$deliverycdays)  #so many \\N values and some negative values also

# Assining 0 to the values where deliverycdays are "\\N" and negative values
filtered_data$deliverycdays[filtered_data$deliverycdays == "\\N" | filtered_data$deliverycdays < 0] <- 0
filtered_data$deliverycdays <- as.numeric(filtered_data$deliverycdays)

### Analyzing "product_procurement_sla" column
table(filtered_data$product_procurement_sla) #10717 negative values

# Assining 0 to the values where product_procurement_sla having negative values

filtered_data$product_procurement_sla[filtered_data$product_procurement_sla < 0] <- 0
quantile(filtered_data$product_procurement_sla, seq(0,1,.001))
#filtered_data$product_procurement_sla[filtered_data$product_procurement_sla > 15] <- 15  ##Capping the max product_procurement_sla as 15

### Checking for the range of "sla" column 
table(filtered_data$sla)
quantile(filtered_data$sla, seq(0,1,.001)) ##99.9% of data has max sla of 17, so we'll cap as 17 max
filtered_data$sla[filtered_data$sla > 17] <- 17  ##Capping the max sla as 17

### Checking for missing('NA') value
sapply(filtered_data, function(x) sum(is.na(x)))  #No NA values

##############################################################################################
# Converison of Week Number
filtered_data$order_date<-date(filtered_data$order_date)
#create new column week of the year
filtered_data$WeekNumber<-lubridate::week(filtered_data$order_date)
#create new column week of the month
filtered_data$week<-(ceiling(as.numeric(format(filtered_data$order_date,"%d"))/7))
#create new column of date
filtered_data$date <- as.Date(filtered_data$order_date)
class(filtered_data$order_date)

filtered_data$WeekNumber<- ifelse(
  filtered_data$WeekNumber>=26 & year(as.Date(filtered_data$order_date))==2015
  ,filtered_data$WeekNumber-25,
  filtered_data$WeekNumber+27);
filtered_data$Month <-  (filtered_data$Month + 6)%%12
filtered_data[filtered_data$Month==0,"Month"] <- 12

View(head(filtered_data))
datetimedf<-dplyr::select(filtered_data,Year,Month,WeekNumber,week,order_date)
datetimedf <- datetimedf[!duplicated(datetimedf[c(1,2,3,4,5)]),]
monthweekdf<-dplyr::select(datetimedf,Year,Month,WeekNumber,week)
monthweekdf <- monthweekdf[!duplicated(monthweekdf[c(1,2,3)]),]

data_minimalistic <- dplyr::select(filtered_data,order_date, WeekNumber,Month,Year, gmv,units, deliverybdays , deliverycdays, 
                                   s1_fact.order_payment_type ,sla ,pincode,product_analytic_super_category, 
                                   product_analytic_category , product_analytic_sub_category, product_analytic_vertical,    
                                   product_mrp,product_procurement_sla);


monthlynps$Month <- monthlynps$month_sequence
#data_minimalistic<-filtered_data[,-c(1,3,5,6,13)]
View(head(data_minimalistic))
#Merge nps data
monthly_combined_data <- merge(monthweekdf,monthlynps,by=c("Month","Year" ),all.x = TRUE)
combined_data <- merge(data_minimalistic,monthly_combined_data,by=c("Month","Year","WeekNumber"),all.x = TRUE)
View(head(combined_data))
#Removing Month Sequence
combined_data<-combined_data[,-c(19)]

#merge special sale data

## Creating a column whether a order is placed on spl sale day or not

combined_data$is_spl_sale_day <- ifelse(combined_data$order_date %in% special_sale_dates, "Y", "N")

## Creating one more column "spl_sale_day" which stores which spl day it was (like Diwali, Eid etc.)
combined_data$special_sale_day='Regular Day'

combined_data <- within(combined_data, {
  special_sale_day[order_date  %in% (special_sale_dates[1:2])]='Eid & Rathayatra'
  special_sale_day[order_date  %in% (special_sale_dates[3:5])]='Independence Day'
  special_sale_day[order_date  %in% (special_sale_dates[6:8])]='Rakshabandhan'
  special_sale_day[order_date  %in% (special_sale_dates[9:11])]='Daussera'
  special_sale_day[order_date  %in% (special_sale_dates[12:19])]='Diwali'
  special_sale_day[order_date  %in% (special_sale_dates[20:29])]='Christmas & New Year'
  special_sale_day[order_date  %in% (special_sale_dates[30:32])]='Republic Day'
  special_sale_day[order_date  %in% (special_sale_dates[33:34])]='BED'
  special_sale_day[order_date  %in% (special_sale_dates[35:36])]='Valentine Day'
  special_sale_day[order_date  %in% (special_sale_dates[37:38])]='FHSD'
  special_sale_day[order_date  %in% (special_sale_dates[39:41])]='BSD'
  special_sale_day[order_date  %in% (special_sale_dates[42:44])]='Pacman'
})


######################################### merge marketing data##################################################

media_investments_data$Month = (media_investments_data$Month + 6) %% 12
media_investments_data[12,2] = 12
media_investments_combined_data <- merge(monthweekdf,media_investments_data,by = c("Year","Month") ,all.x = TRUE)
combined_data <- merge(media_investments_combined_data,combined_data,by=c("Month","Year","WeekNumber"),all.x = TRUE)

#Removing week.y
combined_data<-combined_data[,-c(29)]

#Convert Combined data nps score from monthly to weekly
combined_data$nps_score <- combined_data$nps_score/4.30

View(head(combined_data))

combined_data <- subset(combined_data, (product_mrp*units)>=gmv)
#############################Finding no. of holidays in each week##################################################
holidays <- special_sale_dates   #Coverting the date vector into Date format
WeekNumber <- as.numeric(strftime(holidays, format = "%V"))  #Extracting the weeks out of date
Year <- as.numeric(format(as.POSIXct(holidays, format="%Y-%m-%d"),"%Y")  )

holiday_details <- data.frame(cbind(Year,WeekNumber))  
str(holiday_details)
holiday_details$WeekNumber<- ifelse(
  holiday_details$WeekNumber>=26 & (holiday_details$Year)==2015
  ,holiday_details$WeekNumber-25,
  holiday_details$WeekNumber+27)

holiday_details$holidays <- holidays
holiday_details$holiday_count <- 1
holiday_details <- aggregate(holiday_count~Year+WeekNumber, holiday_details, sum) 

combined_data <- merge(combined_data, holiday_details, by = c("Year", "WeekNumber"), all.x = TRUE)
sum(is.na(combined_data$holiday_count))
#Replacing 365109 NA with zero
combined_data$holiday_count[is.na(combined_data$holiday_count)] <- 0

### Further filtering the data for the product sub-categories- camera accessory, home audio and gaming accessory. 

data_CameraAccessory <- subset(combined_data, product_analytic_sub_category == "CameraAccessory")
data_HomeAudio <- subset(combined_data, product_analytic_sub_category == "HomeAudio")
data_GamingAccessory <- subset(combined_data, product_analytic_sub_category == "GamingAccessory")

##################### exploratory data analysis ########################################################################
#Share of Different types of Camera Accessories ordered
unique(factor(data_CameraAccessory$product_analytic_vertical))

bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                  legend.position="Left")

#Camera Accessory category plot 
p1 <- ggplot(data_CameraAccessory,  aes(x = product_analytic_vertical, fill = product_analytic_vertical) ) +
  geom_bar() + bar_theme + ggtitle("Camera Accessory Category")

#Home Audio
p2 <- ggplot(data_HomeAudio,  aes(x = product_analytic_vertical, fill = product_analytic_vertical) ) +
  geom_bar() + bar_theme + ggtitle("Home Audio Category")

#Game Accessory
p3<- ggplot(data_GamingAccessory,  aes(x = product_analytic_vertical, fill = product_analytic_vertical) ) +
  geom_bar() + bar_theme + ggtitle("Game Accessory Category")


library(cowplot)
plot_grid(p1,p2,p3,labels = "AUTO")
barfill <- "#4271AE"
barlines <- "#1F3552"



p11 <- ggplot(data_CameraAccessory, aes(x = WeekNumber)) +
  geom_histogram(aes(y = ..count..), binwidth = 10,
                 colour = barlines, fill = barfill) +
  scale_y_continuous(name = "Count") +
  ggtitle("No. of orders based on different Payment Methods for camera accessories") +
  theme_bw() +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9)) +
  facet_grid(. ~ s1_fact.order_payment_type, scales = "free") 

p22 <- ggplot(data_HomeAudio, aes(x = WeekNumber)) +
  geom_histogram(aes(y = ..count..), binwidth = 10,
                 colour = barlines, fill = barfill) +
  scale_y_continuous(name = "Count") +
  ggtitle("No. of orders based on different Payment Methods for Home Audio") +
  theme_bw() +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9)) +
  facet_grid(. ~ s1_fact.order_payment_type, scales = "free")

p33 <- ggplot(data_GamingAccessory, aes(x = WeekNumber)) +
  geom_histogram(aes(y = ..count..), binwidth = 10,
                 colour = barlines, fill = barfill) +
  scale_y_continuous(name = "Count") +
  ggtitle("No. of orders based on different Payment Methods for Home Audio") +
  theme_bw() +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9)) +
  facet_grid(. ~ s1_fact.order_payment_type, scales = "free")


plot_grid(p11,p22,p33,labels = "AUTO")

#Spend in Media AD marketing on special days
plot_grid(ggplot(subset(data_CameraAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(TV, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs(title =" New title", x = "Special Days", y = "TV marketing"),
          
          #Spend in digital marketing on special days
          ggplot(subset(data_CameraAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(Digital, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Digital marketing"),
          
          #Spend in Sponsorship on special days
          ggplot(subset(data_CameraAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(Sponsorship, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Sponsorship"),
          
          #Spend in Content marketing on special days
          ggplot(subset(data_CameraAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(`Content Marketing`, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Content marketing"),
          
          #Spend in Online marketing on special days
          ggplot(subset(data_CameraAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(`Online marketing`, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Online marketing"))

#Spend in Media AD marketing on all days
plot_grid(ggplot( subset(data_CameraAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, TV )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in digital marketing on all days
          ggplot(subset(data_CameraAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, Digital )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Sponsorship on all days
          ggplot( subset(data_CameraAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, Sponsorship )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Content marketing on all days
          ggplot(  subset(data_CameraAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, `Content Marketing` )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Online marketing on all days
          ggplot(subset(data_CameraAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, `Online marketing` )) + geom_smooth()+ background_grid(major = "xy", minor = "none"))



# Media Marketing Spend on Home Audio accessories on Special day
plot_grid(ggplot(subset(data_HomeAudio,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(TV, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "TV marketing"),
          
          #Spend in digital marketing on special days
          ggplot(subset(data_HomeAudio,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(Digital, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Digital marketing"),
          
          #Spend in Sponsorship on special days
          ggplot(subset(data_HomeAudio,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(Sponsorship, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Sponsorship"),
          
          #Spend in Content marketing on special days
          ggplot(subset(data_HomeAudio,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(`Content Marketing`, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Content marketing"),
          
          #Spend in Online marketing on special days
          ggplot(subset(data_HomeAudio,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(`Online marketing`, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Online marketing"))

#Spend in Media AD marketing on Regular days for Home Audio Accesory
plot_grid(ggplot( subset(data_HomeAudio,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, TV )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in digital marketing on all days
          ggplot(subset(data_HomeAudio,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, Digital )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Sponsorship on all days
          ggplot( subset(data_HomeAudio,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, Sponsorship )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Content marketing on all days
          ggplot(  subset(data_HomeAudio,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, `Content Marketing` )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Online marketing on all days
          ggplot(subset(data_HomeAudio,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, `Online marketing` )) + geom_smooth()+ background_grid(major = "xy", minor = "none"))

str(data_GamingAccessory)


# Media Marketing Spend on Gaming accessories on Special day
plot_grid(ggplot(subset(data_GamingAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(TV, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "TV marketing"),
          
          #Spend in digital marketing on special days
          ggplot(subset(data_GamingAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(Digital, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Digital marketing"),
          
          #Spend in Sponsorship on special days
          ggplot(subset(data_GamingAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(Sponsorship, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Sponsorship"),
          
          #Spend in Content marketing on special days
          ggplot(subset(data_GamingAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(`Content Marketing`, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Content marketing"),
          
          #Spend in Online marketing on special days
          ggplot(subset(data_GamingAccessory,!(special_sale_day %in% ("Regular Day"))),
                 aes(y=mean(`Online marketing`, na.rm = TRUE),x=special_sale_day,fill=))+
            geom_bar(stat = "identity")+bar_theme+ labs( x = "Special Days", y = "Online marketing"))

#Spend in Media AD marketing on Regular days for Gaming Accesories
plot_grid(ggplot( subset(data_GamingAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, TV )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in digital marketing on all days
          ggplot(subset(data_GamingAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, Digital )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Sponsorship on all days
          ggplot( subset(data_GamingAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, Sponsorship )) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Content marketing on all days
          ggplot(  subset(data_GamingAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber,`Content Marketing`)) + geom_smooth()+ background_grid(major = "xy", minor = "none"),
          #Spend in Online marketing on all days
          ggplot(subset(data_GamingAccessory,(special_sale_day %in% ("Regular Day"))), aes( WeekNumber, `Online marketing` )) + geom_smooth()+ background_grid(major = "xy", minor = "none"))

##########################################################################
# Creating the adstock for each media investment
#converting monthly to weekly
media_investments_combined_data3 <- cbind(media_investments_combined_data[,c(1,2,3,4)], media_investments_combined_data[,-c(1,2,3,4)]/4.30)

#Creating a csv for further calculations of adstock variable
adstock_rate = 0.50
df <- monthweekdf
for(i in 5:ncol(media_investments_combined_data3)){
  
  df[[paste0(colnames(media_investments_combined_data3)[i],"_adstock")]] <- stats::filter(x=media_investments_combined_data3[i], 
                                                                                          filter=adstock_rate, method="recursive")
  
}
groupand_select <-function(df_subset)
{
  df_merge<-merge(df,df_subset,by = c("Year","Month","WeekNumber"))
  df_to_group<-dplyr::select(df_merge,WeekNumber,`Total Investment_adstock`,TV_adstock,Digital_adstock,Sponsorship_adstock,
                             `Content Marketing_adstock`,`Online marketing_adstock`,Affiliates_adstock,SEM_adstock,
                             sla,gmv,units,product_procurement_sla,nps_score)
  return(df_to_group);  
}
data_CameraAccessory_Final <-groupand_select( data_CameraAccessory )
data_HomeAudio_Final <-groupand_select( data_HomeAudio )
data_GamingAccessory_Final <-groupand_select( data_GamingAccessory )
View(head(data_CameraAccessory_Final))
View(head(data_CameraAccessory_Final))
View(head(data_CameraAccessory_Final))
#####################exploratory data analysis###########################

title_Camera <- ggdraw() + draw_label("Camera Accessory", fontface='bold')
plot_grid(title_Camera,
          ggplot(data_CameraAccessory_Final, aes(TV_adstock,gmv))+ geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "TV Adstock", y = "GMV"),
          #digital marketing w.r.t gmv
          ggplot(data_CameraAccessory_Final, aes( Digital_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Digital Adstock", y = "GMV"),
          #Sponsorship w.r.t gmv
          ggplot(data_CameraAccessory_Final, aes(Sponsorship_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Sponsorship Adstock", y = "GMV"),
          #Content marketing w.r.t gmv
          ggplot(data_CameraAccessory_Final, aes(`Content Marketing_adstock`,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Content Marketing Adstock", y = "GMV"),
          #Online marketing w.r.t gmv
          ggplot(data_CameraAccessory_Final, aes(`Online marketing_adstock`,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Online Marketing Adstock", y = "GMV"),
          #Affiliates marketing w.r.t gmv
          ggplot(data_CameraAccessory_Final, aes(Affiliates_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Affiliates Adstock", y = "GMV"),
          #SEM marketing w.r.t gmv
          ggplot(data_CameraAccessory_Final, aes(SEM_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "SEM Adstock", y = "GMV"))

title_HomeAudio <- ggdraw() + draw_label("HomeAudio Accessory", fontface='bold')
plot_grid(title_HomeAudio,
          ggplot(data_HomeAudio_Final, aes(TV_adstock,gmv))+ geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "TV Adstock", y = "GMV"),
          #digital marketing w.r.t gmv
          ggplot(data_HomeAudio_Final, aes( Digital_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Digital Adstock", y = "GMV"),
          #Sponsorship w.r.t gmv
          ggplot(data_HomeAudio_Final, aes(Sponsorship_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Sponsorship Adstock", y = "GMV"),
          #Content marketing w.r.t gmv
          ggplot(data_HomeAudio_Final, aes(`Content Marketing_adstock`,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Content Marketing Adstock", y = "GMV"),
          #Online marketing w.r.t gmv
          ggplot(data_HomeAudio_Final, aes(`Online marketing_adstock`,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Online Marketing Adstock", y = "GMV"),
          #Affiliates marketing w.r.t gmv
          ggplot(data_HomeAudio_Final, aes(Affiliates_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Affiliates Adstock", y = "GMV"),
          #SEM marketing w.r.t gmv
          ggplot(data_HomeAudio_Final, aes(SEM_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "SEM Adstock", y = "GMV"))

title_Gaming <- ggdraw() + draw_label("Gaming Accessory", fontface='bold')
plot_grid(title_Gaming,
          ggplot(data_GamingAccessory_Final, aes(TV_adstock,gmv))+ geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "TV Adstock", y = "GMV"),
          #digital marketing w.r.t gmv
          ggplot(data_GamingAccessory_Final, aes( Digital_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Digital Adstock", y = "GMV"),
          #Sponsorship w.r.t gmv
          ggplot(data_GamingAccessory_Final, aes(Sponsorship_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Sponsorship Adstock", y = "GMV"),
          #Content marketing w.r.t gmv
          ggplot(data_GamingAccessory_Final, aes(`Content Marketing_adstock`,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Content Marketing Adstock", y = "GMV"),
          #Online marketing w.r.t gmv
          ggplot(data_GamingAccessory_Final, aes(`Online marketing_adstock`,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Online Marketing Adstock", y = "GMV"),
          #Affiliates marketing w.r.t gmv
          ggplot(data_GamingAccessory_Final, aes(Affiliates_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "Affiliates Adstock", y = "GMV"),
          #SEM marketing w.r.t gmv
          ggplot(data_GamingAccessory_Final, aes(SEM_adstock,gmv )) + geom_smooth()+ background_grid(major = "xy", minor = "none")+ labs( x = "SEM Adstock", y = "GMV"))


