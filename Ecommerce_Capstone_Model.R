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
library(DataCombine)
library(imputeTS)
library(memisc)

###############ALL MASTER DATA##############################################
setwd("E:/ecommerce/Capstone")
raw_data <- read.csv(file="ConsumerElectronics.csv",stringsAsFactors = FALSE)

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


media_investments_data <- read_xlsx("Media data and other information.xlsx", sheet = "Media Investment", skip = 2)
#####################ALL MASTER DATA END####################################

######Function to remove columns by name 
removebyname <-function(df_,nameofcolumn)
{
  drops <- c(nameofcolumn)
  return(df_[ , !(names(df_) %in% drops)])
}
######


top_three_category_data <-subset(raw_data, product_analytic_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio"));

top_three_category_data$order_date <- as.Date(top_three_category_data$order_date);

period_data <-  dplyr::filter(top_three_category_data ,
                                top_three_category_data$order_date >=  as.Date("2015-07-01"),
                                top_three_category_data$order_date <=as.Date("2016-06-30"));



#######################DATA CLEANING#####################################
filtered_data<- period_data;
# "order_date"    "order_id"      "order_item_id" "units"
filtered_data <- filtered_data[!duplicated(filtered_data[c(2,5,6,8)]),]

filtered_data <- filtered_data[complete.cases(filtered_data[,13]), ]

### Filtering out the rows/order where Product_MRP equals to '0'
filtered_data <- subset(filtered_data, product_mrp != 0) #512904 rows

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
filtered_data <- subset(filtered_data, units <= 4) #512502 rows


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
table(filtered_data$product_procurement_sla) 

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

#remove rows which have gmv>=mrp*units 
filtered_data <- subset(filtered_data, (product_mrp*units)>=gmv);


filtered_data <- removebyname(filtered_data,"order_id");
filtered_data <- removebyname(filtered_data,"order_item_id");
filtered_data <- removebyname(filtered_data,"ï..fsn_id");
filtered_data <- removebyname(filtered_data,"cust_id");
filtered_data <- removebyname(filtered_data,"pincode")

#filtered_data <- removebyname(filtered_data,"product_analytic_vertical")
filtered_data <- removebyname(filtered_data,"product_analytic_super_category")



cleaned_data <- filtered_data;
#######################DATA CLEANING END#####################################


##########KPI ENGINEERING#############################
period_days <- as.data.frame(c(0:365));
colnames(period_days)[1] <- "Day";
period_dates <-as.data.frame(as.Date(as.Date("2015-07-01")+  period_days$Day));
colnames(period_dates)[1] <- "Dates";
period_weeks <-lubridate::isoweek(period_dates$Dates);
period_months <- lubridate::month(period_dates$Dates);
period_week_month<- cbind(period_dates,period_weeks,period_months);
colnames(period_week_month )[3] <- "Month";




#promotional_offer
cleaned_data$list_price <- cleaned_data$gmv/cleaned_data$units;
cleaned_data$promotional_offer <- ((cleaned_data$product_mrp - cleaned_data$list_price)/cleaned_data$product_mrp)*100;

#price tag


#is prepaid
cleaned_data$is_prepaid <- ifelse(cleaned_data$s1_fact.order_payment_type == "Prepaid",1,0)


#is special week
cleaned_data$is_special_sale_day <- ifelse(cleaned_data$order_date %in% special_sale_dates,1,0)





#nps
nps_week_month <-as.data.frame(period_week_month);
nps_week_month$period_weeks<-ifelse(  nps_week_month$period_weeks>=27 ,nps_week_month$period_weeks-26,  nps_week_month$period_weeks+27);


nps_week <- merge(nps_week_month,monthlynps,by = "Month")
nps_week_grouped <-sqldf("select
   period_weeks as WeekNumber
 , avg(nps_score) as nps_score
from
   nps_week
   group by
   period_weeks");
nps_final <- nps_week_grouped;

#ad stock
investment_week <- merge(period_week_month,media_investments_data,by = "Month")
weekly_converter <- 4.29;
investment_week$Total_Investment <-investment_week$`Total Investment`/weekly_converter;
investment_week$TV <-investment_week$TV/ weekly_converter; 
investment_week$Digital <-investment_week$Digital/ weekly_converter; 
investment_week$Sponsorship <-investment_week$Sponsorship/ weekly_converter; 
investment_week$Content_Marketing <-investment_week$`Content Marketing`/ weekly_converter; 
investment_week$Online_marketing <-investment_week$`Online marketing`/ weekly_converter; 
investment_week$Affiliates <-investment_week$Affiliates/ weekly_converter; 
investment_week$SEM <-investment_week$SEM/ weekly_converter; 
investment_week$Radio <-investment_week$Radio/ weekly_converter; 
investment_week$Other <-investment_week$Other/ weekly_converter; 

investments_grouped_by_week <- sqldf("select
                         period_weeks as WeekNumber
                         , avg(Total_Investment) as Total_Investment
                         , avg(TV) as TV
                         , avg(Digital) as Digital
                         , avg(Sponsorship) as Sponsorship
                         , avg(Content_Marketing) as Content_Marketing
                         , avg(Online_marketing) as Online_marketing
                         , avg(Affiliates) as Affiliates
                         , avg(SEM) as SEM
                         , avg(Radio) as Radio
                         , avg(Other) as Other
                         from
                         investment_week
                         group by
                         period_weeks");


investments_grouped_by_week$Radio[is.na(investments_grouped_by_week$Radio)]<-0;
investments_grouped_by_week$Other[is.na(investments_grouped_by_week$Other)]<-0; 


investments_grouped_by_week$WeekNumber<-ifelse(investments_grouped_by_week$WeekNumber>=27 ,
                                               investments_grouped_by_week$WeekNumber-26,
                                               investments_grouped_by_week$WeekNumber+27);

investments_grouped_by_week <- arrange(investments_grouped_by_week,WeekNumber)
AdStock <- 0.5;
adstock<-function(x,rate=0){
  return(as.numeric(stats::filter(x=x,filter=rate,method="recursive")))
}
  
investments_grouped_by_week$Total_Investment_Adstock  <- adstock(investments_grouped_by_week$Total_Investment,AdStock);
investments_grouped_by_week$TV_Adstock  <- adstock(investments_grouped_by_week$TV,AdStock);
investments_grouped_by_week$Digital_Adstock  <- adstock(investments_grouped_by_week$Digital,AdStock);
investments_grouped_by_week$Sponsorship_Adstock  <- adstock(investments_grouped_by_week$Sponsorship,AdStock);
investments_grouped_by_week$Content_Marketing_Adstock  <- adstock(investments_grouped_by_week$Content_Marketing,AdStock);
investments_grouped_by_week$Online_marketing_Adstock  <- adstock(investments_grouped_by_week$Online_marketing,AdStock);
investments_grouped_by_week$Affiliates_Adstock  <- adstock(investments_grouped_by_week$Affiliates,AdStock);
investments_grouped_by_week$SEM_Adstock  <- adstock(investments_grouped_by_week$SEM,AdStock);
investments_grouped_by_week$Radio_Adstock  <- adstock(investments_grouped_by_week$Radio,AdStock);
investments_grouped_by_week$Other_Adstock  <- adstock(investments_grouped_by_week$Other,AdStock);

investmenst_final <- dplyr::select(investments_grouped_by_week,WeekNumber,Total_Investment_Adstock,TV_Adstock,
              Digital_Adstock,Sponsorship_Adstock,Content_Marketing_Adstock,Online_marketing_Adstock,
              Affiliates_Adstock,SEM_Adstock,Radio_Adstock,Other_Adstock);

addLag <- function( data_to_lag,lag_var){
 
  firstLag <- slide(data_to_lag, Var = lag_var, slideBy = -1);
  secondLag <- slide(firstLag, Var = lag_var, slideBy = -2);
  thirdLag <- slide(secondLag, Var = lag_var, slideBy = -3);
  lastcol<- ncol(thirdLag);
  thirdLag[, lastcol] <- imputeTS::na.ma(thirdLag[, lastcol], k=3, weighting = "simple")
  thirdLag[, lastcol-1] <- imputeTS::na.ma(thirdLag[, lastcol-1], k=2, weighting = "simple")
  thirdLag[, lastcol-2] <- imputeTS::na.ma(thirdLag[, lastcol-2], k=1, weighting = "simple")
  return(thirdLag);
}

group_and_merge_all <- function(data, category){

  #data<-cleaned_data;
  #category<-"CameraAccessory";
  
#KPI Product category
inp_dataframe <- subset(data, product_analytic_sub_category ==category);

cluster <- aggregate(cbind(units,list_price, product_mrp)~product_analytic_vertical, inp_dataframe, mean);
if(nrow(cluster)>2){
  cluster$units_1 <- scale(cluster$units)
  cluster$list_price_1 <- scale(cluster$list_price)
  cluster$product_mrp_1 <- scale(cluster$product_mrp)
  
  k11 <- cluster[,-c(1:3)]
  
  # Applying clustering algorithm
  clust <- kmeans(k11, centers = 3, iter.max = 50, nstart = 50)
  cluster$price_tag <- as.factor(clust$cluster)
  cluster <- cluster[, c(1,8)]
  
  # Adding columns generated from the clustering algorithm to the inp_dataframeset
  inp_dataframe <- merge(inp_dataframe, cluster, by=c("product_analytic_vertical"), all.x = TRUE)
  
  k22 <-count(inp_dataframe, inp_dataframe$price_tag)[2]
  
  levels(inp_dataframe$price_tag)[which(k22==max(count(inp_dataframe, price_tag)[2]))] <- "Mass_Product"
  levels(inp_dataframe$price_tag)[which(k22==min(count(inp_dataframe, price_tag)[2]))] <- "Premium_Product"
  levels(inp_dataframe$price_tag)[which(k22!=max(count(inp_dataframe, price_tag)[2]) & k22!=min(count(inp_dataframe, price_tag)[2]))] <- "Aspiring_Product"
  
} else{
  inp_dataframe$price_tag <- NA
  inp_dataframe$product_analytic_vertical <- factor(inp_dataframe$product_analytic_vertical)
  
  if(tapply(inp_dataframe$product_mrp, inp_dataframe$product_analytic_vertical, mean)[[1]] > tapply(inp_dataframe$product_mrp, inp_dataframe$product_analytic_vertical, mean)[[2]])
  {
    inp_dataframe$price_tag[which(inp_dataframe$product_analytic_vertical == levels(inp_dataframe$product_analytic_vertical)[1])] <- "Aspiring_Product"
    inp_dataframe$price_tag[is.na(inp_dataframe$price_tag)] <- "Mass_Product"
  }else{
    inp_dataframe$price_tag[which(inp_dataframe$product_analytic_vertical == levels(inp_dataframe$product_analytic_vertical)[2])] <- "Aspiring_Product"
    inp_dataframe$price_tag[is.na(inp_dataframe$price_tag)] <- "Mass_Product"
  }
  
}

kpi_loaded_data <- inp_dataframe;


# Weekly sale data

kpi_loaded_data$WeekNumber <-lubridate::isoweek(kpi_loaded_data$order_date);
kpi_loaded_data$WeekNumber<-ifelse(kpi_loaded_data$WeekNumber>=27,
                                   kpi_loaded_data$WeekNumber-26,
                                   kpi_loaded_data$WeekNumber+27);
kpi_loaded_data <- arrange(kpi_loaded_data,WeekNumber);

weekly_sale <-sqldf("
select
  WeekNumber
, price_tag
, avg(deliverycdays) as deliverycdays
, (sum(is_prepaid)/count(1)) * 100  as prepaid_percentage
, avg(promotional_offer) as promotional_offer
, avg(sla) as sla
, avg(product_procurement_sla) as product_procurement_sla
, avg(deliverybdays) as deliverybdays
, sum(gmv) as gmv
, max(is_special_sale_day) is_special_week
from
kpi_loaded_data
group by WeekNumber,price_tag
"); 


#merge nps, investment, weekly sale data
weekly_sale_nps <- merge(weekly_sale,nps_final,by="WeekNumber")
weekly_sale_nps_adstock <- merge(weekly_sale_nps,investmenst_final,by="WeekNumber")

base_data_for_building_model <- dplyr::select(
  weekly_sale_nps_adstock,
  price_tag,
  deliverycdays,
  prepaid_percentage,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv);

#LAG variables 
data_to_lag <- base_data_for_building_model;



data_to_lag<-addLag( data_to_lag,'deliverycdays');
data_to_lag<-addLag( data_to_lag,'prepaid_percentage');
data_to_lag<-addLag( data_to_lag,'promotional_offer');
data_to_lag<-addLag( data_to_lag,'sla');
data_to_lag<-addLag( data_to_lag,'product_procurement_sla');
data_to_lag<-addLag( data_to_lag,'deliverybdays');
data_to_lag<-addLag( data_to_lag,'is_special_week');
data_to_lag<-addLag( data_to_lag,'nps_score');
data_to_lag<-addLag( data_to_lag,'Total_Investment_Adstock');
data_to_lag<-addLag( data_to_lag,'TV_Adstock');
data_to_lag<-addLag( data_to_lag,'Digital_Adstock');
data_to_lag<-addLag( data_to_lag,'Sponsorship_Adstock');
data_to_lag<-addLag( data_to_lag,'Content_Marketing_Adstock');
data_to_lag<-addLag( data_to_lag,'Online_marketing_Adstock');
data_to_lag<-addLag( data_to_lag,'Affiliates_Adstock');
data_to_lag<-addLag( data_to_lag,'SEM_Adstock');
data_to_lag<-addLag( data_to_lag,'Radio_Adstock');
data_to_lag<-addLag( data_to_lag,'Other_Adstock');
data_to_lag<-addLag( data_to_lag,'gmv');

return_df <- data_to_lag;
return(return_df);
}


#camera linera model

weekly_grouped_camera <- group_and_merge_all(cleaned_data, "CameraAccessory");

##Building Linear Model for Camera Accessory#################################
#===========================================================================#
lm_data_camera <- dplyr::select(
                         weekly_grouped_camera,
                         price_tag,
                         deliverycdays,
                         prepaid_percentage,
                         promotional_offer,
                         sla,
                         product_procurement_sla,
                         deliverybdays,
                         is_special_week,
                         nps_score,
                         Total_Investment_Adstock,
                         TV_Adstock,
                         Digital_Adstock,
                         Sponsorship_Adstock,
                         Content_Marketing_Adstock,
                         Online_marketing_Adstock,
                         Affiliates_Adstock,
                         SEM_Adstock,
                         Radio_Adstock,
                         Other_Adstock,
                         gmv)

#Linear_model_camera<-lm(gmv~.,lm_data_camera)
#summary(Linear_model_camera)


levels(lm_data_camera$price_tag)
colSums(is.na(lm_data_camera))

colnames(lm_data_camera)

dummy_2 <- data.frame(model.matrix( ~price_tag, data = lm_data_camera))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
lm_data_camera <- cbind(lm_data_camera[,-1], dummy_2)
lm_data_camera_1 <- lm_data_camera

colnames(lm_data_camera_1)

lm_data_camera_1[, c(1:18,20:21)] <- scale(lm_data_camera_1[, c(1:18,20:21)])

colnames(lm_data_camera_1)
str(lm_data_camera_1)
summary(lm_data_camera_1)

set.seed(100) 
#Model Building
Linear_model_camera<-lm(gmv~.,lm_data_camera_1)
alias(Linear_model_camera)
summary(Linear_model_camera)



Linear_model_camera_2<-stepAIC(Linear_model_camera,direction = "both")
summary(Linear_model_camera_2)
options(scipen=999)

Linear_model_camera_3 <- lm(formula = gmv ~ deliverycdays + promotional_offer + sla + 
                              product_procurement_sla + is_special_week + nps_score + Total_Investment_Adstock + 
                              TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                              Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                              Radio_Adstock + Other_Adstock + price_tagMass_Product + price_tagAspiring_Product, 
                            data = lm_data_camera_1)

summary(Linear_model_camera_3)
vif(Linear_model_camera_3)

#Removing content Radio Adstock which no significance and high vif value
Linear_model_camera_4 <- lm(formula = gmv ~ deliverycdays + promotional_offer + sla + 
                              product_procurement_sla + is_special_week + nps_score + Total_Investment_Adstock + 
                              TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                              Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                              Other_Adstock + price_tagMass_Product + price_tagAspiring_Product, 
                            data = lm_data_camera_1)

summary(Linear_model_camera_4)
vif(Linear_model_camera_4)

#Removal of price_tagPremium_Product due to insignificance value
Linear_model_camera_5 <- lm(formula = gmv ~ deliverycdays + promotional_offer + sla + 
                              product_procurement_sla + is_special_week + nps_score + Total_Investment_Adstock + 
                              TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                              Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                              Other_Adstock + price_tagMass_Product, 
                            data = lm_data_camera_1)
summary(Linear_model_camera_5)
vif(Linear_model_camera_5)

#Removal of Other_Adstock due to insignificance and high vif value
Linear_model_camera_6 <- lm(formula = gmv ~ deliverycdays + promotional_offer + sla + 
                              product_procurement_sla + is_special_week + nps_score +  
                               Digital_Adstock +  
                              Online_marketing_Adstock +SEM_Adstock + 
                              price_tagMass_Product, 
                            data = lm_data_camera_1)

summary(Linear_model_camera_6)
vif(Linear_model_camera_6)


cv.lm(data = lm_data_camera_1, form.lm = Linear_model_camera_6, m=5)
#===========================End of Linear Mode Camera Accessory ===================#

#HomeAudio

weekly_grouped_homeaudio <- group_and_merge_all(cleaned_data, "HomeAudio")

##Building Linear Model for Camera Accessory#################################
#===========================================================================#
lm_data_homeaudio <- dplyr::select(
  weekly_grouped_homeaudio,
  price_tag,
  deliverycdays,
  prepaid_percentage,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv)

#Linear_model_camera<-lm(gmv~.,lm_data_homeaudio)
#summary(Linear_model_camera)


levels(lm_data_homeaudio$price_tag)
colSums(is.na(lm_data_homeaudio))

colnames(lm_data_homeaudio)

dummy_2 <- data.frame(model.matrix( ~price_tag, data = lm_data_homeaudio))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
lm_data_homeaudio <- cbind(lm_data_homeaudio[,-1], dummy_2)
lm_data_homeaudio_1 <- lm_data_homeaudio

colnames(lm_data_homeaudio_1)
str(lm_data_homeaudio_1)
lm_data_homeaudio_1[,c(1:18,20:21)] <- scale(lm_data_homeaudio_1[ ,c(1:18,20:21)])

colnames(lm_data_homeaudio_1)
str(lm_data_homeaudio_1)
summary(lm_data_homeaudio_1)

#Model Building
Linear_model_homeaudio<-lm(gmv~.,lm_data_homeaudio_1)
alias(Linear_model_homeaudio)
summary(Linear_model_homeaudio)



Linear_model_homeaudio_2<-stepAIC(Linear_model_homeaudio,direction = "both")
summary(Linear_model_homeaudio_2)
options(scipen=999)

Linear_model_homeaudio_3 <- lm(formula = gmv ~ prepaid_percentage + promotional_offer + product_procurement_sla + 
                                 is_special_week + SEM_Adstock + price_tagMass_Product + price_tagAspiring_Product, 
                               data = lm_data_homeaudio_1)

summary(Linear_model_homeaudio_3)
vif(Linear_model_homeaudio_3)

#Removing content prepaid_percentage which no significance 
Linear_model_homeaudio_4 <- lm(formula = gmv ~ promotional_offer + product_procurement_sla + 
                                 is_special_week + SEM_Adstock + price_tagMass_Product + price_tagAspiring_Product, 
                               data = lm_data_homeaudio_1)

summary(Linear_model_homeaudio_4)
vif(Linear_model_homeaudio_4)

#Removal of price_tagAspiring_Product and price_tagAspiring_Product due to insignificance value
Linear_model_homeaudio_5 <-  lm(formula = gmv ~ promotional_offer + 
                                  is_special_week + SEM_Adstock + price_tagMass_Product , 
                                data = lm_data_homeaudio_1)
summary(Linear_model_homeaudio_5)
vif(Linear_model_homeaudio_5)




cv.lm(data = lm_data_homeaudio_1, form.lm = Linear_model_homeaudio_5, m=5)
##################################End of Home Audio Linear Model #########################



#GamingAccessory  Model

weekly_grouped_gaming <- group_and_merge_all(cleaned_data, "GamingAccessory")

##Building Linear Model for Gaming Accessory#################################
#===========================================================================#

lm_data_gaming <- dplyr::select(
  weekly_grouped_gaming,
  price_tag,
  deliverycdays,
  prepaid_percentage,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv)


levels(lm_data_gaming$price_tag)
colSums(is.na(lm_data_gaming))

colnames(lm_data_gaming)

dummy_2 <- data.frame(model.matrix( ~price_tag, data = lm_data_gaming))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
lm_data_gaming <- cbind(lm_data_gaming[,-1], dummy_2)
lm_data_gaming_1 <- lm_data_gaming

colnames(lm_data_gaming_1)

lm_data_gaming_1[, c(1:18,20:21)] <- scale(lm_data_gaming_1[, c(1:18,20:21)])

colnames(lm_data_gaming_1)
str(lm_data_gaming_1)
summary(lm_data_gaming_1)

#Model Building
Linear_model_gaming<-lm(gmv~.,lm_data_gaming_1)
alias(Linear_model_gaming)
summary(Linear_model_gaming)



Linear_model_gaming_2<-stepAIC(Linear_model_gaming,direction = "both")
summary(Linear_model_gaming_2)
options(scipen=999)

Linear_model_gaming_3 <- lm(formula = gmv ~ deliverycdays + prepaid_percentage + promotional_offer + 
                              sla + product_procurement_sla + is_special_week + nps_score + 
                              Total_Investment_Adstock + Digital_Adstock + Sponsorship_Adstock + 
                              Content_Marketing_Adstock + Online_marketing_Adstock + Radio_Adstock + 
                              Other_Adstock + price_tagAspiring_Product + price_tagMass_Product, 
                            data = lm_data_gaming_1)

summary(Linear_model_gaming_3)
vif(Linear_model_gaming_3)

#Removing content Radio Adstock which no significance and high vif value
Linear_model_gaming_4 <- lm(formula = gmv ~ deliverycdays + prepaid_percentage + promotional_offer + 
                              sla + product_procurement_sla + is_special_week + nps_score + 
                              Total_Investment_Adstock + Digital_Adstock + Sponsorship_Adstock + 
                              Content_Marketing_Adstock + Online_marketing_Adstock +  
                              Other_Adstock + price_tagAspiring_Product + price_tagMass_Product, 
                            data = lm_data_gaming_1)

summary(Linear_model_gaming_4)
vif(Linear_model_gaming_4)

#Removal of prepaid_percentage due to insignificance value
Linear_model_gaming_5 <- lm(formula = gmv ~ deliverycdays + promotional_offer + 
                              sla + product_procurement_sla + is_special_week + nps_score + 
                              Total_Investment_Adstock + Digital_Adstock + Sponsorship_Adstock + 
                              Content_Marketing_Adstock + Online_marketing_Adstock +  
                              Other_Adstock + price_tagAspiring_Product + price_tagMass_Product, 
                            data = lm_data_gaming_1)
summary(Linear_model_gaming_5)
vif(Linear_model_gaming_5)

#Removal of Affiliates_Adstock due to insignificance and high vif value
Linear_model_gaming_6 <- lm(formula = gmv ~ deliverycdays +
                               is_special_week + nps_score +
                              price_tagAspiring_Product + price_tagMass_Product, 
                            data = lm_data_gaming_1)

summary(Linear_model_gaming_6)
vif(Linear_model_gaming_6)



cv.lm(data = lm_data_gaming_1, form.lm = Linear_model_gaming_6, m=5)

mtable("Camera Linear Model"=Linear_model_camera_6, 
       "Home Audio Linear Model"=Linear_model_homeaudio_5, 
       "Gaming  Linear Model"=Linear_model_gaming_6, coef.style="horizontal",
       summary.stats = c("R-squared","sigma","F","adj. R-squared"))


#=================== End Of Linear Model ========================================#

#Multiplicative Model for CamerAccessory

#camera multiplicative model
#===========================

ml_data_camera <- dplyr::select(
  weekly_grouped_camera,
  price_tag,
  deliverycdays,
  deliverybdays,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv)


#View(head(ml_data_camera))
data_CameraAccessory_Multi<-ml_data_camera
colnames(ml_data_camera)
str(data_CameraAccessory_Multi)
summary(data_CameraAccessory_Multi)

#data_CameraAccessory_Multi<-na.omit(data_CameraAccessory_Multi)


levels(factor(data_CameraAccessory_Multi$price_tag))
#data_CameraAccessory_Multi$price_tag<-as.integer(data_CameraAccessory_Multi$price_tag)

dummy_2 <- data.frame(model.matrix( ~price_tag, data = data_CameraAccessory_Multi))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
data_CameraAccessory_Multi <- cbind(data_CameraAccessory_Multi[,-1], dummy_2)
colnames(data_CameraAccessory_Multi)
data_CameraAccessory_Multi_1 <- data_CameraAccessory_Multi

data_CameraAccessory_Multi_1[data_CameraAccessory_Multi_1==0]<-0.1
summary(data_CameraAccessory_Multi)

#data_CameraAccessory_Multi$price_tag<-as.numeric(data_CameraAccessory_Multi$price_tag)
##Taking log of data
data_CameraAccessory_Multi_1<-log(data_CameraAccessory_Multi_1)
summary(data_CameraAccessory_Multi_1)
sapply(data_CameraAccessory_Multi_1, function(x) sum(is.nan(x))) 
sapply(data_CameraAccessory_Multi_1, function(x) sum(is.na(x)))
sapply(data_CameraAccessory_Multi_1, function(x) sum(is.infinite(x)))

#Model Building
#Camera data
Multiplicative_model_camera_1<-lm(gmv~.,data_CameraAccessory_Multi_1)
summary(Multiplicative_model_camera_1)
Multiplicative_model_camera_2<-stepAIC(Multiplicative_model_camera_1,direction = "both")
summary(Multiplicative_model_camera_2)
vif(Multiplicative_model_camera_2)

Multiplicative_model_camera_3 <- lm(formula = gmv ~ sla + product_procurement_sla + is_special_week + 
                                      nps_score + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                                      Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                                      Radio_Adstock + price_tagMass_Product, data = data_CameraAccessory_Multi_1)
summary(Multiplicative_model_camera_3)
vif(Multiplicative_model_camera_3)


#Removing is_special_week
Multiplicative_model_camera_4 <- lm(formula = gmv ~ sla + product_procurement_sla +  
                                      nps_score + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                                      Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                                      Radio_Adstock + price_tagMass_Product, data = data_CameraAccessory_Multi_1)
summary(Multiplicative_model_camera_4)
vif(Multiplicative_model_camera_4)



#Removing product_procurement_sla
Multiplicative_model_camera_5 <- lm(formula = gmv ~ sla +   
                                      nps_score + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                                      Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                                      Radio_Adstock + price_tagMass_Product, data = data_CameraAccessory_Multi_1)
summary(Multiplicative_model_camera_5)
vif(Multiplicative_model_camera_5)


#Removing Sponsorship_Adstock
Multiplicative_model_camera_6 <- lm(formula = gmv ~ sla +   
                                      nps_score + Digital_Adstock +  Content_Marketing_Adstock + 
                                      Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                                      Radio_Adstock, data = data_CameraAccessory_Multi_1)
summary(Multiplicative_model_camera_6)
vif(Multiplicative_model_camera_6)

cv.lm(data = data_CameraAccessory_Multi_1, form.lm = Multiplicative_model_camera_6, m=5)


#==================MUltiplicative for HomeAudio Accessory ========================= #
#"GamingAccessory","HomeAudio"
#Home Audio 
ml_data_homeaudio <- dplyr::select(
  weekly_grouped_homeaudio,
  price_tag,
  deliverycdays,
  deliverybdays,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv)


#View(head(ml_data_homeaudio))
data_HomeAudio_Multi<-ml_data_homeaudio
colnames(ml_data_homeaudio)
str(data_HomeAudio_Multi)
summary(data_HomeAudio_Multi)

#data_CameraAccessory_Multi<-na.omit(data_CameraAccessory_Multi)


levels(factor(data_HomeAudio_Multi$price_tag))
#data_CameraAccessory_Multi$price_tag<-as.integer(data_CameraAccessory_Multi$price_tag)

dummy_2 <- data.frame(model.matrix( ~price_tag, data = data_HomeAudio_Multi))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
data_HomeAudio_Multi <- cbind(data_HomeAudio_Multi[,-1], dummy_2)
colnames(data_HomeAudio_Multi)
sapply(data_HomeAudio_Multi, function(x) sum(is.nan(x))) 
sapply(data_HomeAudio_Multi, function(x) sum(is.na(x)))
sapply(data_HomeAudio_Multi, function(x) sum(is.infinite(x)))
data_HomeAudio_Multi_1 <- data_HomeAudio_Multi


data_HomeAudio_Multi_1[data_HomeAudio_Multi_1==0]<-0.1

data_HomeAudio_Multi_1$deliverybdays[which(data_HomeAudio_Multi_1$deliverybdays==0.00)]<-0.1

summary(data_HomeAudio_Multi_1)
sapply(data_HomeAudio_Multi_1, function(x) sum(is.nan(x))) 
sapply(data_HomeAudio_Multi_1, function(x) sum(is.na(x)))
sapply(data_HomeAudio_Multi_1, function(x) sum(is.infinite(x)))
summary(data_HomeAudio_Multi_1)


#data_CameraAccessory_Multi$price_tag<-as.numeric(data_CameraAccessory_Multi$price_tag)
##Taking log of data
data_HomeAudio_Multi_2<-log(data_HomeAudio_Multi_1)
summary(data_HomeAudio_Multi_2)
sapply(data_HomeAudio_Multi_2, function(x) sum(is.nan(x))) 
sapply(data_HomeAudio_Multi_2, function(x) sum(is.na(x)))
sapply(data_HomeAudio_Multi_2, function(x) sum(is.infinite(x)))

#Model Building
#HomeAudi data
Multiplicative_model_homeaudio_1<-lm(gmv~.,data_HomeAudio_Multi_2)
summary(Multiplicative_model_homeaudio_1)
Multiplicative_model_homeaudio_2<-stepAIC(Multiplicative_model_homeaudio_1,direction = "both")
summary(Multiplicative_model_homeaudio_2)
vif(Multiplicative_model_homeaudio_2)

Multiplicative_model_homeaudio_3 <- lm(formula = gmv ~ deliverybdays + promotional_offer + sla + 
                                         product_procurement_sla + is_special_week + nps_score + Total_Investment_Adstock + 
                                         TV_Adstock + Digital_Adstock + Content_Marketing_Adstock + 
                                         Online_marketing_Adstock + SEM_Adstock + price_tagMass_Product + 
                                         price_tagAspiring_Product, data = data_HomeAudio_Multi_2)
summary(Multiplicative_model_homeaudio_3)
vif(Multiplicative_model_homeaudio_3)


#Removing deliverybdays
Multiplicative_model_homeaudio_4 <- lm(formula = gmv ~  promotional_offer + sla + 
                                         product_procurement_sla + is_special_week + nps_score + Total_Investment_Adstock + 
                                         TV_Adstock + Digital_Adstock + Content_Marketing_Adstock + 
                                         Online_marketing_Adstock + SEM_Adstock + price_tagMass_Product + 
                                         price_tagAspiring_Product, data = data_HomeAudio_Multi_2)
summary(Multiplicative_model_homeaudio_4)
vif(Multiplicative_model_homeaudio_4)



#Removing product_procurement_sla
Multiplicative_model_homeaudio_5 <- lm(formula = gmv ~  promotional_offer + sla + 
                                         is_special_week + nps_score + Total_Investment_Adstock + 
                                         TV_Adstock + Digital_Adstock + Content_Marketing_Adstock + 
                                         Online_marketing_Adstock + SEM_Adstock + price_tagMass_Product + 
                                         price_tagAspiring_Product, data = data_HomeAudio_Multi_2)
summary(Multiplicative_model_homeaudio_5)
vif(Multiplicative_model_homeaudio_5)


#Removing Sponsorship_Adstock
Multiplicative_model_homeaudio_6 <- lm(formula = gmv ~  promotional_offer + 
                                            price_tagMass_Product + nps_score +
                                         price_tagAspiring_Product, data = data_HomeAudio_Multi_2)
summary(Multiplicative_model_homeaudio_6)
vif(Multiplicative_model_homeaudio_6)

cv.lm(data = data_HomeAudio_Multi_2, form.lm = Multiplicative_model_homeaudio_6, m=5)

#===== End of Multiplicative Home Audion Model =====================================#

#==================MUltiplicative for Gaming Accessory ========================= #
#"GamingAccessory"
weekly_grouped_gaming <- group_and_merge_all(cleaned_data, "GamingAccessory")
ml_data_gaming <- dplyr::select(
  weekly_grouped_gaming,
  price_tag,
  deliverycdays,
  deliverybdays,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv)


#View(head(ml_data_gaming))
data_Gaming_Multi<-ml_data_gaming
colnames(ml_data_gaming)
str(data_Gaming_Multi)
summary(data_Gaming_Multi)

#data_CameraAccessory_Multi<-na.omit(data_CameraAccessory_Multi)


levels(factor(data_Gaming_Multi$price_tag))

dummy_2 <- data.frame(model.matrix( ~price_tag, data = data_Gaming_Multi))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
data_Gaming_Multi <- cbind(data_Gaming_Multi[,-1], dummy_2)
colnames(data_Gaming_Multi)
data_Gaming_Multi_1 <- data_Gaming_Multi

data_Gaming_Multi_1[data_Gaming_Multi_1==0]<-0.1
summary(data_Gaming_Multi_1)

#data_CameraAccessory_Multi$price_tag<-as.numeric(data_CameraAccessory_Multi$price_tag)
##Taking log of data
data_Gaming_Multi_2<-log(data_Gaming_Multi_1)
summary(data_Gaming_Multi_2)
sapply(data_Gaming_Multi_2, function(x) sum(is.nan(x))) 
sapply(data_Gaming_Multi_2, function(x) sum(is.na(x)))
sapply(data_Gaming_Multi_2, function(x) sum(is.infinite(x)))

#Model Building
#Gaming data
Multiplicative_model_gaming_1<-lm(gmv~.,data_Gaming_Multi_2)
summary(Multiplicative_model_gaming_1)

Multiplicative_model_gaming_2<-stepAIC(Multiplicative_model_gaming_1,direction = "both")
summary(Multiplicative_model_gaming_2)
vif(Multiplicative_model_gaming_2)

Multiplicative_model_gaming_3 <- lm(formula = gmv ~ promotional_offer + sla + product_procurement_sla + 
                                      is_special_week + nps_score + Total_Investment_Adstock + 
                                      TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                                      Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                                      Radio_Adstock + price_tagAspiring_Product + price_tagPremium_Product, 
                                    data = data_Gaming_Multi_2)
summary(Multiplicative_model_gaming_3)
vif(Multiplicative_model_gaming_3)


#Removing product_procurement_sla
Multiplicative_model_gaming_4 <- lm(formula = gmv ~  sla +  
                                      is_special_week + nps_score + Total_Investment_Adstock + 
                                      TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                                      Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                                      price_tagAspiring_Product + price_tagPremium_Product, 
                                    data = data_Gaming_Multi_2)
summary(Multiplicative_model_gaming_4)
vif(Multiplicative_model_gaming_4)



#Removing sla, is_special_week
Multiplicative_model_gaming_5 <- lm(formula = gmv ~  sla + nps_score + Total_Investment_Adstock + 
                                      TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                                      Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                                      price_tagAspiring_Product + price_tagPremium_Product, 
                                    data = data_Gaming_Multi_2)
summary(Multiplicative_model_gaming_5)
vif(Multiplicative_model_gaming_5)


#Removing sla
Multiplicative_model_gaming_6 <- lm(formula = gmv ~   nps_score + Total_Investment_Adstock + 
                                      TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Content_Marketing_Adstock + 
                                      Online_marketing_Adstock + Affiliates_Adstock + SEM_Adstock + 
                                      price_tagAspiring_Product + price_tagPremium_Product, 
                                    data = data_Gaming_Multi_2)
summary(Multiplicative_model_gaming_6)
vif(Multiplicative_model_gaming_6)

cv.lm(data = data_Gaming_Multi_2, form.lm = Multiplicative_model_gaming_6, m=5)


mtable("Camera Multiplicative Model"=Multiplicative_model_camera_6, 
       "Home Audio Multiplicative Model"=Multiplicative_model_homeaudio_6, 
       "Gaming  Multiplicative Model"=Multiplicative_model_gaming_6, coef.style="horizontal",
       summary.stats = c("R-squared","sigma","F","adj. R-squared"))


#### Finding elasticity from Multiplicative models
# Plotting the elasticity for Camera


elasticity_camera_1 <- function(var){
  elasticity_camera_Multi <- as.numeric(Multiplicative_model_camera_6$coefficients[var]*mean(data_CameraAccessory_Multi_1[,var])/mean(data_CameraAccessory_Multi_1$gmv))
  return(elasticity_camera_Multi)
  
} 

elasticity_camera_Multi_var_list <- list()

for(i in 2:length(Multiplicative_model_camera_6$coefficients)){
  elasticity_camera_Multi_var_list[i-1] <- elasticity_camera_1(names(Multiplicative_model_camera_6$coefficients)[i])
  
}


elasticity_camera_Multi.outputs <- data.frame(names(Multiplicative_model_camera_6$coefficients[2:length(Multiplicative_model_camera_6$coefficients)]))
elasticity_camera_Multi.outputs <- cbind(elasticity_camera_Multi.outputs,do.call(rbind.data.frame, elasticity_camera_Multi_var_list))
colnames(elasticity_camera_Multi.outputs) <- c("Variable","Elasticity")

elasticity_camera_Multi.outputs$Direction <- ifelse(elasticity_camera_Multi.outputs$Elasticity > 0, "Positive", "Negative")

Multi_camera_plot <- ggplot(elasticity_camera_Multi.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-1),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("CameraAccessory - Multiplicative Model") +xlab("Variables")  


####elasticity Home_Audio ##############################################
elasticity_HomeAudio_1 <- function(var){
  elasticity_HomeAudio_Multi <- as.numeric(Multiplicative_model_homeaudio_6$coefficients[var]*mean(data_HomeAudio_Multi_2[,var])/mean(data_HomeAudio_Multi_2$gmv))
  return(elasticity_HomeAudio_Multi)
  
} 

elasticity_HomeAudio_Multi_var_list <- list()

for(i in 2:length(Multiplicative_model_homeaudio_6$coefficients)){
  elasticity_HomeAudio_Multi_var_list[i-1] <- elasticity_HomeAudio_1(names(Multiplicative_model_homeaudio_6$coefficients)[i])
  
}

elasticity_HomeAudio_Multi.outputs <- data.frame(names(Multiplicative_model_homeaudio_6$coefficients[2:length(Multiplicative_model_homeaudio_6$coefficients)]))
elasticity_HomeAudio_Multi.outputs <- cbind(elasticity_HomeAudio_Multi.outputs,do.call(rbind.data.frame, elasticity_HomeAudio_Multi_var_list))
colnames(elasticity_HomeAudio_Multi.outputs) <- c("Variable","Elasticity")

elasticity_HomeAudio_Multi.outputs$Direction <- ifelse(elasticity_HomeAudio_Multi.outputs$Elasticity > 0, "Positive", "Negative")

Multi_HomeAudio_plot <- ggplot(elasticity_HomeAudio_Multi.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-1),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Multiplicative Model") +xlab("Variables")


###########Elasticity Gaming Accessory Plot###############################

elasticity_Gaming_1 <- function(var){
  elasticity_Gaming_Multi <- as.numeric(Multiplicative_model_gaming_6$coefficients[var]*mean(data_Gaming_Multi_2[,var])/mean(data_Gaming_Multi_2$gmv))
  return(elasticity_Gaming_Multi)
  
} 

elasticity_Gaming_Multi_var_list <- list()

for(i in 2:length(Multiplicative_model_gaming_6$coefficients)){
  elasticity_Gaming_Multi_var_list[i-1] <- elasticity_Gaming_1(names(Multiplicative_model_gaming_6$coefficients)[i])
  
}

elasticity_Gaming_Multi.outputs <- data.frame(names(Multiplicative_model_gaming_6$coefficients[2:length(Multiplicative_model_gaming_6$coefficients)]))
elasticity_Gaming_Multi.outputs <- cbind(elasticity_Gaming_Multi.outputs,do.call(rbind.data.frame, elasticity_Gaming_Multi_var_list))
colnames(elasticity_Gaming_Multi.outputs) <- c("Variable","Elasticity")

elasticity_Gaming_Multi.outputs$Direction <- ifelse(elasticity_Gaming_Multi.outputs$Elasticity > 0, "Positive", "Negative")

Multi_Gaming_plot <- ggplot(elasticity_Gaming_Multi.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-1),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Gaming - Multiplicative Model") +xlab("Variables")

plot_grid(Multi_Gaming_plot, Multi_HomeAudio_plot, Multi_camera_plot)



######################### End of Multiplicative Gaming ###############################
#=================Kyock Model======================================================== 
#======================================================================================


#camera kyock model

weekly_grouped_camera <- group_and_merge_all(cleaned_data, "CameraAccessory");

##Building Linear Model for Camera Accessory#################################
#===========================================================================#
Kyock_data_camera <- dplyr::select(
  weekly_grouped_camera,
  price_tag,
  deliverycdays,
  prepaid_percentage,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv,
  "gmv-1")

#Kyock_model_camera<-lm(gmv~.,Kyock_data_camera)
#summary(Kyock_model_camera)


levels(Kyock_data_camera$price_tag)
colSums(is.na(Kyock_data_camera))

colnames(Kyock_data_camera)

dummy_2 <- data.frame(model.matrix( ~price_tag, data = Kyock_data_camera))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
Kyock_data_camera <- cbind(Kyock_data_camera[,-1], dummy_2)
Kyock_data_camera_1 <- Kyock_data_camera

colnames(Kyock_data_camera_1)

Kyock_data_camera_1[, c(1:18,21:22)] <- scale(Kyock_data_camera_1[, c(1:18,21:22)])

colnames(Kyock_data_camera_1)
str(Kyock_data_camera_1)
summary(Kyock_data_camera_1)

#set.seed(100) 
#Model Building
Kyock_model_camera<-lm(gmv~.,Kyock_data_camera_1)
alias(Kyock_model_camera)
summary(Kyock_model_camera)



Kyock_model_camera_2<-stepAIC(Kyock_model_camera,direction = "both")
summary(Kyock_model_camera_2)
options(scipen=999)

Kyock_model_camera_3 <- lm(formula = gmv ~  deliverycdays + promotional_offer + sla + 
          product_procurement_sla + is_special_week + nps_score +  Digital_Adstock +  
          Online_marketing_Adstock +SEM_Adstock +  price_tagAspiring_Product, 
          data = Kyock_data_camera_1)
summary(Kyock_model_camera_3)
vif(Kyock_model_camera_3)

Kyock_model_camera_4 <- lm(formula = gmv ~  deliverycdays + promotional_offer + sla + 
                             product_procurement_sla + is_special_week + nps_score +  Digital_Adstock +  
                             SEM_Adstock + price_tagAspiring_Product, 
                           data = Kyock_data_camera_1)
summary(Kyock_model_camera_4)
vif(Kyock_model_camera_4)

cv.lm(data = Kyock_data_camera_1, form.lm = Kyock_model_camera_4, m=5)
#===========================End of Kyock Model Camera Accessory ===================#

#HomeAudio Kyock data prep=================#

weekly_grouped_homeaudio <- group_and_merge_all(cleaned_data, "HomeAudio");

##Building Kyock Model for Home Audio Accessory#################################
#===========================================================================#

Kyock_data_homeaudio <- dplyr::select(
  weekly_grouped_homeaudio,
  price_tag,
  deliverycdays,
  prepaid_percentage,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv,
  "gmv-1")



levels(Kyock_data_homeaudio$price_tag)
colSums(is.na(Kyock_data_homeaudio))

colnames(Kyock_data_homeaudio)

dummy_2 <- data.frame(model.matrix( ~price_tag, data = Kyock_data_homeaudio))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
Kyock_data_homeaudio <- cbind(Kyock_data_homeaudio[,-1], dummy_2)
Kyock_data_homeaudio_1 <- Kyock_data_homeaudio

colnames(Kyock_data_homeaudio_1)
str(Kyock_data_homeaudio_1)
Kyock_data_homeaudio_1[,c(1:18,21:22)] <- scale(Kyock_data_homeaudio_1[ ,c(1:18,21:22)])

colnames(Kyock_data_homeaudio_1)
str(Kyock_data_homeaudio_1)
summary(Kyock_data_homeaudio_1)

#Model Building
Kyock_model_homeaudio<-lm(gmv~.,Kyock_data_homeaudio_1)
alias(Kyock_model_homeaudio)
summary(Kyock_model_homeaudio)



Kyock_model_homeaudio_2<-stepAIC(Kyock_model_homeaudio,direction = "both")
summary(Kyock_model_homeaudio_2)
options(scipen=999)


Kyock_model_homeaudio_3 <- lm(formula = gmv ~ prepaid_percentage + promotional_offer + product_procurement_sla + 
                                is_special_week + SEM_Adstock + `gmv-1` + price_tagAspiring_Product + 
                                price_tagPremium_Product, data = Kyock_data_homeaudio_1)
summary(Kyock_model_homeaudio_3)
vif(Kyock_model_homeaudio_3)

Kyock_model_homeaudio_4 <- lm(formula = gmv ~ promotional_offer + product_procurement_sla + 
                                is_special_week + SEM_Adstock + `gmv-1` + price_tagAspiring_Product + 
                                price_tagPremium_Product, data = Kyock_data_homeaudio_1)

summary(Kyock_model_homeaudio_4)
vif(Kyock_model_homeaudio_4)

#Removing content `gmv-1`
Kyock_model_homeaudio_5 <- lm(formula = gmv ~ promotional_offer + product_procurement_sla + 
                                is_special_week + SEM_Adstock +  price_tagAspiring_Product + 
                                price_tagPremium_Product, data = Kyock_data_homeaudio_1)


summary(Kyock_model_homeaudio_5)
vif(Kyock_model_homeaudio_5)

#Removal of product_procurement_sla due to insignificance 
Kyock_model_homeaudio_6 <- lm(formula = gmv ~ promotional_offer +  
                                is_special_week + SEM_Adstock +  price_tagAspiring_Product + 
                                price_tagPremium_Product, data = Kyock_data_homeaudio_1)

summary(Kyock_model_homeaudio_6)
vif(Kyock_model_homeaudio_6)


cv.lm(data = Kyock_data_homeaudio_1, form.lm = Kyock_model_homeaudio_6, m=5)
##################################End of Home Audio Linear Model #########################



#GamingAccessory  Model

weekly_grouped_gaming <- group_and_merge_all(cleaned_data, "GamingAccessory")

##Building Linear Model for Gaming Accessory#################################
#===========================================================================#

Kyock_data_gaming <- dplyr::select(
  weekly_grouped_gaming,
  price_tag,
  deliverycdays,
  prepaid_percentage,
  promotional_offer,
  sla,
  product_procurement_sla,
  deliverybdays,
  is_special_week,
  nps_score,
  Total_Investment_Adstock,
  TV_Adstock,
  Digital_Adstock,
  Sponsorship_Adstock,
  Content_Marketing_Adstock,
  Online_marketing_Adstock,
  Affiliates_Adstock,
  SEM_Adstock,
  Radio_Adstock,
  Other_Adstock,
  gmv,
  "gmv-1")


levels(Kyock_data_gaming$price_tag)
colSums(is.na(Kyock_data_gaming))

colnames(Kyock_data_gaming)

dummy_2 <- data.frame(model.matrix( ~price_tag, data = Kyock_data_gaming))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "price_tag". 
dummy_2 <- dummy_2[,-1]

#Combine the dummy variables to the main data set, after removing the original categorical "price_tag" column
Kyock_data_gaming <- cbind(Kyock_data_gaming[,-1], dummy_2)
Kyock_data_gaming_1 <- Kyock_data_gaming

colnames(Kyock_data_gaming_1)

Kyock_data_gaming_1[, c(1:18,21:22)] <- scale(Kyock_data_gaming_1[, c(1:18,21:22)])

colnames(Kyock_data_gaming_1)
str(Kyock_data_gaming_1)
summary(Kyock_data_gaming_1)

#Model Building
Kyock_model_gaming<-lm(gmv~.,Kyock_data_gaming_1)
alias(Kyock_model_gaming)
summary(Kyock_model_gaming)



Kyock_model_gaming_2<-stepAIC(Kyock_model_gaming,direction = "both")
summary(Kyock_model_gaming_2)
options(scipen=999)

Kyock_model_gaming_3 <- lm(formula = gmv ~ deliverycdays + promotional_offer + sla + 
                             product_procurement_sla + is_special_week + nps_score + Total_Investment_Adstock + 
                             TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Online_marketing_Adstock + 
                             Radio_Adstock + Other_Adstock + `gmv-1` + price_tagAspiring_Product + 
                             price_tagMass_Product, data = Kyock_data_gaming_1)

summary(Kyock_model_gaming_3)
vif(Kyock_model_gaming_3)

#Removing content Radio Adstock, promtional_offer and product_procurement_sla  which no significance and high vif value
Kyock_model_gaming_4 <- lm(formula = gmv ~ deliverycdays +  sla + is_special_week +
                             nps_score + Total_Investment_Adstock + 
                             TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Online_marketing_Adstock + 
                             Other_Adstock + `gmv-1` + price_tagAspiring_Product + 
                             price_tagMass_Product, data = Kyock_data_gaming_1)

summary(Kyock_model_gaming_4)
vif(Kyock_model_gaming_4)

#Removal of Sponsorhsip_Adstock , Online_marketing_Adstock due to insignificance value
Kyock_model_gaming_5 <- lm(formula = gmv ~ deliverycdays +  sla + is_special_week +
                             nps_score + Total_Investment_Adstock + 
                             TV_Adstock + Digital_Adstock + 
                             Other_Adstock + `gmv-1` + price_tagMass_Product + 
                             price_tagAspiring_Product, data = Kyock_data_gaming_1)
summary(Kyock_model_gaming_5)
vif(Kyock_model_gaming_5)

#Removal of Other_Adstock and Digital_Adtsock due to insignificance and high vif value
Kyock_model_gaming_6 <- lm(formula = gmv ~ deliverycdays +  sla + is_special_week +
                             nps_score + `gmv-1` + price_tagMass_Product + 
                             price_tagAspiring_Product, data = Kyock_data_gaming_1)

summary(Kyock_model_gaming_6)
vif(Kyock_model_gaming_6)



cv.lm(data = Kyock_data_gaming_1, form.lm = Kyock_model_gaming_6, m=5)

mtable("Camera Kyock Model"=Kyock_model_camera_4, 
       "Home Audio Kyock Model"=Kyock_model_homeaudio_6, 
       "Gaming  Kyock  Model"=Kyock_model_gaming_6, coef.style="horizontal",
       summary.stats = c("R-squared","sigma","F","adj. R-squared"))
#===========================================================================================#

#### Finding elasticity from Kyock models
# Plotting the elasticity for Camera


elasticity_camera_1 <- function(var){
  elasticity_camera_kyock <- as.numeric(Kyock_model_camera_4$coefficients[var]*mean(Kyock_data_camera_1[,var])/mean(Kyock_data_camera_1$gmv))
  return(elasticity_camera_kyock)
  
} 

elasticity_camera_kyock_var_list <- list()

for(i in 2:length(Kyock_model_camera_4$coefficients)){
  elasticity_camera_kyock_var_list[i-1] <- elasticity_camera_1(names(Kyock_model_camera_4$coefficients)[i])
  
}


elasticity_camera_kyock.outputs <- data.frame(names(Kyock_model_camera_4$coefficients[2:length(Kyock_model_camera_4$coefficients)]))
elasticity_camera_kyock.outputs <- cbind(elasticity_camera_kyock.outputs,do.call(rbind.data.frame, elasticity_camera_kyock.outputs))
colnames(elasticity_camera_kyock.outputs) <- c("Variable","Elasticity")

elasticity_camera_kyock.outputs$Direction <- ifelse(elasticity_camera_kyock.outputs$Elasticity > 0, "Positive", "Negative")

Kyock_camera_plot <- ggplot(elasticity_camera_kyock.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-1),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("CameraAccessory - Kyock Model") +xlab("Variables")  


####elasticity Home_Audio ##############################################
elasticity_HomeAudio_1 <- function(var){
  elasticity_HomeAudio_kyock <- as.numeric(Kyock_model_homeaudio_6$coefficients[var]*mean(Kyock_data_homeaudio_1[,var])/mean(Kyock_data_homeaudio_1$gmv))
  return(elasticity_HomeAudio_kyock)
  
} 

elasticity_HomeAudio_kyock_var_list <- list()

for(i in 2:length(Kyock_model_homeaudio_6$coefficients)){
  elasticity_HomeAudio_kyock_var_list[i-1] <- elasticity_HomeAudio_1(names(Kyock_model_homeaudio_6$coefficients)[i])
  
}

elasticity_HomeAudio_kyock.outputs <- data.frame(names(Kyock_model_homeaudio_6$coefficients[2:length(Kyock_model_homeaudio_6$coefficients)]))
elasticity_HomeAudio_kyock.outputs <- cbind(elasticity_HomeAudio_kyock.outputs,do.call(rbind.data.frame, elasticity_HomeAudio_kyock.outputs))
colnames(elasticity_HomeAudio_kyock.outputs) <- c("Variable","Elasticity")

elasticity_HomeAudio_kyock.outputs$Direction <- ifelse(elasticity_HomeAudio_kyock.outputs$Elasticity > 0, "Positive", "Negative")

Kyock_HomeAudio_plot <- ggplot(elasticity_HomeAudio_kyock.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-1),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Kyock Model") +xlab("Variables")


###########Elasticity Gaming Accessory Plot###############################

elasticity_Gaming_1 <- function(var){
  elasticity_Gaming_kyock <- as.numeric(Kyock_model_gaming_6$coefficients[var]*mean(Kyock_data_gaming_1[,var])/mean(Kyock_data_gaming_1$gmv))
  return(elasticity_Gaming_kyock)
  
} 

elasticity_Gaming_kyock_var_list <- list()

for(i in 2:length(Kyock_model_gaming_6$coefficients)){
  elasticity_Gaming_kyock_var_list[i-1] <- elasticity_Gaming_1(names(Kyock_model_gaming_6$coefficients)[i])
  
}

elasticity_Gaming_kyock.outputs <- data.frame(names(Kyock_model_gaming_6$coefficients[2:length(Kyock_model_gaming_6$coefficients)]))
elasticity_Gaming_kyock.outputs <- cbind(elasticity_Gaming_kyock.outputs,do.call(rbind.data.frame, elasticity_Gaming_kyock.outputs))
colnames(elasticity_Gaming_kyock.outputs) <- c("Variable","Elasticity")

elasticity_Gaming_kyock.outputs$Direction <- ifelse(elasticity_Gaming_kyock.outputs$Elasticity > 0, "Positive", "Negative")

Kyock_Gaming_plot <- ggplot(elasticity_Gaming_kyock.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-1),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Gaming - Kyock  Model") +xlab("Variables")

plot_grid(Kyock_Gaming_plot, Kyock_HomeAudio_plot, Kyock_camera_plot)


#=========================End of Kyock Model"=====================================#

#==============================DLag Model ========================================#


GetDLAGDf <- function(data){
  
  data_dlag <-sqldf("
                    select
                    avg(deliverycdays) as deliverycdays
                    , avg(prepaid_percentage)  as prepaid_percentage
                    , avg(promotional_offer) as promotional_offer
                    , avg(sla) as sla
                    , avg(product_procurement_sla) as product_procurement_sla
                    , avg(deliverybdays) as deliverybdays
                    , avg(nps_score) as nps_score
                    , sum(gmv) as gmv
                    , max(is_special_week) is_special_week,
                    max(Total_Investment_Adstock) Total_Investment_Adstock,
                    max(TV_Adstock) TV_Adstock,
                    max(Digital_Adstock) Digital_Adstock,
                    max(Sponsorship_Adstock) Sponsorship_Adstock,
                    max(Content_Marketing_Adstock) Content_Marketing_Adstock,
                    max(Online_marketing_Adstock) Online_marketing_Adstock,
                    max(Affiliates_Adstock) Affiliates_Adstock,
                    max(SEM_Adstock) SEM_Adstock,
                    max(Radio_Adstock) Radio_Adstock,
                    max(Other_Adstock) Other_Adstock
                    from
                    data
                    group by WeekNumber
                    ");
  data_to_lag<-addLag( data_dlag,'deliverycdays');
  data_to_lag<-addLag( data_to_lag,'prepaid_percentage');
  data_to_lag<-addLag( data_to_lag,'promotional_offer');
  data_to_lag<-addLag( data_to_lag,'sla');
  data_to_lag<-addLag( data_to_lag,'product_procurement_sla');
  data_to_lag<-addLag( data_to_lag,'deliverybdays');
  data_to_lag<-addLag( data_to_lag,'is_special_week');
  data_to_lag<-addLag( data_to_lag,'nps_score');
  data_to_lag<-addLag( data_to_lag,'gmv');
  return (data_to_lag);
}

data_dlag_camera <- GetDLAGDf(weekly_grouped_camera)
dlag_camera <- lm(gmv~., data_dlag_camera);

dlag_camera_aic<-stepAIC(dlag_camera,direction = "both")
dlag_camera_aic<-lm(formula = gmv ~ deliverycdays + prepaid_percentage + promotional_offer + 
                      sla + product_procurement_sla + deliverybdays + nps_score + 
                      Total_Investment_Adstock + Digital_Adstock + Sponsorship_Adstock + 
                      Content_Marketing_Adstock + `max(Online_marketing_Adstock)` + 
                      `max(Affiliates_Adstock)` + `max(SEM_Adstock)` + `max(Radio_Adstock)` + 
                      `deliverycdays-1` + `deliverycdays-2` + `deliverycdays-3` + 
                      `prepaid_percentage-1` + `prepaid_percentage-3` + `promotional_offer-1` + 
                      `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-2` + 
                      `product_procurement_sla-3` + `deliverybdays-1` + `deliverybdays-2` + 
                      `deliverybdays-3` + `is_special_week-1` + `is_special_week-2` + 
                      `is_special_week-3` + `nps_score-2` + `nps_score-3` + `gmv-1` + 
                      `gmv-2` + `gmv-3`, data = data_dlag_camera)
summary(dlag_camera_aic)

dlag_camera_aic<-lm(formula = gmv ~ deliverycdays + prepaid_percentage + promotional_offer + 
                      sla + product_procurement_sla + deliverybdays + nps_score + 
                      Total_Investment_Adstock + Digital_Adstock + Sponsorship_Adstock + 
                      Content_Marketing_Adstock + Online_marketing_Adstock + Affiliates_Adstock + 
                      SEM_Adstock + Radio_Adstock + `deliverycdays-1` + `deliverycdays-2` + 
                      `deliverycdays-3` + `prepaid_percentage-1` + `prepaid_percentage-3` + 
                      `promotional_offer-1` + `promotional_offer-3` + `sla-1` + 
                      `sla-2` + `sla-3` + `product_procurement_sla-2` + `product_procurement_sla-3` + 
                      `deliverybdays-1` + `deliverybdays-2` + `deliverybdays-3` + 
                      `is_special_week-1` + `is_special_week-2` + `is_special_week-3` + 
                      `nps_score-2` + `nps_score-3` + `gmv-1` + `gmv-2` + `gmv-3`, 
                    data = data_dlag_camera)

summary(dlag_camera_aic)
dlag_camera_removedvariables<-lm(formula = gmv ~ deliverycdays + prepaid_percentage + promotional_offer + 
                                   sla + product_procurement_sla + deliverybdays + nps_score + 
                                   
                                   `deliverycdays-3` + 
                                   `promotional_offer-3` + 
                                   `sla-3` + `product_procurement_sla-2` + `product_procurement_sla-3` + 
                                   `deliverybdays-2` +  
                                   
                                   `gmv-3`, 
                                 data = data_dlag_camera)
summary(dlag_camera_removedvariables)


weekly_grouped_homeaudio <- group_and_merge_all(cleaned_data, "HomeAudio");
data_dlag_home_audio <- GetDLAGDf(weekly_grouped_homeaudio)
dlag_home_audio <- lm(gmv~., data_dlag_home_audio);

dlag_home_audio_aic<-stepAIC(dlag_home_audio,direction = "both")
summary(dlag_home_audio)

dlag_home_audio_aic <-lm(formula = gmv ~ deliverycdays + prepaid_percentage + promotional_offer + 
                           sla + product_procurement_sla + deliverybdays + nps_score + 
                           is_special_week + Digital_Adstock + Sponsorship_Adstock + 
                           Content_Marketing_Adstock + Online_marketing_Adstock + Affiliates_Adstock + 
                           SEM_Adstock + Radio_Adstock + Other_Adstock + `deliverycdays-1` + 
                           `deliverycdays-2` + `deliverycdays-3` + `prepaid_percentage-1` + 
                           `prepaid_percentage-3` + `promotional_offer-2` + `promotional_offer-3` + 
                           `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                           `product_procurement_sla-2` + `product_procurement_sla-3` + 
                           `deliverybdays-1` + `deliverybdays-2` + `deliverybdays-3` + 
                           `is_special_week-1` + `nps_score-1` + `nps_score-3` + `gmv-1` + 
                           `gmv-2` + `gmv-3`, data = data_dlag_home_audio)



dlag_home_audio_remove <-lm(formula = gmv ~ deliverycdays +  promotional_offer + 
                              product_procurement_sla + deliverybdays + nps_score + 
                              is_special_week + Digital_Adstock +  
                              Online_marketing_Adstock + Affiliates_Adstock + 
                              SEM_Adstock + Radio_Adstock + Other_Adstock + `deliverycdays-1` + 
                              `deliverycdays-2` + `deliverycdays-3` + `prepaid_percentage-1` + 
                              `promotional_offer-2` +  
                              `sla-1` + `sla-3` + `product_procurement_sla-1` +
                              
                              `deliverybdays-1` + `deliverybdays-2` + `deliverybdays-3` + 
                              `gmv-1` + 
                              `gmv-2` + `gmv-3`, data = data_dlag_home_audio)

summary(dlag_home_audio_remove) 


dlag_home_audio_remove <-lm(formula = gmv ~ deliverycdays +  promotional_offer + 
                              product_procurement_sla + deliverybdays + nps_score + 
                              is_special_week + Digital_Adstock +  
                              Online_marketing_Adstock +  
                              SEM_Adstock + Radio_Adstock +  `deliverycdays-1` + 
                              `deliverycdays-2` + `deliverycdays-3` + `prepaid_percentage-1` + 
                              `promotional_offer-2` +  
                              `sla-1` + `sla-3` + `product_procurement_sla-1` +
                              `deliverybdays-1` + `deliverybdays-2` + `deliverybdays-3` + 
                              `gmv-1` +                               `gmv-2` + `gmv-3`, data = data_dlag_home_audio)

summary(dlag_home_audio_remove) 

dlag_home_audio_remove <-lm(formula = gmv ~ deliverycdays +  promotional_offer + 
                              product_procurement_sla + deliverybdays + nps_score + 
                              Digital_Adstock +  
                              Online_marketing_Adstock +  
                              SEM_Adstock + Radio_Adstock +  `deliverycdays-1` + 
                              `deliverycdays-3` + `prepaid_percentage-1` + 
                              `promotional_offer-2` +  
                              `sla-1` + `sla-3` + `product_procurement_sla-1` +
                              `deliverybdays-1` + `deliverybdays-2` + `deliverybdays-3` + 
                              `gmv-1` +                               `gmv-2` + `gmv-3`, data = data_dlag_home_audio)

summary(dlag_home_audio_remove) 


dlag_home_audio_remove <-lm(formula = gmv ~ deliverycdays +  promotional_offer + 
                              product_procurement_sla + deliverybdays + nps_score + 
                              Digital_Adstock +  
                              Online_marketing_Adstock +  
                              SEM_Adstock + Radio_Adstock +  `deliverycdays-1` + 
                              `deliverycdays-3` + `prepaid_percentage-1` + 
                              `promotional_offer-2` +  
                              `sla-1` + `sla-3` + `product_procurement_sla-1` +
                              `deliverybdays-1` + `deliverybdays-2` + `deliverybdays-3` + 
                              `gmv-1` +                               `gmv-2` + `gmv-3`, data = data_dlag_home_audio)

summary(dlag_home_audio_remove) 


weekly_grouped_GamingAccessory <- group_and_merge_all(cleaned_data, "GamingAccessory");
data_dlag_GamingAccessory <- GetDLAGDf(weekly_grouped_GamingAccessory)
dlag_GamingAccessory <- lm(gmv~., data_dlag_GamingAccessory);
summary(dlag_GamingAccessory)
dlag_GamingAccessory_aic<-stepAIC(dlag_GamingAccessory,direction = "both")
summary(dlag_GamingAccessory)
dlag_GamingAccessory_aic <- lm(gmv ~ deliverycdays + prepaid_percentage + promotional_offer + 
                                 sla + deliverybdays + nps_score + is_special_week + Total_Investment_Adstock + 
                                 TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Online_marketing_Adstock + 
                                 Affiliates_Adstock + SEM_Adstock + Radio_Adstock + Other_Adstock + 
                                 `deliverycdays-1` + `deliverycdays-2` + `prepaid_percentage-2` + 
                                 `prepaid_percentage-3` + `promotional_offer-1` + `promotional_offer-2` + 
                                 `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                                 `product_procurement_sla-2` + `deliverybdays-1` + `deliverybdays-2` + 
                                 `deliverybdays-3` + `is_special_week-2` + `is_special_week-3` + 
                                 `nps_score-1` + `nps_score-2` + `nps_score-3` + `gmv-1` + 
                                 `gmv-3`,data_dlag_GamingAccessory)

summary(dlag_GamingAccessory_aic)


dlag_GamingAccessory_remove <- lm(gmv ~ deliverycdays + prepaid_percentage +  
                                    sla + deliverybdays + nps_score + is_special_week + Total_Investment_Adstock + 
                                    TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Online_marketing_Adstock + 
                                    Affiliates_Adstock + SEM_Adstock + Radio_Adstock + Other_Adstock + 
                                    `deliverycdays-1` + `deliverycdays-2` + `prepaid_percentage-2` + 
                                    `prepaid_percentage-3` + `promotional_offer-1` + `promotional_offer-2` + 
                                    `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                                    `product_procurement_sla-2` + `deliverybdays-1` + `deliverybdays-2` + 
                                    `deliverybdays-3` + `is_special_week-2` + `is_special_week-3` + 
                                    `nps_score-1` + `nps_score-2` + `nps_score-3` + `gmv-1` + 
                                    `gmv-3`,data_dlag_GamingAccessory)

summary(dlag_GamingAccessory_remove)


dlag_GamingAccessory_remove <- lm(gmv ~ deliverycdays + prepaid_percentage +  
                                    sla + deliverybdays + nps_score + is_special_week + Total_Investment_Adstock + 
                                    TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Online_marketing_Adstock + 
                                    Affiliates_Adstock + SEM_Adstock + Radio_Adstock + Other_Adstock + 
                                    `deliverycdays-1` + `deliverycdays-2` + `prepaid_percentage-2` + 
                                    `prepaid_percentage-3` + `promotional_offer-1` + `promotional_offer-2` + 
                                    `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                                    `product_procurement_sla-2` + `deliverybdays-1` + `deliverybdays-2` + 
                                    `deliverybdays-3` + `is_special_week-2` + `is_special_week-3` + 
                                    `nps_score-1` + `nps_score-2` + `nps_score-3` + `gmv-1` + 
                                    `gmv-3`,data_dlag_GamingAccessory)

summary(dlag_GamingAccessory_remove)


dlag_GamingAccessory_remove <- lm(gmv ~ deliverycdays + prepaid_percentage +  
                                    sla + deliverybdays + nps_score + is_special_week + Total_Investment_Adstock + 
                                    TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Online_marketing_Adstock + 
                                    Affiliates_Adstock +  Radio_Adstock + Other_Adstock + 
                                    `deliverycdays-1` + `deliverycdays-2` + `prepaid_percentage-2` + 
                                    `prepaid_percentage-3` + `promotional_offer-1` + `promotional_offer-2` + 
                                    `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                                    `product_procurement_sla-2` + `deliverybdays-1` + `deliverybdays-2` + 
                                    `deliverybdays-3` + `is_special_week-2` + `is_special_week-3` + 
                                    `nps_score-1` + `nps_score-2` + `nps_score-3` + `gmv-1` + 
                                    `gmv-3`,data_dlag_GamingAccessory)

summary(dlag_GamingAccessory_remove)
View(vif(dlag_GamingAccessory_remove))


dlag_GamingAccessory_remove <- lm(gmv ~ deliverycdays + prepaid_percentage +  
                                    sla + deliverybdays + nps_score + is_special_week + Total_Investment_Adstock + 
                                    TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Online_marketing_Adstock + 
                                    Affiliates_Adstock +  Radio_Adstock + Other_Adstock + 
                                    `prepaid_percentage-2` + 
                                    `prepaid_percentage-3` + `promotional_offer-1` + `promotional_offer-2` + 
                                    `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                                    `product_procurement_sla-2` + `deliverybdays-1` +  
                                    `deliverybdays-3` + `is_special_week-2` + `is_special_week-3` + 
                                    `nps_score-1` + `nps_score-2` + `nps_score-3` + `gmv-1` + 
                                    `gmv-3`,data_dlag_GamingAccessory)

summary(dlag_GamingAccessory_remove)
View(vif(dlag_GamingAccessory_remove))

dlag_GamingAccessory_remove <- lm(gmv ~ deliverycdays + prepaid_percentage +  
                                    sla + deliverybdays + nps_score + is_special_week + Total_Investment_Adstock + 
                                    TV_Adstock + Digital_Adstock + Sponsorship_Adstock + Online_marketing_Adstock + 
                                    Affiliates_Adstock +  Radio_Adstock + Other_Adstock + 
                                    `prepaid_percentage-2` + 
                                    `prepaid_percentage-3` + `promotional_offer-1` + `promotional_offer-2` + 
                                    `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                                    `product_procurement_sla-2` + `deliverybdays-1` +  
                                    `deliverybdays-3` + `is_special_week-2` + `is_special_week-3` + 
                                    `nps_score-1` + `nps_score-2` + `nps_score-3` + `gmv-1` + 
                                    `gmv-3`,data_dlag_GamingAccessory)

summary(dlag_GamingAccessory_remove)
View(vif(dlag_GamingAccessory_remove))


dlag_GamingAccessory_remove <- lm(gmv ~  prepaid_percentage +  
                                    sla + deliverybdays + nps_score + is_special_week +  
                                    TV_Adstock + Digital_Adstock + Sponsorship_Adstock +  
                                    Radio_Adstock + Other_Adstock + 
                                    `prepaid_percentage-2` + 
                                    `prepaid_percentage-3` + `promotional_offer-1` + `promotional_offer-2` + 
                                    `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                                    `product_procurement_sla-2` +   
                                    `deliverybdays-3` + `is_special_week-2` + `is_special_week-3` + 
                                    `nps_score-3` + `gmv-1` + 
                                    `gmv-3`,data_dlag_GamingAccessory)

summary(dlag_GamingAccessory_remove)
View(vif(dlag_GamingAccessory_remove))



dlag_GamingAccessory_remove <- lm(gmv ~  prepaid_percentage +  
                                    sla + deliverybdays + nps_score + is_special_week +  
                                    TV_Adstock + Digital_Adstock + Sponsorship_Adstock +  
                                    `prepaid_percentage-2` + 
                                    `prepaid_percentage-3` + `promotional_offer-1` + `promotional_offer-2` + 
                                    `promotional_offer-3` + `sla-1` + `sla-2` + `sla-3` + `product_procurement_sla-1` + 
                                    `product_procurement_sla-2` +   
                                    `deliverybdays-3` + `is_special_week-2` + `is_special_week-3` + 
                                    `nps_score-3` + `gmv-1` + 
                                    `gmv-3`,data_dlag_GamingAccessory)

summary(dlag_GamingAccessory_remove)
View(vif(dlag_GamingAccessory_remove))

#Since Lag values does  not have significant p values and detoriating model performance  , we stop proceeding further removal 

