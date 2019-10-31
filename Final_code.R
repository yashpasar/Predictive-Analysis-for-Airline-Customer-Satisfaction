#Saving the csv file to a dataframe
survey <- read.csv("Satisfaction Survey.csv")

#Analyse the dataset
summary(survey)
str(survey)

#table(survey$Airline.Name)

#trim the dataset to remove unnecessary spaces
trimws(survey$Satisfaction, which = c("both"))
trimws(survey$Airline.Status, which = c("both"))
trimws(survey$Airline.Code, which = c("both"))
trimws(survey$Airline.Name, which = c("both"))
trimws(survey$Age, which = c("both"))
trimws(survey$Gender, which = c("both"))
trimws(survey$Price.Sensitivity, which = c("both"))
trimws(survey$Year.of.First.Flight, which = c("both"))
trimws(survey$No.of.Flights.p.a., which = c("both"))
trimws(survey$No..of.other.Loyalty.Cards, which = c("both"))
trimws(survey$X..of.Flight.with.other.Airlines, which = c("both"))
trimws(survey$Type.of.Travel, which = c("both"))
trimws(survey$Shopping.Amount.at.Airport, which = c("both"))
trimws(survey$Eating.and.Drinking.at.Airport, which = c("both"))
trimws(survey$Class, which = c("both"))
trimws(survey$Day.of.Month, which = c("both"))
trimws(survey$Flight.Distance, which = c("both"))
trimws(survey$Flight.date, which = c("both"))
trimws(survey$Flight.cancelled, which = c("both"))
trimws(survey$Flight.time.in.minutes,which = c("both"))
trimws(survey$Orgin.City, which = c("both"))
trimws(survey$Origin.State,which = c("both"))
trimws(survey$Destination.City, which = c("both"))
trimws(survey$Destination.State, which = c("both"))
trimws(survey$Scheduled.Departure.Hour, which = c("both"))
trimws(survey$Departure.Delay.in.Minutes, which = c("both"))
trimws(survey$Arrival.Delay.in.Minutes, which = c("both"))
trimws(survey$Arrival.Delay.greater.5.Mins, which = c("both"))



#Change the column names
colnames(survey)[colnames(survey)=="Airline.Status"] <- "Airline_Status"
colnames(survey)[colnames(survey)=="Price.Sensitivity"] <- "Price_Sensitivity"
colnames(survey)[colnames(survey)=="Year.of.First.Flight"] <- "Year_of_First_Flight"
colnames(survey)[colnames(survey)=="No.of.Flights.p.a."] <- "No_of_Flights_per_annum"
colnames(survey)[colnames(survey)=="X..of.Flight.with.other.Airlines"] <- "Percent_of_Flight_with_other_Airlines"
colnames(survey)[colnames(survey)=="Type.of.Travel"] <- "Type_of_Travel"
colnames(survey)[colnames(survey)=="No..of.other.Loyalty.Cards"] <- "No_of_other_Loyalty_Cards"
colnames(survey)[colnames(survey)=="Shopping.Amount.at.Airport"] <- "Shopping_Amount_at_Airport"
colnames(survey)[colnames(survey)=="Eating.and.Drinking.at.Airport"] <- "Eating_and_Drinking_at_Airport"
colnames(survey)[colnames(survey)=="Day.of.Month"] <- "Day_of_Month"
colnames(survey)[colnames(survey)=="Flight.date"] <- "Flight_date"
colnames(survey)[colnames(survey)=="Airline.Code"] <- "Airline_Code"
colnames(survey)[colnames(survey)=="Airline.Name"] <- "Airline_Name"
colnames(survey)[colnames(survey)=="Origin.City"] <- "Origin_City"
colnames(survey)[colnames(survey)=="Origin.State"] <- "Origin_State"
colnames(survey)[colnames(survey)=="Destination.City"] <- "Destination_City"
colnames(survey)[colnames(survey)=="Destination.State"] <- "Destination_State"
colnames(survey)[colnames(survey)=="Scheduled.Departure.Hour"] <- "Scheduled_Departure_Hour"
colnames(survey)[colnames(survey)=="Departure.Delay.in.Minutes"] <- "Departure_Delay_in_Minutes"
colnames(survey)[colnames(survey)=="Arrival.Delay.in.Minutes"] <- "Arrival_Delay_in_Minutes"
colnames(survey)[colnames(survey)=="Flight.cancelled"] <- "Flight_cancelled"
colnames(survey)[colnames(survey)=="Flight.time.in.minutes"] <- "Flight_time_in_minutes"
colnames(survey)[colnames(survey)=="Flight.Distance"] <- "Flight_Distance"
colnames(survey)[colnames(survey)=="Arrival.Delay.greater.5.Mins"] <- "Arrival_Delay_greater_5_Mins"


#Converting the NAs in the three columns to their mean values
survey$Flight_time_in_minutes[is.na(survey$Flight_time_in_minutes)] <- round(mean(survey$Flight_time_in_minutes, na.rm = TRUE))
survey$Arrival_Delay_in_Minutes[is.na(survey$Arrival_Delay_in_Minutes)] <- round(mean(survey$Arrival_Delay_in_Minutes, na.rm = TRUE))
survey$Departure_Delay_in_Minutes[is.na(survey$Departure_Delay_in_Minutes)] <- round(mean(survey$Departure_Delay_in_Minutes, na.rm = TRUE))
summary(survey)

# Converting the data types to numeric.

numberize <- function(survey){
  for (i in 1:28){
    survey[,i] <- as.numeric(survey[,i])
    
    
  }
  return(survey)
}

#Verify
str(survey)

install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

res <- survey %>% group_by(Age) %>% summarise(Freq=n(), percLess3 = sum(Satisfaction<3, na.rm = TRUE))
res$perc <- (res$percLess3/res$Freq)*100

res_Airline <- survey %>% group_by(Airline_Name) %>% summarise(Freq=n(), percLess3= sum(Satisfaction<3, na.rm = TRUE))
res_Airline$perc <- (res_Airline$percLess3/res_Airline$Freq)*100

ggplot(res_Airline, aes(x=Airline_Name ,y = Freq, fill=perc))+geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Data set for the Southeast Airlines Co.
Airline1_df <- filter(survey, Airline_Name == "Southeast Airlines Co. ")

#Data Set for the FlyFast Airways Inc
Airline2_df <- filter(survey, Airline_Name == "FlyFast Airways Inc. ")


#linear regression
Airline1_df$Satisfaction <- as.numeric(Airline1_df$Satisfaction)
Airline1_df <- numberize(Airline1_df)

model1 <- lm(formula = Satisfaction ~., data = Airline1_df)
summary(model1)

#Multiple R-squared:  0.3984
# Adjusted R-squared:  0.3979


Airline2_df <- filter(survey, Airline_Name == "FlyFast Airways Inc. ")
Airline2_df$Satisfaction <- as.numeric(Airline2_df$Satisfaction)
Airline2_df <- numberize(Airline2_df)
str(Airline2_df)

model2 <- lm(formula = Satisfaction ~., data = Airline2_df)
summary(model2)

#Single Regression for Airline 1

modelsr11 <- lm(formula = Satisfaction ~ Airline1_df$Airline_Status, data = Airline1_df)
summary(modelsr11)
plot(Airline1_df$Airline_Status, Airline1_df$Satisfaction, xlab = "Airline Status", ylab = "Satisfaction")
abline(modelsr11)
#Multiple R-squared:  0.1131
#Adjusted R-squared:  0.113
#p-value: < 2.2e-16

modelsr12 <- lm(formula = Satisfaction ~ Airline1_df$Age, data = Airline1_df)
summary(modelsr12)
plot(Airline1_df$Age, Airline1_df$Satisfaction, xlab = "Age", ylab = "Satisfaction")
abline(modelsr12)
#Multiple R-squared:  0.04005
#Adjusted R-squared:  0.039995
#p-value: < 2.2e-16

modelsr13 <- lm(formula = Satisfaction ~ Airline1_df$Gender, data = Airline1_df)
summary(modelsr13)
plot(Airline1_df$Gender, Airline1_df$Satisfaction, xlab = "Gender", ylab = "Satisfaction")
abline(modelsr13)
#Multiple R-squared:  0.02129
#Adjusted R-squared:  0.02119
#p-value: < 2.2e-16

modelsr14 <- lm(formula = Satisfaction ~ Airline1_df$Price_Sensitivity, data = Airline1_df)
summary(modelsr14)
plot(Airline1_df$Price_Sensitivity, Airline1_df$Satisfaction, xlab = "Price Sensitivity", ylab = "Satisfaction")
abline(modelsr14)
#Multiple R-squared:  0.005561
#Adjusted R-squared:  0.005457
#p-value: < 2.731e-13

modelsr15 <- lm(formula = Satisfaction ~ Airline1_df$Year_of_First_Flight, data = Airline1_df)
summary(modelsr15)
plot(Airline1_df$Year_of_First_Flight, Airline1_df$Satisfaction, xlab = "Year of first flight", ylab = "Satisfaction")
abline(modelsr15)
#Multiple R-squared:  0.0006154
#Adjusted R-squared:  0.000511
#p-value: < 0.0152

modelsr16 <- lm(formula = Satisfaction ~ Airline1_df$No_of_Flights_per_annum, data = Airline1_df)
summary(modelsr16)
plot(Airline1_df$No_of_Flights_per_annum, Airline1_df$Satisfaction, xlab = "Number of flights per annum", ylab = "Satisfaction")
abline(modelsr16)
#Multiple R-squared:  0.05444
#Adjusted R-squared:  0.05434
#p-value: < 2.2e-16

modelsr17 <- lm(formula = Satisfaction ~ Airline1_df$Percent_of_Flight_with_other_Airlines, data = Airline1_df)
summary(modelsr17)
plot(Airline1_df$Percent_of_Flight_with_other_Airlines, Airline1_df$Satisfaction, xlab = "Percent of flight with other Airlines", ylab = "Satisfaction")
abline(modelsr17)
#Multiple R-squared:  0.004987
#Adjusted R-squared:  0.004883
#p-value: < 4.553e-12

modelsr18 <- lm(formula = Satisfaction ~ Airline1_df$Type_of_Travel, data = Airline1_df)
summary(modelsr18)
plot(Airline1_df$Type_of_Travel, Airline1_df$Satisfaction, xlab = "Type of Travel", ylab = "Satisfaction")
abline(modelsr18)
#Multiple R-squared:  0.2791
#Adjusted R-squared:  0.279
#p-value: < 2.2e-16

modelsr19 <- lm(formula = Satisfaction ~ Airline1_df$No_of_other_Loyalty_Cards, data = Airline1_df)
summary(modelsr19)
plot(Airline1_df$No_of_other_Loyalty_Cards, Airline1_df$Satisfaction, xlab = "Number of other Loyalty Cards", ylab = "Satisfaction")
abline(modelsr19)
#Multiple R-squared:  0.007508
#Adjusted R-squared:  0.007405
#p-value: < 2.2e-16

modelsr110 <- lm(formula = Satisfaction ~ Airline1_df$Shopping_Amount_at_Airport, data = Airline1_df)
summary(modelsr110)
plot(Airline1_df$Shopping_Amount_at_Airport, Airline1_df$Satisfaction, xlab = "Shopping Amount at the Airport", ylab = "Satisfaction")
abline(modelsr110)
#Multiple R-squared:  0.0002682
#Adjusted R-squared:  0.0001638
#p-value: < 0.109

modelsr111 <- lm(formula = Satisfaction ~ Airline1_df$Eating_and_Drinking_at_Airport, data = Airline1_df)
summary(modelsr111)
plot(Airline1_df$Eating_and_Drinking_at_Airport, Airline1_df$Satisfaction, xlab = "Eating and Drinking at the airport", ylab = "Satisfaction")
abline(modelsr111)
#Multiple R-squared:  0.0001009
#Adjusted R-squared:  -3.532e-06
#p-value: < 0.3257

modelsr112 <- lm(formula = Satisfaction ~ Airline1_df$Class, data = Airline1_df)
summary(modelsr112)
plot(Airline1_df$Class, Airline1_df$Satisfaction, xlab = "Class", ylab = "Satisfaction")
abline(modelsr112)
#Multiple R-squared:  0.001133
#Adjusted R-squared:  0.001029
#p-value: < 0.0009867

modelsr113 <- lm(formula = Satisfaction ~ Airline1_df$Day_of_Month, data = Airline1_df)
summary(modelsr113)
plot(Airline1_df$Day_of_Month, Airline1_df$Satisfaction, xlab = "Day of the Month", ylab = "Satisfaction")
abline(modelsr113)
#Multiple R-squared:  0.0001969
#Adjusted R-squared:  9.244e-05
#p-value: < 0.11698

modelsr114 <- lm(formula = Satisfaction ~ Airline1_df$Flight_date, data = Airline1_df)
summary(modelsr114)
plot(Airline1_df$Flight_date, Airline1_df$Satisfaction, xlab = "Flight Date", ylab = "Satisfaction")
abline(modelsr114)
#Multiple R-squared:  0.000228
#Adjusted R-squared:  0.0001236
#p-value: < 0.1395

modelsr115 <- lm(formula = Satisfaction ~ Airline1_df$Airline_Code, data = Airline1_df)
summary(modelsr115)
plot(Airline1_df$Airline_Code, Airline1_df$Satisfaction, xlab = "Airline Code", ylab = "Satisfaction")
abline(modelsr115)
#Multiple R-squared:  0.0002682
#Adjusted R-squared:  0.0001638
#p-value: < 0.109

modelsr116 <- lm(formula = Satisfaction ~ Airline1_df$Airline_Name, data = Airline1_df)
summary(modelsr116)
plot(Airline1_df$Airline_Name, Airline1_df$Satisfaction, xlab = "Airline Name", ylab = "Satisfaction")
abline(modelsr116)
#Multiple R-squared:  0.0002682
#Adjusted R-squared:  0.0001638
#p-value: < 0.109

modelsr117 <- lm(formula = Satisfaction ~ Airline1_df$Orgin.City, data = Airline1_df)
summary(modelsr117)
plot(Airline1_df$Orgin.City, Airline1_df$Satisfaction, xlab = "Origin City", ylab = "Satisfaction")
abline(modelsr117)
#Multiple R-squared:  4.631e-05
#Adjusted R-squared:  -5.812e-05
#p-value: < 0.5055

modelsr118 <- lm(formula = Satisfaction ~ Airline1_df$Origin_State, data = Airline1_df)
summary(modelsr118)
plot(Airline1_df$Origin_State, Airline1_df$Satisfaction, xlab = "Origin State", ylab = "Satisfaction")
abline(modelsr118)
#Multiple R-squared:  9.086e-05
#Adjusted R-squared:  -1.357e-05
#p-value: < 0.351

modelsr119 <- lm(formula = Satisfaction ~ Airline1_df$Destination_City, data = Airline1_df)
summary(modelsr119)
plot(Airline1_df$Destination_City, Airline1_df$Satisfaction, xlab = "Destination City", ylab = "Satisfaction")
abline(modelsr119)
#Multiple R-squared:  0.0002352
#Adjusted R-squared:  0.0001308
#p-value: < 0.1334

modelsr120 <- lm(formula = Satisfaction ~ Airline1_df$Destination_State, data = Airline1_df)
summary(modelsr120)
plot(Airline1_df$Destination_State, Airline1_df$Satisfaction, xlab = "Destination State", ylab = "Satisfaction")
abline(modelsr120)
#Multiple R-squared:  4.08e-05
#Adjusted R-squared:  -6.363e-05
#p-value: < 0.5319

modelsr121 <- lm(formula = Satisfaction ~ Airline1_df$Scheduled_Departure_Hour, data = Airline1_df)
summary(modelsr121)
plot(Airline1_df$Scheduled_Departure_Hour, Airline1_df$Satisfaction, xlab = "Scheduled Departure Hour", ylab = "Satisfaction")
abline(modelsr121)
#Multiple R-squared:  0.0009554
#Adjusted R-squared:  0.000851
#p-value: < 0.002485

modelsr122 <- lm(formula = Satisfaction ~ Airline1_df$Departure_Delay_in_Minutes, data = Airline1_df)
summary(modelsr122)
plot(Airline1_df$Departure_Delay_in_Minutes, Airline1_df$Satisfaction, xlab = "Departure Delay in Minutes", ylab = "Satisfaction")
abline(modelsr122)
#Multiple R-squared:  0.002393
#Adjusted R-squared:  0.002289
#p-value: < 1.672e-06

modelsr123 <- lm(formula = Satisfaction ~ Airline1_df$Arrival_Delay_in_Minutes, data = Airline1_df)
summary(modelsr123)
plot(Airline1_df$Arrival_Delay_in_Minutes, Airline1_df$Satisfaction, xlab = "Arrival Delay in Minutes", ylab = "Satisfaction")
abline(modelsr123)
#Multiple R-squared:  0.005191
#Adjusted R-squared:  0.005087
#p-value: < 1.673e-12

modelsr124 <- lm(formula = Satisfaction ~ Airline1_df$Flight_cancelled, data = Airline1_df)
summary(modelsr124)
plot(Airline1_df$Flight_cancelled, Airline1_df$Satisfaction, xlab = "Flight Cancelled", ylab = "Satisfaction")
abline(modelsr124)
#Multiple R-squared:  0.003189
#Adjusted R-squared:  0.003085
#p-value: < 3.207e-08

modelsr125 <- lm(formula = Satisfaction ~ Airline1_df$Flight_time_in_minutes, data = Airline1_df)
summary(modelsr125)
plot(Airline1_df$Flight_time_in_minutes, Airline1_df$Satisfaction, xlab = "Flight time in minutes", ylab = "Satisfaction")
abline(modelsr125)
#Multiple R-squared: 0.0001149
#Adjusted R-squared: 1.052e-05
#p-value: < 0.2941

modelsr126 <- lm(formula = Satisfaction ~ Airline1_df$Flight_Distance, data = Airline1_df)
summary(modelsr126)
plot(Airline1_df$Flight_Distance, Airline1_df$Satisfaction, xlab = "Flight Distance", ylab = "Satisfaction")
abline(modelsr126)
#Multiple R-squared:  1.432e-06
#Adjusted R-squared:  -0.000103
#p-value: < 0.9071

modelsr127 <- lm(formula = Satisfaction ~ Airline1_df$Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(modelsr127)
plot(Airline1_df$Arrival_Delay_greater_5_Mins, Airline1_df$Satisfaction, xlab = "Arrival Delay greater than 5 minutes", ylab = "Satisfaction")
abline(modelsr127)
#Multiple R-squared:  0.01559
#Adjusted R-squared:  0.01549
#p-value: < 2.2e-16


#Single Regression for Airline 2

modelsr21 <- lm(formula = Satisfaction ~ Airline2_df$Airline_Status, data = Airline2_df)
summary(modelsr21)
plot(Airline2_df$Airline_Status, Airline2_df$Satisfaction, xlab = "Airline Status", ylab = "Satisfaction")
abline(modelsr21)
#Multiple R-squared:  0.04005
#Adjusted R-squared:  0.039995
#p-value: < 2.2e-16

modelsr22 <- lm(formula = Satisfaction ~ Airline2_df$Age, data = Airline2_df)
summary(modelsr22)
plot(Airline2_df$Age, Airline2_df$Satisfaction, xlab = "Age", ylab = "Satisfaction")
abline(modelsr22)
#Multiple R-squared:  0.0258
#Adjusted R-squared:  0.02573
#p-value: < 2.2e-16

modelsr23 <- lm(formula = Satisfaction ~ Airline2_df$Gender, data = Airline2_df)
summary(modelsr23)
plot(Airline2_df$Gender, Airline2_df$Satisfaction, xlab = "Gender", ylab = "Satisfaction")
abline(modelsr23)
#Multiple R-squared:  0.02129
#Adjusted R-squared:  0.02119
#p-value: < 2.2e-16

modelsr24 <- lm(formula = Satisfaction ~ Airline2_df$Price_Sensitivity, data = Airline2_df)
summary(modelsr24)
plot(Airline2_df$Price_Sensitivity, Airline2_df$Satisfaction, xlab = "Price Sensitivty", ylab = "Satisfaction")
abline(modelsr24)
#Multiple R-squared:  0.00771
#Adjusted R-squared:  0.007646
#p-value: < 2.2e-16

modelsr25 <- lm(formula = Satisfaction ~ Airline2_df$Year_of_First_Flight, data = Airline2_df)
summary(modelsr25)
plot(Airline2_df$Year_of_First_Flight, Airline2_df$Satisfaction, xlab = "Year of first flight", ylab = "Satisfaction")
abline(modelsr25)
#Multiple R-squared:  0.0005808
#Adjusted R-squared:  0.0005159
#p-value: < 0.002776

modelsr26 <- lm(formula = Satisfaction ~ Airline2_df$No_of_Flights_per_annum, data = Airline2_df)
summary(modelsr26)
plot(Airline2_df$No_of_Flights_per_annum, Airline2_df$Satisfaction, xlab = "Number of flights per annum", ylab = "Satisfaction")
abline(modelsr26)
#Multiple R-squared:  0.05704
#Adjusted R-squared:  0.05698
#p-value: < 2.2e-16

modelsr27 <- lm(formula = Satisfaction ~ Airline2_df$Percent_of_Flight_with_other_Airlines, data = Airline2_df)
summary(modelsr27)
plot(Airline2_df$Percent_of_Flight_with_other_Airlines, Airline2_df$Satisfaction, xlab = "Percent of flights with other Airlines", ylab = "Satisfaction")
abline(modelsr27)
#Multiple R-squared:  0.005143
#Adjusted R-squared:  0.005078
#p-value: < 2.2e-16

modelsr28 <- lm(formula = Satisfaction ~ Airline2_df$Type_of_Travel, data = Airline2_df)
summary(modelsr28)
plot(Airline2_df$Type_of_Travel, Airline2_df$Satisfaction, xlab = "Type of Travel", ylab = "Satisfaction")
abline(modelsr28)
#Multiple R-squared:  0.2908
#Adjusted R-squared:  0.2907
#p-value: < 2.2e-16

modelsr29 <- lm(formula = Satisfaction ~ Airline2_df$No_of_other_Loyalty_Cards, data = Airline2_df)
summary(modelsr29)
plot(Airline2_df$No_of_other_Loyalty_Cards, Airline2_df$Satisfaction, xlab = "Number of other loyalty cards", ylab = "Satisfaction")
abline(modelsr29)
#Multiple R-squared:  0.007344
#Adjusted R-squared:  0.00728
#p-value: < 2.2e-16

modelsr210 <- lm(formula = Satisfaction ~ Airline2_df$Shopping_Amount_at_Airport, data = Airline2_df)
summary(modelsr210)
plot(Airline2_df$Shopping_Amount_at_Airport, Airline2_df$Satisfaction, xlab = "Shopping amount at airport", ylab = "Satisfaction")
abline(modelsr210)
#Multiple R-squared:  0.0005364
#Adjusted R-squared:  0.0004715
#p-value: < 0.004042

modelsr211 <- lm(formula = Satisfaction ~ Airline2_df$Eating_and_Drinking_at_Airport, data = Airline2_df)
summary(modelsr211)
plot(Airline2_df$Eating_and_Drinking_at_Airport, Airline2_df$Satisfaction, xlab = "Eating and drinking at airport", ylab = "Satisfaction")
abline(modelsr211)
#Multiple R-squared:  0.0003544
#Adjusted R-squared:  0.0002895
#p-value: < 0.01945

modelsr212 <- lm(formula = Satisfaction ~ Airline2_df$Class, data = Airline2_df)
summary(modelsr212)
plot(Airline2_df$Class, Airline2_df$Satisfaction, xlab = "Class", ylab = "Satisfaction")
abline(modelsr212)
#Multiple R-squared:  0.002636
#Adjusted R-squared:  0.002571
#p-value: < 1.816e-10

modelsr213 <- lm(formula = Satisfaction ~ Airline2_df$Day_of_Month, data = Airline2_df)
summary(modelsr213)
plot(Airline2_df$Day_of_Month, Airline2_df$Satisfaction, xlab = "Day of the month", ylab = "Satisfaction")
abline(modelsr213)
#Multiple R-squared:  8.479e-05
#Adjusted R-squared:  1.988e-05
#p-value: < 0.2531

modelsr214 <- lm(formula = Satisfaction ~ Airline2_df$Flight_date, data = Airline2_df)
summary(modelsr124)
plot(Airline2_df$Flight_date, Airline2_df$Satisfaction, xlab = "Flight Date", ylab = "Satisfaction")
abline(modelsr214)
#Multiple R-squared:  0.003189
#Adjusted R-squared:  0.003085
#p-value: < 3.207e-08

modelsr215 <- lm(formula = Satisfaction ~ Airline2_df$Airline_Code, data = Airline2_df)
summary(modelsr215)
plot(Airline2_df$Airline_Code, Airline2_df$Satisfaction, xlab = "Airline Code", ylab = "Satisfaction")
abline(modelsr215)
#Multiple R-squared:  0.0002682
#Adjusted R-squared:  0.0001638
#p-value: < 0.109

modelsr216 <- lm(formula = Satisfaction ~ Airline2_df$Airline_Name, data = Airline2_df)
summary(modelsr216)
plot(Airline2_df$Airline_Name, Airline2_df$Satisfaction, xlab = "Airline Name", ylab = "Satisfaction")
abline(modelsr216)
#Multiple R-squared:  0.0002682
#Adjusted R-squared:  0.0001638
#p-value: < 0.109

modelsr217 <- lm(formula = Satisfaction ~ Airline2_df$Orgin.City, data = Airline2_df)
summary(modelsr217)
plot(Airline2_df$Orgin.City, Airline2_df$Satisfaction, xlab = "Origin City", ylab = "Satisfaction")
abline(modelsr217)
#Multiple R-squared:  1.78e-05
#Adjusted R-squared:  -4.711e-05
#p-value: < 0.6005

modelsr218 <- lm(formula = Satisfaction ~ Airline2_df$Origin_State, data = Airline2_df)
summary(modelsr218)
plot(Airline2_df$Origin_State, Airline2_df$Satisfaction, xlab = "Origin State", ylab = "Satisfaction")
abline(modelsr218)
#Multiple R-squared:  4.872e-05
#Adjusted R-squared:  -1.619e-05
#p-value: < 0.3863

modelsr219 <- lm(formula = Satisfaction ~ Airline2_df$Destination_City, data = Airline2_df)
summary(modelsr219)
plot(Airline2_df$Destination_City, Airline2_df$Satisfaction, xlab = "Destination City", ylab = "Satisfaction")
abline(modelsr219)
#Multiple R-squared:  1.994e-06
#Adjusted R-squared:  -6.292e-05
#p-value: < 0.8609

modelsr220 <- lm(formula = Satisfaction ~ Airline2_df$Destination_State, data = Airline2_df)
summary(modelsr220)
plot(Airline2_df$Destination_State, Airline2_df$Satisfaction, xlab = "Destination State", ylab = "Satisfaction")
abline(modelsr220)
#Multiple R-squared:  0.000124
#Adjusted R-squared:  5.908e=05
#p-value: < 0.167

modelsr221 <- lm(formula = Satisfaction ~ Airline2_df$Scheduled_Departure_Hour, data = Airline2_df)
summary(modelsr221)
plot(Airline2_df$Scheduled_Departure_Hour, Airline2_df$Satisfaction, xlab = "Scheduled departure Hour", ylab = "Satisfaction")
abline(modelsr221)
#Multiple R-squared:  7.699e-06
#Adjusted R-squared:  -5.721e-05
#p-value: < 0.7306

modelsr222 <- lm(formula = Satisfaction ~ Airline2_df$Departure_Delay_in_Minutes, data = Airline2_df)
summary(modelsr222)
plot(Airline2_df$Departure_Delay_in_Minutes, Airline2_df$Satisfaction, xlab = "Departure delay in minutes", ylab = "Satisfaction")
abline(modelsr222)
#Multiple R-squared:  0.004668
#Adjusted R-squared:  0.004603
#p-value: < 2.2e-16

modelsr223 <- lm(formula = Satisfaction ~ Airline2_df$Arrival_Delay_in_Minutes, data = Airline2_df)
summary(modelsr223)
plot(Airline2_df$Arrival_Delay_in_Minutes, Airline2_df$Satisfaction, xlab = "Arrival delay in minutes", ylab = "Satisfaction")
abline(modelsr223)
#Multiple R-squared:  0.005868
#Adjusted R-squared:  0.005803
#p-value: < 2.2e-16

modelsr224 <- lm(formula = Satisfaction ~ Airline2_df$Flight_cancelled, data = Airline2_df)
summary(modelsr224)
plot(Airline2_df$Flight_cancelled, Airline2_df$Satisfaction, xlab = "Flight Cancelled", ylab = "Satisfaction")
abline(modelsr224)
#Multiple R-squared:  0.003726
#Adjusted R-squared:  0.003663
#p-value: < 3.61e-14

modelsr225 <- lm(formula = Satisfaction ~ Airline2_df$Flight_time_in_minutes, data = Airline2_df)
summary(modelsr225)
plot(Airline2_df$Flight_time_in_minutes, Airline2_df$Satisfaction, xlab = "Flight time in minutes", ylab = "Satisfaction")
abline(modelsr225)
#Multiple R-squared: 0.0004713
#Adjusted R-squared: 0.0004064
#p-value: < 0.007042

modelsr226 <- lm(formula = Satisfaction ~ Airline2_df$Flight_Distance, data = Airline2_df)
summary(modelsr226)
plot(Airline2_df$Flight_Distance, Airline2_df$Satisfaction, xlab = "Flight Distance", ylab = "Satisfaction")
abline(modelsr226)
#Multiple R-squared:  5.785e-06
#Adjusted R-squared:  -5.913e-05
#p-value: < 0.7653

modelsr227 <- lm(formula = Satisfaction ~ Airline2_df$Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(modelsr227)
plot(Airline2_df$Arrival_Delay_greater_5_Mins, Airline2_df$Satisfaction, xlab = "Arrival delay greater than 5 minutes", ylab = "Satisfaction")
abline(modelsr227)
#Multiple R-squared:  0.0.01525
#Adjusted R-squared:  0.01519
#p-value: < 2.2e-16

#--------------------------------------------------------------------------------------------
#Multiple R-squared:  0.3827
#Adjusted R-squared:  0.3811



#For South East Airline

# Airline_Status, Age ,Gender, Year_of_First_Flight, No_of_Flights_per_annum, Type_of_Travel, Scheduled_Departure_Hour, Flight_cancelled,Arrival_Delay_greater_5_Mins


#Grouping all the significant variables

model11 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model11)


#Multiple R-squared:  0.3804
#Adjusted R-squared:  0.3798


#without + Arrival_Delay_greater_5_Mins


model12 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled, data = Airline1_df)
summary(model12)

#Multiple R-squared:  0.3622
#Adjusted R-squared:  0.3616

# + Flight_cancelled

model13 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour + Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model13)


#Multiple R-squared:  0.3793
#Adjusted R-squared:  0.3788

#Scheduled_Departure_Hour+

model14 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model14)

#Multiple R-squared:  0.3795
#Adjusted R-squared:  0.3789

#Type_of_Travel
model15 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum +Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model15)

#Multiple R-squared:  0.2158
#Adjusted R-squared:  0.2152

# + No_of_Flights_per_annum

model16 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight + Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model16)

#Multiple R-squared:  0.3777
#Adjusted R-squared:  0.3772

#Year_of_First_Flight

model17 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model17)

#Multiple R-squared:  0.3795
#Adjusted R-squared:  0.379

# +Gender

model18 <- lm(formula = Satisfaction ~Airline_Status+Age +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model18)

#Multiple R-squared:  0.3725
#Adjusted R-squared:  0.372

#Age

model19 <- lm(formula = Satisfaction ~Airline_Status+Gender +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model19)

#Multiple R-squared:  0.3789
#Adjusted R-squared:  0.3783

#Airline_Status

model20 <- lm(formula = Satisfaction ~Age +Gender +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model20)

#Multiple R-squared:  0.3149
#Adjusted R-squared:  0.3143



##Year_of_First_Flight

#model17 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
#summary(model17)

#Multiple R-squared:  0.3795
#Adjusted R-squared:  0.379

model171 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled, data = Airline1_df)
summary(model171)

#Multiple R-squared:  0.3612
#Adjusted R-squared:  0.3608

#Flight_cancelled
model172 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model172)

#Multiple R-squared:  0.3784
#Adjusted R-squared:  0.378


#+Scheduled_Departure_Hour
model173 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel + Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model173)
#Multiple R-squared:  0.3786
#Adjusted R-squared:  0.3781

#+ Type_of_Travel
model174 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum +Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model174)
#Multiple R-squared:  0.2153
# Adjusted R-squared:  0.2148
#+ No_of_Flights_per_annum

model175 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model175)
#Multiple R-squared:  0.3769
#Adjusted R-squared:  0.3764


#+Gender
model176 <- lm(formula = Satisfaction ~Airline_Status+Age + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model176)
#Multiple R-squared:  0.3716
#Adjusted R-squared:  0.3711

#+Age
model177 <- lm(formula = Satisfaction ~Airline_Status +Gender + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model177)
#Multiple R-squared:  0.378
#Adjusted R-squared:  0.3776

# Airline_Status+
model178 <- lm(formula = Satisfaction ~ Age+Gender + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model178)

#Multiple R-squared:  0.3132
#Adjusted R-squared:  0.3127



#+Scheduled_Departure_Hour
#model173 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel + Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
#summary(model173)
#Multiple R-squared:  0.3786
#Adjusted R-squared:  0.3781


#Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel + Flight_cancelled+ Arrival_Delay_greater_5_Mins


#+ Arrival_Delay_greater_5_Mins
model1731 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel + Flight_cancelled, data = Airline1_df)
summary(model1731)

#Multiple R-squared:  0.3606, Adjusted R-squared:  0.3602

# Flight_cancelled
model1732 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel + Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model1732)



#Multiple R-squared:  0.3775, Adjusted R-squared:  0.3771


#+ Type_of_Travel
model1733 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum + Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model1733)

#Multiple R-squared:  0.2141, Adjusted R-squared:  0.2136
# + No_of_Flights_per_annum
model1734 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + Type_of_Travel + Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model1734)
#Multiple R-squared:  0.376, Adjusted R-squared:  0.3756
#+Gender
model1735 <- lm(formula = Satisfaction ~Airline_Status+Age + No_of_Flights_per_annum+ Type_of_Travel + Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model1735)
#Multiple R-squared:  0.3707, Adjusted R-squared:  0.3703
# +Age
model1736 <- lm(formula = Satisfaction ~Airline_Status +Gender + No_of_Flights_per_annum+ Type_of_Travel + Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model1736)

#Multiple R-squared:  0.3771, Adjusted R-squared:  0.3767

# Airline_Status

model1737 <- lm(formula = Satisfaction ~ Age+Gender + No_of_Flights_per_annum+ Type_of_Travel + Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline1_df)
summary(model1737)

#Multiple R-squared:  0.3121, Adjusted R-squared:  0.3117

#Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel + Arrival_Delay_greater_5_Mins

#For Fareast airline

#Grouping all the significant variables

model11 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model11)

#Multiple R-squared:  0.3944, Adjusted R-squared:  0.394



#without + Arrival_Delay_greater_5_Mins


model12 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled, data = Airline2_df)
summary(model12)

#Multiple R-squared:  0.3754
#Adjusted R-squared:  0.3751

# + Flight_cancelled

model13 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour + Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model13)


#Multiple R-squared:  0.3931
#Adjusted R-squared:  0.3928

#Scheduled_Departure_Hour+

model14 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model14)

#Multiple R-squared:  0.3942
#Adjusted R-squared:  0.3939

#Type_of_Travel
model15 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum +Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model15)

#Multiple R-squared:  0.2213
#Adjusted R-squared:  0.2209

# + No_of_Flights_per_annum

model16 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight + Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model16)

#Multiple R-squared:  0.391
#Adjusted R-squared:  0.3906

#Year_of_First_Flight

model17 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model17)

#Multiple R-squared:  0.3936
#Adjusted R-squared:  0.3933

# +Gender

model18 <- lm(formula = Satisfaction ~Airline_Status+Age +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model18)

#Multiple R-squared:  0.386
#Adjusted R-squared:  0.3857

#Age

model19 <- lm(formula = Satisfaction ~Airline_Status+Gender +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model19)

#Multiple R-squared:  0.394
#Adjusted R-squared:  0.3937

#Airline_Status

model20 <- lm(formula = Satisfaction ~Age +Gender +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+Scheduled_Departure_Hour+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model20)

#Multiple R-squared:  0.3277
#Adjusted R-squared:  0.3273

#Scheduled_Departure_Hour+

#model14 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
#summary(model14)

#Multiple R-squared:  0.3942
#Adjusted R-squared:  0.3939

#Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins

#+ Arrival_Delay_greater_5_Mins
model141 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled, data = Airline2_df)
summary(model141)

#Multiple R-squared:  0.3754, Adjusted R-squared:  0.3751

#+ Flight_cancelled
model142 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel + Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model142)

#Multiple R-squared:  0.393, Adjusted R-squared:  0.3927

# Type_of_Travel

model143 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model143)

#Multiple R-squared:  0.2212, Adjusted R-squared:  0.2208

# No_of_Flights_per_annum+
model144 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender+Year_of_First_Flight+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model144)

#Multiple R-squared:  0.3908, Adjusted R-squared:  0.3905

#+Year_of_First_Flight
model145 <- lm(formula = Satisfaction ~Airline_Status+Age+Gender + No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model145)

#Multiple R-squared:  0.3934, Adjusted R-squared:  0.3932


# +Gender

model146 <- lm(formula = Satisfaction ~Airline_Status+Age +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model146)
#Multiple R-squared:  0.3858, Adjusted R-squared:  0.3855

#+Age


model147 <- lm(formula = Satisfaction ~Airline_Status +Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model147)

#Multiple R-squared:  0.3938, Adjusted R-squared:  0.3935


# Airline_Status+

model148<- lm(formula = Satisfaction ~ Age+Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model148)

#Multiple R-squared:  0.3275, Adjusted R-squared:  0.3271

#+Age


#model147 <- lm(formula = Satisfaction ~Airline_Status +Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
#summary(model147)

#Multiple R-squared:  0.3938, Adjusted R-squared:  0.3935

#Airline_Status +Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins

#+ Arrival_Delay_greater_5_Mins
model1471 <- lm(formula = Satisfaction ~Airline_Status +Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled, data = Airline2_df)
summary(model1471)

#Multiple R-squared:  0.375, Adjusted R-squared:  0.3747
#+ Flight_cancelled
model1472 <- lm(formula = Satisfaction ~Airline_Status +Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel + Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model1472)
#Multiple R-squared:  0.3926, Adjusted R-squared:  0.3923
# Type_of_Travel

model1473 <- lm(formula = Satisfaction ~Airline_Status +Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model1473)

#Multiple R-squared:  0.2011, Adjusted R-squared:  0.2008

# No_of_Flights_per_annum+
model1474 <- lm(formula = Satisfaction ~Airline_Status +Gender+Year_of_First_Flight+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model1474)

#Multiple R-squared:  0.3899, Adjusted R-squared:  0.3896


#+Year_of_First_Flight
model1475 <- lm(formula = Satisfaction ~Airline_Status +Gender + No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model1475)
#Multiple R-squared:  0.393, Adjusted R-squared:  0.3928
# +Gender

model1476 <- lm(formula = Satisfaction ~Airline_Status +Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model1476)

#Multiple R-squared:  0.3853, Adjusted R-squared:  0.3851
# Airline_Status+

model1477<- lm(formula = Satisfaction ~ Gender+Year_of_First_Flight+ No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
summary(model1477)

#Multiple R-squared:  0.3274, Adjusted R-squared:  0.3272
#+Year_of_First_Flight
#model1475 <- lm(formula = Satisfaction ~Airline_Status +Gender + No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins, data = Airline2_df)
#summary(model1475)
#Multiple R-squared:  0.393, Adjusted R-squared:  0.3928


#Airline_Status +Gender + No_of_Flights_per_annum+ Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins


-------------------------------------------- # association rules - flyfast:-------------------------------------------

# airline 2
#Data set for the Southeast Airlines Co.
correcttostring <- function(col)
{
  col <- as.character(col)
  col <- trimws (col, which = c("both"))
#  col <- as.factor(col)
  return (col)
}
#Airline2_df$Satisfaction <- correcttostring(Airline2_df$Satisfaction)
Airline2_df$Satisfaction <- as.numeric(Airline2_df$Satisfaction)

install.packages("arulesViz")
library(arulesViz)

# arules

createBucketSat <- function(vec){ 
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 3] <- "High"
  vBuckets[vec < 3] <- "Low"
  return(vBuckets)
}


createBuckets <- function(vec){
  q <- quantile(vec, c(0.3, 0.6), na.rm = "TRUE")
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

##createBucketsAS <- function(v) {
#  vBucket <- replicate(length(v), "Average")
#  vBucket[v>2] <- "High"
  #vBucket[v<2] <- "Low"
 # return(vBucket)
#}

createBucketsPrice <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>1] <- "High"
  vBucket[v<1] <- "Low"
  return(vBucket)
}

createBucketsFPA <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>24] <- "High"
  vBucket[v<24] <- "Low"
  return(vBucket)
}


CreateBucketsFWOA <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>6] <- "High"
  vBucket[v<6] <- "Low"
  return(vBucket)
}

new_arules_df <- Airline2_df
new_arules_df$Year_of_First_Flight <- NULL
new_arules_df$Day_of_Month <- NULL
new_arules_df$Scheduled_Departure_Hour <- NULL

new_arules_df$Satisfaction <- createBucketSat(new_arules_df$Satisfaction)
new_arules_df$Satisfaction <- as.factor(new_arules_df$Satisfaction)
View(new_arules_df)
new_arules_df$Price_Sensitivity <- createBucketsPrice(new_arules_df$Price_Sensitivity)
new_arules_df$Price_Sensitivity <- as.factor(new_arules_df$Price_Sensitivity)
#new_arules_df$Age <- createBuckets(new_arules_df$Age)
new_arules_df$Age <- as.factor(new_arules_df$Age)
new_arules_df$Airline_Status <- as.factor(new_arules_df$Airline_Status)
new_arules_df$Gender <- as.factor(new_arules_df$Gender)
new_arules_df$Type_of_Travel <- as.factor(new_arules_df$Type_of_Travel)
new_arules_df$Class <- as.factor(new_arules_df$Class)
new_arules_df$Flight_date <- as.factor(new_arules_df$Flight_date)
new_arules_df$Airline_Code <- as.factor(new_arules_df$Airline_Code)
new_arules_df$Airline_Name <- as.factor(new_arules_df$Airline_Name)
new_arules_df$Orgin.City <- as.factor(new_arules_df$Orgin.City)
new_arules_df$Origin_State <- as.factor(new_arules_df$Origin_State)
new_arules_df$Destination_City <- as.factor(new_arules_df$Destination_City)
new_arules_df$Destination_State <- as.factor(new_arules_df$Destination_State)
new_arules_df$Flight_cancelled <- as.factor(new_arules_df$Flight_cancelled)
new_arules_df$Arrival_Delay_greater_5_Mins <- as.factor(new_arules_df$Arrival_Delay_greater_5_Mins)
new_arules_df$No_of_other_Loyalty_Cards <- createBuckets(new_arules_df$No_of_other_Loyalty_Cards)
new_arules_df$No_of_other_Loyalty_Cards <- as.factor(new_arules_df$No_of_other_Loyalty_Cards)
new_arules_df$Shopping_Amount_at_Airport <- createBuckets(new_arules_df$Shopping_Amount_at_Airport)
new_arules_df$Shopping_Amount_at_Airport <- as.factor(new_arules_df$Shopping_Amount_at_Airport)
new_arules_df$No_of_Flights_per_annum <- createBucketsFPA(new_arules_df$No_of_Flights_per_annum)
new_arules_df$No_of_Flights_per_annum <- as.factor(new_arules_df$No_of_Flights_per_annum)
new_arules_df$Percent_of_Flight_with_other_Airlines <- CreateBucketsFWOA(new_arules_df$Percent_of_Flight_with_other_Airlines)
new_arules_df$Percent_of_Flight_with_other_Airlines <- as.factor(new_arules_df$Percent_of_Flight_with_other_Airlines)
new_arules_df$Eating_and_Drinking_at_Airport <- createBuckets(new_arules_df$Eating_and_Drinking_at_Airport)
new_arules_df$Eating_and_Drinking_at_Airport <- as.factor(new_arules_df$Eating_and_Drinking_at_Airport)
new_arules_df$Departure_Delay_in_Minutes <- createBuckets(new_arules_df$Departure_Delay_in_Minutes)
new_arules_df$Departure_Delay_in_Minutes <- as.factor(new_arules_df$Departure_Delay_in_Minutes)
new_arules_df$Arrival_Delay_in_Minutes <- createBuckets(new_arules_df$Arrival_Delay_in_Minutes)
new_arules_df$Arrival_Delay_in_Minutes <- as.factor(new_arules_df$Arrival_Delay_in_Minutes)
new_arules_df$Flight_time_in_minutes <- createBuckets(new_arules_df$Flight_time_in_minutes)
new_arules_df$Flight_time_in_minutes <- as.factor(new_arules_df$Flight_time_in_minutes)
new_arules_df$Flight_Distance <- createBuckets(new_arules_df$Flight_Distance)
new_arules_df$Flight_Distance <- as.factor(new_arules_df$Flight_Distance)
View(new_arules_df)
str(new_arules_df)
library(arules)

rulesetLow <- apriori(new_arules_df, parameter = list(support = 0.1, confidence = 0.6), appearance = list(rhs=c("Satisfaction=Low")))
inspect(rulesetLow)
sorted_rules <- sort(rulesetLow, decreasing=TRUE, by="lift")
inspect(sorted_rules)
plot(sorted_rules)




#----------------------------------------------Support Vector Machine--------------------------------------
#Function for test set

createBuckets <- function(vec){
  q <- quantile(vec, c(0.3, 0.6), na.rm = "TRUE")
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

createBucketsPrice <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>1] <- "High"
  vBucket[v<1] <- "Low"
  return(vBucket)
}

createBucketsFPA <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>24] <- "High"
  vBucket[v<24] <- "Low"
  return(vBucket)
}


CreateBucketsFWOA <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>6] <- "High"
  vBucket[v<6] <- "Low"
  return(vBucket)
}

CreateBucketsSDH <- function(v) {
  vBucket <- replicate(length(v), "Afternoon")
  vBucket[v>16] <- "Night"
  vBucket[v<8] <- "Morning"
  return(vBucket)
}

Training_set <- function(Airline1_df){
  Airline1_df$Satisfaction <- as.character(Airline1_df$Satisfaction)
  Airline1_df[,1] <- as.numeric(Airline1_df[,1])
  Airline1_df$lowSat <- ifelse(Airline1_df[,1]<3, 1, 0)
  randIndex <- sample(1:dim(Airline1_df)[1])   #creating a random sample of Index from the dataset
  cutpoint_2_3 <- floor(2*dim(Airline1_df)[1]/3)  #specifying a cut point in the data set index to segregate the dataset
  trainData <- Airline1_df[randIndex[1:cutpoint_2_3],]
  trainData <- Airline1_df[,c(-1,-15,-16,-17,-19,-21)]
  
  trainData$lowSat <- as.factor(trainData$lowSat)  
  
  
  return(trainData)
}

#Function for test set
Test_set <- function(Airline1_df){
  Airline1_df$Satisfaction <- as.character(Airline1_df$Satisfaction)
  Airline1_df[,1] <- as.numeric(Airline1_df[,1])
  Airline1_df$lowSat <- ifelse(Airline1_df[,1]<3, 1, 0)
  randIndex <- sample(1:dim(Airline1_df)[1])   #creating a random sample of Index from the dataset
  cutpoint_2_3 <- floor(2*dim(Airline1_df)[1]/3)  #specifying a cut point in the data set index to segregate the dataset
  testData <- Airline1_df[randIndex[(cutpoint_2_3+1):dim(Airline1_df)[1]],]
  testData <- Airline1_df[,c(-1,-15,-16,-17,-19,-21)]
  
  testData$lowSat <- as.factor(testData$lowSat)  
  
  
  return(testData)
}

#creating a function for confusion matrix
confusion_matrix <- function(test_set, pred_set){
  compLowSat<- data.frame(test_set, pred_set)
  colnames(compLowSat) <- c("test", "Pred")
  cfmat <- table(test=compLowSat$test, pred=compLowSat$Pred)
  return(cfmat)
}

#Function for error rate
error_rate <- function(cfmat){
  FP_plus_FN <- cfmat[2,"0"] + cfmat[1, "1"]                              #saving the sum of False positive and False negative in one variable.
  FPFNTPTN <- cfmat[2,"0"] + cfmat[1, "1"] + cfmat[1,"0"] + cfmat[2,"1"]
  error_rt <- (FP_plus_FN/FPFNTPTN)*100
  return(error_rt)
}

#Function for Accuracy
Accuracy <- function(cfmat){
  Tp_plus_TN <- cfmat[1,"0"] + cfmat[2,"1"]
  FPFNTPTN <- cfmat[2,"0"] + cfmat[1, "1"] + cfmat[1,"0"] + cfmat[2,"1"]
  accuracy <- (Tp_plus_TN/FPFNTPTN)*100
  return(accuracy)
}

#creating a variable for low satisfaction
trainData <- Training_set(Airline1_df)
testData <- Test_set(Airline1_df)

trainData$Price_Sensitivity <- createBucketsPrice(trainData$Price_Sensitivity)
trainData$Price_Sensitivity <- as.factor(trainData$Price_Sensitivity)
trainData$Age <- createBuckets(trainData$Age)
trainData$Age <- as.factor(trainData$Age)
trainData$No_of_other_Loyalty_Cards <- createBuckets(trainData$No_of_other_Loyalty_Cards)
trainData$No_of_other_Loyalty_Cards <- as.factor(trainData$No_of_other_Loyalty_Cards)
trainData$Shopping_Amount_at_Airport <- createBuckets(trainData$Shopping_Amount_at_Airport)
trainData$Shopping_Amount_at_Airport <- as.factor(trainData$Shopping_Amount_at_Airport)
trainData$No_of_Flights_per_annum <- createBucketsFPA(trainData$No_of_Flights_per_annum)
trainData$No_of_Flights_per_annum <- as.factor(trainData$No_of_Flights_per_annum)
trainData$Percent_of_Flight_with_other_Airlines <- CreateBucketsFWOA(trainData$Percent_of_Flight_with_other_Airlines)
trainData$Percent_of_Flight_with_other_Airlines <- as.factor(trainData$Percent_of_Flight_with_other_Airlines)
trainData$Eating_and_Drinking_at_Airport <- createBuckets(trainData$Eating_and_Drinking_at_Airport)
trainData$Eating_and_Drinking_at_Airport <- as.factor(trainData$Eating_and_Drinking_at_Airport)
trainData$Departure_Delay_in_Minutes <- createBuckets(trainData$Departure_Delay_in_Minutes)
trainData$Departure_Delay_in_Minutes <- as.factor(trainData$Departure_Delay_in_Minutes)
trainData$Arrival_Delay_in_Minutes <- createBuckets(trainData$Arrival_Delay_in_Minutes)
trainData$Arrival_Delay_in_Minutes <- as.factor(trainData$Arrival_Delay_in_Minutes)
trainData$Flight_time_in_minutes <- createBuckets(trainData$Flight_time_in_minutes)
trainData$Flight_time_in_minutes <- as.factor(trainData$Flight_time_in_minutes)
trainData$Flight_Distance <- createBuckets(trainData$Flight_Distance)
trainData$Flight_Distance <- as.factor(trainData$Flight_Distance)
trainData$Scheduled_Departure_Hour <- CreateBucketsSDH(trainData$Scheduled_Departure_Hour)
trainData$Scheduled_Departure_Hour <- as.factor(trainData$Scheduled_Departure_Hour)

testData$Price_Sensitivity <- createBucketsPrice(testData$Price_Sensitivity)
testData$Price_Sensitivity <- as.factor(testData$Price_Sensitivity)
testData$Age <- createBuckets(testData$Age)
testData$Age <- as.factor(testData$Age)
testData$No_of_other_Loyalty_Cards <- createBuckets(testData$No_of_other_Loyalty_Cards)
testData$No_of_other_Loyalty_Cards <- as.factor(testData$No_of_other_Loyalty_Cards)
testData$Shopping_Amount_at_Airport <- createBuckets(testData$Shopping_Amount_at_Airport)
testData$Shopping_Amount_at_Airport <- as.factor(testData$Shopping_Amount_at_Airport)
testData$No_of_Flights_per_annum <- createBucketsFPA(testData$No_of_Flights_per_annum)
testData$No_of_Flights_per_annum <- as.factor(testData$No_of_Flights_per_annum)
testData$Percent_of_Flight_with_other_Airlines <- CreateBucketsFWOA(testData$Percent_of_Flight_with_other_Airlines)
testData$Percent_of_Flight_with_other_Airlines <- as.factor(testData$Percent_of_Flight_with_other_Airlines)
testData$Eating_and_Drinking_at_Airport <- createBuckets(testData$Eating_and_Drinking_at_Airport)
testData$Eating_and_Drinking_at_Airport <- as.factor(testData$Eating_and_Drinking_at_Airport)
testData$Departure_Delay_in_Minutes <- createBuckets(testData$Departure_Delay_in_Minutes)
testData$Departure_Delay_in_Minutes <- as.factor(testData$Departure_Delay_in_Minutes)
testData$Arrival_Delay_in_Minutes <- createBuckets(testData$Arrival_Delay_in_Minutes)
testData$Arrival_Delay_in_Minutes <- as.factor(testData$Arrival_Delay_in_Minutes)
testData$Flight_time_in_minutes <- createBuckets(testData$Flight_time_in_minutes)
testData$Flight_time_in_minutes <- as.factor(testData$Flight_time_in_minutes)
testData$Flight_Distance <- createBuckets(testData$Flight_Distance)
testData$Flight_Distance <- as.factor(testData$Flight_Distance)
testData$Scheduled_Departure_Hour <- CreateBucketsSDH(testData$Scheduled_Departure_Hour)
testData$Scheduled_Departure_Hour <- as.factor(testData$Scheduled_Departure_Hour)





#Significant variables
#Airline_Status, Age, Gender, Price_Sensitivity, Year_of_First_Flight, Flight_cancelled
#No_of_Flights_per_annum, Type_of_Travel,Eating_and_Drinking_at_Airport, Arrival_Delay_greater_5_Mins 
svmModOutput_all <- svmModOutput <- ksvm(lowSat~., data=trainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
svmModOutput_all
svmPred <- predict(svmModOutput_all, testData) 
str(svmPred)
head(svmPred)
#Airline_Status+Age+Gender+No_of_Flights_per_annum+Type_of_Travel+Arrival_Delay_greater_5_Mins
#Confusion matrix
conf_mat <- confusion_matrix(testData$lowSat, svmPred)
conf_mat
error_rate(conf_mat)
Accuracy(conf_mat)
hist(alpha(svmModOutput_all)[[1]])

#For Significant variables
svmModOutput <- ksvm(lowSat~Type_of_Travel+Departure_Delay_in_Minutes+Arrival_Delay_in_Minutes+Percent_of_Flight_with_other_Airlines+Eating_and_Drinking_at_Airport+No_of_other_Loyalty_Cards, data=trainData, kernel= "rbfdot", kpar = "automatic", C = 50, cross = 3, prob.model = TRUE)
svmModOutput
svmPred_reg <- predict(svmModOutput, testData)
conf_mat_reg <- confusion_matrix(testData$lowSat, svmPred_reg)
conf_mat_reg
Accuracy(conf_mat_reg)


#Output - Acc of model - 92%
#Significant variables
#Type_of_Travel+Departure_Delay_in_Minutes+Arrival_Delay_in_Minutes+Percent_of_Flight_with_other_Airlines+Eating_and_Drinking_at_Airport+No_of_other_Loyalty_Cards
#Accuracy - 84.88044



M_1 <- fit(lowSat~., data=trainData, model="svm", kpar=list(sigma=0.10), C=2)
VariableImportance_1 <- Importance(M_1, data=trainData, method = "sensv")

L_1=list(runs=1,sen=t(VariableImportance_1$imp),sresponses=VariableImportance_1$sresponses)
mgraph(L_1,graph="IMP",leg=names(trainData),col="gray",Grid=10)


#-------------------------------------------For FlyFast Airlines----------------------------------------
trainData_1 <- Training_set(Airline2_df)
testData_1 <- Test_set(Airline2_df)

trainData_1$Price_Sensitivity <- createBucketsPrice(trainData_1$Price_Sensitivity)
trainData_1$Price_Sensitivity <- as.factor(trainData_1$Price_Sensitivity)
trainData_1$Age <- createBuckets(trainData_1$Age)
trainData_1$Age <- as.factor(trainData_1$Age)
trainData_1$No_of_other_Loyalty_Cards <- createBuckets(trainData_1$No_of_other_Loyalty_Cards)
trainData_1$No_of_other_Loyalty_Cards <- as.factor(trainData_1$No_of_other_Loyalty_Cards)
trainData_1$Shopping_Amount_at_Airport <- createBuckets(trainData_1$Shopping_Amount_at_Airport)
trainData_1$Shopping_Amount_at_Airport <- as.factor(trainData_1$Shopping_Amount_at_Airport)
trainData_1$No_of_Flights_per_annum <- createBucketsFPA(trainData_1$No_of_Flights_per_annum)
trainData_1$No_of_Flights_per_annum <- as.factor(trainData_1$No_of_Flights_per_annum)
trainData_1$Percent_of_Flight_with_other_Airlines <- CreateBucketsFWOA(trainData_1$Percent_of_Flight_with_other_Airlines)
trainData_1$Percent_of_Flight_with_other_Airlines <- as.factor(trainData_1$Percent_of_Flight_with_other_Airlines)
trainData_1$Eating_and_Drinking_at_Airport <- createBuckets(trainData_1$Eating_and_Drinking_at_Airport)
trainData_1$Eating_and_Drinking_at_Airport <- as.factor(trainData_1$Eating_and_Drinking_at_Airport)
trainData_1$Departure_Delay_in_Minutes <- createBuckets(trainData_1$Departure_Delay_in_Minutes)
trainData_1$Departure_Delay_in_Minutes <- as.factor(trainData_1$Departure_Delay_in_Minutes)
trainData_1$Arrival_Delay_in_Minutes <- createBuckets(trainData_1$Arrival_Delay_in_Minutes)
trainData_1$Arrival_Delay_in_Minutes <- as.factor(trainData_1$Arrival_Delay_in_Minutes)
trainData_1$Flight_time_in_minutes <- createBuckets(trainData_1$Flight_time_in_minutes)
trainData_1$Flight_time_in_minutes <- as.factor(trainData_1$Flight_time_in_minutes)
trainData_1$Flight_Distance <- createBuckets(trainData_1$Flight_Distance)
trainData_1$Flight_Distance <- as.factor(trainData_1$Flight_Distance)
trainData_1$Scheduled_Departure_Hour <- CreateBucketsSDH(trainData_1$Scheduled_Departure_Hour)
trainData_1$Scheduled_Departure_Hour <- as.factor(trainData_1$Scheduled_Departure_Hour)

testData_1$Price_Sensitivity <- createBucketsPrice(testData_1$Price_Sensitivity)
testData_1$Price_Sensitivity <- as.factor(testData_1$Price_Sensitivity)
testData_1$Age <- createBuckets(testData_1$Age)
testData_1$Age <- as.factor(testData_1$Age)
testData_1$No_of_other_Loyalty_Cards <- createBuckets(testData_1$No_of_other_Loyalty_Cards)
testData_1$No_of_other_Loyalty_Cards <- as.factor(testData_1$No_of_other_Loyalty_Cards)
testData_1$Shopping_Amount_at_Airport <- createBuckets(testData_1$Shopping_Amount_at_Airport)
testData_1$Shopping_Amount_at_Airport <- as.factor(testData_1$Shopping_Amount_at_Airport)
testData_1$No_of_Flights_per_annum <- createBucketsFPA(testData_1$No_of_Flights_per_annum)
testData_1$No_of_Flights_per_annum <- as.factor(testData_1$No_of_Flights_per_annum)
testData_1$Percent_of_Flight_with_other_Airlines <- CreateBucketsFWOA(testData_1$Percent_of_Flight_with_other_Airlines)
testData_1$Percent_of_Flight_with_other_Airlines <- as.factor(testData_1$Percent_of_Flight_with_other_Airlines)
testData_1$Eating_and_Drinking_at_Airport <- createBuckets(testData_1$Eating_and_Drinking_at_Airport)
testData_1$Eating_and_Drinking_at_Airport <- as.factor(testData_1$Eating_and_Drinking_at_Airport)
testData_1$Departure_Delay_in_Minutes <- createBuckets(testData_1$Departure_Delay_in_Minutes)
testData_1$Departure_Delay_in_Minutes <- as.factor(testData_1$Departure_Delay_in_Minutes)
testData_1$Arrival_Delay_in_Minutes <- createBuckets(testData_1$Arrival_Delay_in_Minutes)
testData_1$Arrival_Delay_in_Minutes <- as.factor(testData_1$Arrival_Delay_in_Minutes)
testData_1$Flight_time_in_minutes <- createBuckets(testData_1$Flight_time_in_minutes)
testData_1$Flight_time_in_minutes <- as.factor(testData_1$Flight_time_in_minutes)
testData_1$Flight_Distance <- createBuckets(testData_1$Flight_Distance)
testData_1$Flight_Distance <- as.factor(testData_1$Flight_Distance)
testData_1$Scheduled_Departure_Hour <- CreateBucketsSDH(testData_1$Scheduled_Departure_Hour)
testData_1$Scheduled_Departure_Hour <- as.factor(testData_1$Scheduled_Departure_Hour)




#For the complete data set
svmModOutput_all_1 <- ksvm(lowSat~., data=trainData_1, kernel= "rbfdot", kpar = "automatic", C = 50, cross = 3, prob.model = TRUE)
svmModOutput_all_1
hist(alpha(svmModOutput_all_1)[[1]])
svmPred_1 <- predict(svmModOutput_all_1, testData_1)
conf_mat_1 <- confusion_matrix(testData$lowSat, svmPred_reg)
conf_mat_1
Accuracy(conf_mat_1)
#Airline_Status +Gender + No_of_Flights_per_annum+Type_of_Travel+ Flight_cancelled+ Arrival_Delay_greater_5_Mins

#For Significant variables from multiple regression
svmModOutput_1 <- ksvm(lowSat~Type_of_Travel+Flight_time_in_minutes+Shopping_Amount_at_Airport+Airline_Status+Departure_Delay_in_Minutes+Arrival_Delay_in_Minutes+Eating_and_Drinking_at_Airport, data=trainData_1, kernel= "rbfdot", kpar = "automatic", C = 50, cross = 3, prob.model = TRUE)
svmModOutput_1
hist(alpha(svmModOutput_1)[[1]])
svmPred_1 <- predict(svmModOutput_all_1, testData_1)
conf_mat_1 <- confusion_matrix(testData$lowSat, svmPred_reg)
conf_mat_1
Accuracy(conf_mat_1)

class(svmModOutput)

M <- fit(lowSat~., data=trainData_1, model="svm", kpar=list(sigma=0.10), C=2)
VariableImportance <- Importance(M, data=trainData_1, method = "sensv")

L=list(runs=1,sen=t(VariableImportance$imp),sresponses=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(trainData_1),col="gray",Grid=10)













