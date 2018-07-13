Crime <- read.csv("Interview presentation/Crime_Incidents.csv", header=TRUE, na.strings = c(""))
#check structure of dataset
str(Crime)
#transform dataset variables
Crime$City <- as.character(Crime$City)
Crime$State <- as.character(Crime$State)
Crime$Location <- as.character(Crime$Location)
Crime$Incident.Type <- as.character(Crime$Incident.Type)
#Transform date and time variable for report and offencee with lubridate
library(lubridate)
Report.Date <- mdy_hms(as.character(Crime$Report.Date))
Crime$ReportDate <- Report.Date
Offense.Date <- mdy_hms(as.character(Crime$Offense.Date))
Crime$OffenseDate <- Offense.Date

#Create new variabes with weekdays for offence adn report date and time
library(lubridate)
Crime$ReportWeekdays <- weekdays(Crime$ReportDate)
Crime$OffenceWeekdays <- weekdays(Crime$OffenseDate)

#create new variables with year month
Crime$ReportYear <- year(Crime$ReportDate)
Crime$ReportMonth <- month(Crime$ReportDate)
Crime$ReportHour <- hour(Crime$ReportDate)

Crime$OffenceYear <- year(Crime$OffenseDate)
Crime$Offencemonth <- month(Crime$OffenseDate)
Crime$Offencehour <- hour(Crime$OffenseDate)

#split hour into overnight, morning,afternoon,evening for offense and report
Crime$RepTimeOfDay <- cut(Crime$ReportHour, 
                        c(0, 6, 12, 18, 24), 
                        labels = c("Overnight", "Morning", "Afternoon", "Evening"),
                        right = FALSE)
Crime$offTimeOfDay <- cut(Crime$ReportHour, 
                          c(0, 6, 12, 18, 24), 
                          labels = c("Overnight", "Morning", "Afternoon", "Evening"),
                          right = FALSE)

#glimpse at the data by time period classification
table(Crime$ReportHour, Crime$RepTimeOfDay)

#transform location variable
class(Crime$Location)
Crime$Location <- as.character(Crime$Location)

#remove geolocalization from adress and add to raw data.
location1 <- gsub("\\s*\\([^\\)]+\\)","",as.character(Crime$Location))
location2 <- gsub(".*\\((.*)\\).*", "\\1", as.character(Crime$Location))

#Save as a data frame 
location2 <- as.data.frame(location2)
location1 <- as.data.frame(location1)

# write a csv file for adress and geolocalization
write.csv(location1,"location1.csv")
write.csv(location2,"location2.csv")

#combine data location with crime
location1 <- read.csv("Interview presentation/location1.csv", header=FALSE, na.strings = c(""))
location2 <- read.csv("Interview presentation/location2.csv",header=FALSE, na.strings = c(""))
Crime$lat <- location2$V1
Crime$long <- location2$V2
Crime$Adress <- location1$V1

#Create new dataset with relevant variables
Newcrime <- subset(Crime,select = c(2,9,11,15,16,17,19,20,21,22))

#trend of incident per year
library(dplyr)
library(ggplot2)
Yearly = group_by(Newcrime,OffenceYear)
Yearly_counts = summarise(Yearly, count = n())

ggplot(Yearly_counts, aes(x = OffenceYear, y = count)) + geom_point(colour = "red") + 
  geom_line(colour = "red", size = 1.5) + xlim(2011,2018) +
  theme_light(base_size = 12) + xlab("Years") + ylab("Count of indicents") + 
  ggtitle("The Number of Incidents Per Year in Gainesville") + 
  theme(plot.title=element_text(size=16)) 

#Trend of incident by month
Monthly = group_by(Newcrime,Offencemonth)
Monthly_counts = summarise(Monthly, count = n())

ggplot(Monthly_counts, aes(x = Offencemonth, y = count)) + geom_point(colour = "red") + 
  geom_line(colour = "red", size = 1.5) + xlim(1,12) + scale_x_continuous(breaks=c(0:12)) +
  theme_light(base_size = 12) + xlab("Months") + ylab("Count of indicents") + 
  ggtitle("The Number of Incidents Per Month in Gainesville") + 
  theme(plot.title=element_text(size=16)) 

#trends by days per month
Newcrime$Offenceday <- day(Newcrime$OffenseDate)
weekdays1 = group_by(Newcrime,Offencemonth, Offenceday)
Weekdays_counts1 = summarise(weekdays1, count = n()) 

ggplot(Weekdays_counts1, aes(x = Offenceday, y = count)) +
  geom_bar(position = "dodge",stat = "identity", fill = "light blue",col = "red") +
  facet_grid(.~ Offencemonth)+ theme_light(base_size = 12) + xlab("Days") + ylab("Count of indicents") +
  ggtitle("The Number of Incidents by Day and Month in Gainesville") + 
  theme(plot.title=element_text(size=16))

#convert days as a factor
Newcrime$OffenceWeekdays <- as.factor(Newcrime$OffenceWeekdays)

#Trends by Days
Weekdays = group_by(Newcrime,OffenceWeekdays)
Weekdays_counts = summarise(Weekdays, count = n())

  ggplot(Weekdays_counts, aes(x = reorder(OffenceWeekdays,- count), y = count)) +
  geom_bar(position = "dodge",stat = "identity", fill = "light blue",col = "red", width = 0.5) +
  theme_light(base_size = 12) + xlab("Days") + ylab("Count of indicents") + 
  ggtitle("The Number of Incidents by Day in Gainesville") + 
  theme(plot.title=element_text(size=16)) 


  #trends by time of days
  Newcrime$Offenceday <- day(Newcrime$OffenseDate)
  offencebyhour = group_by(Newcrime,Offencehour)
  offensebyhour_counts = summarise(offencebyhour, count = n())
  
  ggplot(offensebyhour_counts, aes(x = Offencehour, y = count)) +
    geom_bar(position = "dodge",stat = "identity", fill = "#FF9933", width = 0.7) +
    theme_light(base_size = 12) + xlab("Hours Of The Days") + ylab("Count of indicents") + 
    scale_x_continuous(breaks=c(0:23))
    ggtitle("The Number of Incidents by Hour in Gainesville") + 
    theme(plot.title=element_text(size=16))
   
 #trends by time of categorize days
 offencebycath = group_by(Newcrime,offTimeOfDay)
 offensebycath_counts = summarise(offencebycath, count = n())

  ggplot(offensebycath_counts, aes(x = offTimeOfDay, y = count)) +
      geom_bar(position = "dodge",stat = "identity", fill = "#FF9933", width = 0.5) +
      theme_light(base_size = 12) + xlab("Hours Categories") + ylab("Count of indicents")
    ggtitle("The Number of Incidents by Hour Categories in Gainesville") + 
      theme(plot.title=element_text(size=16))
 
    #plot by indentity type
    #indentity type have 220 factors, hard to clearly see if graph, so let group them
  incidenttype= group_by(Newcrime,Incident.Type)
  incidenttype_counts = summarise(incidenttype, count = n())

  write.csv(incidenttype_counts,"incident.csv")
  
  #Grouping incidents by categories 
  Newcrime$Incident.Type <- as.character(Newcrime$Incident.Type)
   categories <- function(Incident.Type) {
    
    if (length(grep("ASSAULT", Incident.Type)) > 0){
      return ("ASSAULT")
    } else if (length(grep("BATTERY", Incident.Type)) > 0) {
      return ("ASSAULT")
    } else if (length(grep("DATING", Incident.Type)) > 0) {
      return ("ASSAULT")
    } else if (length(grep("CYBER", Incident.Type)) > 0) {
      return ("ASSAULT")
    } else if (length(grep("STALKING", Incident.Type)) > 0) {
        return ("ASSAULT")
    } else if (length(grep("FALSE", Incident.Type)) > 0) {
      return ("ASSAULT")
    } else if (length(grep("DRIVING UNDER", Incident.Type)) > 0) {
      return ("DRIVING UNDER THE INFLUENCE")
    } else if (length(grep("BURGLARY", Incident.Type)) > 0) {
      return ("BURGLARY")
    } else if (length(grep("THEFT", Incident.Type)) > 0) {
      return ("LARCENY/THEFT")
    } else if (length(grep("ARSON", Incident.Type)) > 0) {
      return ("ARSON")
    } else if (length(grep("DOMESTIC", Incident.Type)) > 0) {
      return ("DOMESTIC")
    } else if (length(grep("WARRANTS", Incident.Type)) > 0) {
      return ("WARRANT ARREST")
    } else if (length(grep("TRESPASS WARNING", Incident.Type)) > 0) {
      return ("TRESPASSING")
    } else if (length(grep("ALCOHOL", Incident.Type)) > 0) {
      return ("LIQUOR LAW")
    } else if (length(grep("LIQUOR", Incident.Type)) > 0) {
      return ("LIQUOR LAW")
    } else if (length(grep("MISSING PERSON", Incident.Type)) > 0) {
      return ("MISSING PERSON")
    } else if (length(grep("FIRE", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("SICK", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("ASSIST", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("INFORMATION", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("DCF", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("DEATH", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("FOUND", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("INJURED", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("RECOVERED", Incident.Type)) > 0) {
      return ("NON INCIDENT")
    } else if (length(grep("DISCHARGE", Incident.Type)) > 0) {
      return ("WEAPONS LAW")
    } else if (length(grep("WEAPONS", Incident.Type)) > 0) {
      return ("WEAPONS LAW")
    } else if (length(grep("EXPLOSIVE", Incident.Type)) > 0) {
      return ("WEAPONS LAW")
    } else if (length(grep("SHOOTING", Incident.Type)) > 0) {
      return ("WEAPONS LAW")
    } else if (length(grep("GAMBLING", Incident.Type)) > 0) {
      return ("GAMBLING/MONEY LAUNDERING")
    } else if (length(grep("MONEY", Incident.Type)) > 0) {
      return ("GAMBLING/MONEY LAUNDERING")
    } else if (length(grep("PORNOGRAPHY", Incident.Type)) > 0) {
      return ("PORNOGRAPHY/PROSTITUTION")
    } else if (length(grep("PROSTITUTION", Incident.Type)) > 0) {
      return ("PROSTITUTION/PORNOGRAPHY")
    } else if (length(grep("DAMAGE", Incident.Type)) > 0) {
      return ("DAMAGE TO PROPERTY")
    } else if (length(grep("CRIMINAL", Incident.Type)) > 0) {
      return ("CRIMINAL OFFENSE")
    } else if (length(grep("CRIMES", Incident.Type)) > 0) {
      return ("CRIMINAL OFFENSE")
    } else if (length(grep("HOMICIDE", Incident.Type)) > 0) {
      return ("CRIMINAL OFFENSE")
    } else if (length(grep("ROBBERY", Incident.Type)) > 0) {
      return ("ROBERRY")
    } else if (length(grep("FRAUD", Incident.Type)) > 0) {
      return ("FRAUD")
    } else if (length(grep("STOLEN", Incident.Type)) > 0) {
      return ("STOLEN PROPERTY")
    } else if (length(grep("LOST", Incident.Type)) > 0) {
      return ("STOLEN PROPERTY")
    } else if (length(grep("RUNAWAY", Incident.Type)) > 0) {
      return ("RUNAWAY")
    } else if (length(grep("HIT", Incident.Type)) > 0) {
      return ("RUNAWAY")
    } else if (length(grep("SEX OFFENDER", Incident.Type)) > 0) {
      return ("SEX OFFENDER")
    } else if (length(grep("SUSPICIOUS", Incident.Type)) > 0) {
      return ("SUSPICIOUS INCIDENT")
    } else if (length(grep("DRUG", Incident.Type)) > 0) {
      return ("DRUG/NARCOTIC")
    } else if (length(grep("TOBACCO", Incident.Type)) > 0) {
      return ("DRUG/NARCOTIC")
    } else if (length(grep("CANNABIS", Incident.Type)) > 0) {
      return ("DRUG/NARCOTIC")
    } else {
      return ("OTHER OFFENCE")
    }
  }
  categories1<- NULL
  for (i in 1:nrow(Newcrime)) {
    categories1 <-c(categories1,categories(Newcrime[i,"Incident.Type"]))
  }
  Newcrime$categories1<-as.factor(categories1)
  str(Newcrime$categories1)
  write.csv(Newcrime,"incidentCategories.csv")
  
  #plot incident categories to see which one occur often
  total.category <- table(Newcrime$categories1)
  order.category <- order(total.category, decreasing = TRUE)
  Newcrime$categories1 <- factor(Newcrime$categories1, levels = names(total.category[order.category]))
  
   ggplot(Newcrime, aes(categories1)) +
    geom_bar(aes(fill = categories1)) +
    scale_y_continuous() +
    scale_x_discrete(limits = levels(Newcrime$categories1)[24:1]) +
    theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  
   #The number of incidents: Hours vs Category
   incidentcategories= group_by(Newcrime,Offencehour, categories1)
   incidentcategories_counts = summarise(incidentcategories, count = n())
   
   ggplot(incidentcategories_counts, aes(x = Offencehour, y = categories1)) + 
     geom_tile(aes(fill = count)) + 
     scale_fill_gradient(name = "Counts", low = "white", high = "red") +
     scale_x_continuous(breaks=c(0:23)) + 
     theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
     theme(plot.title = element_text(size=16)) + 
     ggtitle("The number of incidents: Hour vs. Category")
   
   #The number of incident: category by hour ratio of total
   ratio.Hour <- Newcrime %>%
     group_by(categories1, Offencehour) %>%
     summarize(count = n()) %>%
     group_by(Offencehour) %>%
     mutate (hc = sum(count)) %>%
     ungroup() %>%
     mutate (ratio = count / hc)
   
   ggplot(ratio.Hour, aes(Offencehour, ratio)) + 
     geom_line(aes(colour = categories1)) + 
     facet_wrap(~ categories1, ncol = 6, scales = "free_y") +
     scale_x_continuous(breaks=c(0,5,10,15,20)) +
     expand_limits(y = 0) + 
     theme(legend.position="none")
   
  #LARCENY/THEFT OVERTIME
   library(magrittr)
   Newcrime_theft <- Newcrime %>% filter(grepl("LARCENY/THEFT", categories1))
   Newcrime_theft_daily <- Newcrime_theft %>%
     group_by(OffenceYear) %>%
     summarize(count = n()) %>%
     arrange(OffenceYear)
   
   library(ggplot2)
   library(scales)
   plot <- ggplot(Newcrime_theft_daily, aes(x = OffenceYear, y = count)) +
     geom_smooth(color = "blue") +
     xlim(2011,2018) +
     labs(x = "Year of LARCENY/THEFT", y = "Number of LARCENY/THEFT", title = "LARCENY/THEFT in Gainesville by Year")
   plot
  
   #Number of LARCENY/THEFT by Time and Day
   Newcrime_theft_Time <- Newcrime_theft %>%
     group_by(Offencehour, OffenceWeekdays)%>%
     summarize(count = n()) 

   plot <- ggplot(Newcrime_theft_Time, aes(x = Offencehour, y = OffenceWeekdays , fill = count)) +
     geom_tile() +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.6), 
           legend.title = element_blank(), legend.position ="top", 
           legend.direction="horizontal", legend.key.width=unit(2, "cm"), 
           legend.key.height=unit(0.25, "cm"), 
           legend.spacing = unit(-0.5,"cm"), panel.spacing = element_blank()) +
     scale_x_continuous(breaks =c(0:23)) + 
     labs(x = "Hour of LARCENY/THEFT", y = "Day of Week of LARCENY/THEFT", 
          title = "Number of LARCENY/THEFT in Gainesville, by Time") +
     scale_fill_gradient(low = "white", high = "#27AE60", labels = comma)
   plot
  
  # Number of LARCENCY/THEFT BY MONTH 
   Newcrime_theft_month <- Newcrime_theft %>%
     group_by(Offencemonth)%>%
     summarize(count = n()) 
     
   ggplot(Newcrime_theft_month, aes(x = Offencemonth, y = count)) +
     geom_bar(position = "dodge",stat = "identity", fill = "green",col = "red", width = 0.8) + 
     theme_light(base_size = 12) + xlab("Month") + ylab("Count of Larcency/Theft") + 
     scale_x_continuous(breaks=c(0:12)) +
     ggtitle("The Number of Larcency/Theft by Month in Gainesville") + 
     theme(plot.title=element_text(size=16))
   #remove outliers from the gelocation    
   Newcrime_theft.1 <- subset(Newcrime_theft,Newcrime_theft$lat<30)
   Newcrime_theft.2 <- subset(Newcrime_theft.1,Newcrime_theft.1$lat>29)
   Newcrime_theft.3 <- subset(Newcrime_theft.2,Newcrime_theft.2$lat<29.8)
   Newcrime_theft.4 <- subset(Newcrime_theft.3,Newcrime_theft.3$long > -82.6)
   Newcrime_theft.5 <- subset(Newcrime_theft.4,Newcrime_theft.4$long < -82.1)
   Newcrime_theft.6 <- subset(Newcrime_theft.5,Newcrime_theft.5$lat < 29.775)
   Newcrime_theft.7 <- subset(Newcrime_theft.6,Newcrime_theft.6$long > -82.45)

   #Number of LARCENCY/Theft By week 
   ggplot(Newcrime_theft.7, aes(x = long, y = lat)) + geom_point(aes(colour = factor(OffenceWeekdays)), size = 1.25) + 
     theme_light(base_size = 10) + xlab("X") + ylab("Y") +
     ggtitle("Day of Week") + theme(plot.title=element_text(size = 16))
   
   #Number of LARCENCY/Theft By geolocation 
   library(ggmap)
   qmplot(long,lat,data=Newcrime_theft.7,maptype = "toner-lite",color = I("red"))+geom_density_2d()
   
   #plot mapcrimr and zoom on univesity ave
   mapcrime <- qmap(location = "Gainesville",zoom = 14)
   
   mapcrime +
     geom_point(data = Newcrime_theft.7, aes(x = Newcrime_theft.7$long, y = Newcrime_theft.7$lat, fill = "red", alpha = 0.8), size = 1, shape = 21) +
     guides(fill=FALSE, alpha=FALSE, size=FALSE)
