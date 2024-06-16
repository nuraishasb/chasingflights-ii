
## INTRODUCTION

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readr)

# import csv files
Y1989 <- read_csv("Chasing Flights/1989.csv")
Y1990 <- read_csv("Chasing Flights/1990.csv")
Y1994 <- read_csv("Chasing Flights/1994.csv")
Y1995 <- read_csv("Chasing Flights/1995.csv")
Y1996 <- read_csv("Chasing Flights/1996.csv")
Y2000 <- read_csv("Chasing Flights/2000.csv")
Y2001 <- read_csv("Chasing Flights/2001.csv")
Y2002 <- read_csv("Chasing Flights/2002.csv")
Y2006 <- read_csv("Chasing Flights/2006.csv")
Y2007 <- read_csv("Chasing Flights/2007.csv")

# combine them, by rows, into one database
Database <- do.call("rbind", list(Y1989, Y1990, Y1994, Y1995, Y1996, Y2000, Y2001, Y2002, Y2006, Y2007))

# perform Exploratory Data Analysis (EDA)
str(Database)
head(Database, 5)

# remove irrelevant columns
Database <- Database[-c(12:14,19:21,23,25:29)]

# finding NA values
colSums(is.na(Database)) 

# omitting NA values
Database <- na.omit(Database)

# checking for remaining NA values
colSums(is.na(Database))

#------------------------------
  
## OPTIMAL SCHEDULE

# filtering the non-delayed flights
nondel_flights <- subset(Database, Database$ArrDelay < 15 & Database$DepDelay < 15) 

# finding the monthly frequency
freq_month <- table(nondel_flights$Month)

# creating a data frame
nondel_month <- data.frame(Month = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec"),
                           Freq = c(freq_month))
# graph
month_chart <- ggplot(nondel_month, aes(x=reorder(Month, desc(Freq)), y=Freq)) +  #Assigning data to the plot, and columns to their respective axes and rearranging the values in descending order
  ggtitle("Month") +  #Naming the plot
  geom_bar(stat = "identity", fill=rgb(0.8,0.5,0.0,1)) +  #Assigning the value and color of the bars in the plot
  theme(axis.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5)) + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) #Changing the aesthetics of the plot


# finding the daily frequency
freq_day <- table(nondel_flights$DayOfWeek)

# creating a data frame
nondel_day <- data.frame(Day = c("Monday","Tuesday",'Wednesday','Thursday',"Friday","Saturday","Sunday"), Freq = c(freq_day))

# graph
day_chart <- ggplot(nondel_day, aes(x=reorder(Day, desc(Freq)), y=Freq)) + ggtitle("Day") +
  geom_bar(stat = "identity", fill=rgb(0.8,0.5,0.0,1)) + theme(axis.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                                               plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5)) + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))


# filtering the data based on the CRS Departure Time
Morning <- subset(nondel_flights$CRSDepTime, nondel_flights$CRSDepTime >= 400 & nondel_flights$CRSDepTime < 1200)
Afternoon <- subset(nondel_flights$CRSDepTime, nondel_flights$CRSDepTime >= 1200 & nondel_flights$CRSDepTime < 2000)
Night <- subset(nondel_flights$CRSDepTime, nondel_flights$CRSDepTime < 400 | nondel_flights$CRSDepTime >= 2000)

# creating a new data frame of the quantity of flights per time interval
nondel_time <- data.frame(
  group=c("Morning", "Afternoon", "Night"),
  value=c(length(Morning), length(Afternoon), length(Night)))

# graph
time_chart <- ggplot(nondel_time, aes(x=reorder(group, desc(value)), y=value)) + ggtitle("Time") +
  geom_bar(stat = "identity", fill=rgb(0.8,0.5,0.0,1)) + theme(axis.title = element_blank(), panel.grid.major = element_blank(),
                                                               panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y=element_blank(),
                                                               axis.ticks.y=element_blank(), plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5)) + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

# combining graphs
ggarrange(month_chart, day_chart, time_chart, ncol = 3)

#------------------------------

## EFFICIENCY OF OLD PLANES

# filtering the data
del_flights <- subset(Database, Database$ArrDelay > 15 | Database$DepDelay > 15)

# finding overall total number of flights recorded each year
totalobs <- Database %>% group_by(Year) %>% summarise(totalobs = n())

# finding the total number of delayed flights each year
delflights_total <- del_flights %>% group_by(Year) %>% summarise(delflights = n())

# merging the data frames
delflights_total <- merge(delflights_total, totalobs, by = "Year")

# calculating proportion
delflights_total$proportion <- delflights_total$delflights/delflights_total$totalobs

# graph
delflights_total %>%
  ggplot(aes(x= factor(Year), y= proportion, group = 1)) +
  ggtitle("Proportion of Delayed Flights Over Time") +
  geom_line(color="black") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  theme(axis.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(color="black", size=14, face="bold"))

#------------------------------
  
## FLIGHT ROUTES

# finding the number of flights departed from each airport of origin
totaldep <- Database %>%
  group_by(Origin) %>%
  summarise(freqdep = n())

# finding the number of flights arriving at each destination airport
totalarr <- Database %>%
  group_by(Dest) %>%
  summarise(freqarr = n()) %>%
  rename(Origin = Dest)

# creating a new data frame for total flight movements
totalmovement <- full_join(totaldep, totalarr, by = "Origin") %>%
  replace_na(list(freqdep = 0, freqarr = 0))
totalmovement$total <- rowSums(totalmovement[, c("freqdep", "freqarr")])

# arranging the values in descending order and taking the top 30
majorairports <- totalmovement %>%
  arrange(desc(total)) %>%
  head(30) %>%
  pull(Origin)

# finding the number of flights departed from each airport of origin every year
totaldepflights <- Database %>%
  group_by(Origin, Year) %>%
  summarise(depflights = n())

# finding the number of flights arriving at each destination airport every year
totalarrflights <- Database %>%
  group_by(Dest, Year) %>%
  summarise(arrflights = n()) %>%
  rename(Origin = Dest)

# creating a new data frame for yearly flight movements
airportmovement <- full_join(totaldepflights, totalarrflights, by = c("Origin", "Year")) %>%
  replace_na(list(depflights = 0, arrflights = 0))
airportmovement$totalflights <- rowSums(airportmovement[, c("depflights", "arrflights")])

# merging with total observations and calculating the proportion
totalobs <- Database %>%
  group_by(Year) %>%
  summarise(totalobs = n())
airportmovement <- merge(airportmovement, totalobs, by = "Year")
airportmovement$proportion <- airportmovement$totalflights / airportmovement$totalobs

# filtering to major airports
airportmovement <- airportmovement %>%
  filter(Origin %in% majorairports)

# graph
ggplot(airportmovement, aes(factor(Year), Origin, fill = proportion)) +
  ggtitle("Frequency of Flights Across Major Airports Over Time") +
  geom_tile() +
  scale_fill_gradient(low = "orange", high = "darkred", name = "Ratio") +
  theme(axis.title = element_blank(), plot.title = element_text(color = "black", size = 14, face = "bold"))

#------------------------------

## CASCADING FAILURES

# forming a column for date of flight
del_flights$date <- as.Date(with(del_flights, paste(DayofMonth, Month, Year, sep="-")), "%d-%m-%Y")

# selecting a random observation
set.seed(7)
randomobs <- sample(1:nrow(del_flights), 1)
x <- del_flights[randomobs, ]

# finding flights with the same flight number, tail number, and flight date
E2 <- del_flights[del_flights$FlightNum == x$FlightNum & del_flights$TailNum == x$TailNum & del_flights$date == x$date, ]

# forming filtered data frames for actual times
act_arrtime <- data.frame(Airport = E2$Dest, Hour = E2$ArrTime)
act_deptime <- data.frame(Airport = E2$Origin, Hour = E2$DepTime)
act_time <- rbind(act_arrtime, act_deptime) %>%
  arrange(Hour)

# forming filtered data frames for estimated times
est_arrtime <- data.frame(Airport = E2$Dest, Hour = E2$CRSArrTime)
est_deptime <- data.frame(Airport = E2$Origin, Hour = E2$CRSDepTime)
est_time <- rbind(est_arrtime, est_deptime) %>%
  arrange(Hour)

# graph
ggplot() + 
  ggtitle("Example of a cascading failure due to a delay") +
  geom_point(data = act_time, aes(x = Hour, y = factor(Airport), color = 'Actual time'), size = 3) +
  geom_point(data = est_time, aes(x = Hour, y = factor(Airport), color = 'Estimated time'), size = 3) +
  labs(color = NULL) + 
  theme(axis.title = element_blank(),
        plot.title = element_text(color = "black", size = 14, face = "bold"),
        legend.position = c(0.8, 0.2),
        legend.direction = "horizontal")


#------------------------------

## PREDICTING DELAYS

# filter to top 30 airport
filtered_database <- Database %>% filter(Origin %in% majorairports | Dest %in% majorairports)

# finding sample size
#parameters
confidence_level <- 0.95
margin_of_error <- 0.01
population_size <- 7453215
p <- 0.5

# Z-value for 95% confidence
z <- qnorm(1 - (1 - confidence_level) / 2)

# Initial sample size calculation
n <- (z^2 * p * (1 - p)) / (margin_of_error^2)

# Adjusted sample size calculation
n_adj <- n / (1 + ((n - 1) / population_size))

# Print results
cat("Initial sample size: ", round(n), "\n")
cat("Adjusted sample size: ", round(n_adj), "\n")

#take adjusted sample size for each year
sampled_database <- Database %>% group_by(Year) %>% sample_n(9591, replace = TRUE)

# predicting the minutes of delay based on Day of flight, Time of flight, Month of flight and from the top 30 airport
# create a column for total delay 
sampled_database$TotalDelay <- rowSums(cbind(sampled_database$ArrDelay,sampled_database$DepDelay))

#Assigning the 75% of said data to 'train', and remaining 25% to 'test'
set.seed(101)
Data <- sample(nrow(sampled_database), size = floor(.75*nrow(sampled_database)), replace = F)
train <- sampled_database[Data, ]
test  <- sampled_database[-Data, ]

# regression model
model <- lm(TotalDelay ~ Month + DayOfWeek + CRSDepTime , data = train)
summary(model)

#Testing the model
prediction <- predict(model, test)

#finding the mean squared error (MSE)
mean((test$TotalDelay - prediction)^2)

