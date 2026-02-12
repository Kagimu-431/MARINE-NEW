library(readxl)
df<- read_excel("C:/Users/KAGIMU MOSES/Marine_Manifests_Dataset_Updated from Sept 2025.xlsx")
library(dplyr)
library(readr)
df$`no. of male_adults`<- parse_number(df$`no. of male_adults`)
# replace all non numeric values with numeric values by parsing those with strings and nil with zero
df <- df %>%
  mutate(
    across(
      c(`no. of male_adults`,
        `no of female_adults`,
        `no. of male_juveniles`,
        `no. of female_juveniles`),
      ~ parse_number(as.character(ifelse(tolower(.x) == "nil", 0, .x)))
    )
  )
str(df)
#convert Date column to date format
df$DATE<- as.Date(df$DATE, format= "%d/%m/%Y")
#convert all expected numeric columns to numeric 
df<- df %>%
  mutate(across(c(`no. of male_adults`,`no. of male_juveniles`,`no. of female_juveniles`,`no of female_adults`),as.numeric))
str(df)
#Create a new column called GRAND_TOTAL
df <- df %>%
  mutate(
    GRAND_TOTAL = `no. of male_adults` +
      `no of female_adults` +
      `no. of male_juveniles` +
      `no. of female_juveniles`
  )
#check whether GRAND_TOTAL has been created
str(df)
#convert zone column to factor
df$ZONE<- factor(df$ZONE)
unique(df$DETACH)
df$DETACH<- toupper(df$DETACH)
# Replace each value with its first word
df$DETACH <- sapply(strsplit(df$DETACH, " "), `[`, 1)
#convert DETACH column to factor
df$DETACH<- factor(df$DETACH)
df$DETACH
df$Route<- toupper(df$Route)
summary(df)
#Filter rows from oct to Dec 2025
df_filtered <- df %>%
  filter(DATE >= as.Date("2025-10-01") & DATE <= as.Date("2025-12-31"))
class(df$DATE)
#Detach with highest number of passengers
df_filtered_groupby_daetach<-df_filtered %>%
  group_by(DETACH) %>%
  summarise(total_passengers = sum(GRAND_TOTAL, na.rm = TRUE)) %>%
  arrange(desc(total_passengers))

head(df_filtered_groupby_daetach, 5)
#create datset from dataset called df_filtered_groupby_daetach
df_subset<- df_filtered_groupby_daetach[1:5, ]
#Bar plot of the first five detaches
barplot(df_subset$total_passengers, names.arg= df_subset$DETACH, col= "steelblue",main="DETACHMENTS RECEIVING MOST WATER PASSENGERS")
#Filter by Date
df_filtered_groupyby_date<- df_filtered %>%
  group_by(DATE) %>%
  summarise(total_passengers= sum(GRAND_TOTAL, na.rm = TRUE))
#Average number of passengers who move daily.
mean(df_filtered_groupyby_date$total_passengers)
#Group by Destination
df_filtered_groupby_Destination<- df_filtered %>%
  group_by(`Destination Category`) %>%
  summarise(total_passengers= sum(GRAND_TOTAL))
#Bar plot showing Destination
barplot(df_filtered_groupby_Destination$total_passengers, names.arg = df_filtered_groupby_Destination$`Destination Category`, col = "red", main= "NUMBER OF PASSENGERS PER DESTINATION")
#GROUP BY ROUTE
df_filtered_groupby_Route<- df_filtered %>%
  group_by(Route) %>%
  summarise(total_passengers = sum(GRAND_TOTAL)) %>%
  arrange(desc(total_passengers))
#clean data frame for route.
df_filtered_groupby_Route_clean <- df_filtered_groupby_Route%>%
  filter(!Route %in% c("WATER TRANSIT"))
df_subset_route<- df_filtered_groupby_Route_clean[1:3, ]
#Bar plot showing movement per route for the first five
barplot(df_subset_route$total_passengers, names.arg= df_subset_route$Route, main= "THE BUSIEST ROUTE")
df$ZONE<- factor(df$ZONE)
barplot(df$GRAND_TOTAL, names.arg= df$ZONE, col= "red")
df$ZONE
#Group by zone using df_filtered(oct-Dec 2025)
df_zone <- df_filtered %>%
  group_by(ZONE) %>%
  summarise(total_passengers = sum(GRAND_TOTAL, na.rm= TRUE)) %>%
  arrange(desc(total_passengers))
barplot(df_zone$total_passengers, names.arg = df_zone$ZONE, col="red",width= c(1,1,1,1,1,1,1,1,1), main = "FREQUENCY OF PASSENGERS PER ZONE")
df_zone_filtered <- df_zone[1:4, ]
barplot(df_zone_filtered$total_passengers, names.arg = df_zone_filtered$ZONE, col = "red", main = "FREQUENCY OF PASSENGERS PER ZONE")
