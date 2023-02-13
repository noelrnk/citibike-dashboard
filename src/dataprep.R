
# The following script includes:

#   1) Connection to the SQL Server
#   2) Data loading from the SQL Server and creation of .RData files which can be reloaded and used in other scripts within the environment
#   3) Data overview


###########################################################################################################
# 1) Connection to SQL Server
###########################################################################################################

# Link to video for R to SQL Server connection with RODBC
# https://www.youtube.com/watch?v=PoA23UWvXuc

if (!require("RODBC")) install.packages("RODBC", dependencies=TRUE)
if (!require("odbc")) install.packages("odbc", dependencies=TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)

library(RODBC)
library(odbc)
library(dplyr)
library(ggplot2)
library(data.table)

# Open connection to SQL Server using Windows ODBC DSN
db_conn <- odbcConnect("DBM-DSN", uid="sa", pwd=rstudioapi::askForPassword("Database password"))
# More options for pw entry: https://db.rstudio.com/best-practices/managing-credentials/


###########################################################################################################
# 2) Data loading from SQL
###########################################################################################################

# Queries:
#    2a) Select sample of Citi Bike rides (random sample of each data table)
#    2b) Select weather data table
#    2c) Select statement to get the number of daily rides combined with the average temperature and the precipitation

###########################################################################
# 2a) Select sample of Citi Bike rides (random sample of each data table)

statement_list <- list() # List which will be used in the for loop below
# For-loop to create the SQL statements for data table selection
for (table in c("dbo.citibike0118", "dbo.citibike0218", "dbo.citibike0318", "dbo.citibike0418", "dbo.citibike0518", "dbo.citibike0618", 
                "dbo.citibike0718", "dbo.citibike0818", "dbo.citibike0918", "dbo.citibike1018", "dbo.citibike1118", "dbo.citibike1218",
                "dbo.citibike0119", "dbo.citibike0219", "dbo.citibike0319", "dbo.citibike0419", "dbo.citibike0519", "dbo.citibike0619",
                "dbo.citibike0719")) {
  table <- paste0("SELECT * FROM ",table," WHERE 0.0005 >= CAST(CHECKSUM(NEWID(), starttime) & 0x7fffffff AS float) / CAST (0x7fffffff AS int);")
  statement_list <- append(statement_list, table)
}

# Create dataframes with SQL queries created above
cb_0118 <- sqlQuery(db_conn, statement_list[1], stringsAsFactors = FALSE)
cb_0218 <- sqlQuery(db_conn, statement_list[2], stringsAsFactors = FALSE)
cb_0318 <- sqlQuery(db_conn, statement_list[3], stringsAsFactors = FALSE)
cb_0418 <- sqlQuery(db_conn, statement_list[4], stringsAsFactors = FALSE)
cb_0518 <- sqlQuery(db_conn, statement_list[5], stringsAsFactors = FALSE)
cb_0618 <- sqlQuery(db_conn, statement_list[6], stringsAsFactors = FALSE)
cb_0718 <- sqlQuery(db_conn, statement_list[7], stringsAsFactors = FALSE)
cb_0818 <- sqlQuery(db_conn, statement_list[8], stringsAsFactors = FALSE)
cb_0918 <- sqlQuery(db_conn, statement_list[9], stringsAsFactors = FALSE)
cb_1018 <- sqlQuery(db_conn, statement_list[10], stringsAsFactors = FALSE)
cb_1118 <- sqlQuery(db_conn, statement_list[11], stringsAsFactors = FALSE)
cb_1218 <- sqlQuery(db_conn, statement_list[12], stringsAsFactors = FALSE)
cb_0119 <- sqlQuery(db_conn, statement_list[13], stringsAsFactors = FALSE)
cb_0219 <- sqlQuery(db_conn, statement_list[14], stringsAsFactors = FALSE)
cb_0319 <- sqlQuery(db_conn, statement_list[15], stringsAsFactors = FALSE)
cb_0419 <- sqlQuery(db_conn, statement_list[16], stringsAsFactors = FALSE)
cb_0519 <- sqlQuery(db_conn, statement_list[17], stringsAsFactors = FALSE)
cb_0619 <- sqlQuery(db_conn, statement_list[18], stringsAsFactors = FALSE)
cb_0719 <- sqlQuery(db_conn, statement_list[19], stringsAsFactors = FALSE)

# Merge all citi bike datasets
citibike_merged <- rbind(cb_0118, cb_0218, cb_0318, cb_0418, cb_0518, cb_0618,
                         cb_0718, cb_0818, cb_0918, cb_1018, cb_1118, cb_1218,
                         cb_0119, cb_0219, cb_0319, cb_0419, cb_0519, cb_0619, cb_0719)

# SQL statement for selecting the whole weather data table
sql_weather <- "SELECT * FROM dbo.WeatherNYC"

# Get the weather data
weather <- sqlQuery(db_conn, sql_weather, stringsAsFactors = FALSE)

# Combine citi bike and weather data
df <- left_join(citibike_merged, weather, by=c("startyear" = "DATE"))

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

# Save the dataframe for usage in Shiny dashboard
save(df, file = "df.RData")

###########################################################################
# 2b) Select statement to get the number of daily rides combined with the average temperature and the precipitation

statement_list_2 <- list() # List which will be used in the for loop below
# For-loop to create the SQL statements for data table selection
for (table in c("dbo.citibike0118", "dbo.citibike0218", "dbo.citibike0318", "dbo.citibike0418", "dbo.citibike0518", "dbo.citibike0618", 
                "dbo.citibike0718", "dbo.citibike0818", "dbo.citibike0918", "dbo.citibike1018", "dbo.citibike1118", "dbo.citibike1218",
                "dbo.citibike0119", "dbo.citibike0219", "dbo.citibike0319", "dbo.citibike0419", "dbo.citibike0519", "dbo.citibike0619",
                "dbo.citibike0719")) {
  table <- paste0("SELECT ",table,".startyear, COUNT(",table,".starttime) AS number_of_rides, dbo.WeatherNYC.meantemp, dbo.WeatherNYC.PRCP FROM ",table," LEFT JOIN dbo.WeatherNYC ON ",table,".startyear = dbo.WeatherNYC.DATE GROUP BY ",table,".startyear,dbo.WeatherNYC.meantemp, dbo.WeatherNYC.PRCP ORDER BY ",table,".startyear ASC;")
  statement_list_2 <- append(statement_list_2, table)
  
}  

# SQL queries for loading data into data frames
count_rides_0118 <- sqlQuery(db_conn, statement_list_2[1], stringsAsFactors = FALSE)
count_rides_0218 <- sqlQuery(db_conn, statement_list_2[2], stringsAsFactors = FALSE)
count_rides_0318 <- sqlQuery(db_conn, statement_list_2[3], stringsAsFactors = FALSE)
count_rides_0418 <- sqlQuery(db_conn, statement_list_2[4], stringsAsFactors = FALSE)
count_rides_0518 <- sqlQuery(db_conn, statement_list_2[5], stringsAsFactors = FALSE)
count_rides_0618 <- sqlQuery(db_conn, statement_list_2[6], stringsAsFactors = FALSE)
count_rides_0718 <- sqlQuery(db_conn, statement_list_2[7], stringsAsFactors = FALSE)
count_rides_0818 <- sqlQuery(db_conn, statement_list_2[8], stringsAsFactors = FALSE)
count_rides_0918 <- sqlQuery(db_conn, statement_list_2[9], stringsAsFactors = FALSE)
count_rides_1018 <- sqlQuery(db_conn, statement_list_2[10], stringsAsFactors = FALSE)
count_rides_1118 <- sqlQuery(db_conn, statement_list_2[11], stringsAsFactors = FALSE)
count_rides_1218 <- sqlQuery(db_conn, statement_list_2[12], stringsAsFactors = FALSE)
count_rides_0119 <- sqlQuery(db_conn, statement_list_2[13], stringsAsFactors = FALSE)
count_rides_0219 <- sqlQuery(db_conn, statement_list_2[14], stringsAsFactors = FALSE)
count_rides_0319 <- sqlQuery(db_conn, statement_list_2[15], stringsAsFactors = FALSE)
count_rides_0419 <- sqlQuery(db_conn, statement_list_2[16], stringsAsFactors = FALSE)
count_rides_0519 <- sqlQuery(db_conn, statement_list_2[17], stringsAsFactors = FALSE)
count_rides_0619 <- sqlQuery(db_conn, statement_list_2[18], stringsAsFactors = FALSE)
count_rides_0719 <- sqlQuery(db_conn, statement_list_2[19], stringsAsFactors = FALSE)

# Merge the queried data tables with the number of rides per day
daily_rides_df <- rbind(count_rides_0118, count_rides_0218, count_rides_0318, count_rides_0418, count_rides_0518, count_rides_0618,
                        count_rides_0718, count_rides_0818, count_rides_0918, count_rides_1018, count_rides_1118, count_rides_1218,
                        count_rides_0119, count_rides_0219, count_rides_0319, count_rides_0419, count_rides_0519, count_rides_0619, 
                        count_rides_0719)

# Save the dataframe for usage in Shiny dashboard
save(daily_rides_df, file = "daily_rides.RData")

###########################################################################
# 2d) Select data for map (to show which station was used how often)

statement_list_3 <- list() # List which will be used in the for loop below
# For-loop to create the SQL statements for data table selection
for (table in c("dbo.citibike0118", "dbo.citibike0218", "dbo.citibike0318", "dbo.citibike0418", "dbo.citibike0518", "dbo.citibike0618", 
                "dbo.citibike0718", "dbo.citibike0818", "dbo.citibike0918", "dbo.citibike1018", "dbo.citibike1118", "dbo.citibike1218",
                "dbo.citibike0119", "dbo.citibike0219", "dbo.citibike0319", "dbo.citibike0419", "dbo.citibike0519", "dbo.citibike0619",
                "dbo.citibike0719")) {
  table <- paste0("SELECT start_station_name, start_station_latitude, start_station_longitude, end_station_name, end_station_latitude, end_station_longitude FROM ",table," ;")
  statement_list_3 <- append(statement_list_3, table)
  
}

# SQL queries for loading data into data frames
stations_0118 <- sqlQuery(db_conn, statement_list_3[1], stringsAsFactors = FALSE)
stations_0218 <- sqlQuery(db_conn, statement_list_3[2], stringsAsFactors = FALSE)
stations_0318 <- sqlQuery(db_conn, statement_list_3[3], stringsAsFactors = FALSE)
stations_0418 <- sqlQuery(db_conn, statement_list_3[4], stringsAsFactors = FALSE)
stations_0518 <- sqlQuery(db_conn, statement_list_3[5], stringsAsFactors = FALSE)
stations_0618 <- sqlQuery(db_conn, statement_list_3[6], stringsAsFactors = FALSE)
stations_0718 <- sqlQuery(db_conn, statement_list_3[7], stringsAsFactors = FALSE)
stations_0818 <- sqlQuery(db_conn, statement_list_3[8], stringsAsFactors = FALSE)
stations_0918 <- sqlQuery(db_conn, statement_list_3[9], stringsAsFactors = FALSE)
stations_1018 <- sqlQuery(db_conn, statement_list_3[10], stringsAsFactors = FALSE)
stations_1118 <- sqlQuery(db_conn, statement_list_3[11], stringsAsFactors = FALSE)
stations_1218 <- sqlQuery(db_conn, statement_list_3[12], stringsAsFactors = FALSE)
stations_0119 <- sqlQuery(db_conn, statement_list_3[13], stringsAsFactors = FALSE)
stations_0219 <- sqlQuery(db_conn, statement_list_3[14], stringsAsFactors = FALSE)
stations_0319 <- sqlQuery(db_conn, statement_list_3[15], stringsAsFactors = FALSE)
stations_0419 <- sqlQuery(db_conn, statement_list_3[16], stringsAsFactors = FALSE)
stations_0519 <- sqlQuery(db_conn, statement_list_3[17], stringsAsFactors = FALSE)
stations_0619 <- sqlQuery(db_conn, statement_list_3[18], stringsAsFactors = FALSE)
stations_0719 <- sqlQuery(db_conn, statement_list_3[19], stringsAsFactors = FALSE)

# Merge the queried data tables with the number of rides per day
stations_df <- rbind(stations_0118, stations_0218, stations_0318, stations_0418, stations_0518, stations_0618,
                     stations_0718, stations_0818, stations_0918, stations_1018, stations_1118, stations_1218,
                     stations_0119, stations_0219, stations_0319, stations_0419, stations_0519, stations_0619, 
                     stations_0719)

# Create a dataframe with a column which counts how often a start station was used
start_station_rank <- data.frame(stations_df %>%
                                   group_by(start_station_name) %>%
                                   summarize(count.start=n()) %>%
                                   arrange(desc(count.start)))

# Create a dataframe with a column which counts how often an end station was used
end_station_rank <- data.frame(stations_df %>%
                                 group_by(end_station_name) %>%
                                 summarize(count.end=n()) %>%
                                 arrange(desc(count.end)))

# Combine the start station and end station data frames, so that we have 
# for each station the number of bikes rented and the number of bikes returned
station_usage <- left_join(x=start_station_rank, y=end_station_rank, 
                           by=c("start_station_name"="end_station_name"))

# Replace all NAs with 0 (Stations which no one used should be shown as zero for the subsequent sum)
station_usage <- station_usage %>% 
  mutate_all(~replace(., is.na(.), 0))

# Create a new column which shows the combined number of bikes rented and returned
station_usage$station.count.comb <- rowSums(station_usage[, c("count.start", "count.end")])

# Order the dataframe by the combined number (from largest to smallest) to get a ranking
stations_df <- station_usage[order(-station_usage$station.count.comb),]

# Add latitudes and longitudes
stations_df <- merge(x = stations_df, y = df[ , c("start_station_name", "start_station_latitude", "start_station_longitude")], by = "start_station_name", all.x=TRUE)

# Every stations needs to be in the dataframe only once
stations_df <- distinct(stations_df)

# Rename columns for improved readability
setnames(stations_df, old = c('start_station_name','count.start', 'count.end', 'station.count.comb', 'start_station_latitude', 'start_station_longitude'), 
                      new = c('Station_Name','Bikes_rented', 'Bikes_returned', 'Total', 'Latitude', 'Longitude'))

#Here we add a new column with the info which pops up when the markers are clicked
# paste0() concatenates the strings
# stations_df <- mutate(stations_df, content=paste0('<strong>Station name: </strong>',Station_Name,
#                                                   '<br><strong>Latidue: </strong>',Latitude,
#                                                   '<br><strong>Longitude: </strong>',Longitude,
#                                                   '<br><strong>Bikes rented: </strong>',Bikes_rented,
#                                                   '<br><strong>Bikes returned: </strong>',Bikes_returned,
#                                                   '<br><strong>Total (rented and returned): </strong>',Total))

stations_df <- subset(stations_df, select=-c(content))

stations_df$cat <- ifelse(stations_df$Total >= 250000, ">=250k",
                   ifelse(stations_df$Total < 250000 & stations_df$Total >= 100000, "100k-250k",
                   ifelse(stations_df$Total < 100000 & stations_df$Total >= 25000, "25k-100k",
                   ifelse(stations_df$Total < 25000 & stations_df$Total >= 5000, "5k-25k","<5k" ))))

# Save the dataframe for usage in Shiny dashboard
save(stations_df, file = "stations_df.RData")


###########################################################################
# 2e) Data selection for calculating the ratio between subscribers and customers

statement_list_4 <- list() # List which will be used in the for loop below
# For-loop to create the SQL statements for data table selection
for (table in c("dbo.citibike0118", "dbo.citibike0218", "dbo.citibike0318", "dbo.citibike0418", "dbo.citibike0518", "dbo.citibike0618", 
                "dbo.citibike0718", "dbo.citibike0818", "dbo.citibike0918", "dbo.citibike1018", "dbo.citibike1118", "dbo.citibike1218",
                "dbo.citibike0119", "dbo.citibike0219", "dbo.citibike0319", "dbo.citibike0419", "dbo.citibike0519", "dbo.citibike0619",
                "dbo.citibike0719")) {
  table <- paste0("SELECT usertype FROM ",table,";")
  statement_list_4 <- append(statement_list_4, table)
  
}

# SQL queries for loading data into data frames
type_0118 <- sqlQuery(db_conn, statement_list_4[1], stringsAsFactors = FALSE)
type_0218 <- sqlQuery(db_conn, statement_list_4[2], stringsAsFactors = FALSE)
type_0318 <- sqlQuery(db_conn, statement_list_4[3], stringsAsFactors = FALSE)
type_0418 <- sqlQuery(db_conn, statement_list_4[4], stringsAsFactors = FALSE)
type_0518 <- sqlQuery(db_conn, statement_list_4[5], stringsAsFactors = FALSE)
type_0618 <- sqlQuery(db_conn, statement_list_4[6], stringsAsFactors = FALSE)
type_0718 <- sqlQuery(db_conn, statement_list_4[7], stringsAsFactors = FALSE)
type_0818 <- sqlQuery(db_conn, statement_list_4[8], stringsAsFactors = FALSE)
type_0918 <- sqlQuery(db_conn, statement_list_4[9], stringsAsFactors = FALSE)
type_1018 <- sqlQuery(db_conn, statement_list_4[10], stringsAsFactors = FALSE)
type_1118 <- sqlQuery(db_conn, statement_list_4[11], stringsAsFactors = FALSE)
type_1218 <- sqlQuery(db_conn, statement_list_4[12], stringsAsFactors = FALSE)
type_0119 <- sqlQuery(db_conn, statement_list_4[13], stringsAsFactors = FALSE)
type_0219 <- sqlQuery(db_conn, statement_list_4[14], stringsAsFactors = FALSE)
type_0319 <- sqlQuery(db_conn, statement_list_4[15], stringsAsFactors = FALSE)
type_0419 <- sqlQuery(db_conn, statement_list_4[16], stringsAsFactors = FALSE)
type_0519 <- sqlQuery(db_conn, statement_list_4[17], stringsAsFactors = FALSE)
type_0619 <- sqlQuery(db_conn, statement_list_4[18], stringsAsFactors = FALSE)
type_0719 <- sqlQuery(db_conn, statement_list_4[19], stringsAsFactors = FALSE)

# Merge the queried data tables with the number of rides per day
types_df <- rbind(type_0118, type_0218, type_0318, type_0418, type_0518, type_0618,
                  type_0718, type_0818, type_0918, type_1018, type_1118, type_1218,
                  type_0119, type_0219, type_0319, type_0419, type_0519, type_0619, 
                  type_0719)

# Save the dataframe for usage in Shiny dashboard
save(types_df, file = "types_df.RData")


###########################################################################
# 2d) Data selection for calculating the ratio between female and male customers

statement_list_5 <- list() # List which will be used in the for loop below
# For-loop to create the SQL statements for data table selection
for (table in c("dbo.citibike0118", "dbo.citibike0218", "dbo.citibike0318", "dbo.citibike0418", "dbo.citibike0518", "dbo.citibike0618", 
                "dbo.citibike0718", "dbo.citibike0818", "dbo.citibike0918", "dbo.citibike1018", "dbo.citibike1118", "dbo.citibike1218",
                "dbo.citibike0119", "dbo.citibike0219", "dbo.citibike0319", "dbo.citibike0419", "dbo.citibike0519", "dbo.citibike0619",
                "dbo.citibike0719")) {
  table <- paste0("SELECT gender FROM ",table,";")
  statement_list_5 <- append(statement_list_5, table)
  
}

# SQL queries for loading data into data frames
gender_0118 <- sqlQuery(db_conn, statement_list_5[1], stringsAsFactors = FALSE)
gender_0218 <- sqlQuery(db_conn, statement_list_5[2], stringsAsFactors = FALSE)
gender_0318 <- sqlQuery(db_conn, statement_list_5[3], stringsAsFactors = FALSE)
gender_0418 <- sqlQuery(db_conn, statement_list_5[4], stringsAsFactors = FALSE)
gender_0518 <- sqlQuery(db_conn, statement_list_5[5], stringsAsFactors = FALSE)
gender_0618 <- sqlQuery(db_conn, statement_list_5[6], stringsAsFactors = FALSE)
gender_0718 <- sqlQuery(db_conn, statement_list_5[7], stringsAsFactors = FALSE)
gender_0818 <- sqlQuery(db_conn, statement_list_5[8], stringsAsFactors = FALSE)
gender_0918 <- sqlQuery(db_conn, statement_list_5[9], stringsAsFactors = FALSE)
gender_1018 <- sqlQuery(db_conn, statement_list_5[10], stringsAsFactors = FALSE)
gender_1118 <- sqlQuery(db_conn, statement_list_5[11], stringsAsFactors = FALSE)
gender_1218 <- sqlQuery(db_conn, statement_list_5[12], stringsAsFactors = FALSE)
gender_0119 <- sqlQuery(db_conn, statement_list_5[13], stringsAsFactors = FALSE)
gender_0219 <- sqlQuery(db_conn, statement_list_5[14], stringsAsFactors = FALSE)
gender_0319 <- sqlQuery(db_conn, statement_list_5[15], stringsAsFactors = FALSE)
gender_0419 <- sqlQuery(db_conn, statement_list_5[16], stringsAsFactors = FALSE)
gender_0519 <- sqlQuery(db_conn, statement_list_5[17], stringsAsFactors = FALSE)
gender_0619 <- sqlQuery(db_conn, statement_list_5[18], stringsAsFactors = FALSE)
gender_0719 <- sqlQuery(db_conn, statement_list_5[19], stringsAsFactors = FALSE)

# Merge the queried data tables with the number of rides per day
gender_df <- rbind(gender_0118, gender_0218, gender_0318, gender_0418, gender_0518, gender_0618,
                   gender_0718, gender_0818, gender_0918, gender_1018, gender_1118, gender_1218,
                   gender_0119, gender_0219, gender_0319, gender_0419, gender_0519, gender_0619, 
                   gender_0719)

gender_df[gender_df == "0"] <- "unknown"
gender_df[gender_df == "1"] <- "male"
gender_df[gender_df == "2"] <- "female"


# Save the dataframe for usage in Shiny dashboard
save(gender_df, file = "gender_df.RData")


###########################################################################
# 2e) Data selection for bikeid list

statement_list_6 <- list() # List which will be used in the for loop below
# For-loop to create the SQL statements for data table selection
for (table in c("dbo.citibike0118", "dbo.citibike0218", "dbo.citibike0318", "dbo.citibike0418", "dbo.citibike0518", "dbo.citibike0618", 
                "dbo.citibike0718", "dbo.citibike0818", "dbo.citibike0918", "dbo.citibike1018", "dbo.citibike1118", "dbo.citibike1218",
                "dbo.citibike0119", "dbo.citibike0219", "dbo.citibike0319", "dbo.citibike0419", "dbo.citibike0519", "dbo.citibike0619",
                "dbo.citibike0719")) {
  table <- paste0("SELECT bikeid, tripduration FROM ",table,";")
  statement_list_6 <- append(statement_list_6, table)
  
}

# SQL queries for loading data into data frames
bike_0118 <- sqlQuery(db_conn, statement_list_6[1], stringsAsFactors = FALSE)
bike_0218 <- sqlQuery(db_conn, statement_list_6[2], stringsAsFactors = FALSE)
bike_0318 <- sqlQuery(db_conn, statement_list_6[3], stringsAsFactors = FALSE)
bike_0418 <- sqlQuery(db_conn, statement_list_6[4], stringsAsFactors = FALSE)
bike_0518 <- sqlQuery(db_conn, statement_list_6[5], stringsAsFactors = FALSE)
bike_0618 <- sqlQuery(db_conn, statement_list_6[6], stringsAsFactors = FALSE)
bike_0718 <- sqlQuery(db_conn, statement_list_6[7], stringsAsFactors = FALSE)
bike_0818 <- sqlQuery(db_conn, statement_list_6[8], stringsAsFactors = FALSE)
bike_0918 <- sqlQuery(db_conn, statement_list_6[9], stringsAsFactors = FALSE)
bike_1018 <- sqlQuery(db_conn, statement_list_6[10], stringsAsFactors = FALSE)
bike_1118 <- sqlQuery(db_conn, statement_list_6[11], stringsAsFactors = FALSE)
bike_1218 <- sqlQuery(db_conn, statement_list_6[12], stringsAsFactors = FALSE)
bike_0119 <- sqlQuery(db_conn, statement_list_6[13], stringsAsFactors = FALSE)
bike_0219 <- sqlQuery(db_conn, statement_list_6[14], stringsAsFactors = FALSE)
bike_0319 <- sqlQuery(db_conn, statement_list_6[15], stringsAsFactors = FALSE)
bike_0419 <- sqlQuery(db_conn, statement_list_6[16], stringsAsFactors = FALSE)
bike_0519 <- sqlQuery(db_conn, statement_list_6[17], stringsAsFactors = FALSE)
bike_0619 <- sqlQuery(db_conn, statement_list_6[18], stringsAsFactors = FALSE)
bike_0719 <- sqlQuery(db_conn, statement_list_6[19], stringsAsFactors = FALSE)

# Merge the queried data tables with the number of rides per day
bike_df <- rbind(bike_0118, bike_0218, bike_0318, bike_0418, bike_0518, bike_0618,
                 bike_0718, bike_0818, bike_0918, bike_1018, bike_1118, bike_1218,
                 bike_0119, bike_0219, bike_0319, bike_0419, bike_0519, bike_0619, 
                 bike_0719)

# Compute percentages for the doughnut plot
bike_df <- bike_df %>%
  group_by(bikeid) %>%
  summarise(counts = n(),
            tripduration = sum(tripduration)) %>% 
  rename(
    Usages = counts,
    BikeId = bikeid,
    TripDuration = tripduration)

# Save the dataframe for usage in Shiny dashboard
save(bike_df, file = "bike_df.RData")

##########################################################################################################
# 3) Checks if the data was successfully loaded from SQL Server
###########################################################################################################

View(df)
nrow(df)

View(daily_rides_df)
nrow(daily_rides_df)

View(stations_df)
nrow(stations_df)

View(gender_df)
nrow(gender_df)

View(bike_df)
nrow(bike_df)

# Close connection to database
odbcClose(db_conn)
