
# The following script includes the creation of plots which are used in the Shiny Dashboard.
# Different questions are answered with the plots and tables:

#   1) How do the user numbers develop over the period?
#   2) How much time do users spend on average per bike ride? Does the usage behavior match the pricing model?
#   3) How do user numbers behave during different times of the day, days of the week and seasons?
#   4) How dependent is the service on the weather?
#   5) How dependent is the service on the temperature?
#   6) Customer Analysis: Female to Male Ratio

# Moved all files to the same directory for deployment to Shinyapps.io
# All the data sources are loaded
load("df.RData")
load("daily_rides.RData")
load("types_df_new.RData")
load("gender_df_new.RData")

# # The required libraries installed
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if (!require("data.table")) install.packages("data.table", dependencies=TRUE)
if (!require("xts")) install.packages("xts", dependencies=TRUE)
if (!require("zoo")) install.packages("zoo", dependencies=TRUE)

# And the required libraries imported
library(ggplot2)
library(data.table)
library(xts)
library(zoo)

##################################################################################################################################
# 1) How do the user numbers develop over time?
##################################################################################################################################

## PLOT 1: TIME SERIES
# TO DO: Change to moving average

dates <- daily_rides_df[,1]

# Constructor function for creating an extensible time-series object.
rides_ts <- xts(x = daily_rides_df[,2], order.by=dates)

ts <- autoplot(rides_ts) +
  geom_line(color="#25a5be", size=0.5) +
  labs(x = "", y = "Number of rides") +
  theme_gray() + 
  theme_minimal()

## PLOT 2: DOUGHNUT CHART WITH SUBSCRIBER TO CUSTOMER RATIO

# Compute percentages for the doughnut plot
usertype_fraction <- types_df %>%
  group_by(usertype) %>%
  summarise(counts = n())

usertype_fraction$percentage <- usertype_fraction$counts / sum(usertype_fraction$counts)
usertype_fraction

# Compute the cumulative percentages (top of each rectangle)
usertype_fraction$ymax <- cumsum(usertype_fraction$percentage)

# Compute the bottom of each rectangle
usertype_fraction$ymin = c(0, head(usertype_fraction$ymax, n=-1))

# Compute label position
usertype_fraction$labelPosition <- (usertype_fraction$ymax + usertype_fraction$ymin) / 2

# Compute a good label
usertype_fraction$label <- paste0(usertype_fraction$usertype, "\n", round(usertype_fraction$percentage,4)*100,"%")

usertype_plot = ggplot(usertype_fraction, aes(fill=usertype, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(colour="white") +
  coord_polar(theta="y") +
  geom_text(x=2, aes(y=labelPosition, label=label, color=usertype), size=4.5) + # x here controls label position (inner / outer)
  xlim(c(0, 4)) +
  theme_void() +
  labs(subtitle="Customer = 24-hour pass or 3-day pass user; Subscriber = Annual Member") +
  theme(legend.position = "none")
  
##################################################################################################################################
# 2) How long do users spend on average per loan? Does the usage behaviour match the pricing model?
##################################################################################################################################

# Change categories to ascending order
df$tripduration_cat <- factor(df$tripduration_cat, levels = c("0 to 5 min", "5 to 15 min", "15 to 30 min", "30 to 60 min", "1 to 4 h", "4 to 12 h", "over 12 h"))

tripdur_cat_count <- df %>%
  group_by(tripduration_cat) %>%
  summarise(counts = n())

# Histogram trip duration categories
tripdurations <- ggplot(tripdur_cat_count, aes(x=tripduration_cat, y=counts)) +
  geom_bar(fill = "#25a5be",
           stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) +    # Label above bar
  theme(plot.title = element_text(hjust = 0.5), 
        plot.margin=unit(c(0,0,-0.04,0), "null")) + # Space between figure and caption
  xlab('') +                                        # Remove text on x-axis
  ylab("Number of rides") + 
  theme_minimal()


##################################################################################################################################
# 3) How do user numbers behave during different times of the day, days of the week and seasons?
##################################################################################################################################

## Histogramm Daytime
hist_rides <- ggplot(data.frame(df$daytime), aes(x=df$daytime)) +
  geom_bar(fill='#2980B9') +
  theme(legend.position="none", axis.text.y=element_blank(), plot.title=element_text(hjust=.5)) +
  #ggtitle("Histogram with Number of Rides by Time of Day") +
  #ggeasy::easy_center_title() +
  #theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Daytime") +
  ylab("Number of rides") +
  theme_minimal()


## PLOT: HOURLY DISTRIBUTION OF RIDES

# Hours, Minutes and Seconds
df$onlytime <- as.POSIXct(substr(df$starttime, 12, 19), format="%H:%M:%S")
# Only Hours and Minutes
df$time <- strftime(df$onlytime,format="%H:%M")
# Only Hour
df$hour <- as.numeric(substr(df$time,1,2))

# Order the weekdays
# df$weekdays <- ordered(df$weekdays, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

hourly_dist <- df %>% 
  ggplot(aes(x=hour,fill=factor(weekdays))) +
  scale_fill_manual(values = c('#2980B9', '#2980B9', '#2980B9', '#2980B9', '#2980B9', '#2980B9', '#2980B9')) +
  geom_density(alpha=.2) +
  facet_wrap(~weekdays,ncol=1) +
  theme(legend.position="none", axis.text.y=element_blank())

##################################################################################################################################
# 4) How dependent is the service on the weather?
##################################################################################################################################

## PLOT: SCATTERPLOT WITH PRECIPITATION AND TRIPDURATION

trip_dur_filter <- df %>%
  filter(tripduration < 3000)

rain_scatter <- ggplot(trip_dur_filter, aes(x=PRCP, y=tripduration)) +
  geom_point(color='#2980B9', size=1) + 
  geom_smooth(method=lm, color='darkred', linetype="dashed") +
  xlab("Precipitation (in mm)") +
  ylab("Trip duration (in seconds)") +
  theme_light()


##################################################################################################################################
# 5) How dependent is the service on the temperature?
##################################################################################################################################

## PLOT: SCATTERPLOT WITH AVERAGE TEMPERATURE AND TRIPDURATION

temp_scatter <- ggplot(trip_dur_filter, aes(x=meantemp, y=tripduration)) +
  geom_point(color='#2980B9', size=1) + 
  geom_smooth(method=lm, color='darkred', linetype="dashed") +
  xlab("Average temperature (in Celsius)") +
  ylab("Trip duration (in seconds)") +
  theme_light()


##################################################################################################################################
# 6) Customer Analysis: Female to Male Ratio
##################################################################################################################################

## PLOT: DOUGHNUT PLOT WITH GENDERS

# Compute percentages for the doughnut plot
gender_fraction <- gender_df %>%
  group_by(gender) %>%
  summarise(counts = n())

gender_fraction$percentage <- gender_fraction$counts / sum(gender_fraction$counts)
gender_fraction

# Compute the cumulative percentages (top of each rectangle)
gender_fraction$ymax <- cumsum(gender_fraction$percentage)

# Compute the bottom of each rectangle
gender_fraction$ymin = c(0, head(gender_fraction$ymax, n=-1))

# Compute label position
gender_fraction$labelPosition <- (gender_fraction$ymax + gender_fraction$ymin) / 2

# Compute the label
gender_fraction$label <- paste0(gender_fraction$gender, "\n", round(gender_fraction$percentage,4)*100,"%")

genderratio_plot <- ggplot(gender_fraction, aes(fill=gender, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(colour="white") +
  coord_polar(theta="y") +
  geom_text(x=2, aes(y=labelPosition, label=label, color=gender), size=4.5) + # x here controls label position (inner / outer)
  scale_color_manual(values=c("#F8766D", "#00BFC4", "#91FDC4")) +
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#91FDC4")) +
  xlim(c(0, 4)) +
  theme_void() +
  theme(legend.position = "none")
