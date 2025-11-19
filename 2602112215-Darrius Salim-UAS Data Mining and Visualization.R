# 2602112215 - Darrius Salim

# Read Dataset
data1 <- read.csv(file.choose(), stringsAsFactors = TRUE)

# Import libraries
library(plotly)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(psych)

# Check The Datasets Dimension
dim(data1)
# View/Show The Datasets
View(data1)
# Check The Missing Data in The Datasets
table(complete.cases(data1))
# True 4801 menandakan bahwa ada 4801 baris didalam dataset tersebut yang memiliki data yang lengkap

str(data1)
head(data1)
tail(data1)

summary(data1)
describe(data1)
describeBy(data1, group = data1$BROKERTITLE)

# BEDS & BATH Boxplot
boxplot(data1[, c('BEDS', 'BATH')], main = "Property Stats Boxplot")

# Property Square Footage Boxplot
boxplot(data1$PROPERTYSQFT, main = "Property Square Footage Boxplot")
# Property Square Footage QQ Plot
qqnorm(data1$PROPERTYSQFT, main = "Property Square Footage Normal QQ Plot")

# Set options to display numbers in full form
options(scipen = 999, digits = 10)
# Boxplot for PRICE
boxplot(data1$PRICE, main = "Price Boxplot")
# Reset options to default after creating the boxplot
options(scipen = 0, digits = 7)
# QQ Plot for PRICE
qqnorm(data1$PRICE, main = "PRICE Normal QQ Plot")

# Check if there is any missing data
sapply(data1, function(x) sum(is.na(x)))

# Check if there is any duplicated data
dup_r <- duplicated(data1$ADDRESS)
sum(dup_r)
dup_r1 <- duplicated(data1$LONG_NAME)
sum(dup_r1)
dup_r2 <- duplicated(data1$MAIN_ADDRESS)
sum(dup_r2)
dup_r3 <- duplicated(data1$FORMATTED_ADDRESS)
sum(dup_r3)

# Create scatter plot with multicolor and logarithmic scale
scatter_plot_multicolor_log <- plot_ly(data1, x = ~PROPERTYSQFT, y = ~PRICE, color = ~TYPE, type = "scatter",
                                       mode = "markers", marker = list(size = 10),
                                       text = ~paste('Type: ', TYPE, '<br>Property Sqft: ', PROPERTYSQFT, '<br>Price: ', PRICE))
# Set layout parameters
layout(scatter_plot_multicolor_log, title = 'Price vs. Property Square Footage (Logarithmic Scale)',
       xaxis = list(title = 'Property Square Footage', type = 'log'),
       yaxis = list(title = 'Price', type = 'log'))
# Display the plot
scatter_plot_multicolor_log

# Create box plot with logarithmic y-axis
# Create an empty plot
box_plot_log <- plot_ly()
# Get unique house types in the dataframe
unique_types <- unique(data1$TYPE)
# Loop through each house type to create individual box plots
for (type in unique_types) {
  subset_df <- data1[data1$TYPE == type, ]
  
  box_plot_log <- add_trace(
    box_plot_log,
    y = subset_df$PRICE,
    x = rep(type, nrow(subset_df)),
    type = "box",
    name = type,
    boxpoints = "all",
    jitter = 0.3,
    pointpos = -1.8,
    pointwidth = 0.6,
    marker = list(color = rainbow(length(unique_types))[match(type, unique_types)]),
    line = list(color = 'black')
  )
}
# Set layout parameters
layout(box_plot_log, title = 'Price Distribution by House Type (Logarithmic Scale)',
       xaxis = list(title = 'House Type'),
       yaxis = list(title = 'Price', type = 'log'),
       legend = list(title = 'House Type'))
# Display the plot
box_plot_log

# Create 3D scatter plot
scatter_3d_multicolor <- plot_ly(data1, x = ~BEDS, y = ~BATH, z = ~PRICE, color = ~PRICE,
                                 type = "scatter3d", mode = "markers",
                                 marker = list(size = 3))
# Set layout parameters
layout(scatter_3d_multicolor, title = 'Price vs. Bedrooms vs. Bathrooms',
       scene = list(xaxis = list(title = 'Bedrooms'),
                    yaxis = list(title = 'Bathrooms'),
                    zaxis = list(title = 'Price')))
# Display the plot
scatter_3d_multicolor

# Create violin plot
violin_plot_multicolor_log <- plot_ly(data1, x = ~TYPE, y = ~PROPERTYSQFT, type = "violin",
                                      box = list(visible = TRUE),
                                      points = "all",
                                      jitter = 0.3,
                                      line = list(color = 'black'),
                                      marker = list(color = ~TYPE),
                                      split = ~TYPE,  # Add separate violins for each TYPE
                                      legendgroup = ~TYPE,  # Group for legends
                                      pointpos = -1.8,  # Adjust position of points
                                      text = ~paste('Type: ', TYPE, '<br>Property Sqft: ', PROPERTYSQFT)
)
# Set layout parameters
layout(violin_plot_multicolor_log, title = 'Distribution of Property Square Footage by House Type (Logarithmic Scale)',
       xaxis = list(title = 'House Type'),
       yaxis = list(title = 'Property Square Footage', type = 'log', range = c(0, 5)),
       violinmode = "overlay",  # Overlay violins to show all types in one plot
       height = 570  # Adjust the height of the plot
)
# Display the plot
violin_plot_multicolor_log

# Perform value counts and reset index
type_counts <- table(data1$TYPE)
type_counts <- as.data.frame(type_counts)
colnames(type_counts) <- c('Type', 'Count')
# Create the bar chart with different colors for each property type
fig_bar <- plot_ly(type_counts, x = ~Type, y = ~Count, type = 'bar',
                   color = ~Type,
                   colors = brewer.pal(length(unique(type_counts$Type)), 'Set3'),  # Use color palette from RColorBrewer
                   hoverinfo = "y+name")
# Set layout parameters
layout(fig_bar, title = 'Frequency of Property Types',
       xaxis = list(title = 'Type'),
       yaxis = list(title = 'Count'))
# Display the plot
fig_bar

# Define your background color
background_color <- 'rgba(235,236,240,1)'  # This is a light grey, you can choose any color you like
# Calculate the average price per broker and sort them to find the top 10
top_brokers <- aggregate(PRICE ~ BROKERTITLE, data = data1, FUN = mean)
top_brokers <- top_brokers[order(-top_brokers$PRICE), ][1:10, ]
# Treemap - Top 10 Brokers by Average Price
fig_treemap <- plot_ly(top_brokers, labels = ~BROKERTITLE, parents = ~"", values = ~PRICE, type = 'treemap',
                       text = ~paste('Broker: ', BROKERTITLE, '<br>Average Price: $', round(PRICE, 2)),
                       hoverinfo = 'text',
                       layout = list(title = 'Treemap of Top 10 Brokers by Average Price'))
fig_treemap <- fig_treemap %>% layout(paper_bgcolor = background_color)
layout(fig_treemap, title = 'Treemap of Top 10 Brokers by Average Price')
fig_treemap

# Calculate the base price (average price without considering BEDS, BATH, and PROPERTYSQFT)
base_price <- mean(data1$PRICE)
# Calculate the incremental average price contributions
avg_price_per_bed <- mean(tapply(data1$PRICE, data1$BEDS, mean))
avg_price_per_bath <- mean(tapply(data1$PRICE, data1$BATH, mean))
avg_price_per_sqft <- mean(tapply(data1$PRICE, data1$PROPERTYSQFT, mean))
# Create a data frame for the waterfall chart
waterfall_data <- data.frame(
  step = c("Base Price", "Contribution per Bed", "Contribution per Bath", "Contribution per SqFt", "Final Average Price"),
  value = c(base_price, avg_price_per_bed, avg_price_per_bath, avg_price_per_sqft, base_price + avg_price_per_bed + avg_price_per_bath + avg_price_per_sqft),
  stringsAsFactors = FALSE
)
# Create a waterfall chart using ggplot2
ggplot(waterfall_data, aes(x = step, y = value)) +
  geom_bar(stat = "identity", position = "identity", fill = "blue") +
  geom_text(aes(label = value), vjust = -0.5, size = 4) +
  labs(title = "Contribution of Various Factors to the Final Property Price") +
  theme_minimal()

# Create a Bubble Chart
fig_bubble <- plot_ly(
  data1,
  x = ~PRICE,
  y = ~PROPERTYSQFT,
  type = "scatter",
  mode = "markers",
  sizes = ~BEDS,  # The size of bubbles represents the number of beds
  color = ~BATH,  # Color represents the number of baths
  text = ~BROKERTITLE,  # Show broker title on hover
  marker = list(sizemode = "diameter", sizeref = 0.6, size = data1$BEDS)  # Set bubble size parameters
)
# Update layout for better visualization
fig_bubble <- fig_bubble %>% layout(
  title = 'Bubble Chart: Property Size vs Price with Bed and Bath Dimensions',
  xaxis = list(title = 'Price'),
  yaxis = list(title = 'Property SqFt')
)
# Display the Bubble Chart
fig_bubble

# Select relevant columns
corr_data <- data1[, c('PRICE', 'BEDS', 'BATH', 'PROPERTYSQFT')]
# Compute the correlation matrix
corr_matrix <- cor(corr_data, use = "complete.obs")
# Create a correlation heatmap using corrplot
corrplot(corr_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(50), 
         type = "upper", order = "hclust", addCoef.col = "black", tl.col = "black", tl.srt = 45,
         mar = c(1, 1, 1, 1))  # Set mar parameter for square heatmap
# Set the main title
title(main = "Correlation Heatmap", sub = "")
# Display the plot

# Function to remove outliers based on IQR
remove_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25)
  Q3 <- quantile(df[[column]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  df_filtered <- df[(df[[column]] >= lower_bound) & (df[[column]] <= upper_bound), ]
  return(df_filtered)
}
# Removing outliers from each column
df_filtered <- data1
for (col in c('PRICE', 'BEDS', 'BATH', 'PROPERTYSQFT')) {
  df_filtered <- remove_outliers(df_filtered, col)
}
# Descriptive statistics for the filtered data
descriptive_stats_filtered <- summary(df_filtered[c('PRICE', 'BEDS', 'BATH', 'PROPERTYSQFT')])
# Visualizing the distributions with histograms
par(mfrow=c(2,2))
hist(df_filtered$PRICE, main='Histogram of PRICE', col='skyblue', xlab='PRICE', prob=TRUE)
hist(df_filtered$BEDS, main='Histogram of BEDS', col='skyblue', xlab='BEDS', prob=TRUE)
hist(df_filtered$BATH, main='Histogram of BATH', col='skyblue', xlab='BATH', prob=TRUE)
hist(df_filtered$PROPERTYSQFT, main='Histogram of PROPERTYSQFT', col='skyblue', xlab='PROPERTYSQFT', prob=TRUE)
descriptive_stats_filtered


# Define price thresholds for segments (adjust these as needed)
luxury_threshold <- quantile(df_filtered$PRICE, 0.75)  # Top 25% as luxury
affordable_threshold <- quantile(df_filtered$PRICE, 0.25)  # Bottom 25% as affordable
# Segment the properties
df_filtered$MARKET_SEGMENT <- cut(df_filtered$PRICE, 
                                  breaks=c(0, affordable_threshold, luxury_threshold, Inf), 
                                  labels=c('Affordable', 'Mid-range', 'Luxury'))
# Analyze each segment
# Average price in each segment
average_price_segment <- tapply(df_filtered$PRICE, df_filtered$MARKET_SEGMENT, mean)
average_price_segment <- round(average_price_segment, 3)
# Count of listings in each segment
count_listings_segment <- table(df_filtered$MARKET_SEGMENT)
# Average property size in each segment
average_size_segment <- tapply(df_filtered$PROPERTYSQFT, df_filtered$MARKET_SEGMENT, mean)
average_size_segment <- round(average_size_segment, 3)
# Broker activity in each segment
broker_activity_segment <- table(df_filtered$MARKET_SEGMENT, df_filtered$BROKERTITLE)
# Creating a bar chart for Average Price by Market Segment
fig1 <- ggplot(data.frame(MARKET_SEGMENT=names(average_price_segment), PRICE=average_price_segment), aes(x=MARKET_SEGMENT, y=PRICE)) +
  geom_bar(stat="identity", fill="skyblue") +
  labs(title="Average Price by Market Segment", x="Market Segment", y="Average Price") +
  theme_minimal()
print(fig1)
# Creating a bar chart for Average Property Size by Market Segment
fig2 <- ggplot(data.frame(MARKET_SEGMENT=names(average_size_segment), PROPERTYSQFT=average_size_segment), aes(x=MARKET_SEGMENT, y=PROPERTYSQFT)) +
  geom_bar(stat="identity", fill="skyblue") +
  labs(title="Average Property Size by Market Segment", x="Market Segment", y="Average Property Size (sqft)") +
  theme_minimal()
print(fig2)

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(df_filtered$PRICE, p = 0.8, list = FALSE)
train_data <- df_filtered[train_indices, ]
test_data <- df_filtered[-train_indices, ]
# Train a linear regression model
lm_model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = train_data)
# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)
# Evaluate the model
rmse <- sqrt(mean((predictions - test_data$PRICE)^2))
mae <- mean(abs(predictions - test_data$PRICE))
r_squared <- cor(predictions, test_data$PRICE)^2
# Visualize predicted vs actual values
ggplot() +
  geom_point(aes(x = test_data$PRICE, y = predictions), color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Prices", x = "Actual Prices", y = "Predicted Prices") +
  theme_minimal()
# Display evaluation metrics
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", r_squared, "\n")
