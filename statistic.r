
# Set R to always use binary packages when available
options(install.packages.check.source = "no")

# List of required packages
required_packages <- c("vtable", "ggplot2", "ggcorrplot", "lares", "gridExtra","scales","stargazer")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


# Load the database
df = read.csv("data/phpgBMvy4.csv",sep = ",", header = TRUE)

df = df[df$timedelta > 21,]

# Select relevant variables
var <- c("shares", "n_tokens_title", "n_tokens_content", "num_hrefs", "num_imgs", "num_videos")

# Display the first few rows of selected variables
head(df[var])

# Check for missing values
sum(is.na(df[var]))

# Assign descriptive labels to variables
labs <- c("Number of shares (target)", "Number of words in title", "Number of words in content", "Number of links", "Number of images", "Number of videos")

# Summary statistics
stargazer(df[var], omit.summary.stat = c("p25", "p75"))

# Histogram of number of shares
ggplot(df, aes(x = shares)) +
  geom_histogram(bins = 30, color = "blue", fill = "lightblue") +
  labs(title = "Histogram of Number of Shares", x = "Shares", y = "Frequency")

# Histogram of number of videos
ggplot(df, aes(x = num_videos)) +
  geom_histogram(bins = 20, color = "orange", fill = "lightyellow") +
  labs(title = "Histogram of Number of Videos", x = "Number of Videos", y = "Frequency")

# Categorical variables for channel distribution
cat_vars <- c("data_channel_is_entertainment", "data_channel_is_bus",
               "data_channel_is_socmed", "data_channel_is_tech",
               "data_channel_is_lifestyle", "data_channel_is_world")

# Function to create distribution plots for categorical variables
create_distribution_plot <- function(variable) {
  ggplot(df, aes(x = factor(!!variable))) +
    geom_bar(aes(y = (..count..) / sum(..count..)), stat = "count", fill = "steelblue", color = "black") +
    geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) / sum(..count..))), vjust = -0.25) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0(variable, " Distribution"), x = variable, y = "Proportion")
}

# Create distribution plots for each categorical variable
lapply(cat_vars, create_distribution_plot)  # Loop through variables

# Arrange distribution plots in a grid (3 columns)
grid.arrange(
  create_distribution_plot(cat_vars[1]), create_distribution_plot(cat_vars[2]), create_distribution_plot(cat_vars[3]),
  create_distribution_plot(cat_vars[4]), create_distribution_plot(cat_vars[5]), create_distribution_plot(cat_vars[6]),
  ncol = 3
)

# Analyze day of publication
day_vars <- c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
              "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday",
              "weekday_is_sunday")
day_counts <- colSums(df[, day_vars])
day_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Calculate and format percentages
day_data <- data.frame(Day = day_labels, Count = day_counts)
day_data$Percent <- round(day_data$Count / sum(day_data$Count), 4) * 100
day_data$Percent_Label <- paste0(day_data$Percent, "%")

# Polar plot for day of publication
ggplot(day_data, aes(x = "", y = Count, fill = Day)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = Percent_Label), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) + theme_void()

# Correlation analysis
num_df <- df[, unlist(lapply(df, is.integer))]
corr <- cor(num_df)

# Correlation heatmap
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

# Top correlations (if desired)
corr_cross(num_df, max_pvalue = 0.05, top = 10)

# First Plot: Simple Bar Plot
ggplot(df, aes(x = as.factor(data_channel_is_bus), fill = as.factor(data_channel_is_bus))) + 
  geom_bar(stat = "count", fill = "cornflowerblue", color = "black") +
  labs(x = "Business or Not", y = "Count") +
  theme_minimal()

# Second Plot: Bar Plot with Proportions and Percent Labels
ggplot(df, aes(x = as.factor(data_channel_is_bus))) +
  geom_bar(aes(y = (..count..) / sum(..count..)), fill = "cornflowerblue", color = "black", stat = "count") +
  geom_text(aes(
    y = (..count..) / sum(..count..), 
    label = scales::percent((..count..) / sum(..count..))
  ), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Business or Not", y = "Proportion") +
  theme_minimal()