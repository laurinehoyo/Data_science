# Q2 : What are the factors that influence the duration of a listing for a used car? 

# 1. Histogram of listing duration
# 2. Scatterplot : price - listing age
# 3. Boxplot : kilometers by listing age (?)
# 4. Bar chart : Fuel Type/Transmission/Body Type vs. Listing Duration
# 5. Heatmap - Correlation Matrix
# 6. Violin Plot - Drivetrain vs. Listing Duration

library("ggplot2")
library("tidyverse")
library("patchwork")

golf <- read.csv("golf2_cleaned.csv")
audi <- read.csv("audi2_cleaned.csv")
skoda <- read.csv("skoda2_cleaned.csv")
volvo <- read.csv("volvo2_cleaned.csv")
toyota <- read.csv("toyota_cleaned.csv")

# GRAPH 1 : histogram of listing duration
ggplot(golf, aes(x = listing.age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Listing Duration",
    x = "Listing Duration (Days)",
    y = "Frequency"
  )

ggplot(audi, aes(x = listing.age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Listing Duration",
    x = "Listing Duration (Days)",
    y = "Frequency"
  )

ggplot(skoda, aes(x = listing.age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Listing Duration",
    x = "Listing Duration (Days)",
    y = "Frequency"
  )

ggplot(volvo, aes(x = listing.age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Listing Duration",
    x = "Listing Duration (Days)",
    y = "Frequency"
  )

ggplot(toyota, aes(x = listing.age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Listing Duration",
    x = "Listing Duration (Days)",
    y = "Frequency"
  )

# PUT THEM SIDE BY SIDE 

# Function to create histogram
create_histogram <- function(data, model_name) {
  ggplot(data, aes(x = listing.age)) +
    geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
    labs(
      title = paste("Histogram of Listing Duration for", model_name),
      x = "Listing Duration (Days)",
      y = "Frequency"
    )
}

# Create histograms using lapply and arrange them with patchwork
list_of_datasets <- list(golf, audi, toyota, volvo, skoda)
car_model_names <- c("VW Golf", "Audi A3", "Toyota Yaris", "Volvo XC60", "Skoda Octavia")

histograms <- lapply(seq_along(list_of_datasets), function(i) {
  create_histogram(list_of_datasets[[i]], car_model_names[i])
})

# Combine histograms using the `+` operator from patchwork and display them side by side
histogram_arrangement <- wrap_plots(histograms, ncol = 2)  # Adjust ncol for layout

# Display the combined histograms
histogram_arrangement

# vw : most frequent listings are old 
# audi, toyota, volvo, skoda : most frequent listings are new
# while some models have more new listings than old, there is a pattern : most are either very recent (0 to 30 days), or very old (350+)


# GRAPH 2 : scatterplot : price - listing age
ggplot(golf, aes(x = price, y = listing.age)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Price vs. Listing Duration",
    x = "Price",
    y = "Listing Duration (Days)"
  )

ggplot(audi, aes(x = price, y = listing.age)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Price vs. Listing Duration",
    x = "Price",
    y = "Listing Duration (Days)"
  )

ggplot(skoda, aes(x = price, y = listing.age)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Price vs. Listing Duration",
    x = "Price",
    y = "Listing Duration (Days)"
  )

ggplot(volvo, aes(x = price, y = listing.age)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Price vs. Listing Duration",
    x = "Price",
    y = "Listing Duration (Days)"
  )

ggplot(toyota, aes(x = price, y = listing.age)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Price vs. Listing Duration",
    x = "Price",
    y = "Listing Duration (Days)"
  )

# SIDE BY SIDE
library(patchwork)

# Function to create scatter plot
create_scatter_plot <- function(golf, model_name) {
  ggplot(golf, aes(x = price, y = listing.age)) +
    geom_point(color = "blue", alpha = 0.6) +
    labs(
      title = paste("Price vs. Listing Duration for", model_name),
      x = "Price",
      y = "Listing Duration (Days)"
    )
}

# Create scatter plots using lapply and arrange them with patchwork
list_of_datasets <- list(golf, audi, toyota, volvo, skoda)
car_model_names <- c("VW Golf", "Audi A3", "Toyota Yaris", "Volvo XC60", "Skoda Octavia")

plots <- lapply(seq_along(list_of_datasets), function(i) {
  create_scatter_plot(list_of_datasets[[i]], car_model_names[i])
})

# Combine plots using the `+` operator from patchwork and display them side by side
plot_arrangement <- wrap_plots(plots, ncol = 2)  # Adjust ncol for layout

# Display the combined plots
plot_arrangement

# overall, there is no distinct pattern that explains the relationship between the price and the listing age.

# GRAPH 3 : Boxplot : kilometers by listing age (réessayer)
# Create individual boxplots for each dataset
library(cowplot)  # for plot_grid

# Create individual boxplots for each dataset
plot_model1 <- ggplot(golf, aes(x = as.factor(listing.age), y = kilometers)) +
  geom_boxplot() +
  labs(title = "Model 1")

plot_model1
plot_model2 <- ggplot(audi, aes(x = as.factor(listing.age), y = kilometers)) +
  geom_boxplot() +
  labs(title = "Model 2")

plot_model3 <- ggplot(volvo, aes(x = as.factor(listing.age), y = kilometers)) +
  geom_boxplot() +
  labs(title = "Model 3")

plot_model4 <- ggplot(toyota, aes(x = as.factor(listing.age), y = kilometers)) +
  geom_boxplot() +
  labs(title = "Model 4")

plot_model5 <- ggplot(skoda, aes(x = as.factor(listing.age), y = kilometers)) +
  geom_boxplot() +
  labs(title = "Model 5")

# Arrange plots side by side using patchwork
combined_plots <- plot_model1 + plot_model2 + plot_model3 + plot_model4 + plot_model5
combined_plots

# GRAPHS 4 :
# bar chart - Fuel Type vs. Listing Duration
# golf
average_duration <- golf %>%
  group_by(fuel.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_duration, aes(x = fuel.type, y = avg_duration, fill = fuel.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "VW Golf",
    x = "Fuel Type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# listings that stay the longest are hybrid cars, whereas listings that stay the shortest are electric

# audi
average_duration_audi <- audi %>%
  group_by(fuel.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_duration_audi, aes(x = fuel.type, y = avg_duration, fill = fuel.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Audi A3",
    x = "Fuel Type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# listings that stay the longest are hybrid cars, whereas listings that stay the shortest are CNG

# volvo
average_duration_volvo <- volvo %>%
  group_by(fuel.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_duration_volvo, aes(x = fuel.type, y = avg_duration, fill = fuel.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Volvo XC60",
    x = "Fuel Type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# listings that stay the longest are hybrid cars, whereas listings that stay the shortest are hybrid/diesel

# skoda
average_duration_skoda <- skoda %>%
  group_by(fuel.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_duration_skoda, aes(x = fuel.type, y = avg_duration, fill = fuel.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Skoda Octavia",
    x = "Fuel Type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# listings that stay the longest are CNG cars, whereas listings that stay the shortest are hybrid/electric

# toyota 
average_duration_toyota <- toyota %>%
  group_by(fuel.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_duration_toyota, aes(x = fuel.type, y = avg_duration, fill = fuel.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Toyota Yaris",
    x = "Fuel Type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# listings that stay the longest are diesel cars, whereas listings that stay the shortest are hybrid/electric

# bar chart - Transmission vs. Listing Duration
# golf
average_transmission_duration <- golf %>%
  group_by(transmission) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_duration, aes(x = transmission, y = avg_duration, fill = transmission)) +
  geom_bar(stat = "identity") +
  labs(
    title = "VW Golf",
    x = "Transmission",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# audi
average_transmission_duration_audi <- audi %>%
  group_by(transmission) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_transmission_duration_audi, aes(x = transmission, y = avg_duration, fill = transmission)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Audi A3",
    x = "Transmission",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# volvo
average_transmission_duration_volvo <- volvo %>%
  group_by(transmission) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_transmission_duration_volvo, aes(x = transmission, y = avg_duration, fill = transmission)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Volvo XC60",
    x = "Transmission",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# skoda
average_transmission_duration_skoda <- skoda %>%
  group_by(transmission) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_transmission_duration_skoda, aes(x = transmission, y = avg_duration, fill = transmission)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Skoda Octavia",
    x = "Transmission",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# toyota
average_transmission_duration_toyota <- toyota %>%
  group_by(transmission) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_transmission_duration_toyota, aes(x = transmission, y = avg_duration, fill = transmission)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Toyota Yaris",
    x = "Transmission",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# bar chart - Body type vs. Listing duration
# golf
average_btduration <- golf %>%
  group_by(body.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_btduration, aes(x = body.type, y = avg_duration, fill = body.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "VW Golf",
    x = "body.type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# audi
average_btduration_audi <- audi %>%
  group_by(body.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_btduration_audi, aes(x = body.type, y = avg_duration, fill = body.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Audi A3",
    x = "body.type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# volvo
average_btduration_volvo <- volvo %>%
  group_by(body.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_btduration_volvo, aes(x = body.type, y = avg_duration, fill = body.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Volvo XC60",
    x = "body.type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# skoda
average_btduration_skoda <- skoda %>%
  group_by(body.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_btduration_skoda, aes(x = body.type, y = avg_duration, fill = body.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Skoda Octavia",
    x = "body.type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# toyota
average_btduration_toyota <- toyota %>%
  group_by(body.type) %>%
  summarise(avg_duration = mean(listing.age, na.rm = TRUE))

ggplot(average_btduration_toyota, aes(x = body.type, y = avg_duration, fill = body.type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Toyota Yaris",
    x = "body.type",
    y = "Average Listing Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# GRAPH 5 : Heatmap - Correlation Matrix

library(ggplot2)
library(reshape2)  # For data manipulation

# Calculate correlations between variables and listing duration
correlation_matrix <- cor(golf[, c("price", "kilometers", "power", "vehicle.age", "listing.age")])

# Convert the correlation matrix to a data frame
correlation_df <- melt(correlation_matrix)

# Create a heatmap for correlations with listing duration
ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(
    title = "Correlation Heatmap with Listing Duration",
    x = "Variables",
    y = "Variables"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# most variables are negatively correlated, strongly or not. 

# GRAPH 6 : Violin Plot - Drivetrain vs. Listing Duration

ggplot(golf, aes(x = drivetrain, y = listing.age, fill = drivetrain)) +
  geom_violin() +
  labs(
    title = "Distribution of Listing Durations by Drivetrain",
    x = "Drivetrain",
    y = "Listing Duration"
  ) +
  theme_minimal()

# SIDE BY SIDE
list_of_datasets <- list(golf, audi, toyota, volvo, skoda)
car_model_names <- c("VW Golf", "Audi A3", "Toyota Yaris", "Volvo XC60", "Skoda Octavia")

# Create a function to generate violin plots
create_violin_plot <- function(data, model_name) {
  ggplot(data, aes(x = drivetrain, y = listing.age, fill = drivetrain)) +
    geom_violin() +
    labs(
      title = paste("Distribution of Listing Durations by Drivetrain -", model_name),
      x = "Drivetrain",
      y = "Listing Duration"
    ) +
    theme_minimal()
}

# Create and display violin plots for each dataset
plots <- lapply(seq_along(list_of_datasets), function(i) {
  create_violin_plot(list_of_datasets[[i]], car_model_names[i])
})

# Display the plots side by side using grid.arrange from the gridExtra package
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2)
