# 2602112215 - Darrius Salim

# Import Libraries
library(corrplot)
library(ggplot2)
library(viridis)
library(psych)
library(stats)
library(ggrepel)

# Read Dataset
data1 <- read.csv(file.choose(), stringsAsFactors = TRUE)

# Check the datasets dimension
dim(data1)
# View/Show the datasets
View(data1)
# Check the missing data in the datasets
table(complete.cases(data1))
# False 117 menandakan bahwa ada 117 baris didalam dataset tersebut yang memiliki missing data/data kosong
# True 684 menandakan bahwa ada 684 baris didalam dataset tersebut yang memiliki data yang lengkap

str(data1)
head(data1)
tail(data1)

summary(data1)
describe(data1)
describeBy(data1, group = data1$name)

sapply(data1, function(x) sum(is.na(x)))

boxplot(data1[,c('hp', 'attack', 'defense', 'sp_attack', 'sp_defense', 'speed')], main = "Combat Stats Boxplot")
boxplot(data1$weight_kg, main = "Weight BoxPlot")
boxplot(data1$height_m, main = "Height BoxPlot")

qqnorm(data1$height_m, main = "Height Normal QQ Plot")
qqnorm(data1$weight_kg, main = "Weight Normal QQ Plot")
qqnorm(data1$hp, main = "Normal QQ Plot")
qqnorm(data1$attack, main = "Normal QQ Plot")
qqnorm(data1$defense, main = "Normal QQ Plot")
qqnorm(data1$sp_attack, main = "Normal QQ Plot")
qqnorm(data1$sp_defense, main = "Normal QQ Plot")
qqnorm(data1$speed, main = "Normal QQ Plot")

dup_r <- duplicated(data1$abilities)
sum(dup_r)
dup_r1 <- duplicated(data1$classfication)
sum(dup_r1)
dup_r2 <- duplicated(data1$type1)
sum(dup_r2)
dup_r3 <- duplicated(data1$type2)
sum(dup_r3)


selected_cols <- c('height_m', 'weight_kg', 'hp', 'attack', 'defense', 'sp_attack', 'sp_defense', 'speed')
subset_data <- data1[selected_cols]
correlation_matrix <- cor(subset_data, use = "complete.obs")


# Create a correlation heatmap
corrplot(correlation_matrix, method = "color")
# Scatter plot between height and base stats
pairs(subset_data[, c('height_m', 'hp', 'attack', 'defense', 'sp_attack', 'sp_defense', 'speed')])

# Scatter plot between weight and base stats
pairs(subset_data[, c('weight_kg', 'hp', 'attack', 'defense', 'sp_attack', 'sp_defense', 'speed')])



# Plot histograms for numeric variables (e.g., 'hp', 'attack', 'defense')
par(mfrow = c(2, 2))  # Setting up a 2x2 grid for multiple histograms
hist(data1$hp, main = "HP Distribution", xlab = "HP")
hist(data1$attack, main = "Attack Distribution", xlab = "Attack")
hist(data1$defense, main = "Defense Distribution", xlab = "Defense")


# Density plots for numeric variables
par(mfrow = c(2, 2))
plot(density(data1$hp), main = "HP Distribution", xlab = "HP")
plot(density(data1$attack), main = "Attack Distribution", xlab = "Attack")
plot(density(data1$defense), main = "Defense Distribution", xlab = "Defense")


# Scatter plots for relationships between numeric variables
plot(data1$hp, data1$attack, main = "HP vs. Attack", xlab = "HP", ylab = "Attack")
plot(data1$defense, data1$attack, main = "Defense vs. Attack", xlab = "Defense", ylab = "Attack")


# Pair plots for multiple numeric variables
pairs(data1[, c('hp', 'attack', 'defense', 'sp_attack', 'sp_defense', 'speed')])

# Calculate the correlation matrix
correlation_matrix <- cor(data1[, c('hp', 'attack', 'defense', 'sp_attack', 'sp_defense', 'speed')])

# Create a correlation heatmap
corrplot(correlation_matrix, method = "color")

# Plot histograms with colors and peak range annotations
par(mfrow = c(1, 1))
# par(mfrow = c(2, 2), mar = c(5, 5, 6, 2) + 0.1)

# Function to add peak range annotation
add_peak_annotation <- function(hist_obj, col, main_title) {
  peak <- hist_obj$mids[which.max(hist_obj$counts)]
  text(peak, max(hist_obj$counts), sprintf("Peak: %.2f", peak), col = col, pos = 3)
}

# Function to add exact frequency values to histogram bars
add_exact_freq_values <- function(hist_obj, col) {
  for (i in 1:length(hist_obj$mids)) {
    freq_value <- hist_obj$counts[i]
    text(hist_obj$mids[i], freq_value + 15, labels = freq_value, col = col, pos = 3)
  }
}

# Histogram for 'hp' with color and peak annotation
hp_hist <- hist(data1$hp, col = "skyblue", main = "HP Distribution", xlab = "HP", xlim = c(0, 300), ylim = c(0, 300))
add_peak_annotation(hp_hist, "blue", "HP Distribution")
add_exact_freq_values(hp_hist, "black")

# Histogram for 'attack' with color and peak annotation
attack_hist <- hist(data1$attack, col = "lightcoral", main = "Attack Distribution", xlab = "Attack", xlim = c(0, 200), ylim = c(0, 250))
add_peak_annotation(attack_hist, "red", "Attack Distribution")
add_exact_freq_values(attack_hist, "black")

# Histogram for 'defense' with color and peak annotation
defense_hist <- hist(data1$defense, col = "lightgreen", main = "Defense Distribution", xlab = "Defense", xlim = c(0, 250), ylim = c(0, 250))
add_peak_annotation(defense_hist, "green", "Defense Distribution")
add_exact_freq_values(defense_hist, "black")


# Calculate the average base stats for each type
average_stats <- aggregate(cbind(hp, attack, defense, sp_attack, sp_defense, speed) ~ type1, data = data1, mean)

# Calculate the overall average for each type
average_stats$overall <- rowMeans(average_stats[, -1])

# Create a bar chart
ggplot(average_stats, aes(x = reorder(type1, -overall), y = overall, fill = type1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(overall, 2)), vjust = -0.5, size = 3) +  # Add text annotations
  labs(title = "Overall Strength of Pokémon Types",
       x = "Pokémon Types",
       y = "Overall Average Base Stats") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE) +
  ylim(0, 100)  # Set y-axis limit

# Create a bar chart
ggplot(data1, aes(x = as.factor(generation))) +
  geom_bar(fill = 'skyblue', color = 'black', stat = 'count', show.legend = FALSE) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3) +  # Add text annotations
  labs(title = 'Number of New Pokémon per Generation',
       x = 'Generation',
       y = 'Number of Pokémon') +
  theme_minimal()

# Create a bar chart for the count of each primary type
ggplot(data1, aes(x = type1, fill = type1)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3) +  # Add text annotations
  labs(title = 'Distribution of Pokémon Primary Types',
       x = 'Primary Type',
       y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma")

# Create a bar chart for the count of each secondary type
ggplot(data1, aes(x = type2, fill = type2)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3) +  # Add text annotations
  labs(title = 'Distribution of Pokémon Secondary Types',
       x = 'Secondary Type',
       y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE, option = "inferno")

# Create a table of type combinations
type_combinations_table <- table(data1$type1, data1$type2, useNA = "ifany")
# Convert the table to a data frame
type_combinations_df <- as.data.frame(as.table(type_combinations_table))
# Rename the columns for clarity
colnames(type_combinations_df) <- c("Type1", "Type2", "Count")
# Exclude rows where either Type1 or Type2 is empty
type_combinations_df <- type_combinations_df[!(type_combinations_df$Type1 == "" | type_combinations_df$Type2 == ""), ]
# Order the data frame by count in descending order
type_combinations_df <- type_combinations_df[order(-type_combinations_df$Count), ]
# Select the top 10 most common type combinations
top_10_combinations <- head(type_combinations_df, 10)
# Create a bar chart with values and bar color for the top 10
ggplot(top_10_combinations, aes(x = interaction(Type1, Type2, lex.order = TRUE), y = Count, fill = interaction(Type1, Type2, lex.order = TRUE))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +  # Add text annotations
  labs(title = 'Top 10 Most Common Pokémon Type Combinations',
       x = 'Type Combinations',
       y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter the dataset to include only legendary Pokémon
legendary_df <- subset(data1, is_legendary == 1)
# Create a bar chart of the types of legendary Pokémon
ggplot(legendary_df, aes(x = type1, fill = type1)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3) +  # Add text annotations
  labs(title = 'Distribution of Legendary Pokémon Types',
       x = 'Type',
       y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "D", end = 0.9)

# Create a table of counts
count_table <- table(data1$generation, data1$type1)
# Convert the table to a data frame
count_df <- as.data.frame(as.table(count_table))
# Rename the columns for clarity
colnames(count_df) <- c("Generation", "Type", "Count")
# Create a heatmap
ggplot(count_df, aes(x = Generation, y = Type, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), vjust = 0.5) +
  labs(title = 'Distribution of Primary Types Across Generations',
       x = 'Generation',
       y = 'Primary Type',
       fill = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "red")

# Create a boxplot of capture rates across generations
ggplot(data1, aes(x = as.factor(generation), y = as.numeric(capture_rate), fill = factor(is_legendary))) +
  geom_boxplot(alpha = 0.7) +
  geom_text(aes(label = capture_rate), position = position_dodge(0.8), vjust = -0.5, size = 3) +
  labs(title = 'Pokémon Capture Rate by Generation',
       x = 'Generation',
       y = 'Capture Rate',
       fill = 'Legendary') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "darkred")) +
  guides(fill = guide_legend(title = "Legendary", keywidth = 1, keyheight = 1, reverse = TRUE))

# Create a boxplot of capture rates across Pokémon types, grouped by legendary status
ggplot(data1, aes(x = type1, y = as.numeric(capture_rate), fill = factor(is_legendary))) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.7) +
  geom_text(aes(label = capture_rate), position = position_dodge(0.8), vjust = -0.5, size = 3) +
  labs(title = 'Capture Rates Across Pokémon Types',
       x = 'Pokémon Type',
       y = 'Capture Rate',
       fill = 'Legendary') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Separate comma-separated abilities into separate rows
data1_split <- data1 %>%
  separate_rows(abilities, sep = ', ') %>%
  filter(abilities != "")  # Remove empty abilities
# Count occurrences of each unique number of abilities for each Pokémon
pokemon_ability_counts <- data1_split %>%
  group_by(name, is_legendary) %>%
  summarize(ability_count = n_distinct(abilities))
# Create a bar plot
ggplot(pokemon_ability_counts, aes(x = as.factor(ability_count), fill = factor(is_legendary))) +
  geom_bar(stat = "count", position = "dodge", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = 'Number of Abilities per Pokémon',
       x = 'Number of Abilities',
       y = 'Count',
       fill = 'Legendary') +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "darkred")) +
  guides(fill = guide_legend(title = "Legendary", keywidth = 1, keyheight = 1, reverse = TRUE))

# Identify the top 5 highest and weightiest Pokémon
top_5_highest <- data1[order(-data1$height_m), ][1:5, ]
top_5_weightiest <- data1[order(-data1$weight_kg), ][1:5, ]
# Identify unique Pokémon from the top 5 lists
unique_top_pokemon <- unique(rbind(top_5_highest, top_5_weightiest), by = "name")
# Create a scatterplot with white borders
ggplot(data1, aes(x = weight_kg, y = height_m, color = factor(is_legendary))) +
  geom_point(size = 3, stroke = 1, aes(shape = factor(is_legendary))) +  # Add white border
  geom_text_repel(data = unique_top_pokemon, aes(label = name), size = 3, box.padding = 0.5, point.padding = 0.5) +
  labs(title = 'Top 5 Highest and Weightiest Pokémon',
       x = 'Weight (kg)',
       y = 'Height (m)',
       color = 'Legendary') +
  theme_minimal() +
  scale_color_manual(values = c("0" = "skyblue", "1" = "darkred")) +
  scale_shape_manual(values = c("0" = 16, "1" = 17)) +  # Define shapes for non-legendary (16) and legendary (17)
  guides(color = guide_legend(title = "Legendary"))

# Create a boxplot for Pokémon Base Total by Generation
ggplot(data1, aes(x = factor(generation), y = base_total, fill = factor(is_legendary))) +
  geom_boxplot(outlier.shape = NA) +  # Remove outliers for clarity
  labs(title = 'Pokémon Base Total by Generation',
       x = 'Generation',
       y = 'Base Total',
       fill = 'Legendary') +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "darkred")) +
  guides(fill = guide_legend(title = "Legendary"))

# Subset the data to relevant columns
type_effectiveness_data <- data1[, c('type1', 'against_bug', 'against_dark', 'against_dragon', 'against_electric',
                                     'against_fairy', 'against_fight', 'against_fire', 'against_flying',
                                     'against_ghost', 'against_grass', 'against_ground', 'against_ice',
                                     'against_normal', 'against_poison', 'against_psychic', 'against_rock',
                                     'against_steel', 'against_water')]
# Reshape the data for plotting
type_effectiveness_data_long <- tidyr::gather(type_effectiveness_data, key = 'type', value = 'effectiveness', -type1)
# Create a type effectiveness heatmapN
ggplot(type_effectiveness_data_long, aes(x = type1, y = type, fill = effectiveness)) +
  geom_tile(color = 'white') +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = 1, limits = c(0, 4)) +
  labs(title = 'Type Effectiveness Heatmap',
       x = 'Defender Type',
       y = 'Attacker Type',
       fill = 'Effectiveness') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Find the top 10 most powerful Pokémon
top_10_pokemon <- data1[order(-data1$base_total), ][1:10, ]

# Create a bar plot to display the top 10 most powerful Pokémon
ggplot(top_10_pokemon, aes(x = reorder(name, -base_total), y = base_total, fill = factor(is_legendary))) +
  geom_bar(stat = 'identity') +
  labs(title = 'Top 10 Most Powerful Pokémon',
       x = 'Pokémon',
       y = 'Base Total',
       fill = 'Legendary') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "darkred")) +
  geom_text(aes(label = base_total), vjust = -0.5, size = 3)  # Add labels with base total values
