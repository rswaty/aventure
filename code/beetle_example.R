

### Beetle dataset creation, plotting and saving demo
# Randy Swaty
# January 21, 2023
# Base R

### Save this code now!


###  Create dataset

# Set a seed for reproducibility
set.seed(123)

# Create a vector of beetle names
beetle_names <- c("Ladybug", "Stag Beetle", "Firefly Beetle", "Dung Beetle", "Jewel Beetle")

# Create a vector of beetle lengths
beetle_lengths <- runif(20, 1, 5)  # Random lengths between 1 and 5

# Create a vector of beetle colors
beetle_colors <- sample(c("Red", "Black", "Green", "Yellow", "Blue"), 20, replace = TRUE)

# Create the random beetle dataframe
beetle_df <- data.frame(Name = sample(beetle_names, 20, replace = TRUE),
                        Length = beetle_lengths,
                        Color = beetle_colors)

# save this fake data to data directory-go slow!
write.csv(beetle_df, file = "data/fake_beetle_data.csv")


### Make quick chart

length_chart <- barplot(height = beetle_df$Length, names = beetle_df$Name)


### Save

# save using the r-studio tool.  not the best, but will work for now.  
# 1000x1000 size, "/images/fake_beetle_plot.png"
