# R-script for EC2203 lecture 2
# Author: Jesper Bagger
# Date: 07/10/2020

library(AER) # include Applied Econometrics with R package; contains many datasets

data(CASchools) # Load CASchools data

# Generate a couple of useful variables
CASchools$STR <- CASchools$students/CASchools$teachers  # Student-teacher ratio
CASchools$Score <- (CASchools$read + CASchools$math)/2  # Student test score

# Define population regression function (of x) as f(x) = 695 - 2*x
popregfct <- function(x){695-2*x} 
# Plot a curve of popregfct 
curve(popregfct, 
      from = 5, to = 35, # Evaluate in x values from 5 to 35
      xlab = "Student-teacher ratio", # Label on x-axis
      ylab = "Test score", # Label on y-axis
      xlim = c(10, 30),  # Range of x-axis (from 10 to 30)
      ylim = c(600, 720), # Range of y-axis (from 600 to 720)
      lwd = 3, # Set linewidth to 3
      col = "blue") # Make the plotted line blue

# Add arrows to plot, indicating two errors
curve(popregfct, 
      from = 5, to = 35, # Evaluate in x values from 5 to 35
      xlab = "Student-teacher ratio", # Label on x-axis
      ylab = "Test score", # Label on y-axis
      xlim = c(10, 30),  # Range of x-axis (from 10 to 30)
      ylim = c(600, 720), # Range of y-axis (from 600 to 720)
      lwd = 3, # Set linewidth to 3
      col = "blue") # Make the plotted line blue
# Connect points (24,popregfct(24)) and (24,676.85) by an arrow
arrows(x0 = 24, y0 = popregfct(24), x1 = 24, y1 = 676.85, 
       code = 3, # Arrow head in both ends
       lwd = 3, # Set linewidth to 3
       col = "lightblue") # Make arrow lightblue
points(24,676.85, # Plot data-point (24,676.85)
       col = "blue") # Plot data-point in blue
# Connect points (15.25,popregfct(15.25)) and (15.25,635.45) by an arrow
arrows(x0 = 15.25, y0 = popregfct(15.25), x1 = 15.25, y1 = 635.45, 
       code = 3, # Arrow head in both ends
       lwd = 3, # Set linewidth to 3
       col = "lightblue") # Make arrow lightblue
points(15.25,635.45, # Plot data-point (15,635.45)
       col = "blue") # Plot data-point in blue

# Adding a straight line to a scatter in R
# Scatter plot of Score against STR
plot(CASchools$STR,CASchools$Score, 
     col = "blue", # Color of data points
     xlab = "Student-teacher ratio",  # Label on x-axis
     ylab = "Test score",   # Label on y-axis
     xlim = c(10, 30),  # Range of x-axis (from 10 to 30)
     ylim = c(600, 720)) # Range of y-axis (from 600 to 720)
# Add line with intercept 695 and slope -2 to the plot 
abline(695,-2, 
       col = "gray", # Make the line gray
       lwd = 3) # Set linewidth to 3

# Adding yet another straight line to a scatter in R
# Scatter plot of Score against STR
plot(CASchools$STR,CASchools$Score, 
     col = "blue", # Color of data points
     xlab = "Student-teacher ratio",  # Label on x-axis
     ylab = "Test score",   # Label on y-axis
     xlim = c(10, 30),  # Range of x-axis (from 10 to 30)
     ylim = c(600, 720)) # Range of y-axis (from 600 to 720)
# Add line with intercept 695 and slope -2 to the plot 
abline(695,-2, 
       col = "gray", # Make the line gray
       lwd = 3) # Set linewidth to 3
# Add line with intercept 713 and slope -3 to the plot 
abline(mean(CASchools$Score)+3*mean(CASchools$STR),-3,
       col = "red", # Make the line red
       lwd = 3) # Set linewidth to 3

# Adding predicted Scores to scatter plot (in red)
# Scatter plot of Score against STR
plot(CASchools$STR,CASchools$Score, 
     col = "blue", # Color of data points
     xlab = "Student-teacher ratio",  # Label on x-axis
     ylab = "Test score",   # Label on y-axis
     xlim = c(10, 30),  # Range of x-axis (from 10 to 30)
     ylim = c(600, 720)) # Range of y-axis (from 600 to 720)
# Add line with intercept 695 and slope -2 to the plot 
abline(695,-2, 
       col = "gray", # Make the line gray
       lwd = 3) # Set linewidth to 3
# Add line with intercept 713 and slope -3 to the plot 
abline(mean(CASchools$Score)+3*mean(CASchools$STR),-3,
       col = "red", # Make the line red
       lwd = 3) # Set linewidth to 3
# Add fitted values to the plot
points(CASchools$STR,713 - 3*CASchools$STR,
       col = "red") # Make the fitted values red

# Indicating residuals in the plot
# Scatter plot of Score against STR
plot(CASchools$STR,CASchools$Score, 
     col = "blue", # Color of data points
     xlab = "Student-teacher ratio",  # Label on x-axis
     ylab = "Test score",   # Label on y-axis
     xlim = c(10, 30),  # Range of x-axis (from 10 to 30)
     ylim = c(600, 720)) # Range of y-axis (from 600 to 720)
# Add line with intercept 695 and slope -2 to the plot 
abline(695,-2, 
       col = "gray", # Make the line gray
       lwd = 3) # Set linewidth to 3
# Add line with intercept 713 and slope -3 to the plot 
abline(mean(CASchools$Score)+3*mean(CASchools$STR),-3,
       col = "red", # Make the line red
       lwd = 3) # Set linewidth to 3
# Add fitted values to the plot
points(CASchools$STR,713 - 3*CASchools$STR,
       col = "red") # Make the fitted values red
# Connect points (24,popregfct(24)) and (24,676.85) by an arrow
arrows(x0 = 24, y0 = 713 - 3*24, x1 = 24, y1 = 676.85, 
       code = 3, # Arrow head in both ends
       lwd = 3, # Set linewidth to 3
       col = "pink") # Make arrow green
# Connect points (15,popregfct(15)) and (15,635.45) by an arrow
arrows(x0 = 15.25, y0 = 713 - 3*15.25, x1 = 15.25, y1 = 635.45, 
       code = 3, # Arrow head in both ends
       lwd = 3, # Set linewidth to 3
       col = "pink") # Make arrow green

# Estimate b0,b1 in Score = b0 + b1 STR + u by OLS; 
# then assign output to lm1
lm1 <- lm(Score ~ STR, data = CASchools) 
lm1 # Print output to console

# Add OLS sample regression function to scatter
# Scatter plot of Score against STR
plot(CASchools$STR,CASchools$Score, 
     col = "blue", # Color of data points
     xlab = "Student-teacher ratio",  # Label on x-axis
     ylab = "Test score",   # Label on y-axis
     xlim = c(10, 30),  # Range of x-axis (from 10 to 30)
     ylim = c(600, 720)) # Range of y-axis (from 600 to 720)
# Add line with intercept 695 and slope -2 to the plot 
abline(695,-2, 
       col = "gray", # Make the line gray
       lwd = 3) # Set linewidth to 3
# Add sample regression function to the plot
abline(lm1,
       col = "red", # Make the line red
       lwd = 3) # Set linewidth to 3

