#
# Title:    Introduction to Data Visualization with ggplot2
# Purpose:  (Knowledge Development) Working on learning R for data science and visualization
# Author:   Billy Caughey
# Date:     2020.07.14 - Initial Build
# 

##### Drawing my first plot #####
# Load the ggplot2 package
library(ggplot2)

# Explore the mtcars data frame with str()
str(mtcars)

# Execute the following command
ggplot(mtcars, aes(cyl, mpg)) +
    geom_point()

##### Data columns types affect plot types #####
# Load the ggplot2 package
library(ggplot2)

# Change the command below so that cyl is treated as factor
ggplot(mtcars, aes(factor(cyl), mpg)) +
    geom_point()

##### Mapping data columns to aesthetics #####
# Edit to add a color aesthetic mapped to disp
ggplot(mtcars, aes(wt, mpg, color = disp)) +
    geom_point()

# Change color aesthetic to size aesthetic
ggplot(mtcars, aes(wt, mpg, size = disp)) +
    geom_point()

##### Understanding Variables #####
# What does the error in the code below mean? 
# - Shape requires a categorical, not continuous, variable
ggplot(mtcars, aes(wt, mpg, shape = disp)) +
    geom_point()

##### Adding Geometries #####
# Explore the diamonds data frame with str()
str(diamonds)

# Add geom_point() with +
ggplot(diamonds, aes(carat, price)) +
    geom_point()

# Add geom_smooth() with +
ggplot(diamonds, aes(carat, price)) +
    geom_point() +
    geom_smooth()

##### Changing one geom or every geom ####
# Map the color aesthetic to clarity
ggplot(diamonds, aes(carat, price, color = clarity)) +
    geom_point() +
    geom_smooth()

# Make the points 40% opaque
ggplot(diamonds, aes(carat, price, color = clarity)) +
    geom_point(alpha = 0.40) +
    geom_smooth()

##### Saving plots as variables #####
# Draw a ggplot
plt_price_vs_carat <- ggplot(
    # Use the diamonds dataset
    diamonds,
    # For the aesthetics, map x to carat and y to price
    aes(x = carat, y = price)
)

# Add a point layer to plt_price_vs_carat
plt_price_vs_carat + geom_point()

# Edit this to make points 20% opaque: plt_price_vs_carat_transparent
plt_price_vs_carat_transparent <- plt_price_vs_carat + geom_point(alpha = 0.20)

# See the plot
plt_price_vs_carat_transparent

# Edit this to map color to clarity,
# Assign the updated plot to a new object
plt_price_vs_carat_by_clarity <- plt_price_vs_carat + geom_point(aes(color = clarity))

# See the plot
plt_price_vs_carat_by_clarity

##### All about aesthetics: color, shape and size #####

# Map x to mpg and y to fcyl
ggplot(mtcars, aes(x = fcyl, y = mpg)) +
    geom_point()

# Map wt onto x, mpg ont y, and fcyl onto color
ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
    geom_point()

# Modify th epoint layer of the previous plot by chagning the shape argument to 1 and increasing the size to 4
ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
    geom_point(shape = 1, size = 4)

##### All about aesthetics: color vs fill #####

# Map fcyl to fill
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
    geom_point(shape = 1, size = 4)

# Change shape to 21, and add an alpha argument set to 0.6
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
    geom_point(shape = 21, size = 4, alpha = 0.6)

# in the ggplot() aesthestics map fam to color
ggplot(mtcars, aes(wt, mpg, fill = fcyl, color = fam)) +
    geom_point(shape = 21, size = 4, alpha = 0.6)

##### All about aesthetics: comparing aesthestics #####

# Establish the base layer
plt_mpg_vs_wt <- ggplot(mtcars, aes(x = wt, y = mpg))

# Map fcyl to size
plt_mpg_vs_wt +
    geom_point(aes(size = fcyl))

# Map fcyl to alpha, not size
plt_mpg_vs_wt +
    geom_point(aes(alpha = fcyl))

# Change the mapping again. this time fcyl should be mapped onto shape
plt_mpg_vs_wt +
    geom_point(aes(shape = fcyl))

# Swap the geom layer: change the points to text 
# Change the mapping again. This time fcyl should be mapped onto label
plt_mpg_vs_wt +
    geom_text(aes(label = fcyl))

##### All about attributes: color, shape, size and alpha 

# A hexadecimal color
my_blue <- "#4ABEFF"

ggplot(mtcars, aes(wt, mpg)) +
    # Set the point color and alpha
    geom_point(color = my_blue, alpha = 0.6)

# Change color mapping to fill. 
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
    # Set the point color and alpha
    geom_point(color = my_blue, alpha = 0.6, size = 10, shape = 1)
