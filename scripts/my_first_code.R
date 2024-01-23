weight_kg <- 55 
# cmd enter for running
# assignment operator for mac: alt - 
# explicit names for objects for reproducibility, objectnames cannot start with a number, 
# no dots on variable names, use underscore 
# write command in brackets to get the result in the console i.e. (weight_kg <- 55)

(2.2*weight_kg)

weight_lb <- 2.2*weight_kg
weight_kg <- 100
# if you update a value, you have to rerun other commands again, no automatic updating

# red titles are unsafed files 
# always use tab complete to write something, no typos 

mass <- 47.5
age <- 122
mass <- mass*2
age <- age-20 
mass_index <- mass/age

weight_kg <- sqrt(9)

round(3.14159, digits=2)
args(round) # gives information about the function 

# vector is a series of elements, all in one type, use c()
# don't mix numeric, character and logical 
# use " to indicate a word, otherwise it could be recognized as an object 
weight_g <- c(50, 60, 65, 82)
animals <- c("mouse", "rat", "dog")

# length of vector and type of data
length(animals)
class(animals)
class(weight_g)

# structure of the object
str(animals)

# add element to a vector
animals <- c("cat", animals)
animals <- c(animals, "frog")
typeof(animals)

# subsetting a vector
animals[2]
animals[c(1,2)]
more_animals <- animals[c(1,2,3,2,1,4)]

# operators: <, >, equal ==, different !=, <=, >=
# combining with and &, or |
# ! reverse a logical 

weight_g[c(FALSE, FALSE, TRUE, TRUE)]
weight_g > 63
weight_g[weight_g > 63]
weight_g[weight_g >63 & weight_g <80]
weight_g[weight_g < 58 | weight_g > 80]
weight_g==65

animals[animals == "rat" | animals == "frog"]

# %in% helps us find all elements corresponding to a vector of choice 
animals %in% c("rat", "frog", "cat", "dog", "duck")

# an example of a vector with missing data
heights <- c(2,4,4,NA,6)
mean(heights)
mean(heights, na.rm=TRUE)
# na.rm to remove NA values 
max(heights, na.rm=T)
is.na(heights)
heights[!is.na(heights)]
# omit the missing data
na.omit(heights)
# extract the complete cases
heights[complete.cases(heights)]
