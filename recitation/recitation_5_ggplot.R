library(ggplot2)

# what is ggplot?
# it's a grammar for graphics
# uses consistent building blocks to produce pictures
# it's really pretty looking, but can't do 3D or some other stuff (networks)

# read in some data
housing <- read.csv("inst/extdata/landdata-states.csv")
head(housing[1:5])

# Why is ggplot sometimes bad?
# it's
