library(ggplot2)

# What is a simulation?
# "In repeated samples" : If we gave the same survey questions to a different
# set of people, how would the results change?
# For stat models: if our data change, do we get the same answers?


# Birthday Problem
# 24 people, what is the probability that 2 have the same birthday?
birthday_problem <- function(npeople,
  seed = sample.int(.Machine$integer.max, 1))
{
  days <- seq(1, 365, 1)
  room <- sample(days, npeople, replace = TRUE)
  ifelse(length(unique(room)) < npeople, 1, 0)
}
birthday_problem(npeople = 24)


simulate_birthday_problem <- function(nsims, npeople)
{
  sum(replicate(nsims, birthday_problem(npeople = npeople)))/nsims
}
simulate_birthday_problem(1000, 30)
bday_probs <- sapply(seq(2, 100, 1), simulate_birthday_problem, nsims = 1000)
plot_data <- data.table::data.table(number_of_people_in_room = seq(2, 100, 1),
  probability_of_two_sharing_bday = bday_probs)
ggplot(plot_data, aes(x = number_of_people_in_room,
  y = probability_of_two_sharing_bday)) +
  geom_point() + theme_bw()
