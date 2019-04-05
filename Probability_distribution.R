#Monte Hall problem
#not changing your mind has lower probabilty of success
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors,1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
})

mean(stick)

#changing your mind has higher probabilty of success
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors,1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  switch <- sample(doors[!doors %in% c(my_pick, show)])
  switch == prize_door
})

##
# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- replicate(B,
                       max(rnorm(10000, 100, 15))
                       
)

# Make a histogram of the highest IQ scores.
hist(highestIQ)

mean(stick)

##
beads <- rep(c("red","blue"), times=c(2,3))
X <- ifelse(sample(beads,1) = "blue",1,0)

##
color <- rep(c("green","red","black"),c(2,18,18))
n <- 1000
X <- sample(ifelse( color == "red", "-1", "1"), n, replace = TRUE)
X[1:10]

n <- 1000
X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19,10/19))
X[1:10]
sum(X)

##standard error formular
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Compute the standard error of the random variable
S <- sample(c(17,-17), 1, replace=TRUE, prob=c(p_green,p_not_green))
X <- sample(c(17,-1), 1, replace=TRUE, prob=c(p_green,p_not_green))
X <- sum(X) * p_green
#S <- sum(X) * p_green
(17 - -1)*sqrt(p_green * p_not_green)
#sqrt(sum((S - X)^2)/length(X))

#expected value
X <- sample(c(17,-1), 1, replace=TRUE, prob=c(p_green,p_not_green))
sum(X) * p_green

#random value
X <- sample(c(17,-17), 1, replace=TRUE, prob=c(p_green,p_not_green))
X

#standard error
S <- sample(c(17,-17), 1, replace=TRUE, prob=c(p_green,p_not_green))
X <- sample(c(17,-1), 1, replace=TRUE, prob=c(p_green,p_not_green))
X <- sum(X) * p_green
#S <- sum(X) * p_green
(17 - -1)*sqrt(p_green * p_not_green)

#sum of winnings
X <- sample(c(17,-1),n,replace=TRUE,prob=c(p_green, p_not_green))
S <- sum(X)
S

#sum of 1000 expected outcomes
X <- sample(c(17,-1),1,replace=TRUE,prob=c(p_green,p_not_green))
n * sum(X) * p_green

#standard error
#square root of no of times multiplied by formular for standard error.
X <- sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))
S <- n * sum(X) * p_green
E <- sqrt(n) * abs(17--1)*sqrt(p_green * p_not_green)
E

##replicate over 10000 times
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B, {
  X <- sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))
  sum(X)
})

# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)



##10000 runs##
# The variable `n` specifies the number of independent bets on green
n <- 10000

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)

# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times

S <- replicate(B, {
  X <- sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))
  mean(X)
})

# Compute the average of `S`
mean(S)

# Compute the standard deviation of `S`
sd(S)

##Bank Loan##
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Calcualte the expected loss due to default out of 10,000 loans
10000 * p_default * loss_per_foreclosure

##getting optimal interest rate that would keep business profitable
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.05)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
l <- loss_per_foreclosure
p <- p_default
x <- -l*(n*p - z*sqrt(n*p*(1-p)))/(n*(1-p)+z*sqrt(n*p*(1-p)))

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x/180000
