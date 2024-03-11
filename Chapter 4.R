#Chapter 4
#Selecting Values
deck [ , ]

#Positive Integears
head(deck)

deck[1,1]

deck[1, c(1, 2, 3)]

new <- deck[1, c(1, 2, 3)]
new

deck[c(1, 1), c(1, 2, 3)]

vec <- c(6, 1, 3, 6, 10, 5)
vec[1:3]

deck[1:2, 1:2]

deck[1:2, 1]

deck[1:2, 1, drop = FALSE]

#Negative Integers
deck[-(2:52), 1:3]

deck[c(-1, 1), 1] #Shows error becuae can't have positive and negative

#Zero, useless
deck[0, 0]

#Blank Spaces
deck[1, ]

#logical Values
deck[1, c(TRUE, TRUE, FALSE)]

rows <- c(TRUE, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F, F, F, F, F, F)
deck[rows, ]

#Names
deck[1, c("face", "suit", "value")]

deck[ , "value"]

#Deal a Card (Creating the Fuction)
deal <- function(cards) {
  cards[1, ]
}

deal(deck)

deal(deck)

deal(deck)

#Shuffle the Deck
deck2 <- deck[1:52, ]

head(deck2)

deck3 <- deck[c(2, 1, 3:52), ]

random <- sample(1:52, size = 52)
random

deck4 <- deck[random, ]
head(deck4)

shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}

deal(deck)

deck2 <- shuffle(deck)

deal(deck2)

#Dollar signs and double brackets
deck$value

mean(deck$value)

median(deck$value)

lst <- list(numbers = c(1, 2), logical = TRUE, strings = c("a", "b", "c"))
lst

lst[1]

sum(lst[1])

lst$numbers

sum(lst$numbers)

lst[[1]]

lst["numbers"]

lst[["numbers"]]

