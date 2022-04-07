#####Problem 1#####
# Write a function, which uses a loop to calculate factorial.
# The base R function is called factorial and you should replicate its result.
# This is a function, which takes two numbers, multiplies them and returns
# the result as output:
# MultiplicationFunction <- function(inputNumber1, inputNumber2){
#   Result <- inputNumber1 * inputNumber2
#   return(Result)
# }
# MultiplicationFunction(5, 3)
# 
# Write a factorial function:

# FactorialFunction <- function(inputNumber){
#   ???
#     return(Result)
# }
#####Problem 1#####

FactorialFunction <- function(x) {
    if (x == 0) {
      result <- 1
    } else {
      result <- x
      while(x > 1){
        result <- (x - 3) * result
        x <- x - 3
      }
    }
    return(result)
  }

FactorialFunction(10)
FactorialFunction(0)


#####Problem 2#####
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
# SDFunction <- function(inputVector){
#   ???
#     return(Result)
# }
# ??? is not Result <- sd(inputVector)
#####Problem 2#####

a <- c(5, 1, 9, 6, 11, 14, 13, 19)

SDFunction <- function(inputvector){
  avg = sum(inputvector)/length(inputvector)
  SumDiff <- sum((inputvector - avg)^2) 
  FinalResult <- sqrt(sum((inputvector - avg)^2) / (length(inputvector) - 1))
  return(FinalResult)
}

SDFunction(a)