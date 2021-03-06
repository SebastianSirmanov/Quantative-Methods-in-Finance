#####Problem 1#####
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE. Example:
# i <- 1
# while (i < 6) {
#   print(i)
#   i <- i + 1
# } 
#In your case you want to loop until your budget is > 0.
# Budget <- 100
# while (Budget > 0) {
#   Do something
# } 
#Pay attention to the fact that you can't bet more money than you have.
#If you lose 1, 2, 4, 8, 16, 32. Then your remaining money will be 
#100-32-16-8-4-2-1 = 37, so you can bet max 37 USD.
#####Problem 1#####

ResultVector <- NULL
  for(i in 1:1000){
    Budget <- 100
    Bet <- 1
    BetNumber <- 0
    while (Budget > 0) {
      BetNumber <- BetNumber + 1
      if (Bet > Budget){
        Bet <- Budget
      }
      cointoss <- sample (c("win", "lose"), 1, prob = c(0.486, 0.514))
      if(cointoss == "win"){
      Budget <- Budget + Bet
      Bet <- 1
    } else {
      Budget <- Budget - Bet
      Bet <- Bet * 2
    }
  }
  ResultVector <- c(ResultVector, BetNumber)
}
