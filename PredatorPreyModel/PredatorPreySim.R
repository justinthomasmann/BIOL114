printBiome <- function(biome, chosenXY) {
  cat("Printing location array of Caminalcule locations, 1 = occupied, 0 = unoccupied, X = found\n")
  cat("   A B C D E F G H I J\n")
  for (i in 1:10) {
    if (i != 10) cat(sprintf(" %d ", i))
    else cat(sprintf("%d ", i))
    for (j in 1:10) {
      if (biome[i, j] && !chosenXY[i, j]) {
        cat("1 ")
      } else if (biome[i, j] && chosenXY[i, j]) {
        cat("X ")
      } else {
        cat("0 ")
      }
    }
    cat("\n")
  }
}

printCoordinates <- function(biome, chosenXY) {
  cat("Printing location coordinates of Caminalcule locations(x,y)\n")
  for (i in 1:10) {
    for (j in 1:10) {
      if (biome[i, j]) {
        cat(sprintf("(%s,%d)", letters[i], j))
      }
    }
  }
  
  cat("\nPrinting location coordinates of Pollard locations(x,y)\n")
  for (i in 1:10) {
    for (j in 1:10) {
      if (chosenXY[i, j]) {
        cat(sprintf("(%s,%d)", letters[i], j))
      }
    }
  }
  cat("\n")
}

beginSim <- function(camiPop, pollPop, camiPopMultiply, numGenerations, printArray, printLocations) {
  cat(sprintf("Gen: 0 Starting Caminalcule Pop: %d Starting Pollard Pop: %d\n", camiPop, pollPop))
  
  for (index in 1:numGenerations) {
    found <- 0
    biome <- matrix(FALSE, nrow = 10, ncol = 10)
    
    for (cami in 1:camiPop) {
      x <- sample(1:10, 1)
      y <- sample(1:10, 1)
      
      if (!biome[x, y]) {
        biome[x, y] <- TRUE
      } else {
        while (biome[x, y]) {
          x <- sample(1:10, 1)
          y <- sample(1:10, 1)
        }
        biome[x, y] <- TRUE
      }
    }
    
    chosenXY <- matrix(FALSE, nrow = 10, ncol = 10)
    
    for (polly in 1:pollPop) {
      x <- sample(1:10, 1)
      y <- sample(1:10, 1)
      
      while (chosenXY[x, y]) {
        x <- sample(1:10, 1)
        y <- sample(1:10, 1)
      }
      
      if (biome[x, y] && !chosenXY[x, y]) {
        camiPop <- camiPop - 1
        found <- found + 1
        chosenXY[x, y] <- TRUE
      } else {
        chosenXY[x, y] <- TRUE
      }
    }
    
    camiPop <- camiPop * camiPopMultiply
    if (camiPop < 1) camiPop <- 1
    if (camiPop > 100) camiPop <- 100
    
    lostPop <- floor(pollPop / 3)
    pollPop <- pollPop - lostPop
    pollPop <- pollPop + found + 1
    
    if (pollPop > 75) pollPop <- 75
    if (pollPop < 1) pollPop <- 1
    
    cat(sprintf("Gen: %d Ending Caminalcule Pop: %d Ending Pollard Pop: %d || found: %d\n", index, camiPop, pollPop, found))
    if (printArray) printBiome(biome, chosenXY)
    if (printLocations) printCoordinates(biome, chosenXY)
  }
}

camiPop <- -1
pollPop <- -1
camiPopMultiply <- -1
numGenerations <- -1
printArray <- FALSE
printLocations <- FALSE

while ((camiPop < 1) || (camiPop > 100)) {
  cat("Enter starting Caminalcule population (1-100): ")
  camiPop <- as.integer(readline())
}

while ((pollPop < 1) || (pollPop > 75)) {
  cat("Enter starting Pollard population (1-75): ")
  pollPop <- as.integer(readline())
}

while (camiPopMultiply < 0) {
  cat("Enter Caminalcule Reproduction rate (1-10): ")
  camiPopMultiply <- as.numeric(readline())
}

while (numGenerations < 1) {
  cat("Enter the number of generations to simulate (1-100): ")
  numGenerations <- as.integer(readline())
}

userInput <- "t"
while (TRUE) {
  cat("Would you like to display a population array after each generation (Y/N): ")
  userInput <- toupper(readline())
  if (userInput == "Y") {
    printArray <- TRUE
    break
  } else if (userInput == "N") break
}

userInput <- "t"
while (TRUE) {
  cat("Would you like to display the coordinates for the Caminalcule and Pollards each generation (Y/N): ")
  userInput <- toupper(readline())
  if (userInput == "Y") {
    printLocations <- TRUE
    break
  } else if (userInput == "N") break
}

set.seed(Sys.time())  # Set seed based on system time
beginSim(camiPop, pollPop, camiPopMultiply, numGenerations, printArray, printLocations)
