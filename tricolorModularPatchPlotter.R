infom <- rbind(c(1, 1, 1), c(2, 1, 1), c(2, 2, 1), c(2, 2, 2), c(3, 2, 1), c(3, 2, 2), c(3, 2, 3), c(3, 3, 2), c(3, 3, 3), c(3, 3, 4))

all_orange <- matrix(sample(3, 262144, replace = TRUE, prob = c(1, 0, 0)), nrow = 512) #we wanted the image to be 512x512 pixels
colors <- c("orange", "darkslategray1", "blue")
dir <- "/Users/bhrugubharathi/Desktop/Charisma R/testing/"

##############################################################################################
tricolor_patch_plotter <- function(num1, num2, freqcy){
  if(freqcy > 1){
    stop("Error! Entered frequency is over one. Please decrease the frequency.")
  }
  if(num1 > 10){
    stop("Error! Cannot support more than 10 squares at this time. Please decrease the value.")
  }
  if(num2 > num1){
    stop("Error! The number of off color squares cannot be greater than the total number of squares. Please decrease the value.")
  }
  
  png(file = paste0(dir, num1, "_", num2, "_clustered_distribution_", freqcy, ".png"),
      width=512, height=512, units = "px", pointsize = 0)
  m <- all_orange
  
  edge <- sqrt(((262144*freqcy)/num1))
  
  numrows <- infom[num1, 1] #total number of rows in grid
  numperone <- infom[num1, 2] #1st row number in alternating sequence
  numpertwo <- infom[num1, 3] #2nd row number in alternating sequence
  
  if(hasOverlaps(edge, numrows, numperone)){
    stop("Error! Entered frequency is causing patches to overlap. Please decrease the frequency.")
  }
  
  rowanchor <- (512/numrows)/2
  neededSquares <- num2
  availableSquares <- num1
  
  
  for (i in 1:numrows){
    numperrow <- numperone
    if(i %% 2 == 0){
      numperrow <- numpertwo
    }
    
    colanchor <- (512/numperrow)/2
    for(j in 1:numperrow){
      start_row <- rowanchor - edge/2
      start_col <- colanchor - edge/2
      fillingProb <- (neededSquares/availableSquares)
      if(floor(runif(1, min=0, max=101)) < (fillingProb*100)){
        colorToPlot <- 2
        neededSquares <- neededSquares - 1
      }else{
        colorToPlot <- 3
      }
      availableSquares <- availableSquares - 1
      
      for(k in start_col:(start_col + edge)) {
        for(j in start_row:(start_row + edge)) {
          m[k,j] <- colorToPlot
        }
      }
      colanchor <- colanchor + (512/numperrow)
    }
    rowanchor <- rowanchor + (512/numrows)
  }
  image(1:(nrow(m)), 1:ncol(m), m, col = colors, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  dev.off()
}

hasOverlaps <- function(edge, numrows, numperone){
  if(edge > 512/numrows || edge > 512/numperone){
    return (TRUE)
  }
  return (FALSE)
}

tricolor_patch_plotter(3,1,0.1)
tricolor_patch_plotter(3,1,0.4)
tricolor_patch_plotter(4,0,0.3)
tricolor_patch_plotter(4,2,0.3)
tricolor_patch_plotter(4,2,0.)
tricolor_patch_plotter(10,7,0.3)
tricolor_patch_plotter(10,7,0.3)


