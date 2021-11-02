infom <- rbind(c(1, 1, 1), c(2, 1, 1), c(2, 2, 1), c(2, 2, 2), c(3, 2, 1), c(3, 2, 2), c(3, 2, 3), c(3, 3, 2), c(3, 3, 3), c(3, 3, 4))

all_orange <- matrix(sample(2, 262144, replace = TRUE, prob = c(0, 1)), nrow = 512) #we wanted the image to be 512x512 pixels
colors <- c("blue", "orange")
dir <- "/Users/bhrugubharathi/Desktop/Charisma R/testing/"

patch_plotter <- function(num, freqcy){
  if(freqcy > 1){
    stop("Error! Entered frequency is over one. Please decrease the frequency.")
  }
  
  png(file = paste0(dir, num, "_clustered_distribution_", freqcy, ".png"),
      width=512, height=512, units = "px", pointsize = 0)
  m <- all_orange
  
  edge <- sqrt(((262144*freqcy)/num))
  
  numrows <- infom[num, 1] #total number of rows in grid
  numperone <- infom[num, 2] #1st row number in alternating sequence
  numpertwo <- infom[num, 3] #2nd row number in alternating sequence
  
  if(hasOverlaps(edge, numrows, numperone)){
    stop("Error! Entered frequency is causing patches to overlap. Please decrease the frequency.")
  }

  rowanchor <- (512/numrows)/2
  
  for (i in 1:numrows){
    numperrow <- numperone
    if(i %% 2 == 0){
      numperrow <- numpertwo
    }
    
    colanchor <- (512/numperrow)/2
    for(j in 1:numperrow){
      start_row <- rowanchor - edge/2
      start_col <- colanchor - edge/2
      for(k in start_col:(start_col + edge)) {
        for(j in start_row:(start_row + edge)) {
          m[k, j] <- 1
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

patch_plotter(10, 0.5)


