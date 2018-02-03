remove_row <- function(oneimg){
  
  oneimg[oneimg >= 128 ] <- 255
  oneimg[oneimg < 128 ] <- 0
    
  image_matrix <- matrix(oneimg, nrow = 28, ncol = 28)
  pos_row <- apply(image_matrix,1,sum)
  pos_row <- pos_row > 0
  
  pos_col <- apply(image_matrix,2,sum)
  pos_col <- pos_col > 0
  
  v <- c()
  for(i in 1:length(pos_row)){
    if(!pos_row[i]){
      v <- c(v,-i)
    }
  }
  image_matrix <- image_matrix[v,]
  
  v <- c()
  for(i in 1:length(pos_col)){
    if(!pos_col[i]){
      v <- c(v,-i)
    }
  }
  image_matrix <- image_matrix[,v]
}

resizeImage = function(im) {
  # function to resize an image 
  # im = input image, w.out = target width, h.out = target height
  # Bonus: this works with non-square image scaling.
  
  # initial width/height
  w.out <- 30
  h.out <- 30
  
  w.in <- nrow(im)
  h.in <- ncol(im)
  
  
  # Create empty matrix
  im.out = matrix(rep(0,w.out*h.out), nrow =w.out, ncol=h.out )
  
  # Compute ratios -- final number of indices is n.out, spaced over range of 1:n.in
  w_ratio <- w.in/w.out
  h_ratio <- h.in/h.out
  
  # Do resizing -- select appropriate indices
  im.out <- im[ floor(w_ratio* 1:w.out), floor(h_ratio* 1:h.out)]
  
  return(im.out)
}