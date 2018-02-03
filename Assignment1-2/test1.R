re_wrap <- function(oneimg){
  return(as.matrix(resize(as.cimg(oneimg), size_x = 20, size_y = 20)))
}

test_list <- function(item){
  print(item)
}

to_vect <- function(x){
  as.vector(x[[1]])
}

fin_list <- list()
to_mat2 <- function(x){
 l <- c(x)
}