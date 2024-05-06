## Produto cartesiano
A = c('a', 'b', 'c')
B = c('p', 'q')

prod.cart <- function(A, B){
  w = list()
  k = 1
  for (i in 1:length(A)){
    for (j in 1:length(B)){
      w[[k]] = c(A[i], B[j])
      k = k + 1
    }
  }
  return (w)
}