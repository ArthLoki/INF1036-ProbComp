LCG <- function(seed, a, c, M, nsamples){
  x = seed
  u = c(x)
  
  for (i in 1:nsamples){
    xk = (a * x + c) %% M
    u = c(u, as.double(xk) / as.double(M))
    x = xk
  }
  return (u)
}

MT <- function(nsamples){
  return (runif(nsamples))  # retorna nsamples valores no intervalo [0.0, 1.0)
}

main <- function(){
  x0 = 1
  a = 6
  c = 0
  M = 11
  
  nsamples = M
  
  U1 = LCG(x0, a, c, M, nsamples)
  print(length(U1))
  print(U1)
}

CARA_COROA <- function(U, p){
  cc = c()
  for (i in 1:length(U)){
    if (U[i] < (1.0 - p))
      cc = c(cc, 0)  # cara
    else
      cc = c(cc, 1)  # coroa
  }
  
  return (cc)
}

DADO <- function(U){
  dado = c()
  for (i in 1:length(U)){
    dado = c(dado, as.integer(U[i] * 6.0) + 1)
  }
}

a = 39373
c = 0
M = 2147483647

U = LCG(3, a, c, M, 10000)
hist(U, col = 'green')
CC = CARA_COROA(U, 0.5)
print(sum(CC))


main()