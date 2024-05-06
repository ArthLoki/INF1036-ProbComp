# Teste 1 (G1) 2023.1


# Questão 2
fatorial_rec <- function(n){
  if (n == 1)
    return (1)
  else
    return (n * fatorial_rec(n-1))
}

arranjo.simples <- function(n, p){
  return (fatorial_rec(n)/fatorial_rec(n-p))
}

combinacao.simples <- function(n, p){
  return (fatorial_rec(n)/(fatorial_rec(p) * fatorial_rec(n-p)))
}

perm.rep <- function(n){
  return (fatorial_rec(n-1))
}

calculaEvento <- function(n){
  # n lutadores + n treinadores = 2*n, n >= 4
  retorno = NULL
  
  # i
  count.i = 2*fatorial_rec(n)*fatorial_rec(n)
  print(paste("i)", count.i))
  
  # ii
  count.ii = fatorial_rec(n) * fatorial_rec(n)
  print(paste("ii)", count.ii))
  
  # iii)
  count.iii = n*fatorial_rec(2*n - 1)
  print(paste("iii)", count.iii))
  
  # iv)
  count.iv = n * (n-1) * fatorial_rec(2*n-2)
  print(paste("iv)", count.iv))
  
  retorno = c(count.i, count.ii, count.iii, count.iv)
  return (retorno)
}

simulacaoEvento <- function(n, nsamples){
  retorno = NULL
  
  pessoas = NULL
  for (i in 1:n){
    pessoas = c(pessoas, c("L", "T"))
  }
  
  counter = rep(0, 4)
  
  atende = F
  
  for (i in 1:nsamples){
    fila = sample(pessoas, 2*n, replace=F)
    
    # i
    atende = T
    for (j in 1:n){
      if (fila[j] == "L" & fila[j] == fila[j+1]){
        atende = F
        break
      }
    }
    if (atende){
      counter[1] = counter[1] + 1
    }
    
    # ii
    atende = T
    for (j in 1:n){
      if (fila[j] == "T"){
        atende = F
        break
      }
    }
    if (atende)
      counter[2] = counter[2] + 1
    
    # iii
    if (fila[n] == "T")
      counter[3] = counter[3] + 1
    
    # iv
    if (fila[1] == "T" && fila[1] == fila[2*n])
      counter[4] = counter[4] + 1
  }
  
  retorno = c(counter[1]/nsamples, counter[2]/nsamples, counter[3]/nsamples, counter[4]/nsamples)
  return (retorno)
}

q2 = simulacaoEvento(4, 10000)
for (i in 1:length(q2)){
  print(paste("q2[", i, "] =", q2[i]))
}