### Lista 1

fatorial_rec <- function(n){
  if (n == 1){
    return (n)
  } else {
    return (n * fatorial_rec(n-1))
  }
}

perm.circ <- function(n){
  return (fatorial_rec(n-1))
}

comb.simples <- function(n, p){
  return (fatorial_rec(n) / (fatorial_rec(p) * fatorial_rec(n-p)))
}

## Parte 1

# Questão 25
Q25.P1 <- function(){
  num.total = 6
  n.grupo = 3
  
  # Considerando as 3 amigas como apenas uma pessoa, temos:
  qtd.pessoas.roda = num.total - (n.grupo - 1)  # A princípio, 4
  
  n.rodas.distintas = perm.circ(qtd.pessoas.roda) * fatorial_rec(n.grupo)
  
  print(paste("25.P1) O numero de rodas distintas é de", n.rodas.distintas))
}

# Questão 26
Q26.P1 <- function(){
  n.criancas = 7
  criancas.sep = 2
  
  # Retirando as duas crianças que não podem ficar juntas, temos 
  # uma roda com 5 crianças
  n.criancas.roda = n.criancas - criancas.sep
  
  # Agora, podemos adicionar as duas crianças nos espaços entre as outras crianças
  # No caso, temos 5 espaços vazios para a escolha da primeira criança
  # e 4 espaços vazios para a escolha da segunda criança
  n.espacos = 5
  n.possibilidades.criancas.sep = n.espacos * (n.espacos - 1)
  
  # Pelo princípio da multiplicacao, temos:
  n.modos = perm.circ(n.criancas.roda) * n.possibilidades.criancas.sep
  
  print(paste("26.P1) O numero de modos em que as criancas podem estar dispostas é de", n.modos))
}

# Questão 27
Q27.P1 <- function(){
  n.pessoas = 5
  
  # Letra A
  letra.a = comb.simples(n.pessoas, 3)
  
  # Letra B
  letra.b = comb.simples(n.pessoas, 2)  # ou faz combinaçaõ complementar
  
  print(paste("27.P1.a)", letra.a))
  print(paste("27.P1.b)", letra.b))
}


## Parte 2

# Questão 11
Q11.P2 <- function(){
  nsamples = 10000
  n.dias = 3
  
  n.cadeiras = 2*nsamples
  
  for (i in 1:nsamples){
    for (d in 1:n.dias){
      
      # Letra A
      
    }
  }
}
