# Teste 2 G1

# Questão 1
Q1 <- function(){}

# Questão 2
Q2A <- function(sabem.r, sabem.python, sabem.r.python){
  # P(AeB) = P(A)*P(B)
  # Se P(AeB) == P(A)*P(B), são independentes. Caso contrario, não sao independentes
  if (sabem.r.python == sabem.r * sabem.python){
    print("2.a) Saber R e saber Python são independentes")
  } else {
    print("2.a) Saber R e saber Python não são independentes")
  }
}

Q2B <- function(sabem.r, sabem.python, sabem.r.python){
  # P(sabem.r|sabem.python) = P(sabem.r.python) / p(sabem.python)
  print(paste("2.b) P(sabem.r|sabem.python) =", sabem.r.python / sabem.python))
}

Q2C <- function(sabem.r, sabem.python, sabem.r.python){
  nsamples = 10000
  
  cont.sabe.r = 0
  cont.sabe.python = 0
  cont.sabe.r.python = 0
  
  for (i in 1:nsamples){
    r = sample(c(1, 0), 1, prob=c(sabem.r, 1-sabem.r))
    
    if (r == 1){
      cont.sabe.r = cont.sabe.r + 1
      cont.sabe.r.python = cont.sabe.r.python + sample(c(1, 0), 1, prob=c(sabem.r.python, 1-sabem.r.python))

    } else {
      cont.sabe.python = cont.sabe.python + sample(c(1, 0), 1, prob=c(sabem.python, 1-sabem.python))
    }
  }
  
  print(paste("2.c) P(sabem.python|sabem.r) =", cont.sabe.r.python / cont.sabe.r))
}

Q2 <- function(){
  sabem.r = 0.22
  sabem.python = 0.09
  sabem.r.python = 0.05
  
  Q2A(sabem.r, sabem.python, sabem.r.python)
  Q2B(sabem.r, sabem.python, sabem.r.python)
  Q2C(sabem.r, sabem.python, sabem.r.python)
}

# Questão 3
Q3.A.B <- function(nsamples){
  
  prob.saber.q9 = c(0.4, 0.6)
  prob.acertar.q9 = c(1/5, 4/5)
  
  cont.sabe.q9 = 0
  cont.nao.sabe.q9 = 0
  
  cont.sabe.acerta.q9 = 0
  cont.sabe.erra.q9 = 0
  
  cont.nao.sabe.acerta.q9 = 0
  cont.nao.sabe.erra.q9 = 0
  
  for (i in 1:nsamples){
    sabe.q9 = sample(c(1, 0), 1, prob=prob.saber.q9)
    
    if (sabe.q9 == 1){
      cont.sabe.q9 = cont.sabe.q9 + 1
      
      acerta.q9 = sample(c(1, 0), 1, prob=prob.acertar.q9)
      
      if (acerta.q9 == 1){
        cont.sabe.acerta.q9 = cont.sabe.acerta.q9 + 1
      } else {
        cont.sabe.erra.q9 = cont.sabe.erra.q9 + 1
      }

    } else {
      cont.nao.sabe.q9 = cont.sabe.q9 + 1
      
      acerta.q9 = sample(c(1, 0), 1, prob=prob.acertar.q9)
      
      if (acerta.q9 == 1){
        cont.nao.sabe.acerta.q9 = cont.nao.sabe.acerta.q9 + 1
      } else {
        cont.nao.sabe.erra.q9 = cont.nao.sabe.erra.q9 + 1
      }
    }
    
  }
  
  cont.acerta.q9 = cont.nao.sabe.acerta.q9 + cont.sabe.acerta.q9
  
  print(paste("3.a) Probabilidade de acertar Q9 =", cont.acerta.q9 / nsamples))
  print(paste("3.b) Probabilidade de acertar Q9 chutando =", cont.nao.sabe.acerta.q9 / cont.acerta.q9))
}

Q3C <- function(nsamples){
  n.questoes = 10
  n.alternativas = 5
  nota.minima = 3
  
  prob.acertar.questao = c(1/n.alternativas, 1-(1/n.alternativas))
  
  cont.notas.sup.min = 0
  
  for (i in 1:nsamples){
    nota = NULL
    for (j in 1:n.questoes){
      nota[j] = sample(c(1, 0), 1, prob=prob.acertar.questao)
    }
    
    if (sum(nota) > nota.minima){
      cont.notas.sup.min = cont.notas.sup.min + 1
    }
  }
  
  print(paste("3.c) Probabilidade de obter nota superior a", nota.minima, "=", cont.notas.sup.min / nsamples))
}

Q3 <- function(){
  nsamples = 10000
  
  Q3.A.B(nsamples)
  Q3C(nsamples)
}

# Main
main <- function(){
  Q1()
  Q2()
  Q3()
}

main()