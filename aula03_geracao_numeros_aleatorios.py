# -*- coding: utf-8 -*-
"""
Created on Tue Mar 12 09:14:40 2024

@author: User
"""

import numpy as np
import matplotlib.pyplot as plt


# Geracao de números pseudoaleatorios
def LCG(seed, a, c, M, nsamples):
    x = seed
    u = []
    for i in range(nsamples):
        xk = (a * x + c) % M
        u.append(float(xk) / float(M))  # valores entre 0 e 1
        x = xk
    return u

def MT(nsamples):
    return np.random.sample(nsamples)  # retorna nsamples valores no intervalo [0.0, 1.0)


# Aplicacoes
def get_cara_coroa(U, p):
    n = len(U)
    CC = []
    for i in range(n):
        if (U[i] < (1.0 - p)):
            CC.append(0)  # cara
        else:
            CC.append(1)  # coroa
    return CC

def DADO_V1(U):
    n = len(U)
    dado = []
    for i in range(n):
        if U[i] < 1.0 / 6.0:
            dado.append(1)
        elif U[i] < 2.0 / 6.0:
            dado.append(2)
        elif U[i] < 3.0 / 6.0:
            dado.append(3)
        elif U[i] < 4.0 / 6.0:
            dado.append(4)
        elif U[i] < 5.0 / 6.0:
            dado.append(5)
        else:
            dado.append(6)
    return dado

def DADO_V2(U):
    n = len(U)
    dado = []
    for i in range(n):
        dado.append(int(U[i] * 6.0) + 1)
    return dado

def get_dado(U, faces):
    return [int(x * float(faces)) + 1 for x in U]


# Main
def main():
    x0 = 3
    a = 39373
    c = 0
    M = 2147483647
    
    nsamples = 6000000
    
    U1 = LCG(x0, a, c, M, nsamples)
    print(len(U1))
    # print(U1)
    
    # CC = get_cara_coroa(U1, 0.5)
    # print(sum(CC))
    
    dado = DADO_V1(U1)
    print(len(dado))
    
    plt.hist(dado, 6, facecolor = 'green')
    plt.show()
    
    
    return

main()