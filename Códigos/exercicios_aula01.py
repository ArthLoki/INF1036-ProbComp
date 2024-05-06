# -*- coding: utf-8 -*-
"""
Created on Thu Mar  7 09:26:52 2024

@author: User
"""
import random, numpy

def exemplo1(nsamples):
    dado = [i+1 for i in range(6)]
    nFaces = len(dado)
    
    totalFaces = [0 for _ in range(6)]
    
    for i in range(nsamples):
        face = random.choice(dado)
        totalFaces[face-1] += 1
    
    print("-----> Exemplo 1: Lancamento de dado\n")
    for i in range(nFaces):
        print("Total da face [{}] = {}".format(i+1, totalFaces[i]))
        print("Probabilidade da face [{}] = {}\n".format(i+1, totalFaces[i]/nsamples))
    return

def exemplo2(nsamples):
    moeda = ['CA', 'CO']
    prob = [2/5, 3/5]
    
    contA = 0
    contB = 0
    
    for _ in range(nsamples):
        # random.choices retorna uma lista de tamanho k
        face1 = random.choices(moeda, weights=prob, k=1)[0]
        face2 = random.choices(moeda, weights=prob, k=1)[0]
        face3 = random.choices(moeda, weights=prob, k=1)[0]
        
        if ((face1 == 'CA' and face2 == 'CO') or (face1 == 'CO' and face2 == 'CA')): contA += 1
            
        if (face2 == 'CO' and face3 == 'CO'): contB += 1
            
    PA = contA / nsamples
    PB = contB / nsamples
    
    print(PA)
    print(PB)
    return

def exemplo3(nsamples):
    return


def main():
    # exemplo1(12)
    # exemplo2(10000)
    return

main()