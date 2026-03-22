#### Script para simular competição usando equações de Lotka-Volterra ####
# Prof. Dr. Weverton Trindade
# E-mail: wevertonf1993@gmail.com
# GitHub: https://github.com/wevertonbio/


#' Olá 🖖

#' Esse script contém as funções e instruções necessárias para simular a competição
#' entre duas espécies usando as equações de Lotka-Volterra e isóclinas de
#' crescimento.
#'
#' Antes de começar, vamos carregar os pacotes e chamar as funções, que estão
#' no script "funcoes_competicao.R".

#' Vamos começar vendo em que diretório estamos:
getwd()

#' Dica: No RStudio, você pode ir em Session > Set Working Directory > To Source File Location
#' para garantir que o R encontre o arquivo das funções.'


#' Certifique-se de colocar o arquivo funcoes_competicao.R" diretamente nesse
#' diretório

# Carregar pacotes (e instalar, se necessário)
if (!require("deSolve")) install.packages("deSolve")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")

# Garantir que estão carregados
library(deSolve)
library(ggplot2)
library(gridExtra)

# Carregar funções
# Atenção: coloque o caminho completo para o arquivo!
source("funcoes_competicao.R")

#' Vamos rodar um exemplo.
#' Precisamos definir os seguintes parâmetros (não altere nada por enquanto!)
r1 = 0.2 # Taxa de crescimento da espécie 1
r2 = 0.2 # Taxa de crescimento da espécie 2
N1_ini = 40 # População inicial (nº de individuos) da espécie 1
N2_ini = 10  # População inicial (nº de individuos) da espécie 2
alpha = 1 # Coeficiente de competição da espécie 2 (efeito de spp2 sobre spp1)
beta=0.5 # Coeficiente de competição da espécie 1 (efeito de spp1 sobre spp2)
K1 = 80 # Capacidade de carga da espécie 1 (nº de individuos)
K2 = 120 # Capacidade de carga da espécie 2 (nº de individuos)

# Agora, vamos rodar a simulação com esses parâmetros
exemplo_1 <- simular_competicao(r1 = r1, r2 = r2,
                                N1_ini = N1_ini, N2_ini = N2_ini,
                                alpha = alpha, beta = beta,
                                K1 = K1, K2 = K2)
plot(exemplo_1)

#' Lembrando que a linha com seta preta indica a trajetória das populações:
#' A direita indica crescimento da espécie 1, enquanto a esquerda indica redução
#' Para cima indica crescimento da espécie 2, enquanto para baixo indica redução
#' As setinhas azuis e vermelhor indicam as áreas do gráfico onde as populações
#' das espécies 1 e 2 crescem ou sofrem retração.

#' Perceba que após rodar a função, é mostrado no console o resultado
#' (ex: exclusão competitiva) e quando a exclusão ocorrerá.

#' Podemos salvar a imagem com ggsave (irá salvar direto no seu diretório)
ggsave("exemplo_1.png", #Nome do arquivo com extensão png
       exemplo_1, # Resultado
       dpi = 600, width = 9, height = 4) #Configurações de resolução e tamanho

#' Vamos rodar um exemplo onde haverá equilíbrio estável.
#' Definir parâmetros
r1 = 0.2 # Taxa de crescimento da espécie 1
r2 = 0.2 # Taxa de crescimento da espécie 2
N1_ini = 40 # População inicial (nº de individuos) da espécie 1
N2_ini = 10  # População inicial (nº de individuos) da espécie 2
alpha = 0.7 # Coeficiente de competição da espécie 2 (efeito de spp2 sobre spp1)
beta = 0.5 # Coeficiente de competição da espécie 1 (efeito de spp1 sobre spp2)
K1 = 100 # Capacidade de carga da espécie 1 (nº de individuos)
K2 = 120 # Capacidade de carga da espécie 2 (nº de individuos)

# Agora, vamos rodar a simulação com esses parâmetros
exemplo_2 <- simular_competicao(r1 = r1, r2 = r2,
                                N1_ini = N1_ini, N2_ini = N2_ini,
                                alpha = alpha, beta = beta,
                                K1 = K1, K2 = K2)
plot(exemplo_2)
# Salvar
ggsave("exemplo_2.png", #Nome do arquivo com extensão png
       exemplo_2, # Resultado
       dpi = 600, width = 9, height = 4) #Configurações de resolução e tamanho


#' Agora, vamos fazer um teste com disturbio
#' O disturbio pode ocorrer em ciclos de tempo (ex: a cada 20 anos ou gerações)
#' Ou em ciclos de densidade (ex: cada vez que espécie atinge 80% de K)
#'
#' Vamos fazer uma simulação com disturbio ocorrendo em ciclos de tempo
#' Vamos fazer um disturbio aparecer a cada 20 anos/gerações, matando 70%
#' da competidora mais forte
#' Definir parâmetros
r1 = 0.2 # Taxa de crescimento da espécie 1
r2 = 0.2 # Taxa de crescimento da espécie 2
N1_ini = 40 # População inicial (nº de individuos) da espécie 1
N2_ini = 10  # População inicial (nº de individuos) da espécie 2
alpha = 0.7 # Coeficiente de competição da espécie 2 (efeito de spp2 sobre spp1)
beta = 0.5 # Coeficiente de competição da espécie 1 (efeito de spp1 sobre spp2)
K1 = 100 # Capacidade de carga da espécie 1 (nº de individuos)
K2 = 120 # Capacidade de carga da espécie 2 (nº de individuos)
tipo_disturbio = "tempo" # Por tempo ou por densidade?
frequencia_t = 20 # A cada quando tempo?
intensidade = 0.7 # Disturbio irá "matar" quantos % da competidora mais forte?

# Simular
exemplo_3 <- simular_competicao_disturbio(r1 = r1, r2 = r2,
                                          N1_ini = N1_ini, N2_ini = N2_ini,
                                          alpha = alpha, beta = beta,
                                          K1 = K1, K2 = K2,
                                          tipo_disturbio = tipo_disturbio,
                                          frequencia_t = frequencia_t,
                                          intensidade = intensidade)
# Salvar
ggsave("exemplo_3.png", #Nome do arquivo com extensão png
       exemplo_3, # Resultado
       dpi = 600, width = 9, height = 4, scale = 1.25) #Configurações de resolução e tamanho

#' Agora, uma simulação com disturbio ocorrendo em ciclos de densidade
#' Vamos fazer um disturbio aparecer a cada vez que a competidora mais forte
#' atingir 80% da capacidade de carga (K) do ambiente
#' Definir parâmetros
r1 = 0.2 # Taxa de crescimento da espécie 1
r2 = 0.2 # Taxa de crescimento da espécie 2
N1_ini = 40 # População inicial (nº de individuos) da espécie 1
N2_ini = 10  # População inicial (nº de individuos) da espécie 2
alpha = 0.7 # Coeficiente de competição da espécie 2 (efeito de spp2 sobre spp1)
beta = 0.5 # Coeficiente de competição da espécie 1 (efeito de spp1 sobre spp2)
K1 = 100 # Capacidade de carga da espécie 1 (nº de individuos)
K2 = 120 # Capacidade de carga da espécie 2 (nº de individuos)
tipo_disturbio = "densidade" # Por tempo ou por densidade?
gatilho_k = 0.8 # Disturbio irá aparecer quando espécie atingir ... de K
intensidade = 0.7 # Disturbio irá "matar" quantos % da competidora mais forte?

# Simular
exemplo_4 <- simular_competicao_disturbio(r1 = r1, r2 = r2,
                                          N1_ini = N1_ini, N2_ini = N2_ini,
                                          alpha = alpha, beta = beta,
                                          K1 = K1, K2 = K2,
                                          tipo_disturbio = tipo_disturbio,
                                          gatilho_k = gatilho_k,
                                          intensidade = intensidade)
# Salvar
ggsave("exemplo_4.png", #Nome do arquivo com extensão png
       exemplo_4, # Resultado
       dpi = 600, width = 9, height = 4, scale = 1.25) #Configurações de resolução e tamanho

#### TAREFA DE STORYTELLING CIENTÍFICO ####

#' Agora, você irá criar 2 cenários de competição.
#' Para cada cenário, você irá contar uma história que deve conter as seguintes
#' informações:
#'
#' Que espécies estão competindo? (características das espécies)
#' Pelo qual recurso elas estão competindo?
#' Competição por exploração ou interferência?
#' São duas espécies nativas ou uma delas é exótica/invasora?
#'

#' No primeiro cenário, você irá simular uma competição SEM DISTURBIO!
#' Mude os parâmetros abaixo e conte a história seguindo o resultado encontrado:
#' Exclusão competitiva (quem excluiu quem? Quando excluiu?), equilibrio estável
#' ou equilibrio instavel (quem excluiu quem? Quando excluiu?)
#' Mude os parametros abaixo!!
r1 = 0.2 # Taxa de crescimento da espécie 1
r2 = 0.2 # Taxa de crescimento da espécie 2
N1_ini = 40 # População inicial (nº de individuos) da espécie 1
N2_ini = 10  # População inicial (nº de individuos) da espécie 2
alpha = 1 # Coeficiente de competição da espécie 2 (efeito de spp2 sobre spp1)
beta=0.5 # Coeficiente de competição da espécie 1 (efeito de spp1 sobre spp2)
K1 = 80 # Capacidade de carga da espécie 1 (nº de individuos)
K2 = 120 # Capacidade de carga da espécie 2 (nº de individuos)

# Agora, vamos rodar a simulação com esses parâmetros
cenário_1 <- simular_competicao(r1 = r1, r2 = r2,
                                N1_ini = N1_ini, N2_ini = N2_ini,
                                alpha = alpha, beta = beta,
                                K1 = K1, K2 = K2)
plot(cenário_1)

# Não esqueça de salvar a figura e incluir na sua história:
# Salvar
ggsave("cenario_1.png", #Nome do arquivo com extensão png
       cenário_1, # Resultado
       dpi = 600, width = 9, height = 4) #Configurações de resolução e tamanho

#' No segundo cenário, você irá simular uma competição COM DISTURBIO!
#' Você pode escolher um disturbio por tempo ou por densidade
#' Mude os parâmetros abaixo e conte a história seguindo o resultado encontrado:

r1 = 0.2 # Taxa de crescimento da espécie 1
r2 = 0.2 # Taxa de crescimento da espécie 2
N1_ini = 40 # População inicial (nº de individuos) da espécie 1
N2_ini = 10  # População inicial (nº de individuos) da espécie 2
alpha = 0.7 # Coeficiente de competição da espécie 2 (efeito de spp2 sobre spp1)
beta = 0.5 # Coeficiente de competição da espécie 1 (efeito de spp1 sobre spp2)
K1 = 100 # Capacidade de carga da espécie 1 (nº de individuos)
K2 = 120 # Capacidade de carga da espécie 2 (nº de individuos)
tipo_disturbio = "densidade" # Por tempo ou por densidade?
# Se escolheu por densidade...
gatilho_k = 0.8 # Disturbio irá aparecer quando espécie atingir ... de K
# Se escolheu por tempo...
frequencia_t = 20 # A cada quando tempo?
intensidade = 0.7 # Disturbio irá "matar" quantos % da competidora mais forte?

# Agora, vamos rodar a simulação com esses parâmetros
cenário_2 <- simular_competicao_disturbio(r1 = r1, r2 = r2,
                                          N1_ini = N1_ini, N2_ini = N2_ini,
                                          alpha = alpha, beta = beta,
                                          K1 = K1, K2 = K2,
                                          tipo_disturbio = tipo_disturbio,
                                          gatilho_k = gatilho_k,
                                          frequencia_t = frequencia_t,
                                          intensidade = intensidade)
plot(cenário_2)

# Não esqueça de salvar a figura e incluir na sua história:
# Salvar
ggsave("cenario_2.png", #Nome do arquivo com extensão png
       cenário_2, # Resultado
       dpi = 600, width = 9, height = 4, scale = 1.25) #Configurações de resolução e tamanho
