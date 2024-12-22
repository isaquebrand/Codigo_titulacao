# Carregar o pacotes
library(readxl)
library(ggplot2)
library(gridExtra)

#definir o diretorio
setwd("C:/R_proj")

#carregar o banco de dados
dados <- read_excel("tit_AB.xlsx") 

# Calculando manualmente a média acumulativa de cada dois volumes consecutivos
dados$volume_medio <- c(NA, (dados$vol_ml[-1] + dados$vol_ml[-nrow(dados)]) / 2)


# Calculando a primeira derivada com a diferença absoluta (sempre maior - menor)
dados$derivada_pH <- c(
  NA, # O primeiro valor é NA porque não há anterior
  (pmax(dados$pH[-1], dados$pH[-nrow(dados)]) - pmin(dados$pH[-1], dados$pH[-nrow(dados)])) /
    (pmax(dados$vol_ml[-1], dados$vol_ml[-nrow(dados)]) - pmin(dados$vol_ml[-1], dados$vol_ml[-nrow(dados)]))
)


# Criando o gráfico

g1 <- ggplot(data = dados, aes(x = volume_medio, y = derivada_pH)) +
  geom_line(color = "gray", size = 0.5) +  # Linha conectando os pontos
  geom_point(color = "blue", size = 1) + # Pontos em destaque
  labs(
    title = " \n",
    x = "\n Volume médio (mL)",
    y =  expression(Delta * pH / Delta * V)
  ) +
  theme_test()  # Estilo limpo e moderno

g2 <- ggplot(data = dados, aes(x = volume_medio, y = pH)) +
  geom_line(color = "gray", size = 0.5) +  # Linha conectando os pontos
  geom_point(color = "blue", size = 1) + # Pontos em destaque
  labs(
    title = "Titulação Ácido-Base \n",
    x = "\n Volume médio (mL)",
    y = "pH \n"
  ) +
  theme_test()  # Estilo limpo e moderno

grid.arrange(g2, g1, ncol = 2)

################################################################################

# Encontrar o índice do maior valor da primeira derivada (ignorando NA)
indice_maior_derivada <- which.max(dados$derivada_pH)

# Obter o maior valor da derivada
maior_valor_derivada <- dados$derivada_pH[indice_maior_derivada]

# Obter o volume e pH correspondente ao maior valor da derivada
volume_correspondente <- dados$volume_medio[indice_maior_derivada]

ph_correspondente <- dados$pH[dados$volume_medio == volume_correspondente]  # pH correspondente


# Exibir os resultados
cat("Maior valor da primeira derivada:", maior_valor_derivada, "\n")
cat("Volume correspondente:", volume_correspondente, "\n")
cat("pH correspondente ao volume final:", ph_correspondente, "\n")


################################################################################
#calculo do fator de correção
################################################################################

# Definindo os valores conhecidos
A <- 2.5  # Massa do carbonato de sódio em gramas (g)
B <- 15          # Volume do carbonato de sódio em mL
C <- volume_correspondente        # Volume de ácido gasto na titulação em mL (exemplo)
N_teo <- 0.02    # Normalidade teórica do ácido

# Calculando a normalidade experimental
N_exp <- (A * B) / (53 * C)

# Calculando o fator de correção
fator_correcao <- N_exp / N_teo

# Exibindo os resultados
cat("Normalidade Experimental (N_exp):", N_exp, "N\n")
cat("Fator de Correção (F):", fator_correcao, "\n")


################################################################################
# Criando o gráfico com o volume final destacado
################################################################################

g3 <- ggplot(data = dados, aes(x = volume_medio, y = pH)) +
  geom_line(color = "gray", size = 0.5) +  # Linha conectando os pontos
  geom_point(color = "blue", size = 1) + # Pontos em destaque
  geom_vline(xintercept = volume_correspondente, # Posição da linha vertical no eixo x
             color = "red",              # Cor da linha
             linetype = "dashed",        # Tipo de linha (pontilhada)
             size = 0.6) +
  geom_hline(yintercept = ph_correspondente, # Posição da linha vertical no eixo x
             color = "red",              # Cor da linha
             linetype = "dashed",        # Tipo de linha (pontilhada)
             size = 0.6) +
  annotate( #valor de pH
    "text",
    x = 3,  # Coordenada x onde o texto será posicionado
    y = ph_correspondente,  # Coordenada y (acima da linha horizontal)
    label = ph_correspondente,  # Texto a ser exibido
    color = "blue",  # Cor do texto
    size = 4,
    angle = 0) +        # Tamanho do texto)
  
  annotate( #valor do mL gasto
      "text",
      x = volume_correspondente,  # Coordenada x onde o texto será posicionado
      y = 7,  # Coordenada y (acima da linha horizontal)
      label = volume_correspondente,  # Texto a ser exibido
      color = "blue",  # Cor do texto
      size = 4,
      angle = 90) +        # Tamanho do texto)
  labs(
    title = "Titulação Ácido-Base \n",
    x = "\n Volume médio (mL)",
    y = "pH \n") +
    theme_test()  # Estilo limpo e moderno



texto1 <- sprintf("Fc: %.3f", fator_correcao)
texto2 <- sprintf("N_exp: %.4f", N_exp)
texto3 <- sprintf("1ª Derivada: %.2f", maior_valor_derivada)


g4 <- ggplot(data = dados, aes(x = volume_medio, y = derivada_pH)) +
  geom_line(color = "gray", size = 0.5) +  # Linha conectando os pontos
  geom_point(color = "blue", size = 1) + 
  
  # Pontos em destaque
  geom_vline(xintercept = volume_correspondente, # Posição da linha vertical no eixo x
       color = "red",              # Cor da linha
       linetype = "dashed",        # Tipo de linha (pontilhada)
       size = 0.6) +
  
  geom_hline(yintercept = maior_valor_derivada, # Posição da linha vertical no eixo x
             color = "red",              # Cor da linha
             linetype = "dashed",        # Tipo de linha (pontilhada)
             size = 0.6) +
  
  annotate( #valor do fator de correção
    "text",
    x = volume_correspondente,  # Coordenada x onde o texto será posicionado
    y = maior_valor_derivada,  # Coordenada y (acima da linha horizontal)
    label = texto3,  # Texto a ser exibido
    color = "blue",  # Cor do texto
    size = 4,
    angle = 0) +
  
  annotate( #valor do fator de correção
      "text",
      x = 10,  # Coordenada x onde o texto será posicionado
      y = 0.85,  # Coordenada y (acima da linha horizontal)
      label = texto1,  # Texto a ser exibido
      color = "blue",  # Cor do texto
      size = 4,
      angle = 0) +
  
  annotate( #valor do fator de correção
    "text",
    x = 10,  # Coordenada x onde o texto será posicionado
    y = 0.9,  # Coordenada y (acima da linha horizontal)
    label = texto2,  # Texto a ser exibido
    color = "blue",  # Cor do texto
    size = 4,
    angle = 0) +
  
  labs(
       title = " \n",
       x = "\n Volume médio (mL)",
       y = expression(Delta * pH / Delta * V)) +
  theme_test()  # Estilo limpo e moderno

grafico <- grid.arrange(g3, g4, ncol = 2)


################################################################################
# Salvando o gráfico em PNG
################################################################################

ggsave(
  filename = "grafico.png",   # Nome do arquivo
  plot = grafico,             # Objeto gráfico
  width = 12.5,               # Largura em polegadas (900 pixels / 72 dpi)
  height = 6.25,              # Altura em polegadas (450 pixels / 72 dpi)
  dpi = 600                    # Resolução (pontos por polegada)
)





