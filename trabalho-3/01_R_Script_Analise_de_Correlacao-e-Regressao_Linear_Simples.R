
################# Regressao Linear Simples ####################


# Passo 1: Carregar os pacotes que serao usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diret0rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
getwd()

# Carregamento do arquivo csv
dados <- read.csv2('00_DadosCarros.csv', stringsAsFactors = T)
View(dados)         # Visualizacao dos dados em janela separada

# (from: dplyr)
glimpse(dados)      # Visualizacao de um resumo dos dados


# Passo 3: Analise de Correlacao:

# (from: ggplot2)

ggplot(dados, aes(x=Quilometro, y=Preco)) +
  geom_point() +
  labs(x='Quilômetro', y='Preço', 
       title='Relação entre Quilometragem e Preço')

# Teste de hipotese sobre correlacao nula
cor.test(dados$Quilometro, dados$Preco)


# Passo 4: Estimacao dos Parametros do
## Modelo de Regressao Linear Simples (MRLS)

mod <- lm(Preco ~ Quilometro, data=dados)

# Os coeficientes estimados
mod


# As inferencias
summary(mod)


# Passo 5: Verificacao dos pressupostos 
## para a regressao linear

## Relacao linear entre a VD e a VI:
### VD: Preco
### VI: Quilometro

plot(dados$Quilometro, dados$Preco)


## Analise grafica baseada no modelo estimado:

par(mfrow=c(2,2), mar=c(3,3,3,3))

plot(mod)

## Ver, por exemplo, Interpretacao em:
#https://data.library.virginia.edu/diagnostic-plots/

par(mfrow=c(1,1))


## Normalidade dos residuos:
shapiro.test(mod$residuals)


## Outliers nos residuos:
summary(rstandard(mod))


## Independencia dos residuos (Durbin-Watson):
# (from: car)
durbinWatsonTest(mod)


## Homocedasticidade (Breusch-Pagan):
bptest(mod)


# Passo 4: Analise do modelo
summary(mod)

# Passo 5: Apresentacao Grafica

ggplot(data = dados, mapping = aes(x = Quilometro, y = Preco)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 35, label.y = 29000) +
  labs(x='Qulômetros rodados',y='Preço (R$)',
       title='Ajuste de um Modelo de Regressão Linear Simples',
       subtitle = 'Preço x Quilômetros rodados')
  theme_classic()


# https://pt.stackoverflow.com/questions/6979/como-colocar-a-equa%C3%A7%C3%A3o-da-regress%C3%A3o-em-um-gr%C3%A1fico

  
# Passo 6: Predicao

# Do Ebook: prevendo o valor do Preco para um carro com
## 45000 Km rodados
df.teste <- data.frame(Quilometro = c(45))
df.teste

predict(mod, df.teste)


# Prevendo varios valores:
df.teste <- data.frame(Quilometro = c(29,45,38))
df.teste

predict(mod, df.teste)


