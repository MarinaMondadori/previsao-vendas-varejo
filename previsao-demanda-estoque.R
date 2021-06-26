#Previsão de Vendas de Varejo
#O objetivo é prever a demanda para as próximas duas a três semanas.
#Esse insight valioso pode ajudar muitos profissionais da cadeia de suprimentos 
#a gerenciar corretamente seus níveis de estoque.
#Problema de regressão.

#Diretório do Projeto
setwd("C:/FCD/01-BigData-R-Azure/Projetofinal/Projeto2")
getwd()

#Instalando os pacotes necessários

install.packages("plotrix")

#Carregando os pacotes necessários
library(data.table)
library(ggplot2)
library(dplyr)
library(plotrix)
library(randomForest)


#Carregando os dados
#Dataset disponível em: https://www.kaggle.com/tevecsystems/retail-sales-forecasting?select=mock_kaggle.csv
dados <- read.csv("mock_kaggle.csv")
View(dados)
str(dados)

#Transformar a data para o formato "POSIXct"

dados$data<- as.Date(dados$data)

str(dados)

#Extraindo da data o dia da semana para entender
#em que período mais ocorrem vendas.

dados$dayweek <- as.factor(weekdays(dados$data))

#Transformando os dias da semana em números

dados$dayweek <- as.numeric(ordered(dados$dayweek, 
                                    levels = c("segunda-feira", 
                                               "terça-feira", 
                                               "quarta-feira", 
                                               "quinta-feira", 
                                               "sexta-feira", 
                                               "sábado", 
                                               "domingo")))
#Verificando em qual dia da semana ocorre mais vendas

dados %>% 
  group_by(dayweek) %>%
  summarise(min_venda = min(venda),
            max_venda = max(venda))

      
#Adicionando as informações acima em um plot
#É possível visualizar que o dia em que mais ocorrem vendas é no sábado
#e o que menos ocorre é na quinta.

plot(venda ~ dayweek, data = dados, log = "x")

#Com a criação de um hisrograma, pode-se observar que o volume de vendas de até 100 unidades
#ocorre com mais frequencia.
hist(dados$venda)

#Através da função summary, posso concluir o seguinte:
#Preço: temos valores zero que precisa ser removido do dataset
summary(dados$preco)

#Venda: Valor mínimo é zero e máximo 542. Média de 90 unidades, que pode ser confirmada
#pelo histograma.
summary(dados$venda)

#Estoque: variando de zero unidades à 7228 unidades, mas com maior frequencia
#de até 2000 unidades.
summary(dados$estoque)
hist(dados$estoque)

#Explorando o relacionamento entre as variáveis: Matriz de Correlação
#Correlação fraca entre as variáveis

cor(dados[c("venda", "estoque", "preco")])

#Removendo a coluna dayweek que não entrará no modelo

dados$dayweek<- NULL
dados$data<- NULL

#Usarei todas as variáveis para construção do modelo.
#Dividindo o dataset em dados de treino e teste:

dados_treino <- dados[1:656, ]
dados_teste <- dados[657:937, ]

#Criando o modelo de regressão

modelo2 <- randomForest(venda ~ estoque + preco, 
                        data = dados,
                        ntree = 40, 
                        nodesize = 5)
?randomForest
summary(modelo2)

previsao1<- predict(modelo2, dados_teste)
round(previsao1, digits=0)

# Visualizando os valores previstos e observados
resultados <- cbind(round(previsao1, digits=0), dados_teste$venda) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
resultados

#Avaliando a performance do modelo
# Calculando R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum((mean(dados$venda) - resultados$Real)^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2

#Resultado final do moelo ficou em 76%.
#Ainda pode melhorar, mas vou ficando por aqui.






