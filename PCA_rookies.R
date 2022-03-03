# instalando os pacotes necessários para tal


pacotes <- c("plotly","tidyverse","knitr","kableExtra","PerformanceAnalytics",
             "factoextra","reshape2","psych","ggrepel", "readxl", "ggplot2", "ggrepel", "tibble")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#carregando o data frame com os rookies criado na outra etapa. dados da temporada de 2021-22 até o dia
#28/02/2022
nba_rookies_2022 <- read.csv(file = 'rookies_stats.csv')

#excluir a variável de ano, já que não será necessária na analise nesse momento
excluir <- c('year')
nba_rookies_2022 <- nba_rookies_2022[,!(names(nba_rookies_2022)%in% excluir)] 

#uma visualização da produção ofensiva e defensiva dos rookies
graf_rookies <- ggplot(nba_rookies_2022, aes(x=off_rating, y=def_rating)) +
  geom_point(color = "blue", size = 3) +
  labs(subtitle = "Rookies",
       y = "Defensive Rating", x = "Ofensive Rating",
       caption = "Fonte: NBA") +
  geom_label_repel(aes(label = player_name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()
plot(graf_rookies)

#outras variaveis que iremos excluir nesse momento são o nome do time, os turnovers, ts_pct
#minutos, jogos jogados e plus_minus. o plus_minus vamos subsituir pelo 
#net rating calculado na sequencia, os jogos e os minutos vão no total de minutos, 
#e, por fim, rookies cometem muitos turnovers. não devemos achar que isso impacta no ranking final
excluir <- c('team_abbreviation','tov', 'ts_pct', 'gp', 'min', 'plus_minus')
nba_rookies_2022_std <- nba_rookies_2022[,!(names(nba_rookies_2022)%in% excluir)] 
nba_rookies_2022['net_rating'] <- nba_rookies_2022['off_rating'] - nba_rookies_2022['def_rating']

#após criar o net_rating, vamos excluir, então, o ofensive rating e o defensive rating
excluir <- c('off_rating','def_rating')
nba_rookies_2022 <- nba_rookies_2022[,!(names(nba_rookies_2022)%in% excluir)] 


#ANALISE entre variaveis

#Agora,iremos analisar, uma a uma, cada variavel e sua analise.
nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = w_pct, y = total_mins), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = w_pct, y = total_mins),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "w_pct",
       y = "total_mins") + 
  theme_bw() 

#aparenta existir uma correlação levemente negativa entre minutos jogados e percentual de vitórias


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = w_pct, y = net_rating), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = w_pct, y = net_rating),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "w_pct",
       y = "net_rating") + 
  theme_bw() 

#aparenta existir uma correlação positiva entre vitórias e net_rating. acredito que
#ambas sejam as variáveis que levam em conta o desempenho coletivo

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = w_pct, y = reb), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = w_pct, y = reb),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "w_pct",
       y = "reb") + 
  theme_bw() 


#aparenta existir uma correlação levemente negativa entre rebotes e percentual de vitórias

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = w_pct, y = ast), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = w_pct, y = ast),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "w_pct",
       y = "ast") + 
  theme_bw() 

#a mesma coisa para assistências

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = w_pct, y = stl), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = w_pct, y = stl),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "w_pct",
       y = "stl") + 
  theme_bw() 

#a mesma coisa para roubos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = w_pct, y = blk), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = w_pct, y = blk),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "w_pct",
       y = "blk") + 
  theme_bw() 

#existe uma leve correlação positiva em vitórias

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = w_pct, y = pts), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = w_pct, y = pts),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "w_pct",
       y = "pts") + 
  theme_bw() 

#a mesma coisa para pontos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = w_pct, y = fg3m), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = w_pct, y = fg3m),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "w_pct",
       y = "fg3m") + 
  theme_bw() 

#a mesma coisa para 3 pontos

#deu para entender que existe uma grande separação entre variáveis coletivas e individuais
#quanto melhor os rookies estão nas variáveis individuais, piroes eles vão nas coletivas
#o que faz todo o sentido, já que times com muito espaço de rookies tendem a ser piores

#agora vamos as stats individuais
nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = reb, y = ast), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = reb, y = ast),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "reb",
       y = "ast") + 
  theme_bw() 

#existe uma correlação positiva entre rebotes e assitencias
nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = reb, y = stl), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = reb, y = stl),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "reb",
       y = "stl") + 
  theme_bw() 

#existe uma correlação positiva entre rebotes e roubos, menor que a anterior
nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = reb, y = blk), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = reb, y = blk),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "reb",
       y = "blk") + 
  theme_bw() 

#existe uma correlação positiva entre rebotes e tocos, maior  que a anterior

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = reb, y = pts), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = reb, y = pts),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "reb",
       y = "pts") + 
  theme_bw() 

#existe uma correlação positiva entre rebotes e pontos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = reb, y = fg3m), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = reb, y = fg3m),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "reb",
       y = "fg3m") + 
  theme_bw() 

#não existe correlaçao entre rebotes e cestas de 3 pontos (bigmen pegam mais rebotes?)

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = reb, y = total_mins), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = reb, y = total_mins),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "reb",
       y = "total_mins") + 
  theme_bw() 

#existe positiva correlaçao entre rebotes e minutos (mais tempo em quadra, mais stats)


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = reb, y = net_rating), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = reb, y = net_rating),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "reb",
       y = "net_rating") + 
  theme_bw() 

#não existe correlaçao entre rebotes e net_rtg

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = ast, y = stl), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = ast, y = stl),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "ast",
       y = "stl") + 
  theme_bw() 

#existe uma correlação positiva entre assistencias e roubos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = ast, y = blk), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = ast, y = blk),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "ast",
       y = "blk") + 
  theme_bw() 

#existe uma correlação positiva baixa entre assistencias e tocos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = ast, y = pts), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = ast, y = pts),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "ast",
       y = "pts") + 
  theme_bw() 

#existe uma correlação super positiva entre assistências e pontos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = ast, y = fg3m), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = ast, y = fg3m),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "ast",
       y = "fg3m") + 
  theme_bw() 

#existe uma correlação  positiva entre assistencia e cestas de 3 pontos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = ast, y = total_mins), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = ast, y = total_mins),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "ast",
       y = "total_mins") + 
  theme_bw() 

#existe positiva correlaçao entre assistencias e minutos (mais tempo em quadra, mais stats)


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = ast, y = net_rating), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = ast, y = net_rating),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "ast",
       y = "net_rating") + 
  theme_bw() 

#existe uma leve correlação negativa entre assisências e net_rtg


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = stl, y = blk), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = stl, y = blk),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "stl",
       y = "blk") + 
  theme_bw() 

#existe uma correlação positiva baixa entre roubos e tocos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = stl, y = pts), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = stl, y = pts),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "stl",
       y = "pts") + 
  theme_bw() 

#existe uma correlação super positiva entre roubos e pontos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = stl, y = fg3m), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = stl, y = fg3m),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "stl",
       y = "fg3m") + 
  theme_bw() 

#existe uma correlação  positiva entre roubos e cestas de 3 pontos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = stl, y = total_mins), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = stl, y = total_mins),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "stl",
       y = "total_mins") + 
  theme_bw() 

#existe positiva correlaçao entre roubos e minutos (mais tempo em quadra, mais stats)


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = stl, y = net_rating), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = stl, y = net_rating),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "stl",
       y = "net_rating") + 
  theme_bw() 

#existe uma leve correlação negativa entre roubos e net_rtg


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = blk, y = pts), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = blk, y = pts),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "blk",
       y = "pts") + 
  theme_bw() 

#existe uma correlação  positiva entre tocos e pontos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = blk, y = fg3m), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = blk, y = fg3m),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "blk",
       y = "fg3m") + 
  theme_bw() 

#existe uma correlação  negativa entre tocos e cestas de 3 pontos (estatísticas de bigman?)

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = blk, y = total_mins), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = blk, y = total_mins),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "blk",
       y = "total_mins") + 
  theme_bw() 

#existe positiva correlaçao entre tocos e minutos (mais tempo em quadra, mais stats)


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = blk, y = net_rating), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = blk, y = net_rating),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "blk",
       y = "net_rating") + 
  theme_bw() 

#existe uma leve correlação postiva entre roubos e net_rtg. única estatisica individual que impacta
#na melhora do jogador coletivamente?


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = pts, y = fg3m), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = pts, y = fg3m),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "pts",
       y = "fg3m") + 
  theme_bw() 

#existe uma correlação  positiva entre pontos e cestas de 3 pontos

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = pts, y = total_mins), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = pts, y = total_mins),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "pts",
       y = "total_mins") + 
  theme_bw() 

#existe positiva correlaçao entre pontos e minutos (mais tempo em quadra, mais stats)


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = pts, y = net_rating), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = pts, y = net_rating),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "pts",
       y = "net_rating") + 
  theme_bw() 

#existe uma leve correlação negativa entre pontos e net_rating

nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = fg3m, y = total_mins), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = fg3m, y = total_mins),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "fg3m",
       y = "total_mins") + 
  theme_bw() 

#existe positiva correlaçao entre 3 pontos e minutos (mais tempo em quadra, mais stats)


nba_rookies_2022 %>% 
  ggplot() +
  geom_point(aes(x = fg3m, y = net_rating), 
             color = "dodgerblue4",
             size = 2)  +
  geom_smooth(aes(x = fg3m, y = net_rating),
              color = "darkgoldenrod3", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.2) +
  labs(x = "fg3m",
       y = "net_rating") + 
  theme_bw() 

#existe uma leve correlação negativa entre 3 pontos e net_rating

#analisando, podemos dividir em 2 grandes grupos de variaveis:
#coletivas: net_rating e w_pct e que são impactadas negativamente por mais tempo e,
#consequentemente mais stats de rookies
#individuais: que com mais tempo, melhoram. apenas toco acaba influenciando o coletivo
#nas individuais, podemos dividir em 2 partes, pelo menos. as de frontcourt e backcourt
#frontcourt: tocos, rebotes, pontos e um pouco de roubos
#backcourt: roubos, assistencias, pontos, 3pts

#salvando a matriz de correlações
rho <- cor(nba_rookies_2022[,2:10])

chart.Correlation(nba_rookies_2022[,2:10])
#analisando as correlações:
#w_pct: alta positivamente com net_rating
#rebotes: alta positiva com assistencias, stl, pontos, minutos
#assistencias: alta positiva com rebotes, stl, pontos, 3pts e minutos
#stl: alta positiva com assistências, rebotes, pontos, fg3m e minutos
#blk: alta positiva com rebotes, tocos e minutos
#pts: alta positiva com stl, ast, rebs, 3pts e minutos
#3pts: alta positiva com pts, stl e minutos
#minutos: alta positiva com 3pts, pts, tocos, roubos, assistencias, rebotes
#net_rtg: alta com w_pct


# Construindo um mapa de calor a partir das correlações
rho %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

#realizando o teste de esfericidade batlett para conferir se o pca é aplicável
cortest.bartlett(R = rho)

#o teste de esfericidade de bartlett comprova que a matriz de correlações não é estatisticamente uma matrix
#de identidade


#vamos rodar a PCA

nba_rookies_2022_std <- nba_rookies_2022 %>% 
  column_to_rownames("player_name") %>% 
  scale() %>% 
  data.frame()
afpc <- prcomp(nba_rookies_2022_std)
summary(afpc)

#pelo critério da raiz latente, aparentemente iremos utilizar 3 fatores

data.frame(afpc$rotation) %>%
  mutate(var = names(nba_rookies_2022[2:10])) %>% 
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

#vamos analisar o scree plot
ggplotly(
  fviz_eig(X = afpc,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod3")
)

#segundo o scree plot, aparentemente, são 3 ou 4 fatores

k <- sum((afpc$sdev ^ 2) > 1) #número de variáveis presentes na base de dados
k

#segundo o critério da raiz latente, o ideal serão 3 fatores mesmo

#visualizando as cargas fatoriais
cargas_fatoriais <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k])
cargas_fatoriais
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#visualizando as comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#todos as variâncias das variaveis são capturadas em, pelo menos, 70% do todo

# Relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotagem das Cargas Fatoriais COM 2 FATORES
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpc)$importance[2,1] * 100,
                                    digits = 2),
                              "%)")),
       y = paste("F2", paste0("(",
                              round(summary(afpc)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()

#Plotagem das cargas fatoriais incluindo o F3
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X3)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpc)$importance[2,1] * 100,
                                    digits = 2),
                              "%)")),
       y = paste("F3", paste0("(",
                              round(summary(afpc)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()

#aparentemente, por essa analise, e, levando em conta a importância de stats individuais para avaliar
# a primeira temporada de um rookie, iriamos inverter o eixo do F1 e manter o eixo do F2 e F3

# Scores Fatoriais
scores_fatoriais <- t(afpc$rotation)/afpc$sdev 
colnames(scores_fatoriais) <- colnames(nba_rookies_2022_std)
scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2,
         PC3 = 3) %>%
  select(PC1, PC2, PC3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
# A seguir, vamos criar um objeto que servirá como um receptáculo para os
# k fatores (36, no caso estudado) a serem calculados:
fatores <- list()
# Agora, utilizaremos a função for
for(i in 1:nrow(scores_fatoriais)){
  fatores[[i]] <- rowSums(x = sweep(x = nba_rookies_2022_std, 
                                    MARGIN = 2, 
                                    STATS = scores_fatoriais[i,], 
                                    FUN = `*`))
}
# Feito isso, para fins didáticos, vamos transformar o objeto fatores em um
# data frame para melhor visualização e assimiliação de seu conteúdo:
fatores_df <- data.frame((sapply(X = fatores, FUN = c)))
fatores_df

#como iremos utilizar apenas 3 fatores, iremos excluir os outros
manter <- c('X1', 'X2', 'X3')
fatores_df <- fatores_df[manter]

fatores_df %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
# Combinando a base original 'nba_rookies_2022' com o objeto 'fatores_df':
nba_rookies_2022_final <-  cbind(nba_rookies_2022,
                              fatores_df) %>% 
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) 
# Calculando as correlações entre as variáveis e originais e os fatores
correlacoes_entre_fatores <- cor(nba_rookies_2022_final[,2:13])
correlacoes_entre_fatores %>% 
  melt() %>% 
  filter(Var1 %in% c("F1","F2","F3") &
           Var2 %in% c("w_pct","reb", "ast", "stl",
                       "blk","pts", "total_mins", "net_rating")) %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

#Assumindo-se apenas o F1 e F2 como indicadores, calculam-se os scores 
#fatorias
score_D1 <- scores_fatoriais[1,]
score_D1

score_D2 <- scores_fatoriais[2,]
score_D2

score_D3 <- scores_fatoriais[3,]
score_D3

#Estabelecendo o ranking dos indicadores assumido
F1 <- t(apply(nba_rookies_2022_std, 1, function(x) x * score_D1))
F2 <- t(apply(nba_rookies_2022_std, 1, function(x) x * score_D2))
F3 <- t(apply(nba_rookies_2022_std, 1, function(x) x * score_D3))
#Na construção de rankings no R, devemos efetuar a multiplicação por -1, 
#visto que os scores fatoriais das observações mais fortes são, por padrão, 


#apresentados acompanhados do sinal de menos.
F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * -1)

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

F2 <- data.frame(F2) %>%
  mutate(fator2 = rowSums(.) * 1)

F2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

F3 <- data.frame(F3) %>%
  mutate(fator3 = rowSums(.) * 1)

F3 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

nba_rookies_2022["Fator1"] <- F1$fator1
nba_rookies_2022["Fator2"] <- F2$fator2
nba_rookies_2022["Fator3"] <- F2$fator2

#Criando um ranking pela soma ponderada dos fatores por sua variância
#compartilhada:

#Calculando a variância compartilhada
var_compartilhada <- (afpc$sdev ^ 2/sum(afpc$sdev ^ 2))
var_compartilhada

nba_rookies_2022 %>%
  mutate(pontuacao = Fator1 * var_compartilhada[1] +
           Fator2 * var_compartilhada[2] + Fator3 * var_compartilhada[3]) -> nba_rookies_2022
nba_rookies_2022 %>%
  arrange(desc(pontuacao)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
