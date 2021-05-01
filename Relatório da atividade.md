Avaliação Técnica
================
José Otávio Souza

Carregamento dos pacotes e banco de dados utilizados:

``` r
library(tidyverse)
library(ggplot2)
library(readxl)
library(kableExtra)

#bd <- read_excel("bd_surveyquaest.xlsx") #carregar banco de dados apresentado
load("bd.Rda") 
```

## Qual o percentual de intenção de voto para cada candidato?

O cálculo da intenção de voto foi realizado a partir do total de
respostas. Entretanto, vale notar que por vezes interessa saber a
intenção de voto considerando somente os votos válidos, uma vez que esse
número representa as intenções de voto em termos mais próximos do que
seriam os resultados de uma eleição real. Caso fosse necessário observar
os votos válidos, bastaria criar um nova variável pelo comando seguinte:

``` r
#Dos votos válidos (excluindo 'não sabe/não respondeu' e 'Ninguém/Branco/Nulo')
bd$voto1_val <-  bd %>% select(voto1) %>%  mutate(voto1=na_if(voto1, "NS/NR")) %>% mutate(voto1=na_if(voto1, "Ninguém/Branco/Nulo")) #votos não válidos viram missing data (NA)
```

A distribuição das intenções de voto (total da amostra) pode ser
observada na tabela 1. A tabela foi estruturada como ‘data frame’, de
modo a facilitar sua edição, inclusive através da exportação para outros
programas de análise de dados, caso fosse desejável.

``` r
#Do total de respostas
t1<- bd %>% count(voto1) %>% mutate(perc = (n / nrow(bd) *100)) #data frame com candidato, número de observações (n) e intenção de voto em porcentagem
t1 %>% kbl(caption = "Tabela 1: Distribuição das intenções de voto (%)") %>% kable_classic(full_width = F, html_font = "Calibri") #elementos estéticos
```

<table class=" lightable-classic" style="font-family: Calibri; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Tabela 1: Distribuição das intenções de voto (%)
</caption>
<thead>
<tr>
<th style="text-align:left;">
voto1
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
perc
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Candidato 1
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
4.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 10
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
2.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 11
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.3
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 12
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.6
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 13
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.3
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 14
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.6
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 2
</td>
<td style="text-align:right;">
523
</td>
<td style="text-align:right;">
52.3
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 3
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
1.6
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 4
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
1.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 5
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
2.5
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 6
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
1.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.7
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 8
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
2.6
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 9
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
1.9
</td>
</tr>
<tr>
<td style="text-align:left;">
Ninguém/Branco/Nulo
</td>
<td style="text-align:right;">
142
</td>
<td style="text-align:right;">
14.2
</td>
</tr>
<tr>
<td style="text-align:left;">
NS/NR
</td>
<td style="text-align:right;">
140
</td>
<td style="text-align:right;">
14.0
</td>
</tr>
</tbody>
</table>

A distribuição das intenções de voto também pode ser observada pelo
gráfico 1, criado a partir da tabela 1.

``` r
#gráfico de intenção de voto
  ggplot(t1, aes(x = voto1, y = perc)) + geom_bar(stat = "identity", fill ="#104E8B") + ylab(" ") + xlab(" ") + ggtitle("Gráfico 1: Distribuição das intenções de voto (%)") + geom_text(aes( label =perc),  nudge_y=2) + coord_flip() + theme_minimal()
```

![](Title_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Como indicado, o candidato 2 está muito à frente dos demais candidatos,
com 52,3% das intenções de voto. Todos os demais candidatos exibem taxas
de intenção de voto inferiores a 5%. Parte considerável do eleitorado
não optou por nenhum candidato: 14% respondeu não sabe/não respondeu e
14,2% respondeu em ninguém/branco/nulo, somando 28,2%.

## Qual candidato com maior intenção de voto entre as mulheres?

Como indicado na tabela 2 abaixo, o candidato 2 tem a maior intenção de
voto entre mulheres, com 54% das intenções de voto nesse segmento. O
mesmo dado pode ser observado no gráfico 2.

``` r
#Tabela de distribuição da intenção de voto por gênero
t2 <-  table(bd$voto1, bd$sexo) %>% prop.table(margin=2) %>% round(digits = 2)  %>% addmargins(1) %>%  as.data.frame.matrix() #criação de tabela e conversão dela em uma matriz, para que possa ser modificada
rownames(t2)[rownames(t2) == "Sum"] <-  "Total" #renomear linha
t2 <- rownames_to_column(t2, "Candidato") #transformar nomes das linhas (candidatos) em coluna
t2$Feminino <- t2$Feminino*100 #conversão de proporção para porcentagem
t2$Masculino <- t2$Masculino*100 #conversão de proporção para porcentagem
t2 %>% kbl(caption = "Tabela 2: Intenção de voto por sexo (%)") %>% kable_classic(full_width = F, html_font = "Calibri") #elementos estéticos
```

<table class=" lightable-classic" style="font-family: Calibri; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Tabela 2: Intenção de voto por sexo (%)
</caption>
<thead>
<tr>
<th style="text-align:left;">
Candidato
</th>
<th style="text-align:right;">
Feminino
</th>
<th style="text-align:right;">
Masculino
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Candidato 1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 10
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 11
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 12
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 13
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 14
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 2
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
50
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 7
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 8
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 9
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Ninguém/Branco/Nulo
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
NS/NR
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
101
</td>
</tr>
</tbody>
</table>

``` r
#gráfico de distribuição de intenção de voto por sexo
ggplot(bd, aes(x= voto1,  fill=sexo)) + geom_bar(aes(y=..prop.., group=sexo), position = "dodge")  +coord_flip() + theme_minimal() +ggtitle("Gráfico 2: Intenção de voto (%) por sexo") +xlab(" ") +ylab(" ") +scale_y_continuous(labels = scales::percent_format()) 
```

![](Title_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## O candidato que lidera as intenções de voto é o candidato de situação ou oposição ao governo ?

Para responder a essa questão, basta observar como se distribui a
avaliação de governo entre os eleitores com intenção de votar no
candidato. Eleitores que avaliam o governo positivamente tendem a votar
na situação, ao passo que eleitores que o avaliam negativamente, ou
mesmo como regular, tendem a votar na oposição. Seria contraditório, do
ponto de vista do eleitor racional, votar pela continuidade de um
governo avaliado negativamente.

Essa ideia está baseada na teoria da escolha racional aplicada ao
comportamento eleitoral, desenvolvida a partir dos trabalhos de Anthony
Downs em sua Teoria Econômica da Democracia. O pressuposto fundamental é
que o eleitor se comporta de modo racional, escolhendo aquele candidato
que, na sua visão, trará maiores benefícios segundos seus interesses
individuais. Nesse modelo, a avaliação do governo é uma forma de julgar
retrospectivamente seu desempenho e criar expectativas em relação à sua
continuidade, as quais orientam a escolha pelo voto na continuidade ou
mudança de governo.

Para facilitar a análise, as 6 categorias de avaliação de governo foram
colapsadas em 3 - avaliação positiva (boa + ótima), negativa (ruim +
péssima) e regular (regular positiva + regular negativa). A tabela 3
mostra a intenção de voto e distriubição da avaliação de governo por
candidato.

Considerando que 87% dos que pretendem votar no candidato 2 (mais
votado) avaliaram o governo positivamente, é muito provável que ele seja
da situação.

``` r
#recodificação das 6 categorias em 3
bd$aval_gov_3 <-  case_when(bd$aval_gov == "Boa"| bd$aval_gov=="Ótima" ~ "Positiva", bd$aval_gov == "Ruim"| bd$aval_gov=="Péssima" ~ "Negativa", bd$aval_gov == "Regular positiva"| bd$aval_gov=="Regular negativa" ~ "Regular") 

#criação de tabela de avaliação de governo por candidato
t3 <- table(bd$voto1, bd$aval_gov_3) %>% prop.table(margin = 1) %>% round(digits = 2) %>%  as.data.frame.matrix() #criação da tabela e conversão para uma matriz, de modo a permitir sua manipulação

#inserir coluna intenção de voto (todas as respostas) na tabela
col_votos <- table(bd$voto1) %>% prop.table() %>% round(digits = 2) #separação da coluna em um vetor
t3 <- cbind(t3, col_votos) #adição da coluna à tabela
t3$Var1 <- NULL #remoção de coluna duplicada
t3 <- t3[,c(4,2,3,1)] #reorganização das colunas
t3 <- t3*100 #conversão de proporção para porcentagem
t3 <- rename(t3, "Intenção de Voto" = "Freq") #renomear coluna

#tabela final
t3 %>% kbl(caption = "Tabela 3: Intenção de voto e avaliação de governo (%)") %>% kable_classic(full_width = F, html_font = "Calibri") #elementos estéticos
```

<table class=" lightable-classic" style="font-family: Calibri; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Tabela 3: Intenção de voto e avaliação de governo (%)
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Intenção de Voto
</th>
<th style="text-align:right;">
Positiva
</th>
<th style="text-align:right;">
Regular
</th>
<th style="text-align:right;">
Negativa
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Candidato 1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 10
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 11
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
67
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 12
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 13
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 14
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 2
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
87
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 3
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 5
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 7
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 8
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
Candidato 9
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
Ninguém/Branco/Nulo
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
NS/NR
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
27
</td>
</tr>
</tbody>
</table>
