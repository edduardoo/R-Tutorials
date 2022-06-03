library(tidyverse)

# Atualizar caso já tenha instalado.
tidyverse_update()

# Diretório onde está.
system.file(package = "tidyverse")

# Descrição completa do pacote.
packageDescription("tidyverse")

# Funções disponíveis começando com `read`.
apropos("^read")

# Apenas as funções do {readr}.
ls("package:readr") %>%
  str_subset("^read_")

# Endereço web do arquivo, mas poderia ser local.
url <- "http://leg.ufpr.br/~walmes/data/euro_football_players.txt"

# Importa a tabela de dados.
tb <- read_tsv(file = url,
               comment = "#")
head(tb, n = 6)


# atributos de um objeto
attributes(tb)
attr(tb, "spec") <- NULL

class(tb)


methods(class = "tbl_df")
str(tb)

# CRIACAO DE DATA.FRAME
# Cria um tibble data.frame a partir de vetores.
df1 <- tibble(matricula = c(256, 487, 965, 125, 458, 874, 963),
              nome = c("João", "Vanessa", "Tiago", "Luana", "Gisele",
                       "Pedro", "André"),
              curso = c("Mat", "Mat", "Est", "Est", "Est", "Mat", "Est"),
              prova1 = c(80, 75, 95, 70, 45, 55, 30),
              prova2 = c(90, 75, 80, 85, 50, 75, NA),
              prova3 = c(80, 75, 75, 50, NA, 90, 30),
              faltas = c(4, 4, 0, 8, 16, 0, 20))
df2 <- tibble(matricula = c(505, 658, 713),
              nome = c("Bia", "Carlos", "Cris"),
              curso = c("Eng", "Eng", "Eng"),
              prova1 = c(65, 75, 75),
              prova2 = c(85, 80, 90),
              faltas = c(0, 0, 2))


# criação por linhas ou tuplas
df_extra <- tribble(
  ~mat,     ~nome, ~idade, ~bolsista,
  256,  'João'  ,     18,       "S",
  965,  'Tiago' ,     18,       "N",
  285,  'Tiago' ,     22,       "N",
  125,  'Luana' ,     21,       "S",
  874,  'Pedro' ,     19,       "N",
  321,  'Mia'   ,     18,       "N",
  669,  'Luana' ,     19,       "S",
  967,  'André' ,     20,       "N",
)


# ordenação dos registros
df1 %>%
  arrange(matricula)

# por mais de uma variavel
df1 %>%
  arrange(curso, desc(prova1))


# Nome das variáveis.
names(df1)

# Nome das linhas (caso tenha).
rownames(df1)

# Dimensões da tabela
dim(df1)
nrow(df1)


# SELEÇÃO DAS VARIÁVEIS
# Seleção usando a builtin `[`.
df1[, c("nome")]
df1[, c("nome", "prova1", "prova2", "prova3")]



# Seleção com `select()`.
df1 %>%
  select(c("nome", "prova1", "prova2", "prova3"))

df1 %>%
  select(nome, prova1, prova2, prova3) # Dispensa aspas.

df1 %>% 
  select(-nome, -faltas) # Permite excluir.

df1 %>% 
  select(prova1:prova3)  # Permite intervalos.

# seleção pela posição das variáveis
# Usando `[`.
df1[, 1:3]
df1[, c(1, 4)]
df1[, c(-1, -4)]


# Usando `select()`..
df1 %>% 
  select(1:3)
df1 %>%
  select(1, 4)
df1 %>%
  select(-1, -4)



# selecionando apenas as posiçoes pares
df1[, seq(1, ncol(df1), by = 2)]

# selecionando um conjunto de intervalos: 1 a 3  e 5 a 7
df1[, c(1:3, 5:7)]

# selecionando todas, exceto as variaveis de uma lista
v <- c("prova1", "prova2", "prova3")
df1 %>% select(-v)

# selecionar de acordo com o tipo de valor: apenas as vars do tipo numérico
df1 %>% select_if(is.numeric)

?select_if
df1 %>% select(matches("^prova"))
df1 %>% select(matches("\\d$"))
df1 %>% select(matches("^.{6}$"))



# SELEÇÃO POR FATIAMENTO DE LINHAS
# Usando `[` para selecionar linhas pelos índices.
df1[1, ]
df1[3:5, ]
df1[-(3:5), ]
df1[c(3:4, 1:2), ]

# Topo e base da tabela.
tail(df1, n = 2)
head(df1, n=2)

# usando a slice() para fatiar
df1 %>% slice(1)
df1 %>% slice(3:5)
df1 %>% slice(-(3:5))
df1 %>% slice(c(3:4, 1:2))


# AMOSTRA ALEATORIA DOS REGISTROS
# Usando uma máscara lógica.
i <- sample(c(TRUE, FALSE), size = nrow(df1),
            prob = c(0.4, 0.6), replace = TRUE)
df1[i, ]

# Amostra aleatória das linhas.
df1 %>%
  sample_n(size = 3, replace = FALSE)
df1 %>%
  sample_frac(size = 0.5, replace = FALSE)


# SELECAO DE VALORES NA TABELA
df1[2, "nome"] # linha e coluna



# FILTRANDO REGISTROS
# com vetor logico
df1[df1$curso == "Est", ]
df1[df1$faltas == 0, ]
df1[df1$faltas != 0, ]
df1[df1$nome %in% c("Gisele", "Vanessa"), ]

# usando a filter() - (tb passo vetores logicos, mas aqui consigo aproveitar o pipe)
df1 %>%
  filter(curso == "Est")
df1 %>%
  filter(faltas == 0)
df1 %>%
  filter(faltas != 0)
df1 %>%
  filter(nome %in% c("Gisele", "Vanessa"))

# combinando vetores logicos
df1[with(df1,
         prova1 >= 7 & prova2 >= 7), ]
df1[with(df1,
         ((prova1 + prova2 + prova3) >= 21) & faltas < 15), ]


# Lista dos aprovados e reprovados em cálculo.
mask <- with(df1, ((prova1 + prova2 + prova3) >= 21) & faltas < 15)
df1[mask, ]  # Não reprovados (aprovados e exame).

df1[!mask, ] # Os reprovados (! é o operador que inverte valores lógicos).


df1 %>% 
  filter((prova1 + prova2 + prova3) >= 21,
         faltas < 15)


# RENOMEAÇÃO DE VARIÁVEIS
# Renomeia nomes de colunas (variáveis).
df1 %>% 
  rename("mat." = "matricula", "fl" = "faltas")

# função de transformação de strings
names(df1) <- names(df1) %>% 
              str_to_upper()
names(df1) <- names(df1) %>% 
              str_sub(start = 1, stop = 3)

# operações de expressão regular
names(iris)
iris %>%
  rename_with(.fn = str_to_upper, 
              .cols = matches("Length")) %>%
  head(2)

iris %>%
  rename_with(.fn = str_sub,
              start = 1, end = 7, 
              .cols = matches("Sepal")) %>%
  head(2)

iris %>%
  rename_with(.fn = str_remove_all,
              pattern = "[a-z.]", 
              .cols = everything()) %>%
  head(2)

df1 <- df1 %>% 
  rename_with(.fn = str_to_lower)



##########################################
# TRANSFORMAÇÃO DE VARIÁVEIS
##########################################
# transformações matemáticas
# ATTENTION: dessa forma é ruim de tratar os NA's.
df1$media <- with(df1, (prova1 + prova2 + prova3)/3)

# Cria a variável média.
df1$media <- df1 %>% 
  select(prova1:prova3) %>% 
  apply(MARGIN = 1, FUN = sum, na.rm=TRUE)/3

df1 %>% 
  rowwise() %>%
  mutate(media = sum(c_across(starts_with("prova")),
                     na.rm = TRUE)/3) %>%
  ungroup()


# transformações matematicas homogeneas
df1 %>%
  mutate_if(is.numeric, log10)
df1 %>%
  mutate_if(is.character, str_to_upper)


# Com `mutate_at()`.
df1 %>% 
  mutate_at(vars(starts_with("prova")), log10)

# Com `accros()`.
df1 %>%
  mutate(across(where(is.character), str_to_upper))
df1 %>%
  mutate(across(.cols = starts_with("prova"), log10))

# Fazendo várias transformações.
df1 %>% 
  mutate_at(c("prova1", "prova2"),
            c("log10", "sqrt")) %>%
  select(contains("_"))

# Passando nomes para as funções.
df1 %>% 
  mutate_at(c("prova1", "prova2"),
            list(log = "log10", raiz = "sqrt")) %>%
  select(contains("_"))


# TRANSFORMAÇÃO DE AGRUPAR VALORES EM CLASSES
# Cria a variável média.
df1 <- df1 %>%
  rowwise() %>%
  mutate(media = sum(c_across(starts_with("prova")),
                     na.rm = TRUE)/3) %>%
  ungroup()

# Intervalos para corte e rótulos.
inter <- c(0, 40, 70, Inf)
condi <- c("reprovado", "exame", "aprovado")

# Cria a variável que é a condição.
# df1[["condicao"]] <-
df1$condicao <- 
  cut(df1[["media"]], # ou `df1$media`.
      breaks = inter,
      labels = condi,
      right = FALSE,
      include.lowest = TRUE)

# Usando dentro da `mutate()`.
df1 %>% 
  mutate(media = replace_na(media, 0),
         condicao = cut(media, 
                        breaks = inter,
                        labels = condi,
                        right = FALSE,
                        include.lowest = TRUE))




# TRANSFORMAÇÃO DE CONVERSAO DE TIPO DE VALOR
df1 %>%
  mutate_if(is.character, as.factor)
df1 %>%
  mutate_if(is.numeric, as.integer)



# SUBSTITUIÇÃO DE VALORES AUSENTES
# Como testar valores ausentes ou não.
df1$prova2 %>% is.na()
df1$prova3 %>% negate(is.na)()
df1$prova3 %>% is.na() %>% `!`()
df1$prova3 %>% is.finite() # O oposto de NA, NaN, Inf.


# Aplica na variável.
df1$prova2 %>%
  replace_na(replace = 0)

df1 %>%
  mutate(prova2 = replace_na(prova2, 0))

# Aplica para a lista de variável-valor.
df1 %>%
  replace_na(replace = list(prova2 = 0, prova3 = 0, faltas = 60))

# Aplica conforme o tipo da variável.
df1 %>%
  mutate_if(is.numeric, replace_na, replace = 0)


# EXCLUI VARIÁVEIS
df1$media <- NULL
df1$condicao <- NULL


# REARRANJO/REDISPOSIÇÃO (EMPLIHAR E DESEMPILHAR)
# Gather = amontoar.
u <- df1 %>%
  gather(key = "provas", value = "nota", prova1:prova3)
u

# Spread = esparramar.
v <- u %>%
  spread(key = "provas", value = "nota")
v

# Existe outro par de funções: pivot_longer() & pivot_wider().
# Essas funções tem muito mais opções de controle. Use elas.
u <- df1 %>%
  pivot_longer(prova1:prova3, 
               names_to = "provas",
               values_to = "nota")
u
v <- u %>%
  pivot_wider(names_from = "provas",
              values_from = "nota")
v








