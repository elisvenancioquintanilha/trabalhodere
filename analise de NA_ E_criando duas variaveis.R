
rm(list=ls())                  #linha 216 criação das variaveis

pacman::p_load(readr, tidyverse, corrplot, dplyr)

setwd("C:/Users/elisv/Downloads/regressao_p/trabalho/nat")   ## diretorio do trab
dados_orig <- read.table("dados.txt", header = T, check.names = T) 
df <- readRDS('dados_transf.rds')

#### lidando com as categ raras
#####
df <- df %>%
  mutate(
    # 1. parity - agrupar 
    parity_group = case_when(
      parity == 0 ~ "0",
      parity == 1 ~ "1",
      parity == 2 ~ "2",
      parity == 3 ~ "3",
      parity == 4 ~ "4",
      parity == 5 ~ "5",
      parity == 6 ~ "6",
      parity == 7 ~ "7",
      parity >= 8 ~ "8+"
    ),
    
    # 2. race - agrupar categorias minoritárias
    race_group = case_when(
      race == "White" ~ "White",
      race == "Black" ~ "Black",
      race %in% c("Mexican", "Asian", "Mixed") ~ "Other"
    ),
    
    # 3. ed - agrupar níveis educacionais
    ed_group = case_when(
      ed %in% c("Less than 8th grade", "8th-12th grade (did not graduate)") ~ "Less than HS",
      ed == "HS graduate (no other schooling)" ~ "HS grad",
      ed %in% c("HS + trade school", "HS + some college") ~ "Trade/some college",
      ed == "College graduate" ~ "College grad",
      ed == "Trade school HS unclear" ~ "Other/Unknown"
    ),
    
    # 4. drace - mesma lógica da race
    drace_group = case_when(
      drace == "White" ~ "White",
      drace == "Black" ~ "Black",
      drace %in% c("Mexican", "Asian", "Mixed") ~ "Other"
    ),
    
    # 5. ded - mesma lógica da ed
    ded_group = case_when(
      ded %in% c("Less than 8th grade", "8th-12th grade (did not graduate)") ~ "Less than HS",
      ded == "HS graduate (no other schooling)" ~ "HS grad",
      ded %in% c("HS + trade school", "HS + some college") ~ "Trade/some college",
      ded == "College graduate" ~ "College grad",
      ded == "Trade school HS unclear" ~ "Other/Unknown"
    ),
    
    # 6. marital - binária (casado vs não casado)
    marital_bin = case_when(
      marital == "Married" ~ "Married",
      marital %in% c("Legally Separated", "Divorced", "Never Married") ~ "Not married"
    ),
    # Nota: Widowed tem 0 casos, então ignorado
    
    # # 7. inc - agrupar extremos e criar categorias mais balanceadas   #era outaa ideia
    # inc_group = case_when(
    #   inc == "Under 2500" ~ "Under 5000",
    #   inc == "2500-4999" ~ "Under 5000",
    #   inc %in% c("5000-7499", "7500-9999") ~ "5000-9999",
    #   inc %in% c("10000-12499", "12500-14999") ~ "10000-14999",
    #   inc %in% c("15000-17499", "17500-19999") ~ "15000-19999",
    #   inc %in% c("20000-24999", "25000+") ~ "20000+"
    # ),
    
    # 7. inc - abordagem híbrida (mantém under 2500 separado)
    inc_group = case_when(
      inc == "Under 2500"                       ~ "Under 2500",
      inc == "2500-4999"                        ~ "2500-4999",
      inc %in% c("5000-7499", "7500-9999")      ~ "5000-9999",
      inc %in% c("10000-12499", "12500-14999")  ~ "10000-14999",
      inc %in% c("15000-17499", "17500-19999")  ~ "15000-19999",
      inc %in% c("20000-24999", "25000+")       ~ "20000+"
    ),
    
    # # 8. time - agrupar categorias de tempo desde que parou
    # time_group = case_when(
    #   time == "Never smoked" ~ "Never smoked",
    #   time == "Still smokes" ~ "Still smokes",
    #   time == "During current pregnancy" ~ "Quit during pregnancy",
    #   time %in% c("Within 1 year", "1 to 2 years ago") ~ "Quit within 2 years",
    #   time %in% c("2 to 3 years ago", "3 to 4 years ago", 
    #               "5 to 9 years ago", "10+ years ago") ~ "Quit 2+ years ago"
    # ),
    
    # 8. time - agrupar por status de tabagismo na gestação
    time_group = case_when(
      time == "Never smoked"                 ~ "Never smoked",
      time == "Still smokes"                 ~ "Current smoker",
      time == "During current pregnancy"     ~ "Quit during pregnancy",
      time %in% c("Within 1 year", 
                  "1 to 2 years ago",
                  "2 to 3 years ago", 
                  "3 to 4 years ago",
                  "5 to 9 years ago", 
                  "10+ years ago")           ~ "Quit before pregnancy"
    ),
    
    # 9. number - agrupar número de cigarros por dia
    number_group = case_when(
      number == "Never" ~ "Never",
      number %in% c("1-4 cigs/day", "5-9 cigs/day") ~ "Light (1-9/day)",
      number %in% c("10-14 cigs/day", "15-19 cigs/day") ~ "Moderate (10-19/day)",
      number %in% c("20-29 cigs/day", "30-39 cigs/day", 
                    "40-60 cigs/day", "60+ cigs/day") ~ "Heavy (20+/day)"
    )
  )
####

####criando figuras#


####analisandoNA# 

####

# Análise de valores missing (NA)
#####

# Carregar pacote adicional
pacman::p_load(naniar)

# 1. Visão geral dos NA
cat("\n========== VISÃO GERAL DOS NA ==========\n")
cat("Total de observações:", nrow(df), "\n")
cat("Total de valores NA:", sum(is.na(df)), "\n")
cat("Proporção de NA no dataset:", round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 2), "%\n")

# 2. Contagem de NA por variável
cat("\n========== NA POR VARIÁVEL ==========\n")
na_count <- data.frame(
  Variavel = names(df),
  NA_count = sapply(df, function(x) sum(is.na(x))),
  NA_percent = sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 2))
) %>% arrange(desc(NA_count))

print(na_count)

# 3. Variáveis com mais de 10% de NA
cat("\n========== VARIÁVEIS COM >10% NA ==========\n")
na_count %>% filter(NA_percent > 10) %>% print()

# 4. Visualização da proporção de NA
png("figuras/07_na_proporcao.png", width = 10, height = 6, units = "in", res = 300)
gg_miss_var(df, show_pct = TRUE) + 
  labs(title = "Proporção de valores missing por variável") +
  theme_minimal()
dev.off()

# 5. Matriz de missing (visão geral)
png("figuras/08_na_matriz.png", width = 10, height = 8, units = "in", res = 300)
vis_miss(df, cluster = TRUE) + 
  labs(title = "Matriz de valores missing")
dev.off()

# 6. Tabela resumo para o relatório
cat("\n========== TABELA RESUMO PARA RELATÓRIO ==========\n")
tabela_na <- na_count %>%
  filter(NA_count > 0) %>%
  mutate(
    Variavel = case_when(
      Variavel == "wt.1" ~ "Peso da mãe (wt.1)",
      Variavel == "dht" ~ "Altura do pai (dht)",
      Variavel == "dwt" ~ "Peso do pai (dwt)",
      Variavel == "inc" ~ "Renda (inc)",
      Variavel == "number" ~ "Cigarros/dia (number)",
      Variavel == "time" ~ "Tempo desde parada (time)",
      Variavel == "dage" ~ "Idade do pai (dage)",
      Variavel == "ht" ~ "Altura da mãe (ht)",
      Variavel == "gestation" ~ "Gestação (gestation)",
      Variavel == "ded" ~ "Escolaridade do pai (ded)",
      Variavel == "drace" ~ "Raça do pai (drace)",
      Variavel == "age" ~ "Idade da mãe (age)",
      Variavel == "smoke" ~ "Tabagismo (smoke)",
      Variavel == "marital" ~ "Estado civil (marital)",
      Variavel == "ed" ~ "Escolaridade da mãe (ed)",
      TRUE ~ Variavel
    )
  ) %>%
  select(Variavel, NA_count, NA_percent)

print(tabela_na)

# 7. Salvar tabela
write.csv(tabela_na, "figuras/tabela_na.csv", row.names = FALSE)

# 8. Verificar se há padrões nos missing (ex: mãe sem informação = pai sem informação?)
cat("\n========== PADRÕES DE MISSING ==========\n")
# Correlação entre missing de variáveis paternas
if("dht" %in% names(df) & "dwt" %in% names(df)) {
  missing_pai <- df %>% 
    mutate(missing_dht = is.na(dht),
           missing_dwt = is.na(dwt)) %>%
    summarise(
      ambos_presentes = sum(!missing_dht & !missing_dwt),
      ambos_missing = sum(missing_dht & missing_dwt),
      so_dht_missing = sum(missing_dht & !missing_dwt),
      so_dwt_missing = sum(!missing_dht & missing_dwt)
    )
  print(missing_pai)
}
#####

####

####criando variaveis#
#Criando duas variaveis


df <- df %>%
  mutate(
    # Baixo peso ao nascer (categórico)
    baixo_peso_cat = case_when(
      wt < 88.2 ~ "Baixo peso",
      TRUE ~ "Peso normal"
    ),
    
    # Prematuridade (categórico)
    prem_cat = case_when(
      gestation < 259 ~ "Prematuro",
      TRUE ~ "Não prematuro"
    )
  )

# Verificar
table(df$baixo_peso_cat)
table(df$prem_cat)



library(ggplot2)

ggplot(df, aes(x = prem_cat, y = wt, fill = prem_cat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1.5) +
  labs(title = "Peso ao nascer por prematuridade",
       x = "",
       y = "Peso (onças)") +
  scale_x_discrete(labels = c("Não prematuro", "Prematuro")) +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "none")
#####
