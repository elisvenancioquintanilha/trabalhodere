
rm(list=ls())

# carregando pacotes
pacman::p_load(readr, tidyverse)

# carregando dados
setwd("C:/Users/elisv/Downloads/regressao_p/trabalho/nat")
dados_orig <- read.table("dados.txt", header = T, check.names = T) 

df <- readRDS('dados_transf.rds')



## extra

dlookr::diagnose(df)
#View(dlookr::diagnose(df))


# modelo
# wt.1 n tinha antes no dicionario.

df2 <- df[, !names(df) %in% c("dwt" ,"dht" ,"pluralty", "outcome", "sex", "date", "id")] # removendo variaveis com muito NA pra testar
#View(dlookr::diagnose(df2))                                                       # e tbm removendo as que nao faz sentido usar
str(df2)
mod  <- lm( wt ~ . , data = df2)  
summary(mod)


# verificando as variaveis de peso da mae e da criança 


plot(df$wt, df$wt.1 )
df$wt - df$wt.1
s <- df$wt - df$wt.1
s

#valores iguais de peso da criança e peso da mae????????
sum(df2$wt == df2$wt.1, na.rm = TRUE)

dados_limpos <- df2 %>% filter( !(wt == wt.1) ) # limpando os dados


mod2  <- lm( wt ~ . , data = dados_limpos)  
summary(mod)



# modelo4 <- lm( dage ~ age, data = dados_limpos )
# summary(modelo4)
# 
# modelo5 <- lm( age ~ dage, data = dados_limpos )
# summary(modelo5)
# # vamos ter que remover dage ou age
# 
# modelo6 <- lm( ht ~ wt.1, data = dados_limpos )
# summary(modelo6)
# 
# dados2 <- na.omit(dados_limpos)
# modelo7 <- lm( wt ~ ., data = dados2 )
# summary(modelo7)




modelo8 <- lm( wt ~ gestation + ht + wt.1 + smoke, data = dados_limpos)
summary(modelo8)
# 4 variaveis explicam 24%

modelo9 <- lm( wt ~ smoke + gestation , data = dados_limpos ) 
summary(modelo9)
# essas duas variaveis explicam 21%, enquanto todas explicam 30%




modelo10 <- lm(wt ~ smoke + gestation + age + ht + wt.1 +
                 parity + race + ed + inc,
               data=dados_limpos)
summary(modelo10)
# 9 variáveis explicam 27%


# TESTANDO INTERAÇÕES

modelo11 <- lm(wt ~ smoke*gestation + age + ht + wt.1 + parity + ed + race + inc, data=dados_limpos)
summary(modelo11)
# com interação explica 28%

modelo12 <- lm(wt ~ smoke*race + gestation + age + ht + wt.1 + parity + ed + inc, data=dados_limpos)
summary(modelo12)



modelo13 <- lm(wt ~ smoke +
                 gestation + I(gestation^2) +
                 age + I(age^2) +
                 wt.1 + I(wt.1^2) +
                 ht + parity,
               data=dados_limpos)
summary(modelo13)

modelo14 <- lm(log(wt) ~ smoke +
                 gestation +
                 age +
                 wt.1 +
                 ht + parity,
               data=dados_limpos)
summary(modelo14) 




# testando modelo com fatores reduzidos - ideia do chatgpt


dados_limpos$smoke2 <- fct_collapse(
  dados_limpos$smoke,
  Never = "Never",
  Current = "Smokes now",
  Former = c("Until current pregnancy","Once did, not now")
)


race2 <- fct_collapse(dados_limpos$race,
                      White="White",
                      Black="Black",
                      Other=c("Mexican","Asian","Mixed"))


dados_limpos$inc2 <- forcats::fct_collapse(
  dados_limpos$inc,
  
  Baixa = c("Under 2500",
            "2500-4999",
            "5000-7499",
            "7500-9999"),
  
  Media = c("10000-12499",
            "12500-14999",
            "15000-17499",
            "17500-19999"),
  
  Alta = c("20000-24999",
           "25000+")
)


modelo15 <- lm(
  wt ~ smoke2*gestation + ht + wt.1 + parity + race2 + inc2,
  data=dados_limpos)
summary(modelo15)
# deu 28%



# analise do table/frequencia das variáveis.
par(mfrow=c(2,2))
for(i in 1:23){
  variavel <- names(df)[i]
  cat("\n\nVariável:", variavel, "\n")
  print(table(df[[variavel]]))
}


# variáveis que algumas tem poucos níves
variaveis <- c('parity', 'race', 'ed', 'drace', 'ded', 'marital', 'inc', 'time', 'number')

par(mfrow=c(2,2))
for(i in 1:length(variaveis)){
  variavel <- variaveis[i]
  cat("\n\nVariável:", variavel, "\n")
  print(table(df[[variavel]]))
}


# agrupando os niveis raros dessas 9 variaveis qualitativas.

# RARA: O coeficiente para essa categoria terá erro padrão enorme
# Intervalos de confiança extremamente amplos.

library(dplyr)

# Assumindo que seu dataframe se chama 'df'
df <- df %>%
  mutate(
    # 1. parity - agrupar cauda longa
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
    
    # # 7. inc - agrupar extremos e criar categorias mais balanceadas
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

# Verificar os resultados dos agrupamentos
for(var in c('parity_group', 'race_group', 'ed_group', 'drace_group', 
             'ded_group', 'marital_bin', 'inc_group', 'time_group', 'number_group')) {
  cat("\n\nVariável:", var, "\n")
  print(table(df[[var]]))
}

# Opcional: Converter para fatores ordenados quando apropriado
df <- df %>%
  mutate(
    parity_group = factor(parity_group, levels = c("0", "1", "2", "3", "4+")),
    ed_group = factor(ed_group, levels = c("Less than HS", "HS grad", "Some college", "College grad", "Other/Unknown")),
    ded_group = factor(ded_group, levels = c("Less than HS", "HS grad", "Some college", "College grad", "Other/Unknown")),
    inc_group = factor(inc_group, levels = c("Under 5000", "5000-9999", "10000-14999", "15000-19999", "20000+")),
    time_group = factor(time_group, levels = c("Never smoked", "Still smokes", "Quit during pregnancy", "Quit within 2 years", "Quit 2+ years ago")),
    number_group = factor(number_group, levels = c("Never", "Light (1-9/day)", "Moderate (10-19/day)", "Heavy (20+/day)"))
  )

names(df)

names(dados_orig)

summary(df$wt)

summary(df$wt.1)
