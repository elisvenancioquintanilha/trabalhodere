
# modelagem binária


rm(list=ls())

pacman::p_load(readr, tidyverse, corrplot, dplyr)

setwd("C:/Users/elisv/Downloads/regressao_p/trabalho/nat")   ## diretorio do trab
dados_orig <- read.table("dados.txt", header = T, check.names = T) 
df <- readRDS('dados_transf.rds')

names(df)

##tabela de frequência

# variáveis que algumas tem poucos níves
variaveis <- names(df)
variaveis <- c('parity', 'race', 'ed', 'drace', 'ded', 'marital', 'inc', 'time', 'number',"smoke")

par(mfrow=c(2,2))
for(i in 1:length(variaveis)){
  variavel <- variaveis[i]
  cat("\n\nVariável:", variavel, "\n")
  print(table(df[[variavel]]))
}


# agrupando as variaveis que tem umas categorias raras pra balancear

df <- df %>%
  mutate(

    # race - agrupar categorias minoritárias
    race_group = case_when(
      race == "White" ~ "White",
      race == "Black" ~ "Black",
      race %in% c("Mexican", "Asian", "Mixed") ~ "Other"
    ),
    
    # ed - agrupar níveis educacionais
    ed_group = case_when(
      ed %in% c("Less than 8th grade", "8th-12th grade (did not graduate)") ~ "Less than HS",
      ed == "HS graduate (no other schooling)" ~ "HS grad",
      ed %in% c("HS + trade school", "HS + some college") ~ "Trade/some college",
      ed == "College graduate" ~ "College grad",
      ed == "Trade school HS unclear" ~ "Other/Unknown"
    ),
    
    # drace - mesma lógica da race
    drace_group = case_when(
      drace == "White" ~ "White",
      drace == "Black" ~ "Black",
      drace %in% c("Mexican", "Asian", "Mixed") ~ "Other"
    ),
    
    # ded - mesma lógica da ed
    ded_group = case_when(
      ded %in% c("Less than 8th grade", "8th-12th grade (did not graduate)") ~ "Less than HS",
      ded == "HS graduate (no other schooling)" ~ "HS grad",
      ded %in% c("HS + trade school", "HS + some college") ~ "Trade/some college",
      ded == "College graduate" ~ "College grad",
      ded == "Trade school HS unclear" ~ "Other/Unknown"
    ),
    
    # marital - binária (casado vs não casado)
    marital_bin = case_when(
      marital == "Married" ~ "Married",
      marital %in% c("Legally Separated", "Divorced", "Never Married") ~ "Not married"
    ),
    # Nota: Widowed tem 0 casos, então ignorado
    
    # inc - agrupar extremos e criar categorias mais balanceadas   #era outaa ideia
    inc_group = case_when(
      inc == "Under 2500" ~ "Under 5000",
      inc == "2500-4999" ~ "Under 5000",
      inc %in% c("5000-7499", "7500-9999") ~ "5000-9999",
      inc %in% c("10000-12499", "12500-14999") ~ "10000-14999",
      inc %in% c("15000-17499", "17500-19999") ~ "15000-19999",
      inc %in% c("20000-24999", "25000+") ~ "20000+"
    ),
    
    
    # time - agrupar por status de tabagismo na gestação
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
    
    # number - agrupar número de cigarros por dia
    number_group = case_when(
      number == "Never" ~ "Never",
      number %in% c("1-4 cigs/day", "5-9 cigs/day") ~ "Light (1-9/day)",
      number %in% c("10-14 cigs/day", "15-19 cigs/day") ~ "Moderate (10-19/day)",
      number %in% c("20-29 cigs/day", "30-39 cigs/day", 
                    "40-60 cigs/day", "60+ cigs/day") ~ "Heavy (20+/day)"
    ),
    
    # smoke
    df$smoke_bin <- ifelse(df$smoke == "Smokes now", "Fuma atualmente", "Não fuma atualmente")
    
  )

df$smoke_bin <- ifelse(df$smoke == "Smokes now", "Fuma atualmente", "Não fuma atualmente")
df$smoke_bin <- factor(df$smoke_bin)

# df$smoke <- df$smoke_bin

table(df$smoke)
table(df$smoke_bin)

# elis 1 cheio
mod1 <- lm( wt ~ smoke_bin + gestation + parity + race_group + ed_group + marital_bin, data= df )
summary(mod1)

#nat 1 cheio
mod2 <- lm( wt ~ smoke + gestation + parity + race_group + ed_group + marital_bin, data= df )
summary(mod2)

#elis 2 limpo
mod3 <- lm(wt ~ smoke_bin + gestation + parity + race_group, data= df )
summary(mod3)

#nat 2 limpo
mod4 <- lm(wt ~ smoke + gestation + parity + race_group, data= df )
summary(mod4)


plot(mod2) #nat / cheio
plot(mod4) # nat / limpo
plot(mod1)
plot(mod3)

par(mfrow = c(4, 4))  # 4 linhas e 4 colunas


plot(mod2) #nat / cheio
plot(mod4) # nat / limpo
plot(mod1)
plot(mod3)


par(mfrow = c(1, 1))  # volta ao padrão



# modelos finais com smoke e smoke_bin

#modelo final smoke_bin
summary(mod3)

#modelo final smoke - nat
summary(mod4)

AIC(mod3, mod4)
BIC(mod3, mod4)
anova(mod3, mod4)

df[261, ]
df[153, ]

sum(df$inc == 10, na.rm = TRUE)

df$inc[df$inc == 10] <- NA


