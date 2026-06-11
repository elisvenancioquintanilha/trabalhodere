

# ============================================================
# Configuração inicial
# ============================================================

rm(list=ls())

pacman::p_load(readr, tidyverse, corrplot, dplyr)

setwd("C:/Users/elisv/Downloads/regressao_p/trabalho/nat")   ## diretorio do trab
dados_orig <- read.table("dados.txt", header = T, check.names = T) 
df <- readRDS('dados_transf.rds')

# agrupando as variaveis que tem umas categorias raras pra balancear

df <- df %>%
  mutate(
    # # 1. parity - agrupar 
    # parity_group = case_when(              # vamos usar discreta
    #   parity == 0 ~ "0",
    #   parity == 1 ~ "1",
    #   parity == 2 ~ "2",
    #   parity == 3 ~ "3",
    #   parity == 4 ~ "4",
    #   parity == 5 ~ "5",
    #   parity == 6 ~ "6",
    #   parity == 7 ~ "7",
    #   parity >= 8 ~ "8+"
    # ),
    
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
    
    # 7. inc - agrupar extremos e criar categorias mais balanceadas   #era outaa ideia
    inc_group = case_when(
      inc == "Under 2500" ~ "Under 5000",
      inc == "2500-4999" ~ "Under 5000",
      inc %in% c("5000-7499", "7500-9999") ~ "5000-9999",
      inc %in% c("10000-12499", "12500-14999") ~ "10000-14999",
      inc %in% c("15000-17499", "17500-19999") ~ "15000-19999",
      inc %in% c("20000-24999", "25000+") ~ "20000+"
    ),
    
    # # 7. inc - abordagem híbrida (mantém under 2500 separado)
    # inc_group = case_when(
    #   inc == "Under 2500"                       ~ "Under 2500",
    #   inc == "2500-4999"                        ~ "2500-4999",
    #   inc %in% c("5000-7499", "7500-9999")      ~ "5000-9999",
    #   inc %in% c("10000-12499", "12500-14999")  ~ "10000-14999",
    #   inc %in% c("15000-17499", "17500-19999")  ~ "15000-19999",
    #   inc %in% c("20000-24999", "25000+")       ~ "20000+"
    # ),
    
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

# Criar pasta para figuras (se não existir)                # salvar a pasta pra depois so joga
if(!dir.exists("figuras")) {
  dir.create("figuras")
}

# ============================================================
# Figura 1 - Variável resposta (peso ao nascer)           codigos de antes
# ============================================================

png("figuras/01_hist_wt.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow=c(1,2), mar=c(4,4,2,2))
hist(df$wt, 
     main="Histograma do peso ao nascer", 
     xlab="Peso (onças)", 
     col="lightblue", 
     border="white")
boxplot(df$wt, 
        main="Boxplot do peso ao nascer", 
        ylab="Peso (onças)", 
        col="lightblue")
dev.off()

# ============================================================
# Figura 2 - Variáveis quantitativas (gestation, age, ht, wt.1, parity, dage)
# ============================================================

png("figuras/02_quantitativas.png", width = 12, height = 10, units = "in", res = 300)
par(mfrow=c(2,3), mar=c(4,4,2,2))

hist(df$gestation, main="Gestação (dias)", xlab="dias", col="lightblue", border="white")
boxplot(df$gestation, col="lightblue")

hist(df$age, main="Idade da mãe (anos)", xlab="anos", col="lightblue", border="white")
boxplot(df$age, col="lightblue")

hist(df$ht, main="Altura da mãe (polegadas)", xlab="polegadas", col="lightblue", border="white")
boxplot(df$ht, col="lightblue")

hist(df$wt.1, main="Peso da mãe (libras)", xlab="libras", col="lightblue", border="white")
boxplot(df$wt.1, col="lightblue")

hist(df$parity, main="Número de gestações anteriores", xlab="gestações", col="lightblue", border="white")
boxplot(df$parity, col="lightblue")

hist(df$dage, main="Idade do pai (anos)", xlab="anos", col="lightblue", border="white")
boxplot(df$dage, col="lightblue")

dev.off()


# ============================================================
# Figura 3 - Boxplots: peso ao nascer vs variáveis qualitativas
# ============================================================

png("figuras/03_boxplots_qualitativas.png", width = 14, height = 10, units = "in", res = 300)
par(mfrow=c(2,3), mar=c(6,4,3,2))

boxplot(wt ~ race_group, data=df, 
        main="Peso vs Raça da mãe", 
        xlab="", las=2, col="lightblue")

boxplot(wt ~ ed_group, data=df, 
        main="Peso vs Escolaridade da mãe", 
        xlab="", las=2, col="lightblue")

boxplot(wt ~ smoke, data=df, 
        main="Peso vs Tabagismo", 
        xlab="", las=2, col="lightblue")

boxplot(wt ~ time_group, data=df, 
        main="Peso vs Tempo desde parada", 
        xlab="", las=2, col="lightblue")

boxplot(wt ~ number_group, data=df, 
        main="Peso vs Cigarros/dia", 
        xlab="", las=2, col="lightblue")

boxplot(wt ~ marital_bin, data=df, 
        main="Peso vs Estado civil", 
        xlab="", las=2, col="lightblue")

dev.off()

# ============================================================
# Figura 4 - Gráficos de dispersão (wt vs variáveis contínuas)
# ============================================================

png("figuras/04_dispersao.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow=c(2,3), mar=c(4,4,2,2))

plot(wt ~ gestation, data=df, 
     main="Peso vs Gestação", 
     xlab="Dias de gestação", ylab="Peso (onças)",
     pch=16, col="black")

plot(wt ~ age, data=df, 
     main="Peso vs Idade da mãe", 
     xlab="Idade (anos)", ylab="Peso (onças)",
     pch=16, col="black")

plot(wt ~ ht, data=df, 
     main="Peso vs Altura da mãe", 
     xlab="Altura (polegadas)", ylab="Peso (onças)",
     pch=16, col="black")

plot(wt ~ wt.1, data=df, 
     main="Peso vs Peso da mãe", 
     xlab="Peso da mãe (libras)", ylab="Peso (onças)",
     pch=16, col="black")

plot(wt ~ parity, data=df, 
     main="Peso vs N° de gestações anteriores", 
     xlab="Nº gestações anteriores", ylab="Peso (onças)",
     pch=16, col="black")

plot(wt ~ dage, data=df, 
     main="Peso vs Idade do pai", 
     xlab="Idade do pai (anos)", ylab="Peso (onças)",
     pch=16, col="black")

dev.off()

# ============================================================
# Figura 5 - Correlograma
# ============================================================

variaveis_num <- df[, c('gestation','parity','age','ht','wt.1','dage')]
variaveis_num <- na.omit(variaveis_num)

png("figuras/05_correlograma.png", width = 8, height = 8, units = "in", res = 300)
corrplot(cor(variaveis_num), 
         method = "square", 
         type = "upper", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.cex = 1, 
         number.cex = 1,
         diag = FALSE)
dev.off()

# ============================================================
# Figura 6 - Boxplots das variáveis contínuas (outliers)
# ============================================================


png("figuras/06_boxplots_continuas.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow=c(2,3), mar=c(5,5,4,2), cex.main=1.2, cex.lab=1.1, cex.axis=1.0)

boxplot(df$gestation, main="Gestação (dias)", col="lightblue", ylab="dias")
boxplot(df$age, main="Idade da mãe (anos)", col="lightblue", ylab="anos")
boxplot(df$ht, main="Altura da mãe (pol)", col="lightblue", ylab="polegadas")
boxplot(df$wt.1, main="Peso da mãe (lb)", col="lightblue", ylab="libras")
boxplot(df$parity, main="N° de gestações anteriores", col="lightblue", ylab="nº")
boxplot(df$dage, main="Idade do pai (anos)", col="lightblue", ylab="anos")

dev.off()

# ============================================================
# Mensagem de conclusão
# ============================================================

cat("\n========================================\n")
cat("Todas as figuras foram salvas na pasta 'figuras/'\n")
cat("Arquivos gerados:\n")
cat("  - 01_hist_wt.png\n")
cat("  - 02_quantitativas.png\n")
cat("  - 03_boxplots_qualitativas.png\n")
cat("  - 04_dispersao.png\n")
cat("  - 05_correlograma.png\n")
cat("  - 06_boxplots_continuas.png\n")
cat("========================================\n")


