



rm(list=ls())

pacman::p_load(readr, tidyverse, corrplot, dplyr)

setwd("C:/Users/elisv/Downloads/regressao_p/trabalho/nat")   ## diretorio do trab
dados_orig <- read.table("dados.txt", header = T, check.names = T) 
df <- readRDS('dados_transf.rds')

names(df)

##tabela de frequência

# variáveis que algumas tem poucos níves
variaveis <- names(df)
variaveis <- c('parity', 'race', 'ed', 'drace', 'ded', 'marital', 'inc', 'time', 'number')

par(mfrow=c(2,2))
for(i in 1:length(variaveis)){
  variavel <- variaveis[i]
  cat("\n\nVariável:", variavel, "\n")
  print(table(df[[variavel]]))
}


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



# Preparar dados
df_interaction <- df %>% 
  filter(!is.na(wt),
         !is.na(smoke), 
         !is.na(race_group),
         !is.na(time_group),
         !is.na(number_group))




# Criar pasta para figuras (se não existir)
if(!dir.exists("figuras/interacao")) {
  dir.create("figuras/interacao", recursive = TRUE)
}

# Gráfico 1
g1 <- ggplot(df_interaction,
             aes(x = factor(smoke), y = wt, fill = factor(smoke))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    x = "Fuma durante a gravidez?",
    y = "Peso ao nascer (onças)",
    fill = "Fuma?",
    title = "Distribuição do peso por hábito de fumar"
  ) +
  theme_minimal(base_size = 20)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Salvar
ggsave(
  "figuras/interacao/boxplot_fumo.png",
  plot = g1,
  width = 10,
  height = 6,
  dpi = 300
)

# Gráfico 2
g2 <- ggplot(df_interaction,
             aes(x = factor(time_group), y = wt,
                 fill = factor(time_group))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    x = "Tempo de fumo",
    y = "Peso ao nascer (onças)",
    fill = "Tempo",
    title = "Distribuição do peso por tempo de fumo"
  ) +
  theme_minimal(base_size = 20)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "figuras/interacao/boxplot_tempo_fumo.png",
  plot = g2,
  width = 10,
  height = 6,
  dpi = 300
)

# Gráfico 3
g3 <- ggplot(df_interaction,
             aes(x = factor(number_group), y = wt,
                 fill = factor(number_group))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    x = "Número de cigarros",
    y = "Peso ao nascer (onças)",
    fill = "Quantidade",
    title = "Distribuição do peso por número de cigarros"
  ) +
  theme_minimal(base_size = 20)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "figuras/interacao/boxplot_numero_cigarros.png",
  plot = g3,
  width = 10,
  height = 6,
  dpi = 300
)



# Gráfico 1: Médias condicionais
df_summary <- df_interaction %>%
  group_by(smoke, race_group) %>%
  summarise(mean_wt = mean(wt, na.rm = TRUE),
            se_wt = sd(wt, na.rm = TRUE)/sqrt(n()),
            n = n())

ggplot(df_summary, aes(x = factor(smoke), y = mean_wt, 
                       color = race_group, group = race_group)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_wt - se_wt, ymax = mean_wt + se_wt), width = 0.2) +
  labs(x = "Fuma durante gravidez? (0=Não, 1=Sim)", 
       y = "Peso ao nascer (onças)",
       title = "Efeito do fumo no peso segundo raça da mãe",
       color = "Raça") +
  theme_minimal()
###############################################


########################################################################################################################

df_cig <- df %>%
  filter(!is.na(wt),
         !is.na(number_group),
         !is.na(race_group))

# Gráfico de médias
df_cig_summary <- df_cig %>%
  group_by(number_group, race_group) %>%
  summarise(mean_wt = mean(wt, na.rm = TRUE),
            se_wt = sd(wt, na.rm = TRUE)/sqrt(n()))

ggplot(df_cig_summary, aes(x = number_group, y = mean_wt, 
                           color = race_group, group = race_group)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_wt - se_wt, ymax = mean_wt + se_wt), width = 0.2) +
  labs(x = "Consumo de cigarros/dia", 
       y = "Peso médio (onças)",
       title = "Relação dose-resposta do cigarro por raça") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


