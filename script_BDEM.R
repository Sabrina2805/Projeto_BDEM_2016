# script roteiro do BDEM - no repositório Projeto_BDEM_2016
# Antes de começar a fazer qualquer coisa:
# a) Coloque todos os arquivos postados no Classroom (já descompactados) dentro do repositório local Projeto_BDEM_2016
# b) commit este roteiro com a mensagem "dados, arquivos de texto e script roteiro BDEM" e envie para o repositório Projeto_BDEM_2016
# c) salve o script com outro nome (script_BDEM.R) e commit com a mensagem "script BDEM" e envie para o repositório Projeto_BDEM_2016

# Ao inserir os comandos em cada Tarefa de cada Etapa, mantenha as linhas de comentários e orientações colocadas pela professora


##################################
# ETAPA 1: BANCO DE DADOS DO SIM
##################################
# Você deve criar e estar na branch SIM antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SIM_2016 com 1309774 linhas e 87 colunas com o nome de dados_sim
# Verificar se a leitura foi feita corretamente e a estrutura dos dados

library(readr)
library(dplyr)

dados_sim <- read.csv2("SIM_2016.csv", stringsAsFactors = FALSE)

dim(dados_sim)
str(dados_sim)
head(dados_sim)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# As colunas serão: 1, 3, 9, 10, 11, 14, 17, 35, 47
# Nomes das respectivas variáveis: CONTADOR, TIPOBITO, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, CAUSABAS

cols_indices <- c(1, 3, 9, 10, 11, 14, 17, 35, 47)
cols_nomes   <- c("CONTADOR", "TIPOBITO", "IDADE", "SEXO", "RACACOR", 
                  "ESC2010", "CODMUNRES", "TPMORTEOCO", "CAUSABAS")

dados_sim_1 <- dados_sim[, cols_indices]
colnames(dados_sim_1) <- cols_nomes

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11:8344      12:3763     13:16799    14:2157      15:38557     16:2995     17:7490
# 21:34362     22:19187    23:54276    24:21922     25:28041     26:66928    27:20769    28:13516     29:88094
# 31:135257    32:22868    33:141089   35:296359
# 41:74740     42:40270    43:87583
# 50:16749     51:17535    52:38074    53:12050 

uf_codigo <- "42"

dados_sim_2 <- dados_sim_1 %>% 
  filter(substr(as.character(CODMUNRES), 1, 2) == uf_codigo)

nrow(dados_sim_2)

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIM - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis:
# TIPOBITO, SEXO, RACACOR, ESC2010, TPMORTEOCO, CAUSABAS
# Avalie também os valores das variável IDADE (não estranhe mas idade é composta de um dígito inicial que indica a unidade de medida)
# Unidades de medida a serem consideradas em IDADE: 0: minutos, 1: horas, 2: dias, 3: meses, 4: anos, 5: idade maior que 100 anos
# Atenção: a unidade de medida de IDADE no DICIONÀRIO do SIM está errada
# O propósito das avaliações acima é verificar se as categorias estão de acordo com o dicionário do SIM ou se aparecem categorias estranhas

table(dados_sim_2$TIPOBITO, useNA = "ifany")
table(dados_sim_2$SEXO, useNA = "ifany")
table(dados_sim_2$RACACOR, useNA = "ifany")
table(dados_sim_2$ESC2010, useNA = "ifany")
table(dados_sim_2$TPMORTEOCO, useNA = "ifany")
head(table(dados_sim_2$CAUSABAS), 20)

summary(as.numeric(dados_sim_2$IDADE))

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIM - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA

dados_sim_2$SEXO[dados_sim_2$SEXO == 0 | dados_sim_2$SEXO == 9] <- NA
dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] <- NA
dados_sim_2$ESC2010[dados_sim_2$ESC2010 == 9] <- NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] <- NA

dados_sim_2$IDADE[dados_sim_2$IDADE %in% c("999", "9999", "9")] <- NA

# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SIM - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a PRIMEIRA LETRA da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

dados_sim_2$TIPOBITO <- factor(
  dados_sim_2$TIPOBITO, 
  levels = c(1, 2), 
  labels = c("Fetal", "Não fetal")
)

# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SIM - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Criar um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 - SIM.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada

converte_idade_dias <- function(idade) {
  idade_num <- as.numeric(as.character(idade))
  prefixo <- floor(idade_num / 100)
  valor <- idade_num %% 100
  case_when(
    prefixo == 1 ~ 0,
    prefixo == 2 ~ valor,
    prefixo == 3 ~ valor * 30.4375,
    prefixo == 4 ~ valor * 365.25,
    TRUE ~ NA_real_
  )
}

converte_idade_anos <- function(idade) {
  idade_num <- as.numeric(as.character(idade))
  prefixo <- floor(idade_num / 100)
  valor <- idade_num %% 100
  case_when(
    prefixo == 4 ~ valor,
    prefixo %in% 1:3 ~ 0,
    TRUE ~ NA_real_
  )
}

dados_sim_prep <- dados_sim_2 %>%
  mutate(
    idade_dias = converte_idade_dias(IDADE),
    idade_anos = converte_idade_anos(IDADE),
    capitulo_cid = substr(CAUSABAS, 1, 1)
  )

SIM_SC <- dados_sim_prep %>%
  group_by(CODMUNRES) %>%
  summarise(
    ANO = 2016,
    NIVEL = "MUNICIPIO",
    CODMUNRES = unique(CODMUNRES),
    TO = n(),
    TORC = sum(complete.cases(dados_sim[dados_sim$CODMUNRES == unique(CODMUNRES), ]), na.rm = TRUE),
    TORCR = sum(!is.na(TIPOBITO) & !is.na(IDADE) & !is.na(SEXO) & !is.na(RACACOR) & 
                  !is.na(ESC2010) & !is.na(CODMUNRES) & !is.na(TPMORTEOCO) & !is.na(CAUSABAS)),
    TO_NN = sum(capitulo_cid %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_N = sum(!capitulo_cid %in% c("V", "W", "X", "Y") & !is.na(CAUSABAS), na.rm = TRUE),
    TO_CBI = sum(capitulo_cid %in% c("A", "B"), na.rm = TRUE),
    TO_CB_N = sum(capitulo_cid %in% c("C", "D"), na.rm = TRUE),
    TO_CB_C = sum(capitulo_cid == "I", na.rm = TRUE),
    TO_CB_R = sum(capitulo_cid == "J", na.rm = TRUE),
    TO_CB_O = sum(!capitulo_cid %in% c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y") & !is.na(CAUSABAS), na.rm = TRUE),
    TO_M = sum(SEXO == 1, na.rm = TRUE),
    TO_F = sum(SEXO == 2, na.rm = TRUE),
    TO_F_IF = sum(SEXO == 2 & idade_anos >= 15 & idade_anos <= 49, na.rm = TRUE),
    TO_FT = sum(TIPOBITO == "Fetal", na.rm = TRUE),
    TO_NT = sum(idade_dias >= 0 & idade_dias <= 27, na.rm = TRUE),
    TO_NT_P = sum(idade_dias >= 0 & idade_dias <= 6, na.rm = TRUE),
    TO_NT_T = sum(idade_dias >= 7 & idade_dias <= 27, na.rm = TRUE),
    TO_PNT = sum(idade_dias >= 28 & idade_dias <= 364, na.rm = TRUE),
    TONT_B = sum(idade_dias >= 0 & idade_dias <= 27 & RACACOR == 1, na.rm = TRUE),
    TONT_PT = sum(idade_dias >= 0 & idade_dias <= 27 & RACACOR == 2, na.rm = TRUE),
    TONT_A = sum(idade_dias >= 0 & idade_dias <= 27 & RACACOR == 3, na.rm = TRUE),
    TONT_PD = sum(idade_dias >= 0 & idade_dias <= 27 & RACACOR == 4, na.rm = TRUE),
    TONT_I = sum(idade_dias >= 0 & idade_dias <= 27 & RACACOR == 5, na.rm = TRUE),
    TO_MT = sum(TPMORTEOCO %in% c(1, 2, 3, 4, 5), na.rm = TRUE),
    TO_MT_DG = sum(TPMORTEOCO == 1, na.rm = TRUE),
    TO_MT_PT = sum(TPMORTEOCO == 2, na.rm = TRUE),
    TO_MT_AB = sum(TPMORTEOCO == 3, na.rm = TRUE),
    TO_MT_42 = sum(TPMORTEOCO == 4, na.rm = TRUE),
    TO_MT_43 = sum(TPMORTEOCO == 5, na.rm = TRUE),
    TO_MT_P = sum(TPMORTEOCO %in% c(1, 2, 3, 4), na.rm = TRUE),
    TO_MT_P_I = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & idade_anos >= 15 & idade_anos <= 49, na.rm = TRUE),
    TO_MT_P_ES = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 0, na.rm = TRUE),
    TO_MT_P_EFI = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 1, na.rm = TRUE),
    TO_MT_P_EFII = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 2, na.rm = TRUE),
    TO_MT_P_EM = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 3, na.rm = TRUE),
    TO_MT_P_ESI = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 4, na.rm = TRUE),
    TO_MT_P_ESC = sum(TPMORTEOCO %in% c(1, 2, 3, 4) & ESC2010 == 5, na.rm = TRUE),
    .groups = "drop"
  )

ordem_var <- c(
  "ANO", "NIVEL", "CODMUNRES", "TO", "TORC", "TORCR", "TO_NN", "TO_N", 
  "TO_CBI", "TO_CB_N", "TO_CB_C", "TO_CB_R", "TO_CB_O", "TO_M", "TO_F", 
  "TO_F_IF", "TO_FT", "TO_NT", "TO_NT_P", "TO_NT_T", "TO_PNT", "TONT_B", 
  "TONT_PT", "TONT_A", "TONT_PD", "TONT_I", "TO_MT", "TO_MT_DG", "TO_MT_PT", 
  "TO_MT_AB", "TO_MT_42", "TO_MT_43", "TO_MT_P", "TO_MT_P_I", "TO_MT_P_ES", 
  "TO_MT_P_EFI", "TO_MT_P_EFII", "TO_MT_P_EM", "TO_MT_P_ESI", "TO_MT_P_ESC"
)

SIM_SC <- SIM_SC[, ordem_var]

# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SIM - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Exportar o banco de dados com o nome SIM_UF.csv (Exemplo: SIM_RJ.csv)

write.csv2(SIM_SC, file = "SIM_SC.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Ao terminar a Tarefa 8 fazer um commit com o comentário "dados SIM_UF 2016 e script - SIM - tarefas 1 a 8"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 2: BANCO DE DADOS DO SINASC
####################################
# Você deve criar e estar na branch SINASC antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SINASC_2016 com 2857800 linhas e 61 colunas com o nome de dados_sinasc
# Verificar se a leitura foi feita corretamente e a estrutura dos dados
# Por uma questão de padronização coloque todos os nomes das variáveis em letra maiúscula,
# usando o comando names(dados_sinasc) = toupper(names(dados_sinasc))

dados_sinasc <- read.csv2("SINASC_2016.csv")
names(dados_sinasc) <- toupper(names(dados_sinasc))
str(dados_sinasc)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINASC - tarefa 1" e envie para o repositório Projeto_BDEM_2016

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# As colunas serão 3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23, 34, 37, 43, 47, 58, 59, 60, 61
# Nomes das respectivas variáveis: CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO, 
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK, CONTADOR

cols_selecionadas <- c(3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23, 34, 37, 43, 47, 58, 59, 60, 61)
dados_sinasc_1 <- dados_sinasc[, cols_selecionadas]

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 26602     12: 15773     13: 76703     14: 11376     15: 137681    16: 15521      17: 23870
# 21: 110493    22: 46986     23: 126246    24: 45366     25: 56083     26: 130733     27: 48164     28: 32218     29: 199830
# 31: 253520    32: 53413     33: 219129    35: 601437     
# 41: 155066    42: 95313     43: 141411
# 50: 42432     51: 53531     52: 95563     53: 43340 

dados_sinasc_2 <- dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "42", ]

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
# Avalie também os valores das variáveis quantitativas de IDADEMAE, SEMAGESTAC, APGAR5 e PESO

vars_qualitativas <- c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", 
                       "SEXO", "RACACOR", "IDANOMAL", "ESCMAE2010", "RACACORMAE", 
                       "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK")

lapply(dados_sinasc_2[, vars_qualitativas], function(x) table(x, useNA = "always"))

lapply(dados_sinasc_2[, vars_qualitativas], function(x) prop.table(table(x, useNA = "always")) * 100)

vars_quantitativas <- c("IDADEMAE", "SEMAGESTAC", "APGAR5", "PESO")

summary(dados_sinasc_2[, vars_quantitativas])

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SINASC para identificar qual o código das categorias de cada variável
# KOTELCHUCK = 9 significa "Não informado"   TPROBSON = 11 significa "Não classificado por falta de informação"
# Em variáveis quantitativas como IDADEMAE verificar se existem valores como 9999 para NA

vars_com_9 <- c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", 
                "RACACOR", "IDANOMAL", "ESCMAE2010", "RACACORMAE", 
                "TPAPRESENT", "KOTELCHUCK")

for (var in vars_com_9) {
  dados_sinasc_2[[var]][dados_sinasc_2[[var]] == 9] <- NA
}

dados_sinasc_2$SEXO[dados_sinasc_2$SEXO %in% c(0, 9)] <- NA

dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] <- NA

dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] <- NA
dados_sinasc_2$SEMAGESTAC[dados_sinasc_2$SEMAGESTAC == 99] <- NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] <- NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] <- NA

summary(dados_sinasc_2[, c("LOCNASC", "ESTCIVMAE", "SEXO", "TPROBSON", "KOTELCHUCK", "IDADEMAE", "PESO")])

# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

# LOCNASC
dados_sinasc_2$LOCNASC <- factor(dados_sinasc_2$LOCNASC, levels = c(1, 2, 3, 4, 5),
                                 labels = c("Hospital", "Outro estabelecimento de saúde", "Domicílio", "Outro", "Aldeia indígena"))

# ESTCIVMAE
dados_sinasc_2$ESTCIVMAE <- factor(dados_sinasc_2$ESTCIVMAE, levels = c(1, 2, 3, 4, 5),
                                   labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))

# GESTACAO
dados_sinasc_2$GESTACAO <- factor(dados_sinasc_2$GESTACAO, levels = c(1, 2, 3, 4, 5, 6),
                                  labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))

# GRAVIDEZ
dados_sinasc_2$GRAVIDEZ <- factor(dados_sinasc_2$GRAVIDEZ, levels = c(1, 2, 3),
                                  labels = c("Única", "Dupla", "Tripla e mais"))

# PARTO
dados_sinasc_2$PARTO <- factor(dados_sinasc_2$PARTO, levels = c(1, 2),
                               labels = c("Vaginal", "Cesáreo"))

# SEXO
dados_sinasc_2$SEXO <- factor(dados_sinasc_2$SEXO, levels = c(1, 2),
                              labels = c("Masculino", "Feminino"))

# RACACOR
dados_sinasc_2$RACACOR <- factor(dados_sinasc_2$RACACOR, levels = c(1, 2, 3, 4, 5),
                                 labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

# IDANOMAL
dados_sinasc_2$IDANOMAL <- factor(dados_sinasc_2$IDANOMAL, levels = c(1, 2),
                                  labels = c("Sim", "Não"))

# ESCMAE2010
dados_sinasc_2$ESCMAE2010 <- factor(dados_sinasc_2$ESCMAE2010, levels = c(0, 1, 2, 3, 4, 5),
                                    labels = c("Sem escolaridade", "Fundamental I", "Fundamental II", "Médio", "Superior incompleto", "Superior completo"))

# RACACORMAE
dados_sinasc_2$RACACORMAE <- factor(dados_sinasc_2$RACACORMAE, levels = c(1, 2, 3, 4, 5),
                                    labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

# TPAPRESENT
dados_sinasc_2$TPAPRESENT <- factor(dados_sinasc_2$TPAPRESENT, levels = c(1, 2, 3),
                                    labels = c("Cefálica", "Pélvica ou podálica", "Transversa"))

# TPROBSON
dados_sinasc_2$TPROBSON <- factor(dados_sinasc_2$TPROBSON, levels = 1:10,
                                  labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5",
                                             "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))

# PARIDADE (0: Nulípara, 1: Multípara)
dados_sinasc_2$PARIDADE <- factor(dados_sinasc_2$PARIDADE, levels = c(0, 1),
                                  labels = c("Nulípara", "Multípara"))

# KOTELCHUCK
dados_sinasc_2$KOTELCHUCK <- factor(dados_sinasc_2$KOTELCHUCK, levels = c(1, 2, 3, 4, 5),
                                    labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))

str(dados_sinasc_2[, c("LOCNASC", "ESTCIVMAE", "SEXO", "RACACOR", "KOTELCHUCK")])

# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$PEREG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

dados_sinasc_2$F_PESO <- cut(
  dados_sinasc_2$PESO,
  breaks = c(-Inf, 2499, 3999, Inf),
  labels = c("Baixo peso", "Peso normal", "Macrossomia"),
  right = TRUE
)

dados_sinasc_2$F_IDADE <- cut(
  dados_sinasc_2$IDADEMAE,
  breaks = c(-Inf, 14, 19, 24, 29, 34, 39, 44, 49, Inf),
  labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"),
  right = TRUE
)

dados_sinasc_2$F_APGAR5 <- cut(
  dados_sinasc_2$APGAR5,
  breaks = c(-Inf, 6, Inf),
  labels = c("Baixo", "Normal"),
  right = TRUE
)

dados_sinasc_2$PEREG <- ifelse(
  dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES,
  "Não",
  "Sim"
)
dados_sinasc_2$PEREG <- factor(dados_sinasc_2$PEREG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV <- ifelse(
  dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"),
  "Sem companheiro",
  ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA)
)
dados_sinasc_2$ESTCIV <- factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro", "Com companheiro"))

str(dados_sinasc_2[, c("F_PESO", "F_IDADE", "F_APGAR5", "PEREG", "ESTCIV")])
summary(dados_sinasc_2[, c("F_PESO", "F_IDADE", "F_APGAR5", "PEREG", "ESTCIV")])

# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv

tabela_pig <- read.csv2("Tabela_PIG_Brasil.csv", stringsAsFactors = FALSE)
names(tabela_pig) <- toupper(names(tabela_pig))

# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional

dados_sinasc_2 <- merge(
  dados_sinasc_2,
  tabela_pig[, c("SEMAGESTAC", "SEXO", "PESO_P10", "PESO_P90")],
  by = c("SEMAGESTAC", "SEXO"),
  all.x = TRUE
)

dados_sinasc_2 <- dados_sinasc_2[order(dados_sinasc_2$CONTADOR), ]

# Criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

dados_sinasc_2$F_PIG <- NA

cond_unica <- !is.na(dados_sinasc_2$GRAVIDEZ) & dados_sinasc_2$GRAVIDEZ == "Única"
cond_valida <- cond_unica & !is.na(dados_sinasc_2$PESO) & !is.na(dados_sinasc_2$PESO_P10) & !is.na(dados_sinasc_2$PESO_P90)

dados_sinasc_2$F_PIG[cond_valida & dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10] <- "PIG"
dados_sinasc_2$F_PIG[cond_valida & dados_sinasc_2$PESO >= dados_sinasc_2$PESO_P10 & dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90] <- "AIG"
dados_sinasc_2$F_PIG[cond_valida & dados_sinasc_2$PESO > dados_sinasc_2$PESO_P90] <- "GIG"

dados_sinasc_2$F_PIG <- factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))

table(dados_sinasc_2$F_PIG, useNA = "always")

# Ao terminar a Tarefa 8 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 8" e envie para o repositório Projeto_BDEM_2016


# Tarefa 9. Criar um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 9 - SINASC.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada

calcular_indicadores_sinasc <- function(df, df_original_61, ano = 2016, nivel = "UF", cod_mun = 42) {
  
  tn <- nrow(df)
  
  if (nivel == "UF") {
    tnrc <- sum(complete.cases(df_original_61[substr(as.character(df_original_61$CODMUNRES), 1, 2) == as.character(cod_mun), ]))
  } else {
    tnrc <- sum(complete.cases(df_original_61[df_original_61$CODMUNRES == cod_mun, ]))
  }
  
  tnrcr <- sum(complete.cases(df[, c("CODMUNNASC", "LOCNASC", "IDADEMAE", "ESTCIVMAE", "CODMUNRES", 
                                     "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO", "APGAR5", 
                                     "RACACOR", "PESO", "IDANOMAL", "ESCMAE2010", "RACACORMAE", 
                                     "SEMAGESTAC", "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK", "CONTADOR")]))
  
  tgi_15 <- sum(df$IDADEMAE < 15, na.rm = TRUE)   
  tgi_15_19 <- sum(df$IDADEMAE >= 15 & df$IDADEMAE <= 19, na.rm = TRUE)   
  tgi_20_24 <- sum(df$IDADEMAE >= 20 & df$IDADEMAE <= 24, na.rm = TRUE)   
  tgi_25_29 <- sum(df$IDADEMAE >= 25 & df$IDADEMAE <= 29, na.rm = TRUE)   
  tgi_30_34 <- sum(df$IDADEMAE >= 30 & df$IDADEMAE <= 34, na.rm = TRUE)   
  tgi_35_39 <- sum(df$IDADEMAE >= 35 & df$IDADEMAE <= 39, na.rm = TRUE)   
  tgi_40_44 <- sum(df$IDADEMAE >= 40 & df$IDADEMAE <= 44, na.rm = TRUE)   
  tgi_45_49 <- sum(df$IDADEMAE >= 45 & df$IDADEMAE <= 49, na.rm = TRUE)   
  tgi_50 <- sum(df$IDADEMAE >= 50, na.rm = TRUE)
  tgif <- sum(df$IDADEMAE >= 15 & df$IDADEMAE <= 49, na.rm = TRUE)
  
  im_p25 <- quantile(df$IDADEMAE, 0.25, na.rm = TRUE)
  im_p50 <- quantile(df$IDADEMAE, 0.50, na.rm = TRUE)
  im_p75 <- quantile(df$IDADEMAE, 0.75, na.rm = TRUE)
  im_md  <- mean(df$IDADEMAE, na.rm = TRUE)
  im_dp  <- sd(df$IDADEMAE, na.rm = TRUE)
  
  em_s   <- sum(df$ESCMAE2010 == "Sem escolaridade", na.rm = TRUE)
  em_fi  <- sum(df$ESCMAE2010 == "Fundamental I", na.rm = TRUE)
  em_fii <- sum(df$ESCMAE2010 == "Fundamental II", na.rm = TRUE)
  em_m   <- sum(df$ESCMAE2010 == "Médio", na.rm = TRUE)
  em_si  <- sum(df$ESCMAE2010 == "Superior incompleto", na.rm = TRUE)
  em_sc  <- sum(df$ESCMAE2010 == "Superior completo", na.rm = TRUE)
  
  tgrc_b  <- sum(df$RACACORMAE == "Branca", na.rm = TRUE)
  tgrc_pt <- sum(df$RACACORMAE == "Preta", na.rm = TRUE)
  tgrc_a  <- sum(df$RACACORMAE == "Amarela", na.rm = TRUE)
  tgrc_pd <- sum(df$RACACORMAE == "Parda", na.rm = TRUE)
  tgrc_i  <- sum(df$RACACORMAE == "Indígena", na.rm = TRUE)
  
  tgsc <- sum(df$ESTCIV == "Sem companheiro", na.rm = TRUE)
  tgcc <- sum(df$ESTCIV == "Com companheiro", na.rm = TRUE)
  
  tgpri  <- sum(df$PARIDADE == "Nulípara", na.rm = TRUE)
  tgnpri <- sum(df$PARIDADE == "Multípara", na.rm = TRUE)
  
  tgu <- sum(df$GRAVIDEZ == "Única", na.rm = TRUE)
  tgg <- sum(df$GRAVIDEZ %in% c("Dupla", "Tripla e mais"), na.rm = TRUE)
  
  tgd_22    <- sum(df$SEMAGESTAC < 22, na.rm = TRUE)
  tgd_22_27 <- sum(df$SEMAGESTAC >= 22 & df$SEMAGESTAC <= 27, na.rm = TRUE)
  tgd_28_31 <- sum(df$SEMAGESTAC >= 28 & df$SEMAGESTAC <= 31, na.rm = TRUE)
  tgd_32_36 <- sum(df$SEMAGESTAC >= 32 & df$SEMAGESTAC <= 36, na.rm = TRUE)
  tgd_37_41 <- sum(df$SEMAGESTAC >= 37 & df$SEMAGESTAC <= 41, na.rm = TRUE)
  tgd_42    <- sum(df$SEMAGESTAC >= 42, na.rm = TRUE)
  
  tgd_prt <- sum(df$SEMAGESTAC < 37, na.rm = TRUE)
  tgd_at  <- sum(df$SEMAGESTAC >= 37 & df$SEMAGESTAC <= 41, na.rm = TRUE)
  tgd_pst <- sum(df$SEMAGESTAC >= 42, na.rm = TRUE)
  
  dg_p25 <- quantile(df$SEMAGESTAC, 0.25, na.rm = TRUE)
  dg_p50 <- quantile(df$SEMAGESTAC, 0.50, na.rm = TRUE)
  dg_p75 <- quantile(df$SEMAGESTAC, 0.75, na.rm = TRUE)
  dg_md  <- mean(df$SEMAGESTAC, na.rm = TRUE)
  dg_dp  <- sd(df$SEMAGESTAC, na.rm = TRUE)
  
  tkc_nr  <- sum(df$KOTELCHUCK == "Não realizou pré-natal", na.rm = TRUE)
  tkc_id  <- sum(df$KOTELCHUCK == "Inadequado", na.rm = TRUE)
  tkc_it  <- sum(df$KOTELCHUCK == "Intermediário", na.rm = TRUE)
  tkc_ad  <- sum(df$KOTELCHUCK == "Adequado", na.rm = TRUE)
  tkc_mad <- sum(df$KOTELCHUCK == "Mais que adequado", na.rm = TRUE)
  
  tgprg_s <- sum(df$PEREG == "Sim", na.rm = TRUE)
  tgprg_n <- sum(df$PEREG == "Não", na.rm = TRUE)
  
  tpv <- sum(df$PARTO == "Vaginal", na.rm = TRUE)
  tpc <- sum(df$PARTO == "Cesáreo", na.rm = TRUE)
  
  trap_c <- sum(df$TPAPRESENT == "Cefálica", na.rm = TRUE)
  trap_p <- sum(df$TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE)
  trap_t <- sum(df$TPAPRESENT == "Transversa", na.rm = TRUE)
  
  tgrob_1  <- sum(df$TPROBSON == "Grupo 1", na.rm = TRUE)
  tgrob_2  <- sum(df$TPROBSON == "Grupo 2", na.rm = TRUE)
  tgrob_3  <- sum(df$TPROBSON == "Grupo 3", na.rm = TRUE)
  tgrob_4  <- sum(df$TPROBSON == "Grupo 4", na.rm = TRUE)
  tgrob_5  <- sum(df$TPROBSON == "Grupo 5", na.rm = TRUE)
  tgrob_6  <- sum(df$TPROBSON == "Grupo 6", na.rm = TRUE)
  tgrob_7  <- sum(df$TPROBSON == "Grupo 7", na.rm = TRUE)
  tgrob_8  <- sum(df$TPROBSON == "Grupo 8", na.rm = TRUE)
  tgrob_9  <- sum(df$TPROBSON == "Grupo 9", na.rm = TRUE)
  tgrob_10 <- sum(df$TPROBSON == "Grupo 10", na.rm = TRUE)
  
  tnloc_h  <- sum(df$LOCNASC == "Hospital", na.rm = TRUE)
  tnloc_es <- sum(df$LOCNASC == "Outro estabelecimento de saúde", na.rm = TRUE)
  tnloc_d  <- sum(df$LOCNASC == "Domicílio", na.rm = TRUE)
  tnloc_o  <- sum(df$LOCNASC == "Outro", na.rm = TRUE)
  tnloc_ai <- sum(df$LOCNASC == "Aldeia indígena", na.rm = TRUE)
  
  trs_m <- sum(df$SEXO == "Masculino", na.rm = TRUE)
  trs_f <- sum(df$SEXO == "Feminino", na.rm = TRUE)
  
  trrc_b  <- sum(df$RACACOR == "Branca", na.rm = TRUE)
  trrc_pt <- sum(df$RACACOR == "Preta", na.rm = TRUE)
  trrc_a  <- sum(df$RACACOR == "Amarela", na.rm = TRUE)
  trrc_pd <- sum(df$RACACOR == "Parda", na.rm = TRUE)
  trrc_i  <- sum(df$RACACOR == "Indígena", na.rm = TRUE)
  
  trp_bp <- sum(df$F_PESO == "Baixo peso", na.rm = TRUE)
  trp_n  <- sum(df$F_PESO == "Peso normal", na.rm = TRUE)
  trp_m  <- sum(df$F_PESO == "Macrossomia", na.rm = TRUE)
  
  peso_p25 <- quantile(df$PESO, 0.25, na.rm = TRUE)
  peso_p50 <- quantile(df$PESO, 0.50, na.rm = TRUE)
  peso_p75 <- quantile(df$PESO, 0.75, na.rm = TRUE)
  peso_md  <- mean(df$PESO, na.rm = TRUE)
  peso_dp  <- sd(df$PESO, na.rm = TRUE)
  
  trpig_p <- sum(df$F_PIG == "PIG", na.rm = TRUE)
  trpig_a <- sum(df$F_PIG == "AIG", na.rm = TRUE)
  trpig_g <- sum(df$F_PIG == "GIG", na.rm = TRUE)
  
  trapg5_b <- sum(df$F_APGAR5 == "Baixo", na.rm = TRUE)
  trapg5_n <- sum(df$F_APGAR5 == "Normal", na.rm = TRUE)
  
  apg5_md <- mean(df$APGAR5, na.rm = TRUE)
  apg5_dp <- sd(df$APGAR5, na.rm = TRUE)
  
  trac  <- sum(df$IDANOMAL == "Sim", na.rm = TRUE)
  trsac <- sum(df$IDANOMAL == "Não", na.rm = TRUE)
  
  data.frame(
    ANO = ano, NIVEL = nivel, CODMUNRES = cod_mun, TN = tn, TNRC = tnrc, TNRCR = tnrcr,
    TGI_15 = tgi_15, TGI_15_19 = tgi_15_19, `TGI 20 24` = tgi_20_24, `TGI 25 29` = tgi_25_29,
    TGI_30_34 = tgi_30_34, `TGI 35 39` = tgi_35_39, `TGI 40 44` = tgi_40_44, TGI_45_49 = tgi_45_49, `TGI 50` = tgi_50, TGIF = tgif,
    IM_P25 = im_p25, `IM P50` = im_p50, IM_P75 = im_p75, IM_MD = im_md, `IM DP` = im_dp,
    `EM S` = em_s, EM_FI = em_fi, EM_FII = em_fii, EM_M = em_m, `EM SI` = em_si, EM_SC = em_sc,
    TGRC_B = tgrc_b, TGRC_PT = tgrc_pt, TGRC_A = tgrc_a, TGRC_PD = tgrc_pd, TGRC_I = tgrc_i,
    TGSC = tgsc, TGCC = tgcc, TGPRI = tgpri, TGNPRI = tgnpri,
    TGU = tgu, TGG = tgg, TGD_22 = tgd_22, `TGD 22 27` = tgd_22_27, TGD_28_31 = tgd_28_31, `TGD 32 36` = tgd_32_36,
    TGD_37_41 = tgd_37_41, TGD_42 = tgd_42, `TGD PRT` = tgd_prt, TGD_AT = tgd_at, `TGD PST` = tgd_pst,
    `DG P25` = dg_p25, `DG P50` = dg_p50, `DG P75` = dg_p75, DG_MD = dg_md, `DG DP` = dg_dp,
    TKC_NR = tkc_nr, TKC_ID = tkc_id, TKC_IT = tkc_it, TKC_AD = tkc_ad, `TKC MAD` = tkc_mad,
    TGPRG_S = tgprg_s, TGPRG_N = tgprg_n, TPV = tpv, TPC = tpc,
    `TRAP C` = trap_c, `TRAP P` = trap_p, TRAP_T = trap_t,
    TGROB_1 = tgrob_1, TGROB_2 = tgrob_2, `TGROB 3` = tgrob_3, `TGROB 4` = tgrob_4, `TGROB 5` = tgrob_5,
    `TGROB 6` = tgrob_6, `TGROB 7` = tgrob_7, `TGROB 8` = tgrob_8, `TGROB 9` = tgrob_9, `TGROB 10` = tgrob_10,
    TNLOC_H = tnloc_h, TNLOC_ES = tnloc_es, TNLOC_D = tnloc_d, TNLOC_O = tnloc_o, TNLOC_AI = tnloc_ai,
    `TRS M` = trs_m, TRS_F = trs_f, TRRC_B = trrc_b, TRRC_PT = trrc_pt, `TRRC A` = trrc_a, `TRRC PD` = trrc_pd, TRRC_I = trrc_i,
    `TRP BP` = trp_bp, TRP_N = trp_n, TRP_M = trp_m,
    PESO_P25 = peso_p25, `PESO P50` = peso_p50, PESO_P75 = peso_p75, `PESO MD` = peso_md, `PESO DP` = peso_dp,
    `TRPIG P` = trpig_p, TRPIG_A = trpig_a, `TRPIG G` = trpig_g,
    `TRAPG5 B` = trapg5_b, `TRAPG5 N` = trapg5_n, APG5_MD = apg5_md, APG5_DP = apg5_dp,
    TRAC = trac, TRSAC = trsac,
    check.names = FALSE
  )
}

linha_uf_sc <- calcular_indicadores_sinasc(dados_sinasc_2, dados_sinasc, ano = 2016, nivel = "UF", cod_mun = 42)

municipios_sc <- unique(dados_sinasc_2$CODMUNRES)
municipios_sc <- municipios_sc[!is.na(municipios_sc)]

linhas_municipios <- do.call(rbind, lapply(municipios_sc, function(m) {
  sub_df <- dados_sinasc_2[dados_sinasc_2$CODMUNRES == m, ]
  calcular_indicadores_sinasc(sub_df, dados_sinasc, ano = 2016, nivel = "MUNICIPIO", cod_mun = m)
}))

SINASC_SC <- rbind(linha_uf_sc, linhas_municipios);

rownames(SINASC_SC) <- NULL;

# Ao terminar a Tarefa 9 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 9" e envie para o repositório Projeto_BDEM_2016


# Tarefa 10. Exportar o banco de dados com o nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv)
# Ao terminar a Tarefa 10 commit com o comentário "dados SINASC_UF 2016 e script - SIM - tarefas 1 a 10"  e envie para o repositório Projeto_BDEM_2016

write.csv(SINASC_SC, file = "SINASC_SC.csv", row.names = FALSE)

####################################
# ETAPA 3: BANCOS DE DADOS DO SIDRA
####################################
# Você deve criar e estar na branch SIDRA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# dados_sidra_1 para população residente estimada - UF e municípios - 2016 - SIDRA - tabela_6579.csv
# dados_sidra_2 para população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv
# dados_sidra_3 para população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv
# dados_sidra_4 para população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv
# Atenção que agora os arquivos têm nomes e códigos (com 7 dígitos) dos municípios (e alguns UF)

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados

library(readr)

dados_sidra_1 <- read.csv2("população residente estimada - UF e municípios - 2016 - SIDRA - tabela_6579.csv", fileEncoding = "latin1", stringsAsFactors = FALSE)
dados_sidra_2 <- read_delim("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv", delim = ";", locale = locale(encoding = "UTF-8"))
dados_sidra_3 <- read_delim("população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv", delim = ";", locale = locale(encoding = "UTF-8"))
dados_sidra_4 <- read_delim("população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv", delim = ";", locale = locale(encoding = "UTF-8"))

dados_sidra_2 <- as.data.frame(dados_sidra_2)
dados_sidra_3 <- as.data.frame(dados_sidra_3)
dados_sidra_4 <- as.data.frame(dados_sidra_4)

str(dados_sidra_1)
str(dados_sidra_2)
str(dados_sidra_3)
str(dados_sidra_4)

dim(dados_sidra_1)
dim(dados_sidra_2)
dim(dados_sidra_3)
dim(dados_sidra_4)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIDRA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Criar uma nova variável de nome CODUF com os códigos da UF nos bancos dados_sidra_1, dados_sidra_2, dados_sidra_4

dados_sidra_1$CODUF <- substr(as.character(dados_sidra_1[, 1]), 1, 2)
dados_sidra_2$CODUF <- substr(as.character(dados_sidra_2[, 1]), 1, 2)
dados_sidra_3$CODUF <- substr(as.character(dados_sidra_3[, 1]), 1, 2)
dados_sidra_4$CODUF <- substr(as.character(dados_sidra_4[, 1]), 1, 2)

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Selecionar em dados_sidra_ 1 a dados_sidra_4 a UF de responsabilidade do aluno 
# e chamar os bancos de dados, respectivamente por sidra_1, sidra_2, sidra_3 e sidra_4

uf_aluno <- "42"

sidra_1 <- subset(dados_sidra_1, CODUF == uf_aluno)
sidra_2 <- subset(dados_sidra_2, CODUF == uf_aluno)
sidra_3 <- subset(dados_sidra_3, CODUF == uf_aluno)
sidra_4 <- subset(dados_sidra_4, CODUF == uf_aluno)

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4: Criar um banco de dados, de nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 4 - SIDRA.pdf”

library(dplyr)

sidra_faixas <- sidra_3 %>%
  filter(nchar(as.character(CODMUNRES)) > 2) %>%
  group_by(CODMUNRES) %>%
  summarise(
    POPRC_15      = sum(POP[F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")], na.rm = TRUE),
    POPRC_15_49   = sum(POP[F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", 
                                           "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")], na.rm = TRUE),
    POPRC_50      = sum(POP[!F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", 
                                            "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", 
                                            "40 a 44 anos", "45 a 49 anos")], na.rm = TRUE),
    
    POPRC_F_15    = sum(POPF[F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")], na.rm = TRUE),
    POPRC_F_15_49 = sum(POPF[F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", 
                                            "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")], na.rm = TRUE),
    POPRC_F_50    = sum(POPF[!F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", 
                                             "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", 
                                             "40 a 44 anos", "45 a 49 anos")], na.rm = TRUE)
  )


SIDRA_SC <- sidra_1 %>%
  filter(nchar(as.character(CODMUNRES)) > 2) %>%
  select(CODMUNRES, POPRE_T) %>%
  inner_join(
    sidra_2 %>% 
      filter(nchar(as.character(CODMUNRES)) > 2) %>%
      select(CODMUNRES, POPRC_T, POPRC_M, POPRC_F),
    by = "CODMUNRES"
  ) %>%
  inner_join(sidra_faixas, by = "CODMUNRES") %>%
  mutate(
    ANO = 2016,
    NIVEL = "MUNICIPIO"
  ) %>%
  select(
    ANO, NIVEL, CODMUNRES, POPRE_T, POPRC_T, POPRC_M, POPRC_F,
    POPRC_15, POPRC_15_49, POPRC_50, POPRC_F_15, POPRC_F_15_49, POPRC_F_50
  )

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5:Exportar o banco de dados com o nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv)

write.csv2(SIDRA_SC, file = "SIDRA_SC.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Ao terminar a Tarefa 5 commit com o comentário "dados SIDRA_UF 2016 e script - SIDRA - tarefas 1 a 5"  e envie para o repositório Projeto_BDEM_2016


####################################
# ETAPA 4: BANCOS DE DADOS DO ATLAS
####################################
# Você deve criar e estar na branch ATLAS antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# codigos_IBGE_2010 para códigos dos municípios - 2010.csv
# dados_atlas_1 para IDHM - 2010 (CENSO) e 2016 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# dados_atlas_2 para IDHM - 2010 - municípios - Atlas Brasil.csv
# Atenção que agora alguns arquivos só têm os nomes dos municípios e das UFs, mas não têm os códigos

library(tidyverse)
library(readr)
library(dplyr)

codigos_IBGE_2010 <- read_csv2("códigos dos municípios - 2010.csv")

dados_atlas_1 <- read_csv2("IDHM - 2010 (CENSO) e 2016 (PNAD) - total e por sexo - UF - Atlas Brasil.csv", 
                           locale = locale(encoding = "Latin1"))

dados_atlas_2 <- read_csv2("IDHM - 2010 - municípios - Atlas Brasil.csv")


# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados

str(codigos_IBGE_2010)
glimpse(codigos_IBGE_2010)
head(codigos_IBGE_2010)

str(dados_atlas_1)
glimpse(dados_atlas_1)
head(dados_atlas_1)

str(dados_atlas_2)
glimpse(dados_atlas_2)
head(dados_atlas_2)


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - ATLAS - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Manipular o banco de dados e criar o banco de dados ATLAS_UF

# Criar o banco UF_codigo tipo tabela de correspondência

UF_codigo <- tibble(
  CODUF = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  SIGLA = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  UF = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", 
         "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", 
         "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", 
         "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", 
         "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal")
)

# Retirar de dados_atlas_1 a linha do Brasil e adicionar (com merge by UF) as colunas de UF_codigo

dados_atlas_1_filt <- dados_atlas_1 %>% 
  filter(UF != "Brasil") %>% 
  left_join(UF_codigo, by = "UF")

# Criar o banco linha_estado somente com as linhas da UF e com as seguintes colunas:
# ANO=2016, NIVEL=UF, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M e IDHM_CA_F 

linha_estado <- dados_atlas_1_filt %>% 
  mutate(
    ANO = 2016,
    NIVEL = "UF",
    CODMUNRES = as.character(CODUF),
    IDHM_A    = IDHM_2010,
    IDHM_CA   = IDHM_2016,
    IDHM_CA_M = IDHM_2016_M,
    IDHM_CA_F = IDHM_2016_F
  ) %>% 
  select(ANO, NIVEL, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M, IDHM_CA_F)

# Selecionar de linha_estado a UF da responsabilidade do aluno por CODMUNRES

cod_uf_aluno <- "42"

linha_estado_SC <- linha_estado %>% 
  filter(CODMUNRES == cod_uf_aluno)

# Criar em dados_atlas_2 a coluna com UF
# Retirar (UF) da variável município

dados_atlas_2 <- dados_atlas_2 %>% 
  mutate(
    UF = str_extract(município, "(?<=\\().+?(?=\\))"),
    município = str_trim(str_remove(município, "\\s*\\(.*\\)"))
  )

# Acrescentar em codigos_IBGE_2010 a variável CODUF baseado nos dois primeiros dígitos de CODMUNRES

codigos_IBGE_2010 <- codigos_IBGE_2010 %>% 
  mutate(
    CODMUNRES = as.character(CODMUNRES),
    CODUF = as.numeric(substr(CODMUNRES, 1, 2))
  )

# Acrescentar a codigos_IBGE_2010 as variáveis de UF_codigo (merge by CODUF)

codigos_IBGE_2010 <- codigos_IBGE_2010 %>% 
  left_join(UF_codigo, by = "CODUF")

# Associar dados_atlas_2 a codigos_IBGE_2010 e nomear o novo arquivo por atlas_municipio
# Neste caso o merge será by.x = c("município","UF") e by.y = c("município","SIGLA")

atlas_municipio <- merge(
  x = dados_atlas_2, 
  y = codigos_IBGE_2010, 
  by.x = c("município", "UF"), 
  by.y = c("município", "SIGLA")
)

# Remover de atlas_municipio a coluna UF.y criada no merge

atlas_municipio <- atlas_municipio %>% 
  select(-matches("UF\\.y"))

# Selecionar somente a UF de responsabilidade do aluno através dos dois primeiros dógitos de CODMUNRES

atlas_municipio_SC <- atlas_municipio %>% 
  filter(substr(as.character(CODMUNRES), 1, 2) == "42")

# Criar banco ATLAS_MUNICIPIO com as linhas dos municípios e com as seguintes variáveis:
# ANO=2016, NIVEL=MUNICIPIO, CODMUNRES, IDHM_A=NA, IDHM_CA, IDHM_CA_M=NA, IDHM_CA_F=NA

ATLAS_MUNICIPIO <- atlas_municipio_SC %>% 
  mutate(
    ANO = 2016,
    NIVEL = "MUNICIPIO",
    CODMUNRES = as.character(CODMUNRES),
    IDHM_A    = NA_real_,
    IDHM_CA   = IDHM_2010,
    IDHM_CA_M = NA_real_,
    IDHM_CA_F = NA_real_
  ) %>% 
  select(ANO, NIVEL, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M, IDHM_CA_F)

# Criar banco final ATLAS_UF "juntando" os bancos linha_estado e ATLAS_MUNICIPIO

# Converter colunas de IDHM do linha_estado_SC para numéricas

linha_estado_SC <- linha_estado_SC %>% 
  mutate(
    IDHM_A    = as.numeric(IDHM_A),
    IDHM_CA   = as.numeric(IDHM_CA),
    IDHM_CA_M = as.numeric(IDHM_CA_M),
    IDHM_CA_F = as.numeric(IDHM_CA_F)
  )

ATLAS_MUNICIPIO <- ATLAS_MUNICIPIO %>% 
  mutate(IDHM_CA = as.numeric(gsub(",", ".", IDHM_CA)))


ATLAS_UF <- bind_rows(linha_estado_SC, ATLAS_MUNICIPIO)

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - ATLAS - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Exportar o banco de dados com o nome ATLAS_UF.csv (Exemplo: ATLAS_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados ATLAS_UF 2016 e script - ATLAS - tarefas 1 a 3"  e envie para o repositório Projeto_BDEM_2016

write_csv2(ATLAS_UF, "ATLAS_SC.csv")

####################################
# ETAPA 5: BANCOS DE DADOS DO SINISA
####################################
# Você deve criar e estar na branch SINISA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler o bancos de dados abaixo listado com os respectivo nome
# dados_sinisa para agua e esgoto - município - 2016.csv
# Atenção que o arquivo tem códigos e nomes de municípios e muitos NAs. 
# Repare que os valores estão com o milhar indicado por ponto, o que não deve acontecer para o R não entender como decimal

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados
# Remover a pontuação de milhar e converter para formato numérico

dados_sinisa <- read.csv2("agua e esgoto - município - 2016.csv", 
                           encoding = "latin1", 
                           check.names = FALSE, 
                           stringsAsFactors = FALSE)
dados_sinisa$POPR_RA <- as.numeric(gsub("\\.", "", dados_sinisa$POPR_RA))
dados_sinisa$POPR_RE <- as.numeric(gsub("\\.", "", dados_sinisa$POPR_RE))

str(dados_sinisa)
head(dados_sinisa)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINISA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sinisa apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinisa_1

codigo_uf <- "42" 

dados_sinisa_1 <- dados_sinisa[substr(as.character(dados_sinisa$CODMUNRES), 1, 2) == codigo_uf, ]


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Criar um banco de dados, de nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 3 - SINISA.pdf”

linha_uf <- data.frame(
  ANO = 2016,
  NIVEL = "UF",
  CODMUNRES = as.numeric(codigo_uf),
  POPR_RA = sum(dados_sinisa_1$POPR_RA, na.rm = TRUE),
  POPR_RE = sum(dados_sinisa_1$POPR_RE, na.rm = TRUE)
)

dados_mun <- data.frame(
  ANO = 2016,
  NIVEL = "MUNICIPIO",
  CODMUNRES = dados_sinisa_1$CODMUNRES,
  POPR_RA = dados_sinisa_1$POPR_RA,
  POPR_RE = dados_sinisa_1$POPR_RE
)

SINISA_UF <- rbind(linha_uf, dados_mun)

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Exportar o banco de dados com o nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv)

write.csv2(SINISA_UF, "SINISA_SC.csv", row.names = FALSE)

# Ao terminar a Tarefa 4 commit com o comentário "dados SINISA_UF 2016 e script - SINISA - tarefas 1 a 4"  e enviar para o repositório Projeto_BDEM_2016


################################
# ETAPA 6: CRIAÇÃO DE BDEM_UF
################################
# Você deve estar agora em main e antes de inserir qualquer comando desta ETAPA
# deverá fazer os merges de cada uma das 5 branches. A cada merge pode fazer o comentário "merge da branch TAL"
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Agregar os arquivos SIDRA_UF, ATLAS_UF, SINASC_UF, SIM_UF, SINISA_UF no banco BDEM_UF (Exemplo: BDEM_RJ)
# Leitura dos 5 bancos de dados expeortados das etapas anteriores

# Agregação dos bancos
# Lembre-se que SIDRA e ATLAS tem CODMUNRES com 7 dígitos e SINASC, SIM e SINISA com 6 dígitos
# Além disso dentro do merge all = TRUE garante a manutenção de qualquer município presente em um dos bancos envolvidos no merge

SIDRA_SC  <- read_csv2("SIDRA_SC.csv")
ATLAS_SC <- read_csv2("ATLAS_SC.csv")
SINASC_SC <- read.csv("SINASC_SC.csv")
SIM_SC    <- read_csv2("SIM_SC.csv")
SINISA_SC <- read_csv2("SINISA_SC.csv")

SIDRA_SC$CODMUNRES  <- as.numeric(substr(as.character(SIDRA_SC$CODMUNRES), 1, 6))
ATLAS_SC$CODMUNRES  <- as.numeric(substr(as.character(ATLAS_SC$CODMUNRES), 1, 6))

SINASC_SC$CODMUNRES <- as.numeric(as.character(SINASC_SC$CODMUNRES))
SIM_SC$CODMUNRES    <- as.numeric(as.character(SIM_SC$CODMUNRES))
SINISA_SC$CODMUNRES <- as.numeric(as.character(SINISA_SC$CODMUNRES))

BDEM_SC <- merge(SIDRA_SC, ATLAS_SC, by = c("CODMUNRES", "ANO", "NIVEL"), all = TRUE)
BDEM_SC <- merge(BDEM_SC, SINASC_SC, by = c("CODMUNRES", "ANO", "NIVEL"), all = TRUE)
BDEM_SC <- merge(BDEM_SC, SIM_SC, by = c("CODMUNRES", "ANO", "NIVEL"), all = TRUE)
BDEM_SC <- merge(BDEM_SC, SINISA_SC, by = c("CODMUNRES", "ANO", "NIVEL"), all = TRUE)

dim(BDEM_SC)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - BDEM - tarefa 1" e envie para o repositório Projeto_BDEM_2016

# Tarefa 2: Inserir os seguintes indicadores epidemiológicos (com apenas dias casas decimais) no BDEM_UF:
# TFG: Taxa de fecundidade geral
# TMG: Taxa de mortalidade geral
# RMM: Razão de mortalidade materna
# TMM: Taxa de mortalidade materna
# TMM_P: Taxa de mortalidade materna em até 42 dias
# TMN: Taxa de mortalidade neonatal
# TMN_P: Taxa de mortalidade neonatal precoce
# TMN_T: Taxa de mortalidade neonatal tardia
# TMI: Taxa de mortalidade infantil

# Conferir o banco BDEM_UF após inserção dos indicadores

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - BDEM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3: Exportar o banco de dados com o nome BDEM_UF.csv (Exemplo: BDEM_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados BDEM_UF 2016 e script - BDEM - tarefas 1 a 3"  e enviar para o repositório Projeto_BDEM_2016
 