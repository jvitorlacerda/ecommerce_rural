
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#                                     BUSINESS ANALYTICS
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#Pergunta/Oportunidade de Negócio:
#--------------------------------
#"Identificar se há potencial dos moradores das zonas rurais com acesso à internet no Brasil 
#e sua possível integração ao comércio eletrônico"


#Fonte dos dados:
#Cetic.Br: Centro Regional de Estudos para o Desenvolvimento da Sociedade da Informação
#link: https://cetic.br/pt/pesquisa/domicilios/microdados/

#pacotes necessários:
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap", 
             "ade4",
             "readxl",
             "utils")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#                                     DATA WRANGLING
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#XXXXXXXXXXXXXXXXXXXXX
#PREPARANDO AS BASES
#XXXXXXXXXXXXXXXXXXXXX


###BASE INDIVÍDUOS

##carregando a base de dados original
base_orig_indv_2018 <- read.csv2("ticdom_2018_individuos_base_de_microdados_v1.0.csv") 

base_orig_indv_2023 <- read.csv2("tic_domicilios_2023_individuos_base_de_microdados_v1.0.csv")



##dataframe com a ordem das variáveis (colunas) para facilitar a seleção
#2018---------
nomes_colindv_2018 <- colnames(base_orig_indv_2018)

seq_colindv_2018 <- data.frame(Ordem = 1:length(nomes_colindv_2018), Variaveis = nomes_colindv_2018)

#2023---------
nomes_colindv_2023 <- colnames(base_orig_indv_2023)

seq_colindv_2023 <- data.frame(Ordem = 1:length(nomes_colindv_2023), Variaveis = nomes_colindv_2023)



###BASE DOMICILIOS

##carregando a base de dados original
base_orig_domc_2018 <- read.csv2("ticdom_2018_domicilios_base_de_microdados_v1.0.csv")

base_orig_domc_2023 <- read.csv2("tic_domicilios_2023_domicilios_base_de_microdados_v1.0.csv")



##dataframe com a ordem das variáveis (colunas) para facilitar a seleção
#2018---------
nomes_coldomc_2018 <- colnames(base_orig_domc_2018)

seq_coldomc_2018 <- data.frame(Ordem = 1:length(nomes_coldomc_2018), Variaveis = nomes_coldomc_2018)

#2023---------
nomes_coldomc_2023 <- colnames(base_orig_domc_2023)

seq_coldomc_2023 <- data.frame(Ordem = 1:length(nomes_coldomc_2023), Variaveis = nomes_coldomc_2023)



#-------------------------------------------------------------------------------
#Selecionando as variáveis de interesse (pelo índice) para análise descritiva
#-------------------------------------------------------------------------------


#--------Base 2018----------

#seleção base indivíduos
an_desc_i_2018 <- select(base_orig_indv_2018, 1,213,217,24,183,180,25,26,43,175,182,50,86,220)

#QUEST: Número de identificação do questionário
#AREA: Área
#PEA_2: Condição de atividade (trabalha/não trabalha)
#C2A: (O respondente nunca usou a internet) E qual desses motivos é o principal?
#J5: O respondente possui telefone celular?
#J3: O respondente usou a Internet pelo telefone celular nos últimos 3 meses?
#C3: Quando o respondente usou a Internet pela última vez?
#C4: Em média, com que frequência o respondente usou a Internet nos últimos 3 meses?
#C6A: Em qual desses locais o respondente usou a Internet com mais frequência?
#J2_J: Nos últimos 3 meses, o respondente usou o telefone celular para acessar páginas ou sites?
#J3A_B: Quando o respondente usou a Internet pelo telefone celular nos últimos 3 meses, 
#       utilizou conexão wi-fi?
#C8_A: Nos últimos 3 meses, o respondente utilizou a Internet para procurar informações sobre 
#      produtos e serviços?
#H2: Nos últimos 12 meses, o respondente comprou ou encomendou produtos ou serviços pela 
#    Internet, mesmo que o pagamento não tenha sido feito pela Internet?
#C5_DISPOSITIVOS: Dispositivo utilizado pelos usuários de Internet de forma exclusiva ou simultânea

#visualizando a base
glimpse(an_desc_i_2018)

#seleção base domicílios
an_desc_d_2018 <- select(base_orig_domc_2018, 1,29,56,5,30,36)

#QUEST: Número de identificação do questionário
#GRAU_INSTRUCAO: Até que ano de escola o responsável pelo domicílio cursou?
#RENDA_FAMILIAR_2: Renda familiar em salários mínimos
#CLASSE_2015: Classe econômica pelo Critério Brasil 2015
#A1_A: Neste domicílio tem computador de mesa?
#A4: Este domicílio tem acesso à Internet?

#visualizando a base
glimpse(an_desc_d_2018)

#padronizando nome das variáveis
an_desc_d_2018 <- rename(an_desc_d_2018, GRAU_INSTRUCAO = "grau_instrucao")

#verificando transformação
glimpse(an_desc_d_2018)

#verificando se a variável QUEST tem observações repetidas, possibilitando o uso como um ID (identificador único)
dupli_desc_i_2018 <- any(duplicated(an_desc_i_2018$QUEST))
print(dupli_desc_i_2018)

dupli_desc_d_2018 <- any(duplicated(an_desc_d_2018$QUEST))
print(dupli_desc_d_2018)

#junção (trazendo a base domicílios para a base indivíduos)
an_desc_2018 <- left_join(an_desc_i_2018, an_desc_d_2018, by = "QUEST")

#reposicionamento
an_desc_2018 <- select(an_desc_2018, 1,2,15,3,16,17,4,5,18,19,everything())

#QUEST: Número de identificação do questionário
#AREA: Área
#GRAU_INSTRUCAO: Até que ano de escola o responsável pelo domicílio cursou?
#PEA_2: Condição de atividade
#RENDA_FAMILIAR_2: Renda familiar em salários mínimos
#CLASSE_2015: Classe econômica pelo Critério Brasil 2015
#C2A: (O respondente nunca usou a internet) E qual desses motivos é o principal?
#J5: O respondente possui telefone celular?
#A1_A: Neste domicílio tem computador de mesa?
#A4: Este domicílio tem acesso à Internet?
#J3: O respondente usou a Internet pelo telefone celular nos últimos 3 meses?
#C3: Quando o respondente usou a Internet pela última vez?
#C4: Em média, com que frequência o respondente usou a Internet nos últimos 3 meses?
#C6A: Em qual desses locais o respondente usou a Internet com mais frequência?
#J2_J: Nos últimos 3 meses, o respondente usou o telefone celular para acessar páginas ou sites?
#J3A_B: Quando o respondente usou a Internet pelo telefone celular nos últimos 3 meses, 
#       utilizou conexão wi-fi?
#C8_A: Nos últimos 3 meses, o respondente utilizou a Internet para procurar informações sobre 
#      produtos e serviços?
#H2: Nos últimos 12 meses, o respondente comprou ou encomendou produtos ou serviços pela 
#    Internet, mesmo que o pagamento não tenha sido feito pela Internet?
#C5_DISPOSITIVOS: Dispositivo utilizado pelos usuários de Internet de forma exclusiva ou simultânea



#--------Base 2023----------

#seleção base indivíduos
an_desc_i_2023 <- select(base_orig_indv_2023, 1,229,235,21,117,114,22,23,39,109,116,46,76,242)

#QUEST: Número de identificação do questionário
#AREA: Área
#PEA_2: Condição de atividade
#C2A: (O respondente nunca usou a internet) E qual desses motivos é o principal?
#J5: O respondente possui telefone celular?
#J3: O respondente usou a Internet pelo telefone celular nos últimos 3 meses?
#C3: Quando o respondente usou a Internet pela última vez?
#C4: Em média, com que frequência o respondente usou a Internet nos últimos 3 meses?
#C6A: Em qual desses locais o respondente usou a Internet com mais frequência?
#J2_J: Nos últimos 3 meses, o respondente usou o telefone celular para acessar páginas ou sites?
#J3A_B: Quando o respondente usou a Internet pelo telefone celular nos últimos 3 meses, 
#       utilizou conexão wi-fi?
#C8_A: Nos últimos 3 meses, o respondente utilizou a Internet para procurar informações sobre 
#      produtos e serviços?
#H2: Nos últimos 12 meses, o respondente comprou ou encomendou produtos ou serviços pela 
#    Internet, mesmo que o pagamento não tenha sido feito pela Internet?
#C5_DISPOSITIVOS: Dispositivo utilizado pelos usuários de Internet de forma exclusiva ou simultânea

#visualizando a base
glimpse(an_desc_i_2023)

#seleção base domicílios
an_desc_d_2023 <- select(base_orig_domc_2023, 1,24,52,53,28,34)

#QUEST: Número de identificação do questionário
#GRAU_INSTRUCAO: Até que ano de escola o responsável pelo domicílio cursou?
#RENDA_FAMILIAR_2: Renda familiar em salários mínimos
#CLASSE_2015: Classe econômica pelo Critério Brasil 2015
#A1_A: Neste domicílio tem computador de mesa?
#A4: Este domicílio tem acesso à Internet?

#visualizando a base
glimpse(an_desc_d_2023)

##verificando se a variável QUEST tem observações repetidas, possibilitando o uso como um ID (identificador único)
dupli_desc_i_2023 <- any(duplicated(an_desc_i_2023$QUEST))
print(dupli_desc_i_2023)

dupli_desc_d_2023 <- any(duplicated(an_desc_d_2023$QUEST))
print(dupli_desc_d_2023)

#junção (trazendo a base domicílios para a base indivíduos)
an_desc_2023 <- left_join(an_desc_i_2023, an_desc_d_2023, by = "QUEST")

#reposicionamento
an_desc_2023 <- select(an_desc_2023, 1,2,15,3,16,17,4,5,18,19,everything())


#QUEST: Número de identificação do questionário
#AREA: Área
#GRAU_INSTRUCAO: Até que ano de escola o responsável pelo domicílio cursou?
#PEA_2: Condição de atividade
#RENDA_FAMILIAR_2: Renda familiar em salários mínimos
#CLASSE_2015: Classe econômica pelo Critério Brasil 2015
#C2A: (O respondente nunca usou a internet) E qual desses motivos é o principal?
#J5: O respondente possui telefone celular?
#A1_A: Neste domicílio tem computador de mesa?
#A4: Este domicílio tem acesso à Internet?
#J3: O respondente usou a Internet pelo telefone celular nos últimos 3 meses?
#C3: Quando o respondente usou a Internet pela última vez?
#C4: Em média, com que frequência o respondente usou a Internet nos últimos 3 meses?
#C6A: Em qual desses locais o respondente usou a Internet com mais frequência?
#J2_J: Nos últimos 3 meses, o respondente usou o telefone celular para acessar páginas ou sites?
#J3A_B: Quando o respondente usou a Internet pelo telefone celular nos últimos 3 meses, 
#       utilizou conexão wi-fi?
#C8_A: Nos últimos 3 meses, o respondente utilizou a Internet para procurar informações sobre 
#      produtos e serviços?
#H2: Nos últimos 12 meses, o respondente comprou ou encomendou produtos ou serviços pela 
#    Internet, mesmo que o pagamento não tenha sido feito pela Internet?
#C5_DISPOSITIVOS: Dispositivo utilizado pelos usuários de Internet de forma exclusiva ou simultânea



##verificando quais variáveis estão na base e os tipos delas
glimpse(an_desc_2018)
glimpse(an_desc_2023)


##transformando a tipologia das variáveis (integer para factor, com exceção de QUEST)
#2018----------
an_desc_2018[-which(names(an_desc_2018) %in% c("QUEST"))] <- 
  lapply(an_desc_2018[-which(names(an_desc_2018) %in% c("QUEST"))], as.factor)

glimpse(an_desc_2018)

#2023----------
an_desc_2023[-which(names(an_desc_2023) %in% c("QUEST"))] <- 
  lapply(an_desc_2023[-which(names(an_desc_2023) %in% c("QUEST"))], as.factor)

glimpse(an_desc_2023)


##Verificando se contém NA (missing values)
any(is.na(an_desc_2018))
any(is.na(an_desc_2023))

##Verificando as categorias em cada variável
summary(an_desc_2018)
summary(an_desc_2023)

##Foi constatado uma categoria a mais na variável C5_DISPOSITIVOS na base indivíduos 2023.
#Ela precisa ser retirada para fazer uma análise descritiva uniforme entre o ano de 2018 e 2023.
levels(an_desc_2018$C5_DISPOSITIVOS)
levels(an_desc_2023$C5_DISPOSITIVOS)

an_desc_2023 <- an_desc_2023[an_desc_2023$C5_DISPOSITIVOS != "4",]  

an_desc_2023$C5_DISPOSITIVOS <- droplevels(an_desc_2023$C5_DISPOSITIVOS)

levels(an_desc_2023$C5_DISPOSITIVOS)

summary(an_desc_2018)
summary(an_desc_2023)

#visualizando o panorama geral das bases criadas para Análise Descritiva
browseURL("CaminhoDasBases_AnaliseDescritiva.pdf")

#-----------------------------------------------------------------------------------------------------
#Selecionando as variáveis de interesse (pelo índice) para a Análise de Correspondência Múltipla (ACM)
#-----------------------------------------------------------------------------------------------------


#--------Base 2018----------

#seleção base indivíduos
acm_i_2018 <- select(base_orig_indv_2018, 1,213,217,180,25,26,182,50,86,220,78:84,168:173,175:178)

#QUEST: Número de identificação do questionário
#AREA: Área
#PEA_2: Condição de atividade
#J3: O respondente usou a Internet pelo telefone celular nos últimos 3 meses?
#C3: Quando o respondente usou a Internet pela última vez?
#C4: Em média, com que frequência o respondente usou a Internet nos últimos 3 meses?
#J3A_B: Quando o respondente usou a Internet pelo telefone celular nos últimos 3 meses, 
#       utilizou conexão wi-fi?
#C8_A: Nos últimos 3 meses, o respondente utilizou a Internet para procurar informações sobre 
#      produtos e serviços?
#H2: Nos últimos 12 meses, o respondente comprou ou encomendou produtos ou serviços pela 
#    Internet, mesmo que o pagamento não tenha sido feito pela Internet?
#C5_DISPOSITIVOS: Dispositivo utilizado pelos usuários de Internet de forma exclusiva ou simultânea
#     (Nos últimos 12 meses, o respondente usou a Internet para procurar informações ou realizar 
#      serviços públicos relacionados a...)
#G1_A: ... documentos pessoais, como RG, CPF, passaporte, ou carteira de trabalho?
#G1_B: ... saúde pública, como agendamento de consultas, remédios ou outros serviços do sistema público de saúde?
#G1_C: ... educação pública, como ENEM, PROUNI, matrícula em escolas ou universidades públicas?
#G1_D: ... direitos do trabalhador ou previdência social, como INSS, FGTS, seguro-desemprego, auxílio-doença ou aposentadoria?
#G1_E: ... impostos e taxas governamentais, como declaração de imposto de renda, IPVA ou IPTU? 
#G1_F: ... polícia e segurança como boletim de ocorrência, antecedentes criminais ou denúncias?
#G1_G: ... transporte público ou outros serviços urbanos, como limpeza e conservação de vias, iluminação?
#     (Nos últimos 3 meses, ...)
#J2_C: ... o respondente usou o telefone celular para ouvir músicas?
#J2_D: ... o respondente usou o telefone celular para assistir vídeos?
#J2_E: ... o respondente usou o telefone celular para jogar?
#J2_F: ... o respondente usou o telefone celular para tirar fotos?
#J2_G: ... o respondente usou o telefone celular para usar mapas, por exemplo o Google Maps?
#J2_H1: ... o respondente usou o telefone celular para enviar e receber e-mails?
#J2_J: ... o respondente usou o telefone celular para acessar páginas ou sites?
#J2_K: ... o respondente usou o telefone celular para baixar aplicativos?
#J2_L: ... o respondente usou o telefone celular para buscar informações, como por exemplo no Google?
#J2_M: ... o respondente usou o telefone celular para compartilhar fotos, vídeos ou textos?

#visualizando a base
glimpse(acm_i_2018)

#seleção base domicílios
acm_d_2018 <- select(base_orig_domc_2018, 1,29,56,36)

#QUEST: Número de identificação do questionário
#GRAU_INSTRUCAO: Até que ano de escola o responsável pelo domicílio cursou?
#RENDA_FAMILIAR_2: Renda familiar em salários mínimos
#A4: Este domicílio tem acesso à Internet?

#visualizando a base
glimpse(acm_d_2018)

#padronizando nome das variáveis
acm_d_2018 <- rename(acm_d_2018, GRAU_INSTRUCAO = "grau_instrucao")

#verificando transformação
glimpse(acm_d_2018)

#verificando se a variável QUEST tem observações repetidas, possibilitando o uso como um ID (identificador único)
dupli_acm_i_2018 <- any(duplicated(acm_i_2018$QUEST))
print(dupli_acm_i_2018)

dupli_acm_d_2018 <- any(duplicated(acm_d_2018$QUEST))
print(dupli_acm_d_2018)

#junção (trazendo a base domicílios para a base indivíduos)
acm_2018 <- left_join(acm_i_2018, acm_d_2018, by = "QUEST")

#reposicionamento
acm_2018 <- select(acm_2018, 1,2,28,3,29,30,everything())


#QUEST: Número de identificação do questionário
#AREA: Área
#GRAU_INSTRUCAO: Até que ano de escola o responsável pelo domicílio cursou?
#PEA_2: Condição de atividade
#RENDA_FAMILIAR_2: Renda familiar em salários mínimos
#A4: Este domicílio tem acesso à Internet?
#J3: O respondente usou a Internet pelo telefone celular nos últimos 3 meses?
#C3: Quando o respondente usou a Internet pela última vez?
#C4: Em média, com que frequência o respondente usou a Internet nos últimos 3 meses?
#J3A_B: Quando o respondente usou a Internet pelo telefone celular nos últimos 3 meses, 
#       utilizou conexão wi-fi?
#C8_A: Nos últimos 3 meses, o respondente utilizou a Internet para procurar informações sobre 
#      produtos e serviços?
#H2: Nos últimos 12 meses, o respondente comprou ou encomendou produtos ou serviços pela 
#    Internet, mesmo que o pagamento não tenha sido feito pela Internet?
#C5_DISPOSITIVOS: Dispositivo utilizado pelos usuários de Internet de forma exclusiva ou simultânea
#     (Nos últimos 12 meses, o respondente usou a Internet para procurar informações ou realizar 
#      serviços públicos relacionados a...)
#G1_A: ... documentos pessoais, como RG, CPF, passaporte, ou carteira de trabalho?
#G1_B: ... saúde pública, como agendamento de consultas, remédios ou outros serviços do sistema público de saúde?
#G1_C: ... educação pública, como ENEM, PROUNI, matrícula em escolas ou universidades públicas?
#G1_D: ... direitos do trabalhador ou previdência social, como INSS, FGTS, seguro-desemprego, auxílio-doença ou aposentadoria?
#G1_E: ... impostos e taxas governamentais, como declaração de imposto de renda, IPVA ou IPTU? 
#G1_F: ... polícia e segurança como boletim de ocorrência, antecedentes criminais ou denúncias?
#G1_G: ... transporte público ou outros serviços urbanos, como limpeza e conservação de vias, iluminação?
#     (Nos últimos 3 meses, ...)
#J2_C: ... o respondente usou o telefone celular para ouvir músicas?
#J2_D: ... o respondente usou o telefone celular para assistir vídeos?
#J2_E: ... o respondente usou o telefone celular para jogar?
#J2_F: ... o respondente usou o telefone celular para tirar fotos?
#J2_G: ... o respondente usou o telefone celular para usar mapas, por exemplo o Google Maps?
#J2_H1: ... o respondente usou o telefone celular para enviar e receber e-mails?
#J2_J: ... o respondente usou o telefone celular para acessar páginas ou sites?
#J2_K: ... o respondente usou o telefone celular para baixar aplicativos?
#J2_L: ... o respondente usou o telefone celular para buscar informações, como por exemplo no Google?
#J2_M: ... o respondente usou o telefone celular para compartilhar fotos, vídeos ou textos?

glimpse(acm_2018)

#--------Base 2023----------

#seleção base indivíduos
acm_i_2023 <- select(base_orig_indv_2023, 1,229,235,114,22,23,116,46,76,242,180:186,102:107,109:112)

#QUEST: Número de identificação do questionário
#AREA: Área
#PEA_2: Condição de atividade
#J3: O respondente usou a Internet pelo telefone celular nos últimos 3 meses?
#C3: Quando o respondente usou a Internet pela última vez?
#C4: Em média, com que frequência o respondente usou a Internet nos últimos 3 meses?
#J3A_B: Quando o respondente usou a Internet pelo telefone celular nos últimos 3 meses, 
#       utilizou conexão wi-fi?
#C8_A: Nos últimos 3 meses, o respondente utilizou a Internet para procurar informações sobre 
#      produtos e serviços?
#H2: Nos últimos 12 meses, o respondente comprou ou encomendou produtos ou serviços pela 
#    Internet, mesmo que o pagamento não tenha sido feito pela Internet?
#C5_DISPOSITIVOS: Dispositivo utilizado pelos usuários de Internet de forma exclusiva ou simultânea
#     (Nos últimos 12 meses, o respondente usou a Internet para procurar informações ou realizar 
#      serviços públicos relacionados a...)
#G1_A: ... documentos pessoais, como RG, CPF, passaporte, ou carteira de trabalho?
#G1_B: ... saúde pública, como agendamento de consultas, remédios ou outros serviços do sistema público de saúde?
#G1_C: ... educação pública, como ENEM, PROUNI, matrícula em escolas ou universidades públicas?
#G1_D: ... direitos do trabalhador ou previdência social, como INSS, FGTS, seguro-desemprego, auxílio-doença ou aposentadoria?
#G1_E: ... impostos e taxas governamentais, como declaração de imposto de renda, IPVA ou IPTU? 
#G1_F: ... polícia e segurança como boletim de ocorrência, antecedentes criminais ou denúncias?
#G1_G: ... transporte público ou outros serviços urbanos, como limpeza e conservação de vias, iluminação?
#     (Nos últimos 3 meses, ...)
#J2_C: ... o respondente usou o telefone celular para ouvir músicas?
#J2_D: ... o respondente usou o telefone celular para assistir vídeos?
#J2_E: ... o respondente usou o telefone celular para jogar?
#J2_F: ... o respondente usou o telefone celular para tirar fotos?
#J2_G: ... o respondente usou o telefone celular para usar mapas, por exemplo o Google Maps?
#J2_H1: ... o respondente usou o telefone celular para enviar e receber e-mails?
#J2_J: ... o respondente usou o telefone celular para acessar páginas ou sites?
#J2_K: ... o respondente usou o telefone celular para baixar aplicativos?
#J2_L: ... o respondente usou o telefone celular para buscar informações, como por exemplo no Google?
#J2_M: ... o respondente usou o telefone celular para compartilhar fotos, vídeos ou textos?

#visualizando a base
glimpse(acm_i_2023)

#seleção base domicílios
acm_d_2023 <- select(base_orig_domc_2023, 1,24,52,34)

#QUEST: Número de identificação do questionário
#GRAU_INSTRUCAO: Até que ano de escola o responsável pelo domicílio cursou?
#RENDA_FAMILIAR_2: Renda familiar em salários mínimos
#A4: Este domicílio tem acesso à Internet?

#visualizando a base
glimpse(acm_d_2023)

#verificando se a variável QUEST tem observações repetidas, possibilitando o uso como um ID (identificador único)
dupli_acm_i_2023 <- any(duplicated(acm_i_2023$QUEST))
print(dupli_acm_i_2023)

dupli_acm_d_2023 <- any(duplicated(acm_d_2023$QUEST))
print(dupli_acm_d_2023)

#junção (trazendo a base domicílios para a base indivíduos)
acm_2023 <- left_join(acm_i_2023, acm_d_2023, by = "QUEST")

#reposicionamento
acm_2023 <- select(acm_2023, 1,2,28,3,29,30,4,everything())


#QUEST: Número de identificação do questionário
#AREA: Área
#GRAU_INSTRUCAO: Até que ano de escola o responsável pelo domicílio cursou?
#PEA_2: Condição de atividade
#RENDA_FAMILIAR_2: Renda familiar em salários mínimos
#A4: Este domicílio tem acesso à Internet?
#J3: O respondente usou a Internet pelo telefone celular nos últimos 3 meses?
#C3: Quando o respondente usou a Internet pela última vez?
#C4: Em média, com que frequência o respondente usou a Internet nos últimos 3 meses?
#J3A_B: Quando o respondente usou a Internet pelo telefone celular nos últimos 3 meses, 
#       utilizou conexão wi-fi?
#C8_A: Nos últimos 3 meses, o respondente utilizou a Internet para procurar informações sobre 
#      produtos e serviços?
#H2: Nos últimos 12 meses, o respondente comprou ou encomendou produtos ou serviços pela 
#    Internet, mesmo que o pagamento não tenha sido feito pela Internet?
#C5_DISPOSITIVOS: Dispositivo utilizado pelos usuários de Internet de forma exclusiva ou simultânea
#     (Nos últimos 12 meses, o respondente usou a Internet para procurar informações ou realizar 
#      serviços públicos relacionados a...)
#G1_A: ... documentos pessoais, como RG, CPF, passaporte, ou carteira de trabalho?
#G1_B: ... saúde pública, como agendamento de consultas, remédios ou outros serviços do sistema público de saúde?
#G1_C: ... educação pública, como ENEM, PROUNI, matrícula em escolas ou universidades públicas?
#G1_D: ... direitos do trabalhador ou previdência social, como INSS, FGTS, seguro-desemprego, auxílio-doença ou aposentadoria?
#G1_E: ... impostos e taxas governamentais, como declaração de imposto de renda, IPVA ou IPTU? 
#G1_F: ... polícia e segurança como boletim de ocorrência, antecedentes criminais ou denúncias?
#G1_G: ... transporte público ou outros serviços urbanos, como limpeza e conservação de vias, iluminação?
#     (Nos últimos 3 meses, ...)
#J2_C: ... o respondente usou o telefone celular para ouvir músicas?
#J2_D: ... o respondente usou o telefone celular para assistir vídeos?
#J2_E: ... o respondente usou o telefone celular para jogar?
#J2_F: ... o respondente usou o telefone celular para tirar fotos?
#J2_G: ... o respondente usou o telefone celular para usar mapas, por exemplo o Google Maps?
#J2_H1: ... o respondente usou o telefone celular para enviar e receber e-mails?
#J2_J: ... o respondente usou o telefone celular para acessar páginas ou sites?
#J2_K: ... o respondente usou o telefone celular para baixar aplicativos?
#J2_L: ... o respondente usou o telefone celular para buscar informações, como por exemplo no Google?
#J2_M: ... o respondente usou o telefone celular para compartilhar fotos, vídeos ou textos?


##verificando quais variáveis estão na base e os tipos delas
glimpse(acm_2018)
glimpse(acm_2023)


##verificando se contém NA (missing values)
any(is.na(acm_2018))
any(is.na(acm_2023))


##criando as variáveis estatísticas "HAB_DIG_G" e "HAB_DIG_J"

#Passo 1
#recodificando as categorias das variáveis
#2018---------
acm_2018 <- acm_2018 %>% 
  mutate_at(vars(G1_A:G1_G), ~ifelse(. == 1, 1, 0))

acm_2018 <- acm_2018 %>% 
  mutate_at(vars(J2_C:J2_M), ~ifelse(. == 1, 1, 0))

#2023---------
acm_2023 <- acm_2023 %>% 
  mutate_at(vars(G1_A:G1_G), ~ifelse(. == 1, 1, 0))

acm_2023 <- acm_2023 %>% 
  mutate_at(vars(J2_C:J2_M), ~ifelse(. == 1, 1, 0))


#Passo 2
#criando a variável "HAB_DIG_G" e "HAB_DIG_J" condicionadas a pesos lógicos diferentes:

#para G1_A a G1_G (HAB_DIG_G): pelo menos uma das variáveis devem ter valor 1 ("Sim")
#2018---------
acm_2018 <- acm_2018 %>% 
  mutate(HAB_DIG_G = ifelse(G1_A + G1_B + G1_C + G1_D + G1_E + G1_F + G1_G > 0, 1, 0))

#2023---------
acm_2023 <- acm_2023 %>% 
  mutate(HAB_DIG_G = ifelse(G1_A + G1_B + G1_C + G1_D + G1_E + G1_F + G1_G > 0, 1, 0))


#para J2_C a J2_M (HAB_DIG_J): pelo menos duas variáveis devem ter valor 1 ("Sim")
#2018---------
acm_2018 <- acm_2018 %>% 
  mutate(HAB_DIG_J = ifelse(J2_C + J2_D + J2_E + J2_F + J2_G + J2_H1 + J2_J + J2_K + J2_L + J2_M > 1, 1, 0))

#2023---------
acm_2023 <- acm_2023 %>% 
  mutate(HAB_DIG_J = ifelse(J2_C + J2_D + J2_E + J2_F + J2_G + J2_H1 + J2_J + J2_K + J2_L + J2_M > 1, 1, 0))

glimpse(acm_2018)
glimpse(acm_2023)



##Filtrando observações:
#selecionando somente as observações em que existe acesso à internet
#A4 (domicílio tem internet? 1 = "Sim")

#2018---------
acm_2018 <- acm_2018 %>% 
  filter(A4 == "1")


#2023---------
acm_2023 <- acm_2023 %>% 
  filter(A4 == "1")




##Dividindo a base 2018 e 2023 por Área: 1 = urbano; 2 = rural e extraindo as variáveis estatísticas
#condensadas, ou seja, excluindo G1_A:G1_G e J2_C:J2_M

#2018---------
#Urbano
acm_2018u <- acm_2018 %>%
  filter(AREA == "1") %>% 
  select(-(G1_A:G1_G), -(J2_C:J2_M))

#Rural
acm_2018r <- acm_2018 %>%
  filter(AREA == "2") %>% 
  select(-(G1_A:G1_G), -(J2_C:J2_M))


#2023---------
#Urbano
acm_2023u <- acm_2023 %>%
  filter(AREA == "1") %>% 
  select(-(G1_A:G1_G), -(J2_C:J2_M))

#Rural
acm_2023r <- acm_2023 %>%
  filter(AREA == "2") %>% 
  select(-(G1_A:G1_G), -(J2_C:J2_M))


##Eliminando categorias desnecessárias para a ACM

acm_2018u <- acm_2018u %>% filter(RENDA_FAMILIAR_2 < 96) %>%
  filter(J3 < 96) %>%
  filter(C3 < 96) %>%
  filter(C4 < 96) %>%
  filter(J3A_B < 96) %>%
  filter(C8_A < 96) %>%
  filter(H2 < 96) %>%
  filter(C5_DISPOSITIVOS < 96)

acm_2018r <- acm_2018r %>% filter(RENDA_FAMILIAR_2 < 96) %>%
  filter(J3 < 96) %>%
  filter(C3 < 96) %>%
  filter(C4 < 96) %>%
  filter(J3A_B < 96) %>%
  filter(C8_A < 96) %>%
  filter(H2 < 96) %>%
  filter(C5_DISPOSITIVOS < 96)

acm_2023u <- acm_2023u %>% filter(RENDA_FAMILIAR_2 < 96) %>%
  filter(J3 < 96) %>%
  filter(C3 < 96) %>%
  filter(C4 < 96) %>%
  filter(J3A_B < 96) %>%
  filter(C8_A < 96) %>%
  filter(H2 < 96) %>%
  filter(C5_DISPOSITIVOS < 96)
  
acm_2023r <- acm_2023r %>% filter(RENDA_FAMILIAR_2 < 96) %>%
  filter(J3 < 96) %>%
  filter(C3 < 96) %>%
  filter(C4 < 96) %>%
  filter(J3A_B < 96) %>%
  filter(C8_A < 96) %>%
  filter(H2 < 96) %>%
  filter(C5_DISPOSITIVOS < 96)



##Após a divisão das bases (1: 2018 urbano; 2: 2018 rural; 3: 2023 urbano; 4: 2023 rural), excluir
# as variáveis QUEST, AREA e A4 para executar o algoritmo da ACM

acm_2018u <- select(acm_2018u, -QUEST, -AREA, -A4)

acm_2018r <- select(acm_2018r, -QUEST, -AREA, -A4)

acm_2023u <- select(acm_2023u, -QUEST, -AREA, -A4)

acm_2023r <- select(acm_2023r, -QUEST, -AREA, -A4)


##transformando a tipologia das variáveis para factor 
acm_2018u <- as.data.frame(lapply(acm_2018u, factor))

acm_2018r <- as.data.frame(lapply(acm_2018r, factor))

acm_2023u <- as.data.frame(lapply(acm_2023u, factor))

acm_2023r <- as.data.frame(lapply(acm_2023r, factor))


glimpse(acm_2018u)
glimpse(acm_2018r)
glimpse(acm_2023u)
glimpse(acm_2023r)


##verificando se contém NA (missing values)
any(is.na(acm_2018u))
any(is.na(acm_2018r))
any(is.na(acm_2023u))
any(is.na(acm_2023r))


#visualizando o panorama geral das bases criadas para ACM
browseURL("CaminhoDasBases_ACM.pdf")
