## PREPARAÇÃO DE DADOS
library(bit64)
library(data.table)
library(descr)
library(tidyr)
library(dplyr)

dic <- read.table(file = 'Dicionario-Pnad.txt', header=F, sep='\t')
dic <- dic[complete.cases(dic),]
colnames(dic) <- c('inicio', 'tamanho', 'variavel')
 
# Parâmetro com o final de cada campo
end_dic = dic$inicio + dic$tamanho - 1

# Converte os microdados para um arquivo csv

# Criando csv
fwf2csv(fwffile = 'PNADC_012020.txt', csvfile = 'PNADC-012020.csv', names=dic$variavel, begin=dic$inicio, end=end_dic)
fwf2csv(fwffile = 'PNADC_022020.txt', csvfile = 'PNADC-022020.csv', names=dic$variavel, begin=dic$inicio, end=end_dic)
fwf2csv(fwffile = 'PNADC_032020.txt', csvfile = 'PNADC-032020.csv', names=dic$variavel, begin=dic$inicio, end=end_dic)
fwf2csv(fwffile = 'PNADC_042020.txt', csvfile = 'PNADC-042020.csv', names=dic$variavel, begin=dic$inicio, end=end_dic)
fwf2csv(fwffile = 'PNADC_012021.txt', csvfile = 'PNADC-012021.csv', names=dic$variavel, begin=dic$inicio, end=end_dic)
fwf2csv(fwffile = 'PNADC_022021.txt', csvfile = 'PNADC-022021.csv', names=dic$variavel, begin=dic$inicio, end=end_dic)

#########
# Dados #
#########

pnad1 <- read.table(file='PNADC-012020.csv', header=TRUE, sep='\t', dec='.')
pnad2 <- read.table(file='PNADC-022020.csv', header=TRUE, sep='\t', dec='.')
pnad3 <- read.table(file='PNADC-032020.csv', header=TRUE, sep='\t', dec='.')
pnad4 <- read.table(file='PNADC-042020.csv', header=TRUE, sep='\t', dec='.')
pnad5 <- read.table(file='PNADC-012021.csv', header=TRUE, sep='\t', dec='.')
pnad6 <- read.table(file='PNADC-022021.csv', header=TRUE, sep='\t', dec='.')

#########
# Dados #
#########

pnad1 <- pnad1[,c("Ano", "Trimestre", "UF","V1028", "V2005","V2007","V2009","V2010","V3009A","V4009","V4012","V4016","V4017","V4018","V4019","V403411","V403412","V4039C","V4040", "V4020", "V4022", "V4021", "V40171", "VD4010", "V4032")]
pnad2 <- pnad2[,c("Ano", "Trimestre", "UF","V1028", "V2005","V2007","V2009","V2010","V3009A","V4009","V4012","V4016","V4017","V4018","V4019","V403411","V403412","V4039C","V4040", "V4020", "V4022", "V4021", "V40171", "VD4010", "V4032")]
pnad3 <- pnad3[,c("Ano", "Trimestre", "UF","V1028", "V2005","V2007","V2009","V2010","V3009A","V4009","V4012","V4016","V4017","V4018","V4019","V403411","V403412","V4039C","V4040", "V4020", "V4022", "V4021", "V40171", "VD4010", "V4032")]
pnad4 <- pnad4[,c("Ano", "Trimestre", "UF","V1028", "V2005","V2007","V2009","V2010","V3009A","V4009","V4012","V4016","V4017","V4018","V4019","V403411","V403412","V4039C","V4040", "V4020", "V4022", "V4021", "V40171", "VD4010", "V4032")]
pnad5 <- pnad5[,c("Ano", "Trimestre", "UF","V1028", "V2005","V2007","V2009","V2010","V3009A","V4009","V4012","V4016","V4017","V4018","V4019","V403411","V403412","V4039C","V4040", "V4020", "V4022", "V4021", "V40171", "VD4010", "V4032")]
pnad6 <- pnad6[,c("Ano", "Trimestre", "UF","V1028", "V2005","V2007","V2009","V2010","V3009A","V4009","V4012","V4016","V4017","V4018","V4019","V403411","V403412","V4039C","V4040", "V4020", "V4022", "V4021", "V40171", "VD4010", "V4032")]

# FALTANDO VARIAVEL V4022 - existe apenas no 12018 e no 22018

base <- rbind(pnad1, pnad2, pnad3, pnad4, pnad5, pnad6)

# Salvando base 
write.table(base, 'base-tabela-dinamica-naotratada.txt', sep='\t', row.names = F, quote = F, na='')

#######################
# Definindo variaveis #
#######################

# Peso Amostral

# Juntando ANO e TRIMESTRE

base <- base %>% unite(ano_trimestre, c("Ano", "Trimestre"))

# V4012
# Nesse trabalho, ... era: 
# 1	Trabalhador dom?stico
# 2	Militar do ex?rcito, da marinha, da aeron?utica, da pol?cia militar ou do corpo de bombeiros militar
# 3	Empregado do setor privado
# 4	Empregado do setor p?blico (inclusive empresas de economia mista)
# 5	Empregador
# 6	Conta pr?pria
# 7	Trabalhador familiar n?o remunerado

base <- subset(base, V4012==5 | V4012==6)
base$cliente <- base$V4012
base$cliente <- gsub('5', 'Empregador', base$cliente)
base$cliente <- gsub('6', 'Conta Própria', base$cliente)

# V2005
# Condi??o no domic?lio
# 01	Pessoa respons?vel pelo domic?lio 
# 02	C?njuge ou companheiro(a) de sexo diferente
# 03	C?njuge ou companheiro(a) do mesmo sexo
# 04	Filho(a) do respons?vel e do c?njuge 
# 05	Filho(a) somente do respons?vel
# 06	Enteado(a)
# 07	Genro ou nora
# 08	Pai, m?e, padrasto ou madrasta
# 09	Sogro(a)
# 10	Neto(a)
# 11	Bisneto(a) 
# 12	Irm?o ou irm?
# 13	Av? ou av?
# 14	Outro parente
# 15	Agregado(a) - N?o parente que n?o compartilha despesas
# 16	Convivente - N?o parente que compartilha despesas
# 17	Pensionista
# 18	Empregado(a) dom?stico(a)
# 19	Parente do(a) empregado(a) dom?stico(a)

base$domicilio <- paste("ZZXXWW", base$V2005, sep='')
base$domicilio <- gsub("ZZXXWW10", "Outros", base$domicilio)
base$domicilio <- gsub("ZZXXWW11", "Outros",	base$domicilio)
base$domicilio <- gsub("ZZXXWW12", "Outros",	base$domicilio)
base$domicilio <- gsub("ZZXXWW13", "Outros",	base$domicilio)
base$domicilio <- gsub("ZZXXWW14", "Outros",	base$domicilio)
base$domicilio <- gsub("ZZXXWW15", "Outros", base$domicilio)
base$domicilio <- gsub("ZZXXWW16", "Outros", base$domicilio)
base$domicilio <- gsub("ZZXXWW17", "Outros", base$domicilio)
base$domicilio <- gsub("ZZXXWW18", "Outros",	base$domicilio)
base$domicilio <- gsub("ZZXXWW19", "Outros", base$domicilio)
base$domicilio <- gsub("ZZXXWW1", "Chefe de Domicílio", base$domicilio)
base$domicilio <- gsub("ZZXXWW2", "Cônjuge",	base$domicilio)
base$domicilio <- gsub("ZZXXWW3", "Cônjuge",	base$domicilio)
base$domicilio <- gsub("ZZXXWW4", "Filho(a)", base$domicilio)
base$domicilio <- gsub("ZZXXWW5", "Filho(a)",	base$domicilio)
base$domicilio <- gsub("ZZXXWW6", "Filho(a)", base$domicilio)
base$domicilio <- gsub("ZZXXWW7", "Outros", base$domicilio)
base$domicilio <- gsub("ZZXXWW8", "Outros", base$domicilio)
base$domicilio <- gsub("ZZXXWW9", "Outros", base$domicilio)

# V2007
# Sexo
# 1 - Homem
# 2 - Mulher
base$genero <- base$V2007
base$genero <- gsub("1", "Homem", base$genero)
base$genero <- gsub("2", "Mulher", base$genero)

#V2009
# Idade
# V2009
# table(pnad$V2009)
base$faixa_etaria <- cut(base$V2009, c(0,25,35,45,55,65,200), right=FALSE)
base$faixa_etaria <- ifelse(base$faixa_etaria=="[0,25)", "De 0 até 24 anos", ifelse(base$faixa_etaria=="[25,35)", "De 25 até 34 anos", ifelse(base$faixa_etaria=="[35,45)", "De 35 até 44 anos", ifelse(base$faixa_etaria=="[45,55)", "De 45 até 54 anos", ifelse(base$faixa_etaria=="[55,65)", "De 55 até 64 anos", ifelse(base$faixa_etaria=="[65,200)", "De 65 anos a mais", '---'))))))

#Vari?vel faixa_etaria - PowerBi
base$faixa_etaria_2 <- base$faixa_etaria
base$faixa_etaria_2 <- gsub("De 0 até 24 anos", "Até 34 anos" , base$faixa_etaria_2)
base$faixa_etaria_2 <- gsub("De 25 até 34 anos", "Até 34 anos", base$faixa_etaria_2)
base$faixa_etaria_2 <- gsub("De 35 até 44 anos", "De 35 até 54 anos", base$faixa_etaria_2)
base$faixa_etaria_2 <- gsub("De 45 até 54 anos", "De 35 até 54 anos", base$faixa_etaria_2)
base$faixa_etaria_2 <- gsub("De 55 até 64 anos", "De 55 a mais", base$faixa_etaria_2)
base$faixa_etaria_2 <- gsub("De 65 anos a mais", "De 55 a mais", base$faixa_etaria_2)


#V2010
# Cor ou ra?a
# 1	Branca
# 2	Preta
# 3	Amarela
# 4	Parda 
# 5	Ind?gena
# 9	Ignorado
base$cor_raca <- base$V2010
base$cor_raca <- gsub("1", "Branca", base$cor_raca)
base$cor_raca <- gsub("2", "Negra", base$cor_raca)
base$cor_raca <- gsub("4", "Negra", base$cor_raca)
base$cor_raca <- gsub("3", "Outras", base$cor_raca)
base$cor_raca <- gsub("5", "Outras", base$cor_raca)
base$cor_raca <- gsub("9", "Outras", base$cor_raca)

# V3009A
# Qual foi o curso mais elevado que ... frequentou anteriormente?	
# 01	Creche (dispon?vel apenas no question?rio anual de educa??o)
# 02	Pr?-escola
# 03	Classe de alfabetiza??o - CA
# 04	Alfabetiza??o de jovens e adultos
# 05	Antigo prim?rio (elementar)
# 06	Antigo gin?sio (m?dio 1? ciclo)
# 07	Regular do ensino fundamental ou do 1? grau
# 08	Educa??o de jovens e adultos (EJA) ou supletivo do 1? grau
# 09	Antigo cient?fico, cl?ssico, etc. (m?dio 2? ciclo)
# 10	Regular do ensino m?dio ?u do 2? grau
# 11	Educa??o de jovens e adultos (EJA) ou supletivo do 2? grau
# 12	Superior - gradua??o
# 13	Especializa??o de n?vel superior
# 14	Mestrado
# 15	Doutorado
# N?o aplic?vel

base$escolaridade <- base$V3009A
base$escolaridade <- gsub("10", "Médio (completo ou incompleto)", base$escolaridade)
base$escolaridade <- gsub("11", "Médio (completo ou incompleto)", base$escolaridade)
base$escolaridade <- gsub("9", "Médio (completo ou incompleto)", base$escolaridade)
base$escolaridade <- gsub("12", "Superior (incompleto ou mais)", base$escolaridade)
base$escolaridade <- gsub("13", "Superior (incompleto ou mais)", base$escolaridade)
base$escolaridade <- gsub("14", "Superior (incompleto ou mais)", base$escolaridade)
base$escolaridade <- gsub("15", "Superior (incompleto ou mais)", base$escolaridade)
base$escolaridade <- gsub("5", "Fundamental (completo ou incompleto)", base$escolaridade)
base$escolaridade <- gsub("6", "Fundamental (completo ou incompleto)", base$escolaridade)
base$escolaridade <- gsub("7", "Fundamental (completo ou incompleto)", base$escolaridade)
base$escolaridade <- gsub("8", "Fundamental (completo ou incompleto)", base$escolaridade)
base$escolaridade <- gsub("2", "Sem instrução", base$escolaridade)
base$escolaridade <- gsub("3", "Sem instrução", base$escolaridade)
base$escolaridade <- gsub("4", "Sem instrução", base$escolaridade)

# table(is.na(base$escolaridade))
# FALSE   TRUE 
# 766360  69470 

#V4009
# Quantos trabalhos ... tinha na semana de ... a ... (semana de refer?ncia ?
# 1	Um 
# 2	Dois
# 3	Tr?s ou mais
# N?o aplic?vel
base$qtde_trabalhos <- base$V4009
base$qtde_trabalhos <- gsub("1", "Um", base$qtde_trabalhos)
base$qtde_trabalhos <- gsub("2", "Dois", base$qtde_trabalhos)
base$qtde_trabalhos <- gsub("3", "Três ou mais", base$qtde_trabalhos)

#V4016
# Na semana de ... a ... (semana de refer?ncia), quantos empregados trabalhavam nesse neg?cio/empresa que ... tinha ?	
# 1	1 a 5 empregados 
# 2	6 a 10 empregados
# 3	11 a 50 empregados
# 4	51 ou mais empregados
# N?o aplic?vel
base$empregados <- base$V4016
base$empregados <- gsub("1", "De 1 a 5 empregados", base$empregados)
base$empregados <- gsub("2", "De 6 a 10 empregados", base$empregados)
base$empregados <- gsub("3", "De 11 a 50 empregados", base$empregados)
base$empregados <- gsub("4", "De 51 ou mais empregados", base$empregados)

#V4017
# Na semana de ... a ... (semana de  refer?ncia), ... tinha pelo menos um s?cio que trabalhava nesse neg?cio/empresa ?	
# 1	Sim
# 2	N?o
# N?o aplic?vel
base$socios <- base$V4017
base$socios <- gsub("1","Sim", base$socios)
base$socios <- gsub("2","Não", base$socios)

#V40171
# Quantos socios
# 1	1 a 5 s?cios
# 2	6 ou mais s?cios
# N?o aplic?vel
base$qtde_socios <- base$V40171
base$qtde_socios <- gsub("1", "1 a 5 sócios", base$qtde_socios)
base$qtde_socios <- gsub("2", "6 ou mais sócios", base$qtde_socios)

# V4018
# Na semana de ... a ... (semana de refer?ncia), contando com ... , quantas pessoas trabalhavam nesse neg?cio/empresa ?	
# 1	1 a 5 pessoas
# 2	6 a 10 pessoas 
# 3	11 a 50 pessoas
# 4	51 ou mais pessoas
# N?o aplic?vel
base$pessoas <- base$V4018
base$pessoas <- gsub("1", "De 1 a 5 pessoas", base$pessoas)
base$pessoas <- gsub("2", "De 6 a 10 pessoas", base$pessoas)
base$pessoas <- gsub("3", "De 11 a 50 pessoas", base$pessoas)
base$pessoas <- gsub("4", "De 51 ou mais pessoas", base$pessoas)

# V4019
# Esse neg?cio/empresa era registrado no Cadastro Nacional da Pessoa Jur?dica - CNPJ?	
# 1	Sim
# 2	N?o
# N?o aplic?vel
base$cnpj <- base$V4019
base$cnpj <- gsub("1","Sim", base$cnpj)
base$cnpj <- gsub("2","Não", base$cnpj)

#V4020 - 
# Em que tipo de local funcionava esse neg?cio/empresa ?
# 1	Em loja, escrit?rio, galp?o, etc.
# 2	Em fazenda, s?tio, granja, ch?cara, etc.
# 3	N?o tinha estabelecimento para funcionar
# N?o aplic?vel
base$local_1 <- base$V4020
base$local_1 <- gsub("1","Em loja, escritório, galpão, etc.", base$local_1)
base$local_1 <- gsub("2","Em fazenda, sítio, granja, chácara, etc.", base$local_1)
base$local_1 <- gsub("3","Não tinha estabelecimento para funcionar", base$local_1)

#V4021
# ... exercia normalmente o trabalho em estabelecimento desse neg?cio/empresa ?
# 1	Sim
# 2	N?o
# N?o aplic?vel
base$local_2 <- base$V4021
base$local_2 <- gsub("1", "Sim", base$local_2)
base$local_2 <- gsub("2", "Não", base$local_2)

#V4022
# Ent?o onde ... exercia normalmente esse trabalho ?
# 1	Em estabelecimento de outro n?gocio/empresa
# 2	Em local designado pelo empregador, cliente ou fregu?s
# 3	Em domic?lio de empregador, patr?o, s?cio ou fregu?s
# 4	No domic?lio de resid?ncia, em local exclusivo para o desempenho da atividade
# 5	No domic?lio de resid?ncia, sem local exclusivo para o desempenho da atividade
# 6	Em ve?culo automotor (t?xi, ?nibus, caminh?o, autom?vel, embarca??o, etc.)
# 7	Em via ou ?rea p?blica (rua, rio, manguezal, mata p?blica, pra?a, praia etc.)
# 8	Em outro local, especifique
# N?o aplic?vel

base$local_3 <- base$V4022
base$local_3 <- gsub("1", "Em estabelecimento de outra empresa", base$local_3)
base$local_3 <- gsub("2", "Em local designado pelo cliente", base$local_3)
base$local_3 <- gsub("3", "Em domicílio de sócio ou cliente", base$local_3)
base$local_3 <- gsub("4", "No domicílio", base$local_3)
base$local_3 <- gsub("5", "No domicílio", base$local_3)
base$local_3 <- gsub("6", "Em veículo automotor", base$local_3)
base$local_3 <- gsub("7", "Em área ou via pública", base$local_3)
base$local_3 <- gsub("8", "Outro local", base$local_3)

# V403411
# N?mero da faixa do rendimento/retirada em dinheiro	
# 0	0
# 1	1 a [0,5SM]
# 2	[0,5SM]+1 a [1SM]
# 3	[1SM]+1 a [2SM]
# 4	[2SM]+1 a [3SM]
# 5	[3SM]+1 a [5SM]
# 6	[5SM]+1 a [10SM]
# 7	[10SM]+1 a [20SM]
# 8	[20SM]+1 ou mais
# N?o aplic?vel
base$rendimento <- paste("ZXW", base$V403411, sep='')
base$rendimento <- gsub("ZXW1", "Até 1 SM", base$rendimento)
base$rendimento <- gsub("ZXW2", "Até 1 SM", base$rendimento)
base$rendimento <- gsub("ZXW3", "De 1 SM a 2 SM", base$rendimento)
base$rendimento <- gsub("ZXW4", "De 2 SM a 3 SM", base$rendimento)
base$rendimento <- gsub("ZXW5", "De 3 SM a 5 SM", base$rendimento)
base$rendimento <- gsub("ZXW6", "Mais de 5 SM", base$rendimento)
base$rendimento <- gsub("ZXW7", "Mais de 5 SM", base$rendimento)
base$rendimento <- gsub("ZXW8", "Mais de 5 SM", base$rendimento)
base$rendimento <- gsub("ZXW0", "Até 1 SM", base$rendimento)
base$rendimento <- gsub("ZXWNA", NA, base$rendimento)

#V4039C
# Quantas horas ... trabalhou efetivamente na semana de refer?ncia nesse trabalho pincipal?	
# 000 a 120	Horas
# N?o aplic?vel

base$horas_trabalhadas <- cut(base$V4039C, c(0,14,40,45,49,200), right=FALSE)
base$horas_trabalhadas <- ifelse(base$horas_trabalhadas=="[0,14)", "Até 14 horas", 
                                 ifelse(base$horas_trabalhadas=="[14,40)", "De 14 até 40 horas", 
                                        ifelse(base$horas_trabalhadas=="[40,45)", "De 40 até 45 horas", 
                                               ifelse(base$horas_trabalhadas=="[45,49)", "De 45 até 49 horas", 
                                                      ifelse(base$horas_trabalhadas=="[49,200)", "De 49 horas ou mais", '---')))))

#V4040 
# At? o dia ... (?ltimo dia da semana de refer?ncia) fazia quanto tempo que ... estava nesse trabalho ?	
# 1	Menos de 1 m?s 
# 2	De 1 m?s a menos de 1 ano 
# 3	De 1 ano a menos de 2 anos 
# 4	2 anos ou mais 
# N?o aplic?vel
base$tempo_trabalho <- base$V4040
base$tempo_trabalho <- gsub("1","Menos de 1 mês", base$tempo_trabalho)
base$tempo_trabalho <- gsub("2","De 1 mês a menos de 1 ano", base$tempo_trabalho)
base$tempo_trabalho <- gsub("3","De 1 ano a menos de 2 anos", base$tempo_trabalho)
base$tempo_trabalho <- gsub("4","2 anos ou mais", base$tempo_trabalho)

# UF
# Regiao
base$regiao <- substr(base$UF, 1, 1)
base$regiao <- gsub('1', 'N', base$regiao)
base$regiao <- gsub('2', 'NE', base$regiao) 
base$regiao <- gsub('3', 'SE', base$regiao) 
base$regiao <- gsub('4', 'S', base$regiao) 
base$regiao <- gsub('5', 'CO', base$regiao) 

# VD4010
# Grupamentos de atividade principal do empreendimento do trabalho principal da semana de refer?ncia para pessoas de 14 anos ou mais de idade
# 01	Agricultura, pecu?ria, produ??o florestal, pesca e aquicultura 
# 02	Ind?stria geral
# 03	Constru??o
# 04	Com?rcio, repara??o de ve?culos automotores e motocicletas
# 05	Transporte, armazenagem e correio?
# 06	Alojamento e alimenta??o?
# 07	Informa??o, comunica??o e atividades financeiras, imobili?rias, profissionais e administrativas
# 08	Administra??o p?blica, defesa e seguridade social?
# 09	Educa??o, sa?de humana e servi?os sociais
# 10	Outros Servi?os
# 11	Servi?os dom?sticos
# 12	Atividades mal definidas
# N?o aplic?vel
base$setor <- base$VD4010
base$setor <- gsub('10', 'Serviços', base$setor) 
base$setor <- gsub('11', 'Serviços', base$setor) 
base$setor <- gsub('12', 'Outros setores', base$setor) 
base$setor <- gsub('1', 'Agropecuária', base$setor)
base$setor <- gsub('2', 'Indústria', base$setor) 
base$setor <- gsub('3', 'Construção', base$setor) 
base$setor <- gsub('4', 'Comércio', base$setor) 
base$setor <- gsub('5', 'Serviços', base$setor) 
base$setor <- gsub('6', 'Serviços', base$setor) 
base$setor <- gsub('7', 'Serviços', base$setor) 
base$setor <- gsub('8', 'Serviços', base$setor) 
base$setor <- gsub('9', 'Serviços', base$setor) 


base$segmento <- base$VD4010
base$segmento <- gsub('10', 'Outros segmentos', base$segmento) 
base$segmento <- gsub('11', 'Serviços domésticos', base$segmento) 
base$segmento <- gsub('12', 'Outros segmentos', base$segmento) 
base$segmento <- gsub('1', 'Agropecuária', base$segmento)
base$segmento <- gsub('2', 'Indústria', base$segmento) 
base$segmento <- gsub('3', 'Construção', base$segmento) 
base$segmento <- gsub('4', 'Comércio', base$segmento) 
base$segmento <- gsub('5', 'Transporte, armazenagem e correio', base$segmento) 
base$segmento <- gsub('6', 'Alojamento e alimentação', base$segmento) 
base$segmento <- gsub('7', 'Informação, comunicação, financeira, etc', base$segmento) 
base$segmento <- gsub('8', 'Adm. Publ. e outros', base$segmento) 
base$segmento <- gsub('9', 'Educação e saúde', base$segmento) 

# V4019
# Esse neg?cio/empresa era registrado no Cadastro Nacional da Pessoa Jur?dica - CNPJ?	
# 1	Sim
# 2	N?o
# N?o aplic?vel

# V4012
# Nesse trabalho, ... era: 
# 5	Empregador
# 6	Conta pr?pria

# VD4010
# Grupamentos de atividade principal do empreendimento do trabalho principal da semana de refer?ncia para pessoas de 14 anos ou mais de idade
# 01	Agricultura, pecu?ria, produ??o florestal, pesca e aquicultura 
# 02	Ind?stria geral
# 03	Constru??o
# 04	Com?rcio, repara??o de ve?culos automotores e motocicletas
# 05	Transporte, armazenagem e correio?
# 06	Alojamento e alimenta??o?
# 07	Informa??o, comunica??o e atividades financeiras, imobili?rias, profissionais e administrativas
# 08	Administra??o p?blica, defesa e seguridade social?
# 09	Educa??o, sa?de humana e servi?os sociais
# 10	Outros Servi?os
# 11	Servi?os dom?sticos
# 12	Atividades mal definidas
# N?o aplic?vel

base$cliente_2 <- ifelse(
  (base$V4019 == 1 | base$V4019 == 2) 
  & (base$V4012 == 5 | base$V4012 == 6) 
  & (base$VD4010 == 1), 'Produtor Rural', 
  ifelse((base$V4019 == 1) 
         & (base$V4012 == 5 | base$V4012 == 6) 
         & (base$VD4010 != 1), 'Empresário', 
         ifelse((base$V4019 == 2)
                & (base$V4012 == 5 | base$V4012 == 6)
                & (base$VD4010 != 1), 'Candidato a Empresário', '---')))

# V4032
# Era contribuinte de instituto de previd?ncia por esse trabalho ?
# 1	Sim
# 2	N?o
# N?o aplic?vel
base$previdencia <- base$V4032
base$previdencia <- gsub('1','Sim', base$previdencia)
base$previdencia <- gsub('2','Não', base$previdencia)  

# write.table(base, file="20181002-Base-PNAD-Tratada-42015-22018.txt", sep="\t", dec=".")

# base <- read.table("20181002-Base-PNAD-Tratada-42015-22018.txt", header=T, sep ='\t')
# 
base$V2005 <- NULL
base$V2007 <- NULL
base$V2009 <- NULL
base$V2010 <- NULL
base$V3009A <- NULL
base$V4009 <- NULL
base$V4012 <- NULL
base$V4016 <- NULL
base$V4017 <- NULL
base$V4018 <- NULL
base$V4019 <- NULL
base$V403411 <- NULL
base$V403412 <- NULL
base$V4039C <- NULL
base$V4040 <- NULL
base$V4020 <- NULL
base$V4021 <- NULL
base$V4022 <- NULL
base$V40171 <- NULL
base$VD4010 <- NULL
base$RM_RIDE <- NULL
base$V4032 <- NULL

base$UF <- gsub('12','AC', base$UF)
base$UF <- gsub('13','AM', base$UF)
base$UF <- gsub('14','RR', base$UF)
base$UF <- gsub('15','PA', base$UF)
base$UF <- gsub('16','AP', base$UF)
base$UF <- gsub('17','TO', base$UF)
base$UF <- gsub('21','MA', base$UF)
base$UF <- gsub('22','PI', base$UF)
base$UF <- gsub('23','CE', base$UF)
base$UF <- gsub('24','RN', base$UF)
base$UF <- gsub('25','PB', base$UF)
base$UF <- gsub('26','PE', base$UF)
base$UF <- gsub('27','AL', base$UF)
base$UF <- gsub('28','SE', base$UF)
base$UF <- gsub('29','BA', base$UF)
base$UF <- gsub('31','MG', base$UF)
base$UF <- gsub('32','ES', base$UF)
base$UF <- gsub('33','RJ', base$UF)
base$UF <- gsub('35','SP', base$UF)
base$UF <- gsub('41','PR', base$UF)
base$UF <- gsub('42','SC', base$UF)
base$UF <- gsub('43','RS', base$UF)
base$UF <- gsub('50','MS', base$UF)
base$UF <- gsub('51','MT', base$UF)
base$UF <- gsub('52','GO', base$UF)
base$UF <- gsub('53','DF', base$UF)
base$UF <- gsub('11','RO', base$UF)

names(base)[names(base)=='V1028'] <- 'peso_amostral'

write.table(base, file="PNADContinua-Base-Tratada-012020-022021-Cliente_padrao.txt", sep="\t", dec= ',', quote = F, row.names=F, na='')

library(writexl)
write_xlsx(base, "PNADContinua-Base-Tratada-012020-022021-Cliente_padrao.xlsx")
