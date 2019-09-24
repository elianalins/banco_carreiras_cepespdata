#script para análise dos candidatos com título eleitoral missing, e avaliação de um identificador substituto 

#limpando
rm(list = ls())

#pacotes
library(tidyverse)
library(haven)
library(stringr)
library(stringi)

#wds usados
wd_marina <- "C:\\Users\\marin\\Dropbox\\CEPESP\\"
wd_gabi <- "C:\\Users\\gabri\\Dropbox\\"
wd_cloud <- "\\\\fs-eesp-01/eesp-bd-02/BASES-CEPESP/Camara/Scott/WorkingDeputiesR"

setwd(wd_marina)

#df com a relação de meses pra uniformizar as datas de nascimento depois
mesnum <- data.frame(mes = c("JUN","JAN","SEP","AUG","FEB","MAY","DEC","JUL","APR","NOV","OCT","MAR"),
                     mes2 = c("06","01","09","08","02","05","12","07","04","11","10","03"))


#banco criado pelo cepesdata:
WorkingDeputies19982018 <- readRDS("CFIncumbency/Data/WorkingDeputies_R/Codigos_R_Base0/data/base_final.rds") %>%
  filter(num_titulo_eleitoral_candidato != "000000000000") %>% #tirando os votos em legenda
  mutate(data_nasc = data_nascimento) %>%
  separate(data_nasc,c("dia","mes","ano")) %>%
  mutate(ano = ifelse(str_length(ano)==2, paste0("19",ano),ano)) %>%
  left_join(mesnum) %>%
  mutate(mes2 = ifelse(is.na(mes2), mes,mes2),
         mes2 = ifelse(str_length(mes2) == 2,mes2,paste0("0",mes2))) %>%
  mutate(data_nascimento2 = ifelse(is.na(ano),data_nascimento,paste0(dia,mes2,ano))) %>%
  mutate(temcpf = ifelse(str_detect(cpf_candidato,"#N"),0,1), #indica se tem cpf missing
         temniver = ifelse((str_length(data_nascimento2) > 6),1,0), #indica quem tem data de aniversário completa
         ano_nasc = as.numeric(str_sub(data_nascimento2, start= -4)),
         idade_eleicao = ano_eleicao - as.numeric(ano_nasc),
         primeironome = word(nome_candidato, 1),
         primeironome = tolower(stri_trans_general(primeironome, "Latin-ASCII")),
         data_nascimento2 = ifelse(str_length(data_nascimento2) == 7, paste0("0",data_nascimento2), data_nascimento2),
         id_cand2 = paste0(primeironome,data_nascimento2,codigo_sexo,sigla_uf_nascimento), #id alternativo
         temtitulo = ifelse(str_detect(num_titulo_eleitoral_candidato,"#N"),0,1),
         nchartitulo = nchar(num_titulo_eleitoral_candidato),
         tituloincompleto = ifelse(temtitulo ==1 & nchartitulo < 12, 1,0))

unicos <- WorkingDeputies19982018 %>%
 select(id_cand2,num_titulo_eleitoral_candidato,cpf_candidato,nome_candidato,temtitulo,temcpf,temniver,nchartitulo,tituloincompleto,ano_eleicao) %>%
  filter(temtitulo == 0) %>%
  distinct() %>%
  arrange(id_cand2)
  


# #recuperando como estão os missing na base
# 
# #títulos
# titulomissing <- WorkingDeputies19982018 %>%
#   group_by(num_titulo_eleitoral_candidato,cpf_candidato) %>%
#   #filter(codigo_cargo == 6) %>%
#   summarise(n = n())
# #os títulos que estão nulos o codigo é 00000000#NI# 
# 
# #cpf
# cpfmissing <- WorkingDeputies19982018 %>%
#   group_by(num_titulo_eleitoral_candidato,cpf_candidato) %>%
#   #filter(codigo_cargo == 6) %>%
#   summarise(n = n())
# #os cpf que estão nulos o codigo é 00000#NULO#



# #vendo quantos desses tem CPF nulo, além do título nulo
# WorkingDeputies19982018 %>%
#   filter(str_detect(num_titulo_eleitoral_candidato,"#N"))%>%
#   filter(str_detect(cpf_candidato,"#N")) %>%
#   nrow()
# # são 278
# 
# table(missings$descricao_sexo)
# 
# #vendo quantos desses tem CPF nulo e sem a data de aniversário válida
# WorkingDeputies19982018 %>%
#   filter(str_detect(num_titulo_eleitoral_candidato,"#N")) %>%
#   filter(str_detect(cpf_candidato,"#N")) %>%
#   mutate(ano_nascimento = as.numeric(str_sub(data_nascimento, start= -4)),
#          idade_eleicao = ano_eleicao - ano_nascimento) %>%
#   filter(str_length(data_nascimento) < 7 | is.na(data_nascimento) | idade_eleicao < 19) %>%
#   nrow()
# 90 casos dos 278 sem CPF serão descartados por falta de informação do aniversário



#lista de quem tem o título faltando, porém tem CPF
#será usado para recuperar as informações de anos posteriores, caso tenham se recandidatado 
cpf <- WorkingDeputies19982018 %>%
  filter(temcpf ==1 & temniver ==1 & temtitulo ==0 & idade_eleicao > 17) %>%
  select(cpf_candidato) %>%
  distinct()

  
#lista de quem não tem CPF, então recuperamos os anos seguintes 
id_cand2mis <- WorkingDeputies19982018 %>%
  filter(temcpf ==0 & temniver ==1 & temtitulo ==0 & idade_eleicao > 17) %>%
  select(id_cand2) %>%
  distinct()
#10 via id_cand2


#recuperando o título eleitoral de eleições posteriores pelo cpf
   reaparece_cpf <- WorkingDeputies19982018 %>%
     semi_join(cpf) %>%
     filter(temtitulo == 1)  %>%
     mutate(num_titulo_eleitoral_candidato2 = num_titulo_eleitoral_candidato) %>%
     select(cpf_candidato,num_titulo_eleitoral_candidato2) %>%
     distinct()
   
   #recuperando o título eleitoral de eleições posteriores pelo id alternativo
   reaparece_idcand <- WorkingDeputies19982018 %>%
     semi_join(id_cand2mis) %>%
     filter(temtitulo == 1) %>%
     mutate(num_titulo_eleitoral_candidato3 = num_titulo_eleitoral_candidato,
            cpf_candidato2 = cpf_candidato) %>%
     select(id_cand2,num_titulo_eleitoral_candidato3,cpf_candidato2) %>%
     distinct()

   
#incluindo o titulo de eleitor recuperado:   
WorkingDeputies19982018 <- WorkingDeputies19982018 %>%
  left_join(reaparece_cpf,by = "cpf_candidato") %>%
  left_join(reaparece_idcand,by = c("id_cand2")) %>%
  #fazendo o id_cand, dando preferência para o título (inclusive recuperado),CPF e então o id_cand2
  mutate(num_titulo_eleitoral_candidato = ifelse(temtitulo ==0,num_titulo_eleitoral_candidato2, num_titulo_eleitoral_candidato),
         num_titulo_eleitoral_candidato = ifelse(is.na(num_titulo_eleitoral_candidato), num_titulo_eleitoral_candidato3,num_titulo_eleitoral_candidato),
         cpf_candidato = ifelse(temcpf ==0,cpf_candidato2,cpf_candidato),
         id_cand = num_titulo_eleitoral_candidato,
         id_cand = ifelse(is.na(id_cand), cpf_candidato, id_cand),
         id_cand = ifelse(is.na(id_cand),id_cand2,id_cand))

WorkingDeputies19982018 <- WorkingDeputies19982018 %>%
  mutate(origem_id_cand = ifelse(id_cand == num_titulo_eleitoral_candidato,
                                 "titulo",
                                 ifelse(id_cand == cpf_candidato, 
                                        "cpf",
                                        ifelse(id_cand == id_cand2, "idalternativo"))))


ids <- WorkingDeputies19982018 %>%
  filter(temtitulo == 0) %>%
  group_by(num_titulo_eleitoral_candidato2,num_titulo_eleitoral_candidato3) %>%
  summarise(n=n())

sum(ids$n)

WorkingDeputies19982018 %>%
  filter(temtitulo ==0 & temcpf ==0) %>%
  select(id_cand2) %>%
  distinct() %>%
  nrow()
   
 

   



