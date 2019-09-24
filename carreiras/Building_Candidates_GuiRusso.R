#### Guilherme Russo
### Creating a dataset of candidates for party list formation analysis
## February 21, 2019

# Clearing R
rm(list=ls()); cat("\014")

# Working directory
#setwd("/Users/robertaazzi1/Dropbox (Personal)/CEPESP/Post-Doc")
setwd("C:/Users/guilherme.russo/Desktop")

# Packages
packages<-c("cepespR"); lapply(packages, require, character.only=T)

vars<-c("ANO_ELEICAO", "SIGLA_UE", "CODIGO_CARGO", "NUMERO_CANDIDATO", "CPF_CANDIDATO",
        "COD_SITUACAO_CANDIDATURA", "CODIGO_OCUPACAO", "DATA_NASCIMENTO",
        "NUM_TITULO_ELEITORAL_CANDIDATO", "CODIGO_SEXO", "COD_GRAU_INSTRUCAO",
        "CODIGO_ESTADO_CIVIL", "COD_SIT_TOT_TURNO", "QTDE_VOTOS", "NUM_TURNO")

###################
#### President ####
###################
dados<-list(NULL) # creating an empty list
anos<-seq(1998, 2018, 4) # listing the years for state and federal elections
for(d in 1:length(anos)){ # starting the loop
  dados[[d]]<-as.data.frame(get_elections(anos[d], "President", regional_aggregation="Brazil", # requesting presidential candidates
                                           political_aggregation="Candidate", columns_list=vars))
  dados[[d]]<-subset(dados[[d]], dados[[d]]$NUM_TURNO==1 & !duplicated(dados[[d]]$NUM_TITULO_ELEITORAL_CANDIDATO)) # dropping duplicates and repeated candidates in the second round
}
data<-do.call(rbind, dados); nrow(data); head(data) # appending individual dfs as one binded df
data$NUM_TITULO_ELEITORAL_CANDIDATO<-as.character(data$NUM_TITULO_ELEITORAL_CANDIDATO) # changing from factor to character
data$ANO_UF<-paste0(data$ANO_ELEICAO, data$SIGLA_UE) # Creating an identifier for election

names(data) # Looking at the names of vars in dataset
data$ANO_ELEICAO<-NULL; data$SIGLA_UE<-NULL; data$NUM_TURNO<-NULL # dropping 3 vars

nrow(data) # number of observations in this office dataset
df<-data; data<-NULL
nrow(df)

##################
#### Governor ####
##################
dados<-list(NULL)
for(d in 1:length(anos)){
  dados[[d]]<-as.data.frame(get_elections(anos[d], "Governor", regional_aggregation="State", 
                                          political_aggregation="Candidate", columns_list=vars))
  dados[[d]]<-subset(dados[[d]], dados[[d]]$NUM_TURNO==1 & !duplicated(dados[[d]]$NUM_TITULO_ELEITORAL_CANDIDATO))
}
data<-do.call(rbind, dados); nrow(data); head(data)
data$NUM_TITULO_ELEITORAL_CANDIDATO<-as.character(data$NUM_TITULO_ELEITORAL_CANDIDATO)
data$ANO_UF<-paste0(data$ANO_ELEICAO, data$SIGLA_UE)

names(data)
data$ANO_ELEICAO<-NULL; data$SIGLA_UE<-NULL; data$NUM_TURNO<-NULL

nrow(data); nrow(df)
df<-rbind(df, data); data<-NULL
nrow(df)

#################
#### Senator ####
#################
dados<-list(NULL)
for(d in 1:length(anos)){
  dados[[d]]<-as.data.frame(get_elections(anos[d], "Senator", regional_aggregation="State", 
                                          political_aggregation="Candidate", columns_list=vars))
  dados[[d]]<-subset(dados[[d]], !duplicated(dados[[d]]$NUM_TITULO_ELEITORAL_CANDIDATO))
}
data<-do.call(rbind, dados); nrow(data); head(data)
data$NUM_TITULO_ELEITORAL_CANDIDATO<-as.character(data$NUM_TITULO_ELEITORAL_CANDIDATO)
data$ANO_UF<-paste0(data$ANO_ELEICAO, data$SIGLA_UE)

names(data)
data$ANO_ELEICAO<-NULL; data$SIGLA_UE<-NULL; data$NUM_TURNO<-NULL

nrow(data); nrow(df)
df<-rbind(df, data); data<-NULL
nrow(df)

########################
#### Federal Deputy ####
########################
dados<-list(NULL)
for(d in 1:length(anos)){
  dados[[d]]<-as.data.frame(get_elections(anos[d], "Federal Deputy", regional_aggregation="State", 
                                          political_aggregation="Candidate", columns_list=vars))
  dados[[d]]<-subset(dados[[d]], dados[[d]]$NUMERO_CANDIDATO>99 & !duplicated(dados[[d]]$NUM_TITULO_ELEITORAL_CANDIDATO))
}
data<-do.call(rbind, dados); nrow(data); head(data)
data$NUM_TITULO_ELEITORAL_CANDIDATO<-as.character(data$NUM_TITULO_ELEITORAL_CANDIDATO)
data$ANO_UF<-paste0(data$ANO_ELEICAO, data$SIGLA_UE)

names(data)
data$ANO_ELEICAO<-NULL; data$SIGLA_UE<-NULL; data$NUM_TURNO<-NULL

nrow(data); nrow(df)
df<-rbind(df, data); data<-NULL
nrow(df)

######################
#### State Deputy ####
######################
dados<-list(NULL)
for(d in 1:length(anos)){
  dados[[d]]<-as.data.frame(get_elections(anos[d], "State Deputy", regional_aggregation="State", 
                                          political_aggregation="Candidate", columns_list=vars))
  dados[[d]]<-subset(dados[[d]], dados[[d]]$NUMERO_CANDIDATO>99 &
                       !duplicated(dados[[d]]$NUM_TITULO_ELEITORAL_CANDIDATO) &
                                     dados[[d]]$SIGLA_UE!="DF")
  }
data<-do.call(rbind, dados); nrow(data); head(data)
data$NUM_TITULO_ELEITORAL_CANDIDATO<-as.character(data$NUM_TITULO_ELEITORAL_CANDIDATO)
data$ANO_UF<-paste0(data$ANO_ELEICAO, data$SIGLA_UE)

names(data)
data$ANO_ELEICAO<-NULL; data$SIGLA_UE<-NULL; data$NUM_TURNO<-NULL

nrow(data); nrow(df)
df<-rbind(df, data); data<-NULL
nrow(df)

###################################################################
#### Deputado Distrital ("State" Deputy in the Federal District) ####
###################################################################
dados<-list(NULL)

for(d in 1:length(anos)){
  dados[[d]]<-as.data.frame(get_candidates(anos[d], "Deputado Distrital"))
  dados[[d]]<-subset(dados[[d]], dados[[d]]$NUMERO_CANDIDATO>99 & !duplicated(dados[[d]]$NUM_TITULO_ELEITORAL_CANDIDATO))
  temp<-as.data.frame(get_votes(anos[d], "Deputado Distrital"))
  temp<-subset(temp, temp$NUMERO_CANDIDATO>99 & !duplicated(temp$NUMERO_CANDIDATO))
  dados[[d]]<-merge(dados[[d]], temp[, c("NUMERO_CANDIDATO", "QTDE_VOTOS")], by="NUMERO_CANDIDATO")
  dados[[d]]<-dados[[d]][, vars]
}
data<-do.call(rbind, dados); nrow(data); head(data)
data$NUM_TITULO_ELEITORAL_CANDIDATO<-as.character(data$NUM_TITULO_ELEITORAL_CANDIDATO)
data$ANO_UF<-paste0(data$ANO_ELEICAO, data$SIGLA_UE)

names(data)
data$ANO_ELEICAO<-NULL; data$SIGLA_UE<-NULL; data$NUM_TURNO<-NULL

nrow(data); nrow(df)
df<-rbind(df, data); data<-NULL
nrow(df)

###############
#### Mayor ####
###############
dados<-list(NULL)
anos<-seq(2000, 2016, 4)
for(d in 1:length(anos)){
  dados[[d]]<-as.data.frame(get_elections(anos[d], "Mayor", regional_aggregation="Municipality", 
                                          political_aggregation="Candidate", columns_list=vars))
  dados[[d]]<-subset(dados[[d]], dados[[d]]$NUM_TURNO==1 & !duplicated(dados[[d]]$NUM_TITULO_ELEITORAL_CANDIDATO))
}
data<-do.call(rbind, dados); nrow(data); head(data)
data$ANO_UF<-paste0(data$ANO_ELEICAO, data$SIGLA_UE)
data$NUM_TITULO_ELEITORAL_CANDIDATO<-as.character(data$NUM_TITULO_ELEITORAL_CANDIDATO)

data$ANO_ELEICAO<-NULL; data$SIGLA_UE<-NULL; data$NUM_TURNO<-NULL

nrow(data); nrow(df)
df<-rbind(df, data); data<-NULL
nrow(df)

#####################
#### Councillors ####
#####################
dados<-list(NULL)
for(e in 1:length(anos)){
  start<-Sys.time()
  dados[[e]]<-as.data.frame(
    get_elections(year=anos[e], "Councillor", regional_aggregation="Municipality", 
                  political_aggregation="Candidate", columns_list=vars))
  dados[[e]]<-subset(dados[[e]], !duplicated(dados[[e]]$NUM_TITULO_ELEITORAL_CANDIDATO) 
                     & dados[[e]]$NUMERO_CANDIDATO>99 & dados[[e]]$NUM_TURNO==1) # bc of the messy 2000 df
  dados[[e]]$ANO_UF<-paste0(dados[[e]]$ANO_ELEICAO, dados[[e]]$SIGLA_UE)
  dados[[e]]$ANO_ELEICAO<-NULL; dados[[e]]$SIGLA_UE<-NULL 
  print(e); print(Sys.time()-start)
}

data<-do.call(rbind, dados); nrow(data); head(data)
data$ANO_ELEICAO<-NULL; data$SIGLA_UE<-NULL; data$NUM_TURNO<-NULL

nrow(data); nrow(df)
df<-rbind(df, data); data<-NULL
nrow(df)

####################################################################################

table(df$CODIGO_CARGO, substr(df$ANO_UF, 1, 4))

table(is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO), substr(df$ANO_UF, 1, 4))

table(df$CODIGO_CARGO[
  is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO)
])

#table(df$CODIGO_CARGO[
#  is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO) & df$ANO_ELEICAO==2010
#  ])

#table(df$CODIGO_CARGO[
#  is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO) & df$ANO_ELEICAO==2014
#  ])

#table(df$NUMERO_CANDIDATO[
#  is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO) & df$ANO_ELEICAO==2018
#  ])

# Saving the dataset
write.csv(df, "Data/Candidates_DB.csv", row.names=F)
