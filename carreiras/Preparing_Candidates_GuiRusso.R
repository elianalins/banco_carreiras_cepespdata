#### Guilherme Russo
### Creating a dataset of candidates for party list formation analysis
## February 21, 2019

# Clearing R
rm(list=ls()); cat("\014")

# Working directory
# setwd("/Users/robertaazzi1/Dropbox (Personal)/CEPESP/Post-Doc/Data/")
setwd("C:/Users/guilherme.russo/Documents")

# Opening the dataset with candidates to all offices
data<-read.csv("Candidates_DB.csv")
nrow(data); head(data)

table(data$CODIGO_CARGO)

table(is.na(data$NUM_TITULO_ELEITORAL_CANDIDATO))
table(is.na(data$CPF_CANDIDATO))

table(duplicated(data$NUM_TITULO_ELEITORAL_CANDIDATO)) 
table(duplicated(data$NUM_TITULO_ELEITORAL_CANDIDATO), duplicated(data$CPF_CANDIDATO))
prop.table(table(duplicated(data$NUM_TITULO_ELEITORAL_CANDIDATO), 
                 duplicated(data$CPF_CANDIDATO)))
10900/984272 # Roughly 1% of duplicated responses are not identical across Título and CPF, I am going to use Título as the correct id
# When looking only at the duplicated that are not identical across Título, the percentage drops to .4% (hence, .6% for CPF)

data$ANO_ELEICAO<-as.numeric(substr(data$ANO_UF, 1, 4))

data$ele1998<-ifelse(data$ANO_ELEICAO==1998, 1, 0)
data$ele2000<-ifelse(data$ANO_ELEICAO==2000, 1, 0)
data$ele2002<-ifelse(data$ANO_ELEICAO==2002, 1, 0)
data$ele2004<-ifelse(data$ANO_ELEICAO==2004, 1, 0)
data$ele2006<-ifelse(data$ANO_ELEICAO==2006, 1, 0)
data$ele2008<-ifelse(data$ANO_ELEICAO==2008, 1, 0)
data$ele2010<-ifelse(data$ANO_ELEICAO==2010, 1, 0)
data$ele2012<-ifelse(data$ANO_ELEICAO==2012, 1, 0)
data$ele2014<-ifelse(data$ANO_ELEICAO==2014, 1, 0)
data$ele2016<-ifelse(data$ANO_ELEICAO==2016, 1, 0)
data$ele2018<-ifelse(data$ANO_ELEICAO==2018, 1, 0)

table(data$ele1998, data$ele2000)
# No overlapping, which is what we want

lapply(data, class)
data$NUM_TITULO_ELEITORAL_CANDIDATO<-as.character(data$NUM_TITULO_ELEITORAL_CANDIDATO)

names(data)
vars_orig<-c("CODIGO_CARGO", "NUMERO_CANDIDATO", "COD_SITUACAO_CANDIDATURA", 
        "CODIGO_OCUPACAO", "COD_GRAU_INSTRUCAO", "CODIGO_ESTADO_CIVIL",
        "COD_SIT_TOT_TURNO", "QTDE_VOTOS", "ANO_UF")
years<-seq(1998, 2018, 2)
vars<-as.data.frame(expand.grid(vars_orig, years))
vars<-paste(vars$Var1, vars$Var2, sep="_")

df<-as.data.frame(
  matrix(NA, ncol=length(vars), nrow=length(unique(data$NUM_TITULO_ELEITORAL_CANDIDATO)))
); colnames(df)<-vars

df$NUM_TITULO_ELEITORAL_CANDIDATO<-unique(data$NUM_TITULO_ELEITORAL_CANDIDATO)
table(is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO))
df<-subset(df, !is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO))

start<-Sys.time()
for(i in df$NUM_TITULO_ELEITORAL_CANDIDATO){
  temp<-subset(data, data$NUM_TITULO_ELEITORAL_CANDIDATO==i)
ano<-1998  
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele1998==1, vars_orig]
}

ano<-2000  
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2000==1, vars_orig]
}

ano<-2002  
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2002==1, vars_orig]
}

ano<-2004  
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2004==1, vars_orig]
}

ano<-2006
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2006==1, vars_orig]
}

ano<-2008  
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2008==1, vars_orig]
}

ano<-2010  
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2010==1, vars_orig]
}

ano<-2012  
if(any(temp$ANO_ELEICAO==ano)){
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2012==1, vars_orig]
}

ano<-2014  
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2014==1, vars_orig]
}

ano<-2016  
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2016==1, vars_orig]
}

ano<-2018
if(any(temp$ANO_ELEICAO==ano)){
  df[df$NUM_TITULO_ELEITORAL_CANDIDATO==i, 
     paste(vars_orig, ano, sep="_")][1,]<-
    temp[temp$ele2018==1, vars_orig]
}
}
Sys.time()-start

write.csv(df, "DB_Candidates_Wide.csv", row.names=F)

##################################################
# Adding the birthdate and sex of the candidates #
##################################################

tab<-subset(data, !is.na(data$NUM_TITULO_ELEITORAL_CANDIDATO) & 
         !duplicated(data$NUM_TITULO_ELEITORAL_CANDIDATO))
tab<-tab[,c("NUM_TITULO_ELEITORAL_CANDIDATO", "DATA_NASCIMENTO", 
            "CODIGO_SEXO")]
names(df); nrow(df)
df2<-merge(df, tab, by="NUM_TITULO_ELEITORAL_CANDIDATO")
names(df2)

write.csv(df2, "DB_Candidates_Wide_complete.csv", row.names=F)

##################################################

