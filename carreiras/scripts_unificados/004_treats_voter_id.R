#### CANDIDATES WITHOUT VOTER ID ####
df<-readRDS('df.rds') # importing data.
nrow(df) # 3419625

# summary(as.factor(df$temtitulo)) # 1224 without voter ID.
# table(as.factor(df$temcpf),as.factor(df$temtitulo)) 
# 297 without national ID and without voter ID, simultaneously.
# 12985 without national ID, with voter ID.
# 924 without voter ID, with national ID.

# candidates without voter ID, with National ID (CPF) ----
## we will rescue Voter ID from other elections if possible:
cpf<-df %>% # creating data.frame with key joining National ID and Voter ID.
  filter(temcpf == 1 & temtitulo == 1) %>%
  select(CPF_CANDIDATO,NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  distinct()

cpf<-cpf %>%
  mutate(flagCPF = duplicated(CPF_CANDIDATO),
         flagTIT = duplicated(NUM_TITULO_ELEITORAL_CANDIDATO))

# Percentage of duplicated IDs:
sum(cpf$flagCPF==T)/nrow(cpf) # 0.329% of national IDs
sum(cpf$flagTIT==T)/nrow(cpf) # 0.317% of voter IDs


# We are going to test if those duplicated are winners:
only_elected<-df %>%
  filter(DESC_SIT_TOT_TURNO=='2º TURNO'|
         DESC_SIT_TOT_TURNO=='ELEITO POR QP'|
         DESC_SIT_TOT_TURNO=='ELEITO'|
         DESC_SIT_TOT_TURNO== 'ELEITO POR MEDIA') %>%
  filter(temcpf == 1 & temtitulo == 1) %>%
  select(CPF_CANDIDATO,NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  distinct() %>%
  mutate(flagCPF = duplicated(CPF_CANDIDATO),
         flagTIT = duplicated(NUM_TITULO_ELEITORAL_CANDIDATO))

# Percentage of duplicated IDs among elected:
sum(only_elected$flagCPF==T)/nrow(only_elected) # 0.295852% of national IDs
sum(only_elected$flagTIT==T)/nrow(only_elected) # 0.244% of voter IDs


# We are dropping all these observations with duplicated IDs from our auxiliar data.frame before the joins:
cpf<-cpf %>% 
  group_by(CPF_CANDIDATO) %>%
  mutate(flagCPF = sum(flagCPF)) %>% # flagCPF displays 1 if there is another Voter ID for the same National ID.
  ungroup() %>%
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  mutate(flagTIT = sum(flagTIT)) %>% # flagTIT displays 1 if there is another National ID for the same Voter ID.
  ungroup() %>%
  filter(flagTIT == 0 & flagCPF == 0) # deleting all those cases in which there is another voter ID 
#for the same national ID and vice-versa.


# Using this df with national ID and voter ID to recue voter IDs:
no_voter_ID <- df %>%
  filter(temcpf == 1 & temtitulo == 0) %>% # candidates with correct national ID, but without voter ID. (927 obs. before join)
  left_join(cpf, 'CPF_CANDIDATO') # 927 observations after join

no_voter_ID$NUM_TITULO_ELEITORAL_CANDIDATO <- no_voter_ID$NUM_TITULO_ELEITORAL_CANDIDATO.y # replacing voter ID with 
#the ones rescued with the alternative ID. 

df<-df[!(df$temcpf == 1 & df$temtitulo == 0),] # dropping obs. without voter ID but 
# with national ID from master df.
# We will bind it again below already joined with cpf data.frame:
df <- no_voter_ID %>%
  select( colnames(df) ) %>% # ordering columns to have the same as df.
  rbind(df) # pilling up with df.

rm(no_voter_ID) # removing auxiliar data.frames.

# Candidates with voter ID, without national ID ----
no_nat_id <- filter(df, temtitulo == 1, temcpf == 0) # Filtering obs without national ID that have voter ID. (12985 obs.)
no_nat_id <- left_join(no_nat_id, cpf, 'NUM_TITULO_ELEITORAL_CANDIDATO') # Rescues national ID from other elections. (12985 obs.)
no_nat_id$CPF_CANDIDATO <- no_nat_id$CPF_CANDIDATO.y # replacing national ID with the one coming from the previous join.

df<-df[!(df$temcpf == 0 & df$temtitulo == 1),] # dropping obs. without national ID but 
# with voter ID from master df.
# We will bind it again below already joined with cpf data.frame:
df<-no_nat_id %>% 
  select( colnames(df) ) %>% # ordering columns to have the same as df.
  rbind(df) # pilling up with df.

summary(as.numeric(df$CPF_CANDIDATO)) # 8193 NAs left. We've rescued ~ 4 thousands.

rm(no_nat_id,cpf) # removing auxiliar data.frames.

# Candidates without voter ID and national ID ----
summary(as.numeric(df$NUM_TITULO_ELEITORAL_CANDIDATO)) # 845 NAs left
# Let's try to rescue them using another alternative voter ID based on
# first name, year of birth, state of birth and sex:
df <- df %>%
  mutate(id_cand2 = paste0(primeironome,DATA_NASCIMENTO,CODIGO_SEXO,SIGLA_UF_NASCIMENTO)) # defining alternative candidate ID.

## Recovering voter ID:
no_voter_ID <- df %>% # filtering only candidates without voter ID. (845 obs.)
  filter(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==T)

with_voter_ID <- df %>% # filtering only candidates with voter ID. (1506952 obs.)
  filter(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==F) %>%
  select(id_cand2,NUM_TITULO_ELEITORAL_CANDIDATO) %>%
  distinct()

no_voter_ID <- no_voter_ID %>% # join using alternative ID.
  left_join(with_voter_ID, 'id_cand2') # (868 obs.) 
### CHECAR O QUE ESTÁ DUPLICANDO AQUI EM CIMA

sum(is.na(no_voter_ID$NUM_TITULO_ELEITORAL_CANDIDATO.y)==F) # 138 remaining NA voter IDs.

no_voter_ID$NUM_TITULO_ELEITORAL_CANDIDATO <- no_voter_ID$NUM_TITULO_ELEITORAL_CANDIDATO.y # replacing voter ID with 
#the ones rescued with the alternative ID. 

df <- df %>% 
  filter(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==F) # Dropping obs. without voter ID from master data.frame.
# we will reinclude them with rescued ID below:
df<-no_voter_ID %>%
  select(colnames(df))%>%
  rbind(df)

summary(is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO)) # 363 candidates without voter ID.
rm(no_voter_ID,with_voter_ID)


## Recovering National ID with alternative ID:
no_nat_ID <- df %>% # filtering only candidates without national ID.
  filter(is.na(CPF_CANDIDATO)==T)

with_nat_ID <- df %>% # filtering only candidates with national ID. (1506952 obs.)
  filter(is.na(CPF_CANDIDATO)==F) %>%
  select(id_cand2,CPF_CANDIDATO) %>%
  distinct()

no_nat_ID <- no_nat_ID %>% # join using alternative ID.
  left_join(with_nat_ID, 'id_cand2')

sum(is.na(no_nat_ID$CPF_CANDIDATO.y)==F) # we've recovered 1586580 (out of 1589327) national IDs.

no_nat_ID$CPF_CANDIDATO <- no_nat_ID$CPF_CANDIDATO.y # replacing nationa ID with 
#the ones rescued with the alternative ID. 

df <- df %>% 
  filter(is.na(CPF_CANDIDATO)==F) # Dropping obs. without national ID from master data.frame.
# we will reinclude them with rescued ID below:
df<-no_nat_ID %>%
  select(colnames(df))%>%
  rbind(df)

summary(is.na(df$CPF_CANDIDATO)) # 2747 candidates without national ID.


df %>% select(id_cand2,ANO_ELEICAO, NUM_TURNO) %>% distinct() %>% nrow() # 1448780 rows. We clearly have
# duplicated observations in the join.


#### Saving Data with identified candidates ####
saveRDS(df, 'df_identified.rds')
rm(list=ls())
