#### TREATING CANDIDATES WITHOUT VOTER ID ####
# Eliana Lins Morandi, CepespData/FGV, sept.2019 #

df<-readRDS('df.rds') # importing data.
nrow(df) # 3419625

# summary(as.factor(df$temtitulo)) # 1224 without voter ID.
# table(as.factor(df$temcpf),as.factor(df$temtitulo)) 
# 297 without national ID and without voter ID, simultaneously.
# 12985 without national ID, with voter ID.
# 924 without voter ID, with national ID.

#### 1. candidates without voter ID, with National ID (CPF) ####
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

# Percentage of elected among missing IDs:
sum(only_elected$flagCPF==T)/sum(cpf$flagCPF==T) # national IDs: 12.10%
sum(only_elected$flagTIT==T)/sum(cpf$flagTIT==T) # voter IDs: 10.38%

# Percentage of elected among no missing IDs:
sum(only_elected$flagCPF==F)/sum(cpf$flagCPF==F) # national IDs: 13.5%
sum(only_elected$flagTIT==F)/sum(cpf$flagTIT==F) # voter IDs: 13.4%


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

#### 2. Candidates with voter ID, without national ID ####
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

rm(no_nat_id,cpf,only_elected) # removing auxiliar data.frames.

#### 3. Candidates without voter ID and national ID ####
summary(as.numeric(df$NUM_TITULO_ELEITORAL_CANDIDATO)) # 845 NAs left
# Let's try to rescue them using another alternative voter ID based on
# first name, year of birth, state of birth and sex:
df <- df %>%
  mutate(id_cand2 = paste0(primeironome,DATA_NASCIMENTO,CODIGO_SEXO,SIGLA_UF_NASCIMENTO)) # defining alternative candidate ID.

## Recovering voter ID:
no_voter_ID <- df %>% # filtering only candidates without voter ID. (845 obs.)
  filter(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==T)

with_voter_ID <- df %>% 
  filter(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==F) %>% # filtering only candidates with voter ID. (1506952 obs.)
  select(id_cand2,NUM_TITULO_ELEITORAL_CANDIDATO) %>% # selecting alternative ID and voter ID variables only.
  distinct() %>% # removing duplicated observations of the pair <alternativeID-voterID>.
  # But we still might have cases in which there is one alternative ID corresponding to more than one voter ID and vice-versa.
  # Let's remove these from our data.frame:
  mutate(flagTIT = duplicated(NUM_TITULO_ELEITORAL_CANDIDATO)) %>% # creating a variable that identifies... 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% # ...one voter ID for more than one alternative ID. Thus,
  mutate(flagTIT = sum(flagTIT)) %>% # flagTIT equals zero if voter ID appears only once in the data.frame.
  ungroup() %>%
  mutate(flagID = duplicated(id_cand2)) %>% # creating a variable that identifies...
  group_by(id_cand2) %>% # ...one alternative ID for more than one voter ID. Thus,
  mutate(flagID = sum(flagID)) %>% # flagID equals zero if alternative ID appears only once in the data.frame.
  ungroup() %>%
  filter(flagTIT == 0 & flagID == 0) # keeping only observations with alternative and voter IDs 
#that appear only once in our data.frame 

no_voter_ID <- no_voter_ID %>% # join using alternative ID.
  left_join(with_voter_ID, 'id_cand2') # (845 obs.: No duplications)

sum(is.na(no_voter_ID$NUM_TITULO_ELEITORAL_CANDIDATO.y)==F) # 85 remaining NA voter IDs.

no_voter_ID$NUM_TITULO_ELEITORAL_CANDIDATO <- no_voter_ID$NUM_TITULO_ELEITORAL_CANDIDATO.y # replacing voter ID with 
#the ones rescued with the alternative ID. 

df <- df %>% 
  filter(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==F) # Dropping obs. without voter ID from master data.frame.
# we will reinclude them with rescued ID below:
df<-no_voter_ID %>%
  select(colnames(df))%>%
  rbind(df)

sum(is.na(df$NUM_TITULO_ELEITORAL_CANDIDATO)) # 760 candidates without voter ID.

rm(no_voter_ID,with_voter_ID)


## Recovering National ID with alternative ID:
no_nat_ID <- df %>% # filtering only candidates without national ID. (8193 obs.)
  filter(is.na(CPF_CANDIDATO)==T)

with_nat_ID <- df %>%
  filter(is.na(CPF_CANDIDATO)==F) %>% # filtering only candidates with national ID.
  select(id_cand2,CPF_CANDIDATO) %>%
  distinct() %>% # removing duplicated observations of the pair <alternativeID-nationalID>.
  # But we still might have cases in which there is one alternative ID corresponding to more than one national ID and vice-versa.
  # Let's remove these from our data.frame:
  mutate(flagCPF = duplicated(CPF_CANDIDATO)) %>% # creating a variable that identifies... 
  group_by(CPF_CANDIDATO) %>% # ...one national ID for more than one alternative ID. Thus,
  mutate(flagCPF = sum(flagCPF)) %>% # flagCPF equals zero if national ID appears only once in the data.frame.
  ungroup() %>%
  mutate(flagID = duplicated(id_cand2)) %>% # creating a variable that identifies...
  group_by(id_cand2) %>% # ...one alternative ID for more than one national ID. Thus,
  mutate(flagID = sum(flagID)) %>% # flagID equals zero if alternative ID appears only once in the data.frame.
  ungroup() %>%
  filter(flagCPF == 0 & flagID == 0) # keeping only observations with alternative and voter IDs 
#that appear only once in our data.frame 

no_nat_ID <- no_nat_ID %>% # join using alternative ID. (8193 obs. No observations duplicated by the join)
  left_join(with_nat_ID, 'id_cand2')

sum(is.na(no_nat_ID$CPF_CANDIDATO.y)==F) # 609 candidates with NA national IDs.

no_nat_ID$CPF_CANDIDATO <- no_nat_ID$CPF_CANDIDATO.y # replacing nationa ID with 
#the ones rescued with the alternative ID. 

df <- df %>% 
  filter(is.na(CPF_CANDIDATO)==F) # Dropping obs. without national ID from master data.frame.
# we will reinclude them with rescued ID below:
df<-no_nat_ID %>%
  select(colnames(df))%>%
  rbind(df)

summary(is.na(df$CPF_CANDIDATO)) # 7584 candidates without national ID.

df %>% select(id_cand2,ANO_ELEICAO,NUM_TURNO) %>% distinct() %>% nrow() # 2565917 unique <candidate-election-round> in our data.frame


#### Who are the non-identified winners ####
winners_novoterID <- df %>%
  filter(DESC_SIT_TOT_TURNO=='2º TURNO'|
           DESC_SIT_TOT_TURNO=='ELEITO POR QP'| 
           DESC_SIT_TOT_TURNO=='ELEITO'|
           DESC_SIT_TOT_TURNO== 'ELEITO POR MEDIA') %>% # Filtering Winners...
  filter(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==T) # ...without voter ID. (9 winners only)


#### Saving Data with identified candidates ####
saveRDS(df, 'df_identified.rds')
saveRDS(winners_novoterID, 'winners_novoterID.rds')

rm(list=ls()) # cleaning workspace.
