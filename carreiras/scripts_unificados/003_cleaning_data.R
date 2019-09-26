  ### Treating all non-identified candidates ###
# Eliana Lins Morandi, CepespData/FGV, sept.2019 #

all_cand<-readRDS('all_cand.rds') # opening data.frame with all candidates.
df<-filter(all_cand, NUM_TITULO_ELEITORAL_CANDIDATO != "000000000000") # dropping non-nominal votes.
rm(all_cand)

#### Uniformizing birth date ####

# Formats:
# A. "08-APR-52" 
# B. "25091952"
# C. "29/05/1963"

# Format A
# We separate df in 2 parts: birth date format type A and the rest.

df<-df %>% # creating dummy that identifies birthdate format type A.
  mutate(tipoA = if_else(str_count(DATA_NASCIMENTO, "-") == 2,1,0)) 

df %>% filter(tipoA==1)-> tipoA # Filtering birth date format A
df <- df %>% filter(tipoA != 1) # Dropping observations with birth date in format A from master df.

tipoA$DATA_NASCIMENTO <- str_pad(tipoA$DATA_NASCIMENTO, 9, pad = "0") # Inserts a 0 before
#strings with less than 9 digits, for instance 1-JAN-87 (Jan 1st, 1987).
tipoA$mes <- substring(tipoA$DATA_NASCIMENTO,4,6) # Creating new variable: month of birth.

# Replacing month abbreviation with month number:
tipoA$mes[tipoA$mes=="JAN"] <- "01"
tipoA$mes[tipoA$mes=="FEB"] <- "02"
tipoA$mes[tipoA$mes=="MAR"] <- "03"
tipoA$mes[tipoA$mes=="APR"] <- "04"
tipoA$mes[tipoA$mes=="MAY"] <- "05"
tipoA$mes[tipoA$mes=="JUN"] <- "06"
tipoA$mes[tipoA$mes=="JUL"] <- "07"
tipoA$mes[tipoA$mes=="AUG"] <- "08"
tipoA$mes[tipoA$mes=="SEP"] <- "09"
tipoA$mes[tipoA$mes=="OCT"] <- "10"
tipoA$mes[tipoA$mes=="NOV"] <- "11"
tipoA$mes[tipoA$mes=="DEC"] <- "12"

tipoA$DATA_NASCIMENTO <- paste0(substring(tipoA$DATA_NASCIMENTO,1,2),"/",
                                tipoA$mes,"/19",
                                (substring(tipoA$DATA_NASCIMENTO,8, 9)))
tipoA$mes <- NULL


# Birth date format B:
df$DATA_NASCIMENTO <- str_pad(df$DATA_NASCIMENTO, 8, pad = "0") # Inserts 0 in the 
#beginning of a string when it has less than 8 digits. For instance, 1011987 (Jan 1st, 1987).

df$DATA_NASCIMENTO <- str_replace(string = df$DATA_NASCIMENTO,
                                            pattern = "([0-9]{2})([0-9]{2})([0-9]{4})",
                                            replacement = "\\1/\\2/\\3")

df <- rbind(df,tipoA) # pilling up data.frames.
df$DATA_NASCIMENTO<-as.Date(df$DATA_NASCIMENTO, format="%d/%m/%Y") # adjusting date format.
rm(tipoA); df$tipoA <- NULL # removendo auxiliar data.frame and dropping dummy variable.

# With birth dates in as.Date format we can order the variable and identify obviously wrong 
#observations (min and max):
df$ano_nasc <- as.numeric(format(df$DATA_NASCIMENTO,'%Y')) # creating a birth year variable.


##### National ID / CPF ####
nrow(df[as.numeric(df$CPF_CANDIDATO)==0,]) # 13559 candidates with national ID == 0
summary(as.numeric(df$CPF_CANDIDATO)) # 13394 NA national IDs
sum(str_detect(df$CPF_CANDIDATO,"#N")) # 13282. Not all NA CPFs have "#N" in the string:
table( str_detect(df$CPF_CANDIDATO,"#N"), is.na(as.numeric(df$CPF_CANDIDATO))) # 112 cases where NAs do not include "#N".
# Let's see:
x<-df %>%
  filter(str_detect(CPF_CANDIDATO,"#N") == F) %>%
  filter( is.na(as.numeric(CPF_CANDIDATO)) ==T )

x$CPF_CANDIDATO # We can see that there are spaces between strings. We will correct that as well:
df$CPF_CANDIDATO<-stringr::str_replace_all(df$CPF_CANDIDATO, ' ', '') # removing spaces in national IDs.

rm(x) # removing auxiliar data.frame.

# Now, all NAs come can be identified by '#N' in the string:
summary(as.numeric(df$CPF_CANDIDATO)) # 13282 NAs
sum(str_detect(df$CPF_CANDIDATO,"#N")) # 13282 cases of #N. We will replace them by NA:
df$CPF_CANDIDATO<-as.numeric(df$CPF_CANDIDATO)

# Uniformizing number of characters in national ID (CPF):
df$CPF_CANDIDATO <- stringr::str_pad(string=df$CPF_CANDIDATO,width=11,side='left',pad='0')



#### VOTER ID (NUM_TITULO_ELEITORAL_CANDIDATO) ####
summary(as.numeric(df$NUM_TITULO_ELEITORAL_CANDIDATO)) # 3264 NAs
sum(str_detect(df$NUM_TITULO_ELEITORAL_CANDIDATO,"#N")==T) # 1221
# Thus, not all NAs can be identified by a "#N" in the string.
x<-df %>%
  filter(is.na(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO))==T) %>%
  filter(str_detect(NUM_TITULO_ELEITORAL_CANDIDATO,"#N")==F) # 2043 observations.
x$NUM_TITULO_ELEITORAL_CANDIDATO # also caused by spaces. Let's correct that:
df$NUM_TITULO_ELEITORAL_CANDIDATO<-stringr::str_replace_all(df$NUM_TITULO_ELEITORAL_CANDIDATO, ' ', '') # removing spaces in national IDs.
rm(x) # removing auxiliar data.frame.

sum(is.na(as.numeric(df$NUM_TITULO_ELEITORAL_CANDIDATO))) # 1224
sum(str_detect(df$NUM_TITULO_ELEITORAL_CANDIDATO,"#N")) # 1221
# We still have 3 obs in which NA is not caused by #N. Let see:
x<-df %>%
  filter(is.na(as.numeric(NUM_TITULO_ELEITORAL_CANDIDATO))==T) %>%
  filter(str_detect(NUM_TITULO_ELEITORAL_CANDIDATO,"#N")==F)
x$NUM_TITULO_ELEITORAL_CANDIDATO # indeed, those are missing Voter IDs. wrong ID data in this OBS. 
# We will insert NA in these cases as well:
df$NUM_TITULO_ELEITORAL_CANDIDATO<-as.numeric(df$NUM_TITULO_ELEITORAL_CANDIDATO)

# Uniformizing candidate's voter ID:
summary(nchar(df$NUM_TITULO_ELEITORAL_CANDIDATO)) # we find a maximum of 12 digits in voter IDs.
df$NUM_TITULO_ELEITORAL_CANDIDATO <- stringr::str_pad(string=df$NUM_TITULO_ELEITORAL_CANDIDATO,
                                                      width=12,side='left',pad='0')
rm(x) # removing auxiliar data.frame.

#### Creating auxiliar dummies that will be part of our alternative candidate IDs ####
df <- df %>% 
  mutate(semniver = if_else(ANO_ELEICAO - ano_nasc < 17 | ANO_ELEICAO - ano_nasc > 100 | 
                              is.na(ano_nasc)==T,1,0)) %>% # Candidates with non-sence ages 
  # in elections year and NA birth dates.
  mutate(temcpf = ifelse(is.na(CPF_CANDIDATO)==T,0,1), # no missing cpf (national ID).
         temtitulo = ifelse(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==T,0,1), # no missing voter ID.
         primeironome = tolower(stri_trans_general(word(NOME_CANDIDATO, 1), "Latin-ASCII"))) # candidate's first name.

# Saving cleaned data & cleaning workspace -----
saveRDS(df,'df.rds') # Saving 
rm(list=ls()) # removing all objects from workspace.
