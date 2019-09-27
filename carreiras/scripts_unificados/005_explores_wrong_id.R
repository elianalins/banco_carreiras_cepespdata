### CORRECTING WRONG VOTER AND NATIONAL IDs ###
# Eliana Lins Morandi, CepespData/FGV, sept.2019 #

df<-readRDS('df_identified.rds') # Importing data

# Now we are treating the cases in which there is more than one National ID for the same Voter ID and vice-versa.
  # Let's identify those cases:
    dupCPF <- df %>%
      filter(is.na(CPF_CANDIDATO)==F & CPF_CANDIDATO != '00000000000' & CPF_CANDIDATO != '99999999999') %>% # filtering only candidates with national ID.
      select(NUM_TITULO_ELEITORAL_CANDIDATO,CPF_CANDIDATO) %>%
      distinct() %>% # removing duplicated observations of the pair <voterID-nationalID>.
      # But we still might have cases in which there is one national ID corresponding to more than one voter ID.
      # Let's remove these from our data.frame:
      mutate(flagCPF = duplicated(CPF_CANDIDATO)) %>% # creating a variable that identifies... 
      group_by(CPF_CANDIDATO) %>% # ...one national ID for more than one voter ID. Thus,
      mutate(flagCPF = sum(flagCPF)) %>% # flagCPF equals zero if national ID appears only once in the data.frame.
      filter(flagCPF > 0) %>% # keeping only duplicated National IDs.
      ungroup()
    # 9792 duplicated national IDs.
    
    dupTIT <- df %>%
      filter(is.na(NUM_TITULO_ELEITORAL_CANDIDATO)==F & NUM_TITULO_ELEITORAL_CANDIDATO != '000000000302') %>% # filtering only candidates with voter ID.
      select(NUM_TITULO_ELEITORAL_CANDIDATO,CPF_CANDIDATO) %>%
      distinct() %>% # removing duplicated observations of the pair <voterID-nationalID>.
      # But we still might have cases in which there is one voter ID corresponding to more than one national ID.
      # Let's remove these from our data.frame:
      mutate(flagTIT = duplicated(NUM_TITULO_ELEITORAL_CANDIDATO)) %>% # creating a variable that identifies... 
      group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% # ...one voter ID for more than one national ID. Thus,
      mutate(flagTIT = sum(flagTIT)) %>% # flagCPF equals zero if national ID appears only once in the data.frame.
      filter(flagTIT > 0) %>% # keeping only duplicated voter IDs.
      ungroup()
    # 9307 duplicated voter IDs.

# We need to drop those from our master data.frame:
  # Let's pille them up first:
  dup<-rbind(dupCPF[,1:2],dupTIT[,1:2]) # pilling only first two columns: Voter ID and National ID (and not the flag variables).
  dup$TIT_CPF <- paste0(dup$NUM_TITULO_ELEITORAL_CANDIDATO,dup$CPF_CANDIDATO) # creating one string variable containing both national and voter IDs.
  dup<-distinct(dup, TIT_CPF) # we must guarantee we have only one obs. per pair <national ID-Voter ID> (variable TIT_CPF).
  
  # Anti-Join to drop the observations in dupCPF and dupTIT from master data.frame:
  df<-df %>%
    mutate(TIT_CPF = paste0(NUM_TITULO_ELEITORAL_CANDIDATO,CPF_CANDIDATO)) %>%
    anti_join(dup, 'TIT_CPF') 
  # We must reinsert them by the end of all corrections. 

 
# Now, let's correct those IDs using string distances:
  # For the same national ID, we are going to check the distances in voter IDs:
  dupCPF<-dupCPF %>%
    group_by(CPF_CANDIDATO) %>%
    mutate(N=n()) # N is the number of observations for the same national ID, with at least one diverging voter ID.
    # we are only analysing with those cases in which there are more than two observations 
    # since in two-observation cases we cannot infer much about which onw is uncorrect.

  summary(as.factor(dupCPF$N)) # Most of the cases are pairs, so we can't analyze them. We have only 
# 37 trios (111 observations) and one quintet (5 obs.).
  
  # Doing the same for voter IDs:
  # For the same voter ID, we are going to check the distances in national IDs:
  dupTIT<-dupTIT %>%
    group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>%
    mutate(N=n()) 
  summary(as.factor(dupTIT$N)) # 39 trios (117 obs.) and one quartet (4 obs). Most of them are pairs as well.

  # NEXT STEPS: ----
  
  # For those trios, quartet and quintet: try to rescue ID if there is only one of them diverging and all other 
  # info are the same.
  
  # For the duos (most of the cases) try to rescue the right ID using the afilliates CepespData/FGV data base.
  
  # Ref. for string distances: https://cran.r-project.org/web/packages/stringdist/stringdist.pdf (page 20).
  # "Levenshtein distance (method='lv') counts the number of deletions, insertions and substitutions necessary to 
  # turn b into a. This method is equivalent to R’s native adist function."