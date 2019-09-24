## This script builds a pannel with all candidates that ever participated in elections since 1998 ##

#### National & State Elections ####

anos <- seq(from = 1998, to = 2018, by = 4) # years in which there were national elections in Brazil since 1998.
cargos <- c(1:7,9,10) # Positions in national elections.

        ## Aparentemente não funcionaram as posições:
        # position 2 = Vice-Presidente
        # position 4 = Vice-Governador
        # position 9 = 1º suplente
        # position 10 = 2º suplemente

cand_national<-NA # creates the object we are using in the loop below.

# Calling all candidates in national and state elections:
for (i in anos){
  for(j in cargos){
    x<-cepespR::get_candidates(year = i, position = j, only_elected = F, dev = F)
    cand_national<-rbind(cand_national,x)
  }
}

cand_national<-cand_national[2:nrow(cand_national),] # Drops first line of the df since it is only full of NAs 
# because of the way in which the loop was built.

saveRDS(cand_national,'cand_national.rds') # Saving candidates in national elections.
rm(cand_national) # removing df in order to spare memory.





#### Municipal Elections ####
anos <- seq(from = 2000, to = 2016, by = 4) # years of municipal elections in Brazil.

# Positions available in municipal elections: 11 and 13
                   # Insert later: position = 'Vice-Prefeito' (not available yet).

    # creating the objecta we are using in the loop below:
      cand_mayor <- NA
      cand_counc<-NA


      
# Calling all candidates -- Position Mayor -----
for (i in anos){
    x<-cepespR::get_candidates(year = i, position = 11, only_elected = F, dev = F)
    cand_mayor<-rbind(cand_mayor,x)
}
      
cand_mayor<-cand_mayor[2:nrow(cand_mayor),] # Drops first line of the df since it is only full of NAs 
# because of the way in which the loop was built.

saveRDS(cand_mayor,'cand_mayor.rds') # saving mayor candidates.
rm(cand_mayor) # removing mayors data.frame in order to spare memory.



# Calling all candidates -- Position Councilor -----
for (i in anos){
  x<-cepespR::get_candidates(year = i, position = 13, only_elected = F, dev = F)
  cand_counc<-rbind(cand_counc,x)
}

cand_counc<-cand_counc[2:nrow(cand_counc),] # Drops first line of the df since it is only full of NAs 
# because of the way in which the loop was built.

# In 2000, both in mayors and councilors df, there are many more obs. compared to other years. A problem in the original TSE data.
# x<- cand_counc %>% unique() # Only 5 duplicated observations: 3199562 in x versus 3199567 in cand_conc.
#rm(x)

saveRDS(cand_counc,'cand_counc.rds') # saving mayor candidates.

#### Removing remaining objects ####
rm(list=ls())
