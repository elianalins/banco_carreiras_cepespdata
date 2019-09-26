## Building a data.frame with all candidates -- Pilling up separated data.frames ##
              # Eliana Lins Morandi, CepespData/FGV, sept.2019 #

all_cand<-readRDS('cand_national.rds') # open candidates in all national elections.
cand_counc<-readRDS('cand_counc.rds') # open all canditates do city council (councilors).

all_cand<-rbind(all_cand,cand_counc); rm(cand_counc) # piles up national candidates and all candidates to city councils

cand_mayor<-readRDS('cand_mayor.rds') # open all mayor candidates.
all_cand<-rbind(all_cand,cand_mayor); rm(cand_mayor) # piles up mayor candidates with all others.

saveRDS(all_cand,'all_cand.rds') # Saving all candidates.
