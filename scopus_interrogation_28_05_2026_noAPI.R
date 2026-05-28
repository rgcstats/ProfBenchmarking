# You will need to obtain your own API Key in order to access Scopus:
options("elsevier_api_key" = "YOURKEY")

library(rscopus)
library(dplyr)

citation.counter <- function(authorid,year){
  res = author_df(au_id=authorid,verbose = FALSE, general = FALSE)
  names(res)
  all_dat = author_data(au_id=authorid, verbose = FALSE, general = TRUE)
  res2 = all_dat$df
  res2 = res2 %>% 
    rename(journal = `prism:publicationName`,
           title = `dc:title`,
           description = `dc:description`)
  all.scopus.id <- substr(res2$`dc:identifier`,11,99)
  all.year <- as.numeric(substr(res2$`prism:coverDate`,1,4))
  all.scopus.id <- all.scopus.id[all.year<=year]
  K <- length(all.scopus.id)
  cit.counts <- rep(0,K)
  for(k in c(1:K)){
    cit.k.search <- try(scopus_search(query=paste0("ref(,",all.scopus.id[k],")"),verbose=FALSE))
    if(class(cit.k.search)=="is.error") cit.counts[k] <- 0 else{
      if(length(cit.k.search$entries)>0){
        for(j in c(1:length(cit.k.search$entries))){
          cit.kj.year <- as.numeric(substr(cit.k.search$entries[[j]]$`prism:coverDate`,1,4))
          if(length(cit.kj.year)>0) cit.counts[k] <- cit.counts[k]+(cit.kj.year<=year)
        }
      }
    }
  }
  h.index <- 0
  for(h in c(1:K)){
    if(sum(cit.counts>=h)>=h) h.index <- h
  }
  list(cit.counts=cit.counts,num.pubs=K,h.index=h.index)
}

citation.counter("6701453203",2030) # George
citation.counter("6701453203",2018)
citation.counter("8317088900",2023) # Catherine Forbes Monash

citation.counter("6701453203",2009) # Vahid-Araghi

citation.counter("7404476299",2010) # Jiti Gao

citation.counter("7403472483",2015) # Di Cook

citation.counter("7410277543",2021) # Xibin Zhang

citation.counter("7006914313",2003) # Rob Hyndman


citation.counter("6701802955",2013) # Mervyn Silvapulle


citation.counter("23971863300",2000) # Param Silvapulle



citation.counter("18036851700",2016) # Howard Bondell
citation.counter("22950125100",2006) # Aurore Delaigle
citation.counter("7201551348",2011) # Ian Gordon
citation.counter("6701576584",2007) # Borovkov, Konstantin A.


citation.counter("6603467842",2014) # David Warton
citation.counter("6701853591",2016) # Scott Sisson
citation.counter("6603378693",2019) # Spiridon Penev
citation.counter("8872635600",2018) # Jake Olivier

citation.counter("55587868500",2018) # Qiying Wang USyd
citation.counter("7403394636",2009) #  Dingxuan Zhou USyd
citation.counter("7409391345",2016) #  Jean Yang USyd ??

citation.counter("24725805500",2022) #  Dino Sejdinovic
citation.counter("51161515500",2014) #  Lewis Mitchell

citation.counter("7007130603",2001) # Alan Welsh - real date may be earlier 
citation.counter("55482064300",2007) #  Michael Martin - date is definite
citation.counter("55708815900",1999) #  Andrew Wood


citation.counter("7006560742",1998) #  Geoff McLachlan UQ

citation.counter("55605766577",2013) #  James Brown UTS

citation.counter("14012635300",2016) #  Gabrielle Durrant Soton
citation.counter("57203670863",2019) #  Nikos Tzavidis Soton


citation.counter("7007130603",2000) #  Alan Welsh ANU
citation.counter("55482064300",1994) #  Michael Martin ANU
citation.counter("55708815900",1994) #  Andrew Wood ANU



