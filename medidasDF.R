medidasDF <- function(real,predicted,positive=1,digitsRound=4){
  library(dplyr)
  library(tidyr)
  ds <- tibble(predicted=round(predicted,digits=digitsRound),
               real=ifelse(real==positive,'P','N'),
               stringsAsFactors = F) %>%
    group_by(real,predicted) %>%
    summarise(N=n()) %>%
    spread(real,N,fill=0) %>%
    arrange(desc(predicted)) %>%
    mutate(Tot=P+N,
           PAcum=cumsum(P),
           NAcum=cumsum(N),
           TotAcum=cumsum(Tot),
           Sens=PAcum/sum(P),
           UnoMSpec=NAcum/sum(N),
           Precision=PAcum/TotAcum,
           Accuracy=(PAcum+sum(N)-NAcum)/sum(Tot),
           F1Score=2*(Sens*Precision)/(Sens+Precision),
           PropTot=TotAcum/sum(Tot),
           Lift=Precision/(sum(P)/sum(Tot))
    )
  return(ds)
}