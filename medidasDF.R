medidasDF <- function(real,predicted,positive=1,digitsRound=4){
  library(dplyr)
  library(tidyr)
  df <- tibble(predicted=round(predicted,digits=digitsRound),
               real=ifelse(real==positive,'P','N'),
               stringsAsFactors = F) %>%
    group_by(real,predicted) %>%
    summarise(N=n()) %>%
    spread(real,N,fill=0) %>%
    arrange(desc(predicted)) %>%
    #Para que el gráfico comience en (0,0)
    #add_row(predicted=NA,
    #        P=0,
    #        N=0,
    #        .before=1) %>%
    mutate(Tot=P+N,
           PAcum=cumsum(P),
           NAcum=cumsum(N),
           TotAcum=cumsum(Tot),
           Sens=PAcum/sum(P),
           UnoMSpec=NAcum/sum(N),
           Precision=PAcum/TotAcum,
           Accuracy=(PAcum+sum(N)-NAcum)/sum(Tot),
           F1Score=2*(Sens*Precision)/(Sens+Precision),
           PropTot=round(TotAcum/sum(Tot),2)*100,
           Lift=Precision/(sum(P)/sum(Tot))
    )
  return(df)
}