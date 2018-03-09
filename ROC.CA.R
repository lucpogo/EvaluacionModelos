ROC.CA <- function(real,predicted,positive=1,digitsRound=4){
#real: Vector con los valores reales de la clase
#predicted: Vector con las probabilidades de la clase a predecir
#positive: Valor que toman los casos positivos
#digitsRound: Cantidad de decimales a utilizar
library(dplyr)
library(tidyr)
library(plotly)

  if(!('medidasDF' %in% ls())){
    source('medidasDF.R')
  }
  
  df <- medidasDF(real,predicted,positive,digitsRound)

  AUC <- sum((df$Sens+lag(df$Sens,default = 0))/2*(df$UnoMSpec-lag(df$UnoMSpec,default=0)))
  
  plt <- plot_ly(df %>% add_row(predicted=NA,
                                P=0,
                                N=0,
                                Tot=0,
                                PAcum=0,
                                NAcum=0,
                                TotAcum=0,
                                Sens=0,
                                UnoMSpec=0,
                                Precision=NA,
                                Accuracy=0,
                                F1Score=NA,
                                PropTot=0,
                                Lift=NA,
                                .before=1),
                 x=~UnoMSpec,
                 y=~Sens,
                 type='scatter',
                 mode='lines',
                 hoverinfo = 'text',
                 text=~paste('Sensibilidad:',round(Sens,digits=digitsRound),'\n',
                             '1-Especificidad:',round(UnoMSpec,digits=digitsRound),'\n',
                             'Probabilidad:',predicted,'\n',
                             'Población:',TotAcum,paste0('(',PropTot,'%)'),'\n',
                             'Casos Positivos:',PAcum,'\n',
                             'Casos Negativos:',NAcum,'\n',
                             'Accuracy:',round(Accuracy,digits=digitsRound),'\n',
                             'Precisión:',round(Precision,digits=digitsRound),'\n',
                             'F1-Score:',round(F1Score,digits=digitsRound),'\n',
                             'Lift:', round(Lift,digits=digitsRound)
                              ),
                 line = list(color = 'rgb(205, 12, 24)')
                 ) %>%
    add_trace(x=seq(0,1,length.out = nrow(df)+1),
              y=seq(0,1,length.out = nrow(df)+1),
              showlegend=F,
              hoverinfo='none',
              line = list(color = 'rgb(22, 96, 167)',dash='dash')
              ) %>%
    add_annotations(x = 0.8, y = 0.2, showarrow = F, 
                    text = paste0("AUC: ", AUC)) %>%
    layout(xaxis=list(title='1-Especificidad'),
           yaxis=list(title='Sensibilidad')
           )
  
  return(plt)
}

