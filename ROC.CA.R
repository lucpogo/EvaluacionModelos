ROC.CA <- function(real,predicted,positive=1,digitsRound=4){
#real: Vector con los valores reales de la clase
#predicted: Vector con las probabilidades de la clase a predecir
#positive: Valor que toman los casos positivos
#digitsRound: Cantidad de decimales a utilizar
library(dplyr)
library(tidyr)
library(plotly)

df <- tibble(predicted=round(predicted,digits=digitsRound),
             real=ifelse(real==positive,'P','N'),
             stringsAsFactors = F) %>%
      group_by(real,predicted) %>%
      summarise(N=n()) %>%
      spread(real,N,fill=0) %>%
      arrange(desc(predicted)) %>%
      #Para que el gráfico comience en (0,0)
      add_row(predicted=NA,
              P=0,
              N=0,
              .before=1) %>%
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

AUC <- sum((df$Sens+lag(df$Sens,default = 0))/2*(df$UnoMSpec-lag(df$UnoMSpec,default=0)))

plot_ly(df,
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
  add_trace(x=seq(0,1,length.out = nrow(df)),
            y=seq(0,1,length.out = nrow(df)),
            showlegend=F,
            hoverinfo='none',
            line = list(color = 'rgb(22, 96, 167)',dash='dash')
            ) %>%
  add_annotations(x = 0.8, y = 0.2, showarrow = F, 
                  text = paste0("AUC: ", AUC)) %>%
  layout(xaxis=list(title='1-Especificidad'),
         yaxis=list(title='Sensibilidad')
         )
}

