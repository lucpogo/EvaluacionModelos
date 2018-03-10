LIFT.CA <- function(real=NULL,predicted=NULL,ds=NULL,positive=1,digitsRound=4){
  #real: Vector con los valores reales de la clase
  #predicted: Vector con las probabilidades de la clase a predecir
  #ds: Dataframe con las medidas calculadas, salida de la función medidasDF
  #positive: Valor que toman los casos positivos
  #digitsRound: Cantidad de decimales a utilizar
  
  library(dplyr)
  library(tidyr)
  library(plotly)
  
  if(is.null(ds)){
    if(!exists('medidasDF')){
      source('medidasDF.R')
    }
    ds <- medidasDF(real,predicted,positive,digitsRound)
  }
  
  plt <- plot_ly(ds,
                 x=~PropTot,
                 y=~Lift,
                 type='scatter',
                 mode='lines',
                 hoverinfo = 'text',
                 text=~paste('Lift:', round(Lift,digits=digitsRound),'\n',
                             'Sensibilidad:',round(Sens,digits=digitsRound),'\n',
                             '1-Especificidad:',round(UnoMSpec,digits=digitsRound),'\n',
                             'Probabilidad:',predicted,'\n',
                             'Población:',TotAcum,paste0('(',round(PropTot,2)*100,'%)'),'\n',
                             'Casos Positivos:',PAcum,'\n',
                             'Casos Negativos:',NAcum,'\n',
                             'Accuracy:',round(Accuracy,digits=digitsRound),'\n',
                             'Precisión:',round(Precision,digits=digitsRound),'\n',
                             'F1-Score:',round(F1Score,digits=digitsRound)
                              ),
                 line = list(color = 'rgb(205, 12, 24)')
                 ) %>%
        layout(xaxis=list(title='Proporción de la Población')
        )
    
  
  return(plt)
}

