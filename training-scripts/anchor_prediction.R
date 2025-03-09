
anchor_prediction <- function(pred_full, df){
  
  pred_anchor <- data.frame(pred=double(length(pred_full)))
  
  for (i in seq(1,length(pred_full))){
    
    if (df$X2.1.1_customChoiceValue[i]=='Upper management' | df$X2.8[i]==2 | df$X2.3[i] >= 10 |
        (df$X2.3[i]<4 & df$X4.1[i]==2 & df$X4.1.1[i] >=6)){
      
      
      if (df$X2.3[i] <= 1){
        pred_anchor$pred[i] <- pred_full[i] - 3
      } else if (df$X2.3[i] <= 2){
        pred_anchor$pred[i] <- pred_full[i] - 2 
      } else if (df$X2.3[i] <= 5){
        pred_anchor$pred[i] <- pred_full[i] - 1 
      } else {
        pred_anchor$pred[i] <- pred_full[i]
      }
      
      
    } else {
      pred_anchor$pred[i] <- ((10 - df$X2.3[i])/10)*(4.3*df$X2.3[i]) + (df$X2.3[i]/10)*pred_full[i]
      
    }
    
    
  }
  
  return(pred_anchor)
}