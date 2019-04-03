
#Trading Simulation
trumpt2=cbind(a <- trumpt[-intrain, ],trump_test_pred)
write.csv(trumpt2,"trumpt2.csv")


trump_test2 <- trumpt[-intrain, ]
trump_test2$predict=trump_test_pred
trump_test2$cumulative_longonly=0
trump_test2$cum_long_inv=0
trump_test2$long_PL=0
trump_test2$LS_PL


cumulative_longonly=0
cum_LS=0
cum_long_inv =0
long_PL = 0
LS_PL =0
cum_inv=0

for (i in 1:nrow(trump_test2)){
   #to calculate long only strategy
   trump_test2$cumulative_longonly[i]=(cumulative_longonly+100)*(1+trump_test2$toclose[i])
   trump_test2$cum_long_inv[i] = cum_long_inv+100
   trump_test2$long_PL[i] = trump_test2$cumulative_longonly[i] - trump_test2$cum_long_inv[i] 
   
   
  #to calculate long on bull and shor on bear
  if (trump_test_pred[i]=='BULL'){
    trump_test2$buy_sell[i] = 100
    trump_test2$cum_inv[i] = trump_test2$buy_sell[i]+cum_inv
    
    trump_test2$cum_LS[i]=(cum_LS+trump_test2$buy_sell[i])*(1+trump_test2$toclose[i])
  }

  if (trump_test_pred[i]=='BEAR') {
    trump_test2$buy_sell[i] = -100
    trump_test2$cum_inv[i] = trump_test2$buy_sell[i]+cum_inv
    
    trump_test2$cum_LS[i]=(cum_LS+trump_test2$buy_sell[i])*(1+trump_test2$toclose[i])
  }

  cum_inv = trump_test2$cum_inv[i]
  buy_sell = trump_test2$buy_sell[i]
  cum_LS=trump_test2$cum_LS[i]  
  trump_test2$LS_PL[i] = trump_test2$cum_LS[i]-trump_test2$cum_inv[i]
  

   cumulative_longonly=trump_test2$cumulative_longonly[i]
   cum_long_inv = trump_test2$cum_long_inv[i]
   long_PL = trump_test2$long_PL[i]
   
  
}

write.csv(trump_test2,"Lan_wednesday.csv")


 ggplot(trump_test2, aes(created_at)) +
   geom_line(aes(y = LS_PL, color='blue')) +
   geom_line(aes(y = long_PL, color='red')) +
   labs(x = "Time",
        y = "Cumulative PnL $",
        title = "Trading Simulation",
        color = "")+
   scale_colour_manual(labels = c("BullBear_Strategy", "Long Only"), values = c("blue", "red"))

 