
mydata <-data.frame(Ftxt=c('[图片]123','L2CCA2B14LG102558',NA,'','http://www.baidu.com','什么价格','什么东西','明天去玩吗'),stringsAsFactors = F)
rule =c('东西','去玩')
        
        
mydata2 <- df_blackList_tagging(mydata,var_text = 'Ftxt',dict = rule)

View(mydata2)