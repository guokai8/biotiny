setting=element_text(size = 6,color="black",family="Arial")
    
p+theme(axis.title.x = setting,
                 axis.title.y = element_blank())+
        theme(axis.text.x = setting,
              axis.text.y = setting,
              legend.text = setting,
              legend.title = setting)
