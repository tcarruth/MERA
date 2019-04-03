
Qtab<<-function(Answer,selected){
  
  scol<-"#b9e8f9"
  Qtable<-data.frame(Answer)
  
  Qtable %>%
    mutate(
      Answer = ifelse(selected,
                      color_bar(color = scol)(Answer),
                      color_bar(color = "white")(Answer))
    )%>%
    select(everything())%>%
    knitr::kable("html", escape = F,align = "c") %>%
    kable_styling("hover", full_width = F)%>%
    column_spec(1, width = "3cm")
  
}