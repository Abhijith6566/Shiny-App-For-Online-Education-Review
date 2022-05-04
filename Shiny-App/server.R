server <- function(input,output){
  df=read.csv("Data/ONLINE EDUCATION SYSTEM REVIEW.csv",stringsAsFactors = TRUE)
  Gender<- list("Male","Female")
  Homelocation<- list("Urban","Rural")
  Education<-list("School","Under Graduate", "Post Graduate")
  
  
  Economicstatus<- list("Poor","Middle Class", "Rich")
  FamilySize<-list("1","2", "3", "4", "5", "6", "7", "8", "9", "10", "11+")
  #variables<-list("Gender","Level.of.Education","Home.Location","Economic.status")
  
  barplot_action<-eventReactive(input$plot,{filter(df,(Do.elderly.people.monitor.you.==input$mentor & Gender==input$gender))})
  
  output$violin<-renderPlot({
    # df %>% filter(Do.elderly.people.monitor.you.==input$mentor) %>% 
    v1<-barplot_action() 
    v1 %>%
      ggplot() +
      geom_violin(aes(x =Performance.in.online , y = Time.spent.on.social.media..Hours., 
                      fill = Do.elderly.people.monitor.you.),trim=FALSE) +
      labs(x = 'Performance.in.online', y = "Time.spent.on.social.media..Hours.") +
      ggtitle("Performance in online ~ Time spent on socialmedia Hours") +
      theme_bw() +
      theme(axis.text.x = element_text(face = 'bold', size = 10),
            axis.text.y = element_text(face = 'bold', size = 10))
    
  }) 
  
  output$barPlot <- renderPlot({
    v<-barplot_action()
    
    
    
    # draw the barplot 
    v %>%  group_by(Performance.in.online) %>% summarise(N=n()) %>%
      ggplot(aes(x=Performance.in.online,y=N,fill=Performance.in.online))+
      geom_bar(stat = 'identity',color=as.factor('black'))+labs(x="Student Performance Online",y="Number of Students")+
      scale_y_continuous(labels = scales::comma_format(accuracy = 1))+
      geom_text(aes(label=N),vjust=-0.25,size=2) + 
      theme_bw() 
  })
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(df[,'Performance.in.online'] ~ df[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$indepvar)
    summary(fit)
  })
  
  
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    plot(df[,input$indepvar], df[,'Performance.in.online'], main="Scatterplot",
         xlab=input$indepvar, ylab='Performance.in.online', pch=19)
    #abline(lm(df[,'Performance.in.online'] ~ df[,input$indepvar]), col="red")
    #lines(lowess(df[,input$indepvar],df[,'Performance.in.online']), col="blue")
  }) 
  
  
  #demographics tab Bar chart
  action<-eventReactive(input$action,{filter(df,(Gender==input$Gender & Level.of.Education ==input$education_level & Home.Location==input$hometown & Economic.status==input$economic_status ))})
  
  #two_variable<-eventReactive()
  #three_variable<-
  #four_variable<-
  output$dPlot <- renderPlot({
    v<-action()
    
    
    
    # draw the barplot 
    v %>%  group_by(Performance.in.online) %>% summarise(N=n()) %>%
      ggplot(aes(x=Performance.in.online,y=N,fill=Performance.in.online))+
      geom_bar(stat = 'identity',color='black')+labs(x="Student Performance Online",y="Number of Students")+
      scale_y_continuous(labels = scales::comma_format(accuracy = 1))+
      geom_text(aes(label=N),vjust=-0.25,size=2)+
      theme_bw()+
      theme(axis.text = element_text(color='black',face="bold"),
            axis.title = element_text(color='black',face="bold"),
            legend.text = element_text(color='black'),
            legend.title = element_text(color='black'))
  })
  
  #about - table render
  
  
  cols <- reactiveValues()   
  cols$showing <- 1:5    
  
  #show the next five columns 
  observeEvent(input$next_five, {
    #stop when the last column is displayed
    if(cols$showing[[length(cols$showing)]] < 23) {
      hideCols(proxy, cols$showing, reset = FALSE) #hide displayed cols
      cols$showing <- cols$showing + 5
      showCols(proxy, cols$showing, reset = FALSE) #show the next five 
    } 
  })
  
  #similar mechanism but reversed to show the previous cols
  observeEvent(input$prev_five, {
    #stop when the first column is displayed
    if(cols$showing[[1]] > 1) {
      hideCols(proxy, cols$showing, reset = FALSE) #hide displayed cols
      cols$showing <- cols$showing - 5
      showCols(proxy, cols$showing, reset = FALSE) #show previous five
    } 
  })
  
  output$tbl = renderDT(
    df,
    options = list(
      columnDefs = list(list(visible = TRUE, targets = 1:length(df))), #hide all columns
      scrollX = TRUE)  #for when many columns are visible
  )
  
  
  proxy <- dataTableProxy('tbl')
  showCols(proxy, 1:5, reset = FALSE) #show the first five cols (because the colums are now all hidden)
  
  
  
}


# Run the application 
#shinyApp(ui = ui, server = server)
