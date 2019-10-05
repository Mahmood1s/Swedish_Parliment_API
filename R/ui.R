shinyUI<-fluidPage(
   
   
  pageWithSidebar(
     
    headerPanel("Swedish Parliment Member Details"),
     
     sidebarPanel(
       selectInput("report_type", "Select Report", choices=c("Please Select","Member Appointments","Members by County","Gender Participation")),
       conditionalPanel(condition = "input.report_type=='Member Appointments'",
                        uiOutput("Report_Controls")),
      #uiOutput("Report_Controls1")#,
    #  uiOutput("Report_Controls2")#)#,
         conditionalPanel(condition = "input.report_type=='Members by County'",
                           uiOutput("Report_Controls1")),
         conditionalPanel(condition = "input.report_type=='Gender Participation'",
                           uiOutput("Report_Controls2"))
    
       ),
     
     mainPanel(
       
       conditionalPanel(condition = "input.report_type != 'Gender Participation'",
                          tableOutput("ReportTable")),
        conditionalPanel(condition = "input.report_type == 'Gender Participation'",
                         plotOutput("plotchart"))
       
     )
     
  )
   
 )

