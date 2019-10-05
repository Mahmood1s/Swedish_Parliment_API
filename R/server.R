
get_mem <- function(county){

      con_str <- "~/R Packages/lab5/Data/parliment_database.sqlite"
      my_database = dbConnect(SQLite(), dbname=con_str)
      query <- paste("select intressent_id , (efternamn||","' ","'","||tilltalsnamn) as 'Member Name',valkrets from members where valkrets like '%","%'")
      
      member <- dbGetQuery(my_database,query)
      dbDisconnect(my_database)
      return (member)
    }

get_memdata <- function(id,county){

  con_str <- "~/R Packages/lab5/Data/parliment_database.sqlite"
  my_database = dbConnect(SQLite(), dbname=con_str)
  query <- paste("select a.Organ, a.Roll, a.Status,a.[From],a.Tom from members as m inner join appointments as a on m.intressent_id = a.id where a.id like '%",id,"%' and m.valkrets like '%",county,"%'",sep = "")
  member <- dbGetQuery(my_database,query)
  dbDisconnect(my_database)
  return (member)
}

get_genderdata <- function(county){
  con_str <- "~/R Packages/lab5/Data/parliment_database.sqlite"
  my_database = dbConnect(SQLite(), dbname=con_str)
  query <- paste("select valkrets,kon from members where valkrets !='' and valkrets like'%",county,"%'",sep = "")
  participation <- dbGetQuery(my_database,query)
  dbDisconnect(my_database)
  return (participation)
}

server <- function(input, output,session) {

    checkvar <- 0
    my_df <<- get_mem("")
   
    observeEvent(input$counties,{
     
           temp <- which(my_df[,3]==input$counties)
           mem <- my_df[temp,2]
           updateSelectInput(session,inputId = "member_name1",choices =mem )

    })
    
    output$Report_Controls1 <- renderUI({
      
          if(checkvar==1)
          {
             temp <- which(my_df[,3]==input$counties)
             mem <- my_df[temp,2]
            }
          else
          {
             mem <- my_df[,2]
          }
      mem <- my_df[,2]
      county1 <- unique(my_df[,3])
      names(county1[1]) <- "county"
          
       tagList(         selectInput("counties", "Select County", choices=county1),
                        selectInput("member_name1", "Select Member From County", choices=mem)
              )
    })
    
    output$Report_Controls <- renderUI({

            checkvar<<-1
            query_data <- my_df
            mem <- query_data[2]
          
            selectInput("member_name", "Select Member Name", choices=mem)
         
    })
  
    output$Report_Controls2 <- renderUI({
      
      county2 <- unique(my_df[,3])
      county2 <- append("ALL",county2)
      names(county2[1]) <- "county"
      selectInput("counties1", "Select County", choices=county2)
    })
    
    output$ReportTable <- renderTable({
      
      #mem_name <- input$member_name
     # print(mem_name)
    #  print(my_df)
    #  first_name <- strsplit(mem_name," ")[[1]]
    #  second_name <- strsplit(mem_name," ")[[1]]
    #  print(first_name[[1]])
    #  print(second_name[[3]])
          if(input$report_type=="Member Appointments")
          {
            idex <- which(my_df[,2]== input$member_name)
            id <- my_df[idex,1]
            query_data <- get_memdata(id,"")
            data_table <- query_data
            data_table
          }
          else if(input$report_type=="Members by County")
          {    
            idex <- which(my_df[,2]== input$member_name1)
            id <- my_df[idex,1]
            query_data <- get_memdata(id,"")
            data_table <- query_data
            data_table
          }
      
    })
    
    output$plotchart <- renderPlot({
      if(is.null(input$counties1) | input$counties1 == "ALL")
        counties =""
      else
        counties = input$counties1
      dataframe <- get_genderdata(counties)
      tab <- table(dataframe$valkrets,dataframe$kon)
      
      if(input$counties1=="ALL")
        barplot(tab[,1],main = "Gender Distribution",col = c("darkblue","red"),legend = colnames(tab))
      else
        barplot(tab[1,],main = "Gender Distribution",col = c("darkblue","red"),legend = colnames(tab),xlab = input$counties1)
      
    })
    }




