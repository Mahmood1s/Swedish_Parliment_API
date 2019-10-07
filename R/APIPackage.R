##' @name APIpackage
##' @aliases data_fetch
##' @aliases get_mem
##' @aliases get_memdata
##' @aliases get_genderdata
##'
##' @title API to get data from Swedish Parliment open Data site
##'   
##' @param county county of the members
##' @param id member id
##' @rdname APIpackage
##' @describeIn it will fetch the data from API URL, parse it according the requirement and send to create_database function to insert into database
##' @examples
##' \dontrun{
##' data_fetch()
##' }
##'
##' @rdname get_mem
##' @return  this function will return the members of parliment. if specific country is given then it will return member only from this country otherwise all parliment member
##' @examples
##' \dontrun{
##' get_mem("")
##' }
##' 
##' @rdname get_memdata
##' @return  It will return member appoinments data according to the county or member is or both

##' @examples
##' \dontrun{
##' get_memdata("","")
##' }
##' 
##' @rdname get_genderdata
##' @return  this function will return the gender presence in each county
##' @examples
##' \dontrun{
##' get_genderdata("")
##' }
##' 
##' @import DBI
##' @import RSQLite
##' @import httr
##' @import XML
##' @export get_mem
##' @export get_memdata
##' @export get_genderdata
##' @export create_database
##' @export data_fetch


get_mem <- function(county){
  
  
  con_str <- "~/R Packages/lab5/DatabaseDir/parliment_database.sqlite"
  #con_str <-"https://github.com/Mahmood1s/lab5/blob/master/DatabaseDir/parliment_database.sqlite"
  
  my_database = dbConnect(SQLite(), dbname=con_str)
  query <- paste("select intressent_id , (efternamn||","' ","'","||tilltalsnamn) as 'Member Name',valkrets from members where valkrets like '%","%'")
  
  member <- dbGetQuery(my_database,query)
  dbDisconnect(my_database)
  return (member)
}

get_memdata <- function(id,county){
  
  con_str <- "~/R Packages/lab5/DatabaseDir/parliment_database.sqlite"
  my_database = dbConnect(SQLite(), dbname=con_str)
  query <- paste("select a.Organ, a.Roll, a.Status,a.[From],a.Tom from members as m inner join appointments as a on m.intressent_id = a.id where a.id like '%",id,"%' and m.valkrets like '%",county,"%'",sep = "")
  member <- dbGetQuery(my_database,query)
  dbDisconnect(my_database)
  return (member)
}

get_genderdata <- function(county){
  con_str <- "~/R Packages/lab5/DatabaseDir/parliment_database.sqlite"
  my_database = dbConnect(SQLite(), dbname=con_str)
  query <- paste("select valkrets,kon from members where valkrets !='' and valkrets like'%",county,"%'",sep = "")
  participation <- dbGetQuery(my_database,query)
  dbDisconnect(my_database)
  return (participation)
}

create_database<-function(appointment,person_data_frame,csv_file){
  
  my_db_file<-"~/R Packages/lab5/DatabaseDir/parliment_database.sqlite"
  my_database = dbConnect(SQLite(), dbname=my_db_file) 
  
  rs <- dbSendQuery(my_database,"SELECT name FROM sqlite_master WHERE type='table' and name ='members'")
  query_member <-dbFetch(rs)
  dbClearResult(rs)
  rs <- dbSendQuery(my_database,"SELECT name FROM sqlite_master WHERE type='table' and name ='appointments'")
  query_appoint <- dbFetch(rs)
  dbClearResult(rs)
  rs <- dbSendQuery(my_database,"SELECT name FROM sqlite_master WHERE type='table' and name ='voting'")
  query_voting <- dbFetch(rs)
  dbClearResult(rs)
  
  if(length(query_member[,1]>0))
  {
    dbSendQuery(conn=my_database,"delete from members")
  }
  else 
  {
     dbSendQuery(conn=my_database,
                 "CREATE TABLE members(
                   intressent_id TEXT,
                   efternamn TEXT,
                   tilltalsnamn TEXT,
                   parti TEXT,
                   kon TEXT,
                   valkrets TEXT,
                   fodd_ar INT,
                   status TEXT,
                   PRIMARY KEY (intressent_id))
                 ")
  }
 
  if(length(query_appoint[,1]>0))
  {
    dbSendQuery(conn=my_database,"delete from appointments")
  }
  else 
  {
    dbSendQuery(conn=my_database,
                "CREATE TABLE appointments(
                  intressent_id TEXT,
                  uppgift TEXT,
                  roll_kod TEXT,
                  status TEXT,
                  from DATETIME,
                  tom DATETIME,
                  PRIMARY KEY (member_id))
                ")
  }
  
  if(length(query_voting[,1]>0))
  {
    dbSendQuery(conn=my_database,"delete from voting")
  }
  else 
  { 
    dbSendQuery(conn=my_database,
                "CREATE TABLE voting(
                  rm TEXT,
                  intressent_id INT,
                  rost TEXT,
                  avser TEXT,
                  dok_id TEXT)
                ")
  }

  
  member_status <- dbWriteTable(conn=my_database, name="members", person_data_frame,append=T, row.names=F)
  appointments_status <- dbWriteTable(conn=my_database, name="appointments", appointment,append=T, row.names=F)
  voting_status <- dbWriteTable(conn=my_database, name="voting", csv_file,append=T, row.names=F)
  
  dbDisconnect(my_database)
  
  if(member_status==TRUE & voting_status==TRUE & appointments_status==TRUE)
    return(TRUE)
  else
    return(FALSE)
  
   # dbListTables(my_database)
  #dbListFields(db, "members")
  # my_database<- src_sqlite(my_db_file,create = TRUE)
  #copy_to(my_database,df = appointment)
  #copy_to(my_database,df = person_data_frame)
  #copy_to(my_database,df = csv_file)

}

data_fetch<-function(){
  

api_address<-"http://data.riksdagen.se/personlista/?iid=&fnamn=&enamn=&f_ar=&kn=&parti=&valkrets=&rdlstatus=samtliga&org=&utformat=xml&sort=sorteringsnamn&sortorder=asc&termlista="


get_xml<-GET(api_address)

xml_content<- content(get_xml)

content_data<-xmlParse(xml_content)
content_xml <- getNodeSet(content_data,"/personlista", fun=xmlToDataFrame)
content_data_frame <- data.frame(do.call(rbind, content_xml))
person_data_frame <- data.frame(content_data_frame["intressent_id"],content_data_frame["efternamn"],content_data_frame["tilltalsnamn"],content_data_frame["parti"],content_data_frame["kon"],content_data_frame["valkrets"],content_data_frame["fodd_ar"],content_data_frame["status"])
person_data_frame <- person_data_frame[!person_data_frame$intressent_id %in% "",]

# saving person data frame
#save(person_data_frame,file = "~/R Packages/lab5/Data/person.Rda")

xml_1 <- xmlInternalTreeParse(xml_content,useInternalNodes=TRUE)
tags  <- xml_1["//personlista/person/personuppdrag/uppdrag"]
appoint <- do.call(rbind,
                  lapply(tags,function(uppdrag)
                    {
                     # if(as.numeric(xmlValue(uppdrag["intressent_id"][[1]]))==0640627133410)
                      {
                        intressent <- xmlValue(uppdrag["intressent_id"][[1]])
                        uppgift  <- xmlValue(uppdrag["uppgift"][[1]])
                        roll_kod  <- xmlValue(uppdrag["roll_kod"][[1]])
                        status <- xmlValue(uppdrag["status"][[1]])
                        from <- xmlValue(uppdrag["from"][[1]])
                        tom <- xmlValue(uppdrag["tom"][[1]])
                        c(id=intressent,Organ=uppgift,Roll=roll_kod,status=status,From=from,Tom=tom)
                      }
                    }
                    )
                  )
appointment<-data.frame(appoint)

# saving appointment of the persons
#save(appointment,file = "~/R Packages/lab5/Data/appointment.Rda")

api_address<-"http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=10000&utformat=CSV&gruppering="

csv_file<-read.table(text = toString(GET(api_address)),sep = ",",header = TRUE, stringsAsFactors = FALSE)
csv_file <- csv_file[,c("rm","intressent_id","rost","avser","dok_id")]
#excluded_header<-c("hangar_id","rm","beteckning","intressent_id","rost","dok_id")
#write.csv(csv_file,file = "~/R Packages/lab5/Data/voting.csv")

#save(csv_file,file = "~/R Packages/lab5/Data/voting.Rda")

status <- create_database(appointment,person_data_frame,csv_file)
#csv_get<- read.csv(file = "~/R Packages/lab5/Data/voting.csv" , header=TRUE, sep=",")[,excluded_header]

return(status)
}


