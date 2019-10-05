required_packages<-function(){
  
  install.packages("httr")
  install.packages("dplyr")
  install.packages("dbplyr")
  require("httr")
  require("XML")
  require("methods")
  
  require(dplyr)
  require(dbplyr)
  require(RSQLite)
}

create_database<-function(appointment,person_data_frame,csv_file){
  
  my_db_file<-"~/R Packages/lab5/Data/parliment_database.sqlite"
  my_database = dbConnect(SQLite(), dbname=my_db_file)
#  dbSendQuery(conn=my_database,
#              "CREATE TABLE members(
#                intressent_id TEXT,
#                efternamn TEXT,
#                tilltalsnamn TEXT,
#                parti TEXT,
#                kon TEXT,
#                valkrets TEXT,
#                fodd_ar INT,
#                status TEXT,
#                PRIMARY KEY (intressent_id))
#              ")
 # dbSendQuery(conn=my_database,
 #             "CREATE TABLE appointments(
 #               intressent_id TEXT,
 #               uppgift TEXT,
 #               roll_kod TEXT,
 #               status TEXT,
 #               from DATETIME,
 #               tom DATETIME,
 #               PRIMARY KEY (member_id))
 #             ")
 #  
   #  dbSendQuery(conn=my_database,
   #              "CREATE TABLE voting(
   #                rm TEXT,
   #                intressent_id INT,
   #                rost TEXT,
   #                avser TEXT,
   #                dok_id TEXT)
   #              ")
   # dbSendQuery(conn=my_database,
   #             "DROP TABLE voting")
  
  dbWriteTable(conn=my_database, name="members", person_data_frame,append=T, row.names=F)
  dbWriteTable(conn=my_database, name="appointments", appointment,append=T, row.names=F)
  dbWriteTable(conn=my_database, name="voting", csv_file,append=T, row.names=F)
  # dbListTables(my_database)
  #dbListFields(db, "members")
  # my_database<- src_sqlite(my_db_file,create = TRUE)
  #copy_to(my_database,df = appointment)
  #copy_to(my_database,df = person_data_frame)
  #copy_to(my_database,df = csv_file)
  
}

data_fetch<-function(){
  
required_packages()
api_address<-"http://data.riksdagen.se/personlista/?iid=&fnamn=&enamn=&f_ar=&kn=&parti=&valkrets=&rdlstatus=samtliga&org=&utformat=xml&sort=sorteringsnamn&sortorder=asc&termlista="


get_xml<-GET(api_address)

xml_content<- content(get_xml)

content_data<-XML::xmlParse(xml_content)
content_xml <- XML::getNodeSet(content_data,"/personlista", fun=XML::xmlToDataFrame)
content_data_frame <- data.frame(do.call(rbind, content_xml))
person_data_frame <- data.frame(content_data_frame["intressent_id"],content_data_frame["efternamn"],content_data_frame["tilltalsnamn"],content_data_frame["parti"],content_data_frame["kon"],content_data_frame["valkrets"],content_data_frame["fodd_ar"],content_data_frame["status"])
person_data_frame <- person_data_frame[!person_data_frame$intressent_id %in% "",]

# saving person data frame
#save(person_data_frame,file = "~/R Packages/lab5/Data/person.Rda")

xml_1 <- XML::xmlInternalTreeParse(xml_content,useInternalNodes=TRUE)
tags  <- xml_1["//personlista/person/personuppdrag/uppdrag"]
appoint <- do.call(rbind,
                  lapply(tags,function(uppdrag)
                    {
                     # if(as.numeric(XML::xmlValue(uppdrag["intressent_id"][[1]]))==0640627133410)
                      {
                        intressent <- XML::xmlValue(uppdrag["intressent_id"][[1]])
                        uppgift  <- XML::xmlValue(uppdrag["uppgift"][[1]])
                        roll_kod  <- XML::xmlValue(uppdrag["roll_kod"][[1]])
                        status <- XML::xmlValue(uppdrag["status"][[1]])
                        from <- XML::xmlValue(uppdrag["from"][[1]])
                        tom <- XML::xmlValue(uppdrag["tom"][[1]])
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

create_database(appointment,person_data_frame,csv_file)
#csv_get<- read.csv(file = "~/R Packages/lab5/Data/voting.csv" , header=TRUE, sep=",")[,excluded_header]
}



#print(content_data_frame)
