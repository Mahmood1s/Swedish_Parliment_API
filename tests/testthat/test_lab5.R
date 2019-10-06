library(testthat)
library(lab5)
# install.packages(DBI)
# library(DBI)
# library(RSQLite)
#test_check("lab5")
context("APIPackage")


wrong_members <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

wrong_appointments <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
wrong_voting <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

my_db_file<-"~/R Packages/lab5/DatabaseDir/parliment_database.sqlite"
my_database <- DBI::dbConnect(RSQLite::SQLite(), dbname=my_db_file) 

# member_data_frame <-as.data.frame(DBI::dbReadTable(my_database,'members'))
# appointments_data_frame <- as.data.frame(DBI::dbReadTable(my_database,'appointments'))
# voting_data_frame <- as.data.frame(DBI::dbReadTable(my_database,'voting'))

rs <- DBI::dbSendQuery(my_database,"select * from members")
member_data_frame <- as.data.frame(DBI::dbFetch(rs))
DBI::dbClearResult(rs)
rs <- DBI::dbSendQuery(my_database,"select * from appointments")
appointments_data_frame <- as.data.frame(DBI::dbFetch(rs))
DBI::dbClearResult(rs)
rs <- DBI::dbSendQuery(my_database,"select * from voting")
voting_data_frame <- as.data.frame(DBI::dbFetch(rs))
DBI::dbClearResult(rs)

DBI::dbDisconnect(my_database)


test_that("Error messages are returned for erronous input to create_database function.", {

  expect_error(create_database(wrong_members, wrong_appointments,wrong_voting))
  expect_error(create_database(wrong_voting))
  expect_error(create_database(c(1,2,3,4)))
  expect_error(get_genderdata(c(1,2,3,4)))
  expect_error(create_database(wrong_members,wrong_appointments,c("wrong input")))
})

test_that("Function is correct", {
  obj <- create_database(appointments_data_frame,member_data_frame,voting_data_frame)
  expect_true(obj == "TRUE")
  obj <- get_memdata("id","")
  expect_true(length(obj[,1]) == 0)
  obj <- get_memdata("0390912778804","")
  expect_true(class(obj)=="data.frame")
  
})

test_that("inputs are correct create_database/get_member function.", {
  expected_obj <- TRUE
  output_df <-c("Riksdagsledamot")
  expect_equal(create_database(appointments_data_frame,member_data_frame,voting_data_frame), expected_obj)
  expect_equal(get_memdata("0390912778804","")[1,2],output_df[1])
})
