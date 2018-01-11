#############################################################################
#                                                                           #
# Copyright (C) Parvathy Jayaprakasan - All Rights Reserved                 #
# Unauthorized copying of this file, via any medium is strictly prohibited  #
# Proprietary and Confidential                                              #
# Written by Parvathy Jayaprakasan, August 2017                             #
#                                                                           #
#############################################################################


###########################
# READING DATA FILES
###########################
library(gdata)
Student2008<-read.xls("/home/anish/Desktop/NCCUCLASS/Thesis/ThesisData_new.xls",sheet=1,perl="/usr/bin/perl")
Test2008<-read.xls("/home/anish/Desktop/NCCUCLASS/Thesis/ThesisData_new.xls",sheet=2,perl="/usr/bin/perl")
Student2009<-read.xls("/home/anish/Desktop/NCCUCLASS/Thesis/ThesisData_new.xls",sheet=3,perl="/usr/bin/perl")
Test2009<-read.xls("/home/anish/Desktop/NCCUCLASS/Thesis/ThesisData_new.xls",sheet=4,perl="/usr/bin/perl")
Student2010<-read.xls("/home/anish/Desktop/NCCUCLASS/Thesis/ThesisData_new.xls",sheet=5,perl="/usr/bin/perl")
Test2010<-read.xls("/home/anish/Desktop/NCCUCLASS/Thesis/ThesisData_new.xls",sheet=6,perl="/usr/bin/perl")

##### Combining cohorts
Student <- rbind(Student2008,Student2009,Student2010)
Test <- rbind(Test2008,Test2009,Test2010)

### common code for 3 cohorts to get cleaned data
names(Student)[names(Student) == 'LAST.TERM.CUM.GPA'] <- 'CUM.GPA'
names(Student)[names(Student) == 'X1ST.ENG'] <- 'FST_ENG'
names(Student)[names(Student) == 'X1ST.MATH'] <- 'FST_MATH'
Student$CUM.GPA<-as.character(Student$CUM.GPA)
Student$CUM.GPA<-as.numeric(Student$CUM.GPA)

#Code common for the three cohorts to change data type
Student$HS.GPA<-as.character(Student$HS.GPA)
Student$HS.GPA<-as.numeric(Student$HS.GPA)

Student$SATT<-as.character(Student$SATT)
Student$SATT<-as.numeric(Student$SATT)

Student$SATV<-as.character(Student$SATV)
Student$SATV<-as.numeric(Student$SATV)

Student$SATM<-as.character(Student$SATM)
Student$SATM<-as.numeric(Student$SATM)
Student$ACT<-as.character(Student$ACT)
Student$ACT<-as.numeric(Student$ACT)

Student$ENTRY.AGE<-as.character(Student$ENTRY.AGE)
Student$ENTRY.AGE<-as.numeric(Student$ENTRY.AGE)
Student$MAJOR = ifelse(grepl("Biology|Mathematics|Chemistry|Physics|Geography|Environmental Sciences|Computer Science",Student$LAST.MAJOR), "STEM","NONSTEM")
Test$COURSE.HRS.ATTEMPTED<-as.character(Test$COURSE.HRS.ATTEMPTED)
Test$COURSE.HRS.ATTEMPTED<-as.numeric(Test$COURSE.HRS.ATTEMPTED)
Test$COURSE.HRS.EARNED<-as.character(Test$COURSE.HRS.EARNED)
Test$COURSE.HRS.EARNED<-as.numeric(Test$COURSE.HRS.EARNED)
Test$GRADE.TERM <- as.character(Test$GRADE.TERM)
Test$LAST.MAJOR = toupper(Student[match(Test$ID,Student$ID),]$LAST.MAJOR)
Test<- Test[!is.na(Test$COURSE.HRS.EARNED),]

#### New dataframe TABLEMajor created to find the most popular(taken by most students) Course for each Major
Test$LAST.MAJOR = toupper(Student[match(Test$ID,Student$ID),]$LAST.MAJOR)
library(plyr)
new2 = ddply(Test,.(LAST.MAJOR,SUBJ),summarise, findcnt = length(ID))
new2$SUBJ<-as.character(new2$SUBJ)
new3 = ddply(new2, .(LAST.MAJOR),function(x) c(findsubj = x[which.max(x$findcnt),2]))
new3<-new3[-1,]
tableMAJOR<-new3
tableMAJOR$LAST.MAJOR<-toupper(tableMAJOR$LAST.MAJOR)
Test$keysubj <- tableMAJOR[match(Test$LAST.MAJOR,tableMAJOR$LAST.MAJOR),]$findsubj
save(tableMAJOR,file="tableMAJOR.Rda")

Test$COURSE.HRS.EARNED[Test$COURSE.HRS.EARNED=="."]=0
Test = Test[!is.na(Test$COURSE.HRS.EARNED),]
Test$point<-ifelse(Test$GRADE=="A",4,ifelse(Test$GRADE=="B",3,ifelse(Test$GRADE == "C",2,ifelse(Test$GRADE == "D",1,0))))
Test$pointsforCRSE<-(Test$COURSE.HRS.EARNED * Test$point)
Test$LAST.MAJOR = toupper(Student[match(Test$ID,Student$ID),]$LAST.MAJOR)
Test$keysubj <- tableMAJOR[match(Test$LAST.MAJOR,tableMAJOR$LAST.MAJOR),]$findsubj
Test$CRSELEVEL = ifelse((Test$SUBJ==Test$keysubj),"MajorRelatedCourse","NonMajorRelatedCourse")
library(plyr)

## Aggregating to add new column 'NCCUHoursEarned' which is the total number of credit hours completed by a student in NCCU
Test_summary <- ddply(Test, .(ID),summarise, NCCUHoursEarned= sum(COURSE.HRS.EARNED))
Student <- merge(Student,Test_summary,by="ID")
Test_summary1 <- ddply(Test, .(ID,CRSELEVEL),summarise, NumCourses= length(ID),MajorHoursAttempted= sum(COURSE.HRS.ATTEMPTED),MajorHoursEarned= sum(COURSE.HRS.EARNED), NumeratorGPA = sum(pointsforCRSE))
Test_summary1$GPA = Test_summary1$NumeratorGPA/Test_summary1$MajorHoursEarned
Test_summary1$GPA[Test_summary1$MajorHoursEarned==0] = 0
library(reshape2)
Test_summary2 <- dplyr::select(Test_summary1,ID,GPA)
Test_summary3 <- dcast(Test_summary1, ID  ~ CRSELEVEL, value.var = "GPA")
Test_summary3[,4]<-NULL


Test[grep("2008FAL",Test$GRADE.TERM),]$GRADE.TERM= "2008TFAL"
Test[grep("2009FAL",Test$GRADE.TERM),]$GRADE.TERM= "2009TFAL"
Test[grep("2010FAL",Test$GRADE.TERM),]$GRADE.TERM= "2010TFAL"
Test[grep("2011FAL",Test$GRADE.TERM),]$GRADE.TERM= "2011TFAL"
Test[grep("2012FAL",Test$GRADE.TERM),]$GRADE.TERM= "2012TFAL"
Test[grep("2013FAL",Test$GRADE.TERM),]$GRADE.TERM= "2013TFAL"
Test[grep("2014FAL",Test$GRADE.TERM),]$GRADE.TERM= "2014TFAL"
Test[grep("2015FAL",Test$GRADE.TERM),]$GRADE.TERM= "2015TFAL"
Test[grep("2016FAL",Test$GRADE.TERM),]$GRADE.TERM= "2016TFAL"

#### Feature Engineering - Calculating & Adding new columns FreshmanGPA, SophomoreGPA, JuniorGPA, SeniorGPA for each student
sortTERM <- function(x) {
  y <- x[order(x$GRADE.TERM),]
  return(y)
} 

newTest<- ddply(Test, ~ID, sortTERM)

TrackHoursfun <- function(id_test)
{
  TrackSumHours = 0
  id_test$new = 0
  n= nrow(id_test)
  for (i in 1: n)
  {
    TrackSumHours = id_test[i,]$COURSE.HRS.EARNED + TrackSumHours
    id_test[i,]$new = TrackSumHours 
  }
  
  return (id_test)
}

newTest2<- ddply(newTest, ~ID, TrackHoursfun)
names(newTest2)[names(newTest2)=="new"] <- "TrackSumHours"
newTest2$NCCUHoursEarned = Student[match(newTest2$ID,Student$ID),]$NCCUHoursEarned
newTest2$EntryType = Student[match(newTest2$ID,Student$ID),]$ENTRY.TYPE
newTest2$StudentTerm = ifelse(newTest2$EntryType == "FRESHMAN" | newTest2$NCCUHoursEarned >90,findInterval(newTest2$TrackSumHours, c(0,29,59,90)),  findInterval(newTest2$TrackSumHours, c(-10,0,29,59)))

newTest2[grep("A-",newTest2$GRADE),]$GRADE = "A"
newTest2[grep("B",newTest2$GRADE),]$GRADE = "B"
newTest2[grep("C",newTest2$GRADE),]$GRADE = "C"
newTest2[grep("D",newTest2$GRADE),]$GRADE = "D"
newTest2$point<-ifelse(newTest2$GRADE=="A",4,ifelse(newTest2$GRADE=="B",3,ifelse(newTest2$GRADE == "C",2,ifelse(newTest2$GRADE == "D",1,0))))
newTest2$pointsforCRSE<-(newTest2$COURSE.HRS.EARNED * newTest2$point)
newTest2[!(newTest2$GRADE %in% c("A","B","C","D","F","NF","WF")), ]$COURSE.HRS.ATTEMPTED =0
GPAbyYear <- ddply(newTest2,.(ID,StudentTerm),summarise,MajorHoursAttempted= sum(COURSE.HRS.ATTEMPTED),MajorHoursEarned= sum(COURSE.HRS.EARNED), NumeratorGPA = sum(pointsforCRSE))
#GPAbyYear <- GPAbyYear[GPAbyYear$MajorHoursEarned!=0.0,]
GPAbyYear$GPA = GPAbyYear$NumeratorGPA/GPAbyYear$MajorHoursAttempted
GPAbyYear$StudentTerm <- ifelse(GPAbyYear$StudentTerm=="1","FreshmanGPA",ifelse(GPAbyYear$StudentTerm=="2","SophomoreGPA",ifelse(GPAbyYear$StudentTerm=="3","JuniorGPA","SeniorGPA")))
GPA_summary <- dcast(GPAbyYear, ID ~ StudentTerm, value.var = "GPA")
finalDF<-merge(Student,Test_summary3, by ="ID")
finalDF<-merge(finalDF,GPA_summary, by ="ID")

## ADDING THE NEW COLUMN 'GatekeeperGPA' by calculating it from the students' Test score
## 'GatekeeperGPA' is a potential predictor variable in our analysis
GateKeeperAttemptsfun <- function(x)
{
  z = ddply(x,.(ID),summarise,numAttempts = length(ID),numCoursesAttempted=length(unique(COURSE)))
  z$AvgAttempts = z$numAttempts/z$numCoursesAttempted
  return(z)
}
GateKeeperGPAfun <- function(x,GateKeeperforAttempts,CreditHour)
{ x$GRADE <- as.character(x$GRADE)
x$GRADE<- sapply(x$GRADE,function(m) switch(m, A=4, B=3,C=2,D=1,F=0,0))
#View(x)   #debugging
new = ddply(x,.(ID,COURSE),summarise,GRADE = max((GRADE)))
new$COURSE.HRS.ATTEMPTED = CreditHour
new$COURSE.HRS.EARNED = ifelse(new$GRADE==0,0,CreditHour)
new$pointsforCRSE<-(new$COURSE.HRS.EARNED * new$GRADE)
#View(new)
y = ddply(new,.(ID),summarise,GateKeeperHoursAttempted= sum(COURSE.HRS.ATTEMPTED),GateKeeperHoursEarned= sum(COURSE.HRS.EARNED), NumeratorGPA = sum(pointsforCRSE))
y$GateKeeperGPA = y$NumeratorGPA / y$GateKeeperHoursAttempted
#View(y)
z= GateKeeperAttemptsfun(GateKeeperforAttempts)
sent = merge(y,z,by="ID")
return(sent)
}

library(plyr)
newBIOL =  data.frame(ID= Test[Test$COURSE == "BIOL2200" & match(finalDF$ID,Test$ID),]$ID,GRADE=Test[Test$COURSE == "BIOL2200" & match(finalDF$ID,Test$ID),]$GRADE,LAST.MAJOR=Test[Test$COURSE == "BIOL2200" & match(finalDF$ID,Test$ID),]$LAST.MAJOR, COURSE = Test[Test$COURSE == "BIOL2200"  & match(finalDF$ID,Test$ID),]$COURSE ,COURSE.HRS.EARNED = Test[Test$COURSE == "BIOL2200"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.EARNED ,COURSE.HRS.ATTEMPTED = Test[Test$COURSE == "BIOL2200"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.ATTEMPTED )
GateKeeperBiologyforAttempts <- dplyr::filter(newBIOL,LAST.MAJOR == "BIOLOGY")
GateKeeperBiologyforGPA <- dplyr::filter(newBIOL,LAST.MAJOR == "BIOLOGY", GRADE %in% c("A","B","C","D","F","NF","WF"))
FinalGateKeeperBiology <- GateKeeperGPAfun(GateKeeperBiologyforGPA,GateKeeperBiologyforAttempts,4)


newCOMP =  data.frame(ID= Test[Test$COURSE == "COMP2810" & match(finalDF$ID,Test$ID),]$ID,GRADE=Test[Test$COURSE == "COMP2810" & match(finalDF$ID,Test$ID),]$GRADE,LAST.MAJOR=Test[Test$COURSE == "COMP2810" & match(finalDF$ID,Test$ID),]$LAST.MAJOR, COURSE = Test[Test$COURSE == "COMP2810"  & match(finalDF$ID,Test$ID),]$COURSE ,COURSE.HRS.EARNED = Test[Test$COURSE == "COMP2810"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.EARNED ,COURSE.HRS.ATTEMPTED = Test[Test$COURSE == "COMP2810"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.ATTEMPTED)
GateKeeperComputerScienceforAttempts <- dplyr::filter(newCOMP,LAST.MAJOR == "COMPUTER SCIENCE")
GateKeeperComputerScienceforGPA <- dplyr::filter(newCOMP,LAST.MAJOR == "COMPUTER SCIENCE",GRADE %in% c("A","B","C","D","F","NF","WF"))
FinalGateKeeperComputerScience <- GateKeeperGPAfun(GateKeeperComputerScienceforGPA,GateKeeperComputerScienceforAttempts,3 )

newMATH =  data.frame(ID= Test[Test$COURSE == "MATH3020" | Test$COURSE == "MATH2010"  & match(finalDF$ID,Test$ID),]$ID,GRADE=Test[Test$COURSE == "MATH3020" | Test$COURSE == "MATH2010"  & match(finalDF$ID,Test$ID),]$GRADE,LAST.MAJOR=Test[Test$COURSE == "MATH3020"|Test$COURSE == "MATH2010"  & match(finalDF$ID,Test$ID),]$LAST.MAJOR,COURSE = Test[Test$COURSE == "MATH3020" | Test$COURSE == "MATH2010"  & match(finalDF$ID,Test$ID),]$COURSE ,COURSE.HRS.EARNED = Test[Test$COURSE == "MATH3020" | Test$COURSE == "MATH2010"  & match(finalDF$ID,Test$ID),]$COURSE.HRS.EARNED ,COURSE.HRS.ATTEMPTED = Test[Test$COURSE == "MATH3020" | Test$COURSE == "MATH2010"  & match(finalDF$ID,Test$ID),]$COURSE.HRS.ATTEMPTED )
#newMATH2 =  data.frame(ID= Test[Test$COURSE == "MATH3020" & match(finalDF$ID,Test$ID),]$ID,GRADE=Test[Test$COURSE == "MATH3020" & match(finalDF$ID,Test$ID),]$GRADE,LAST.MAJOR=Test[Test$COURSE == "MATH3020" & match(finalDF$ID,Test$ID),]$LAST.MAJOR)
GateKeeperMathematicsforAttempts <- dplyr::filter(newMATH,LAST.MAJOR == "MATHEMATICS" | LAST.MAJOR == "MATHEMATICS, SECONDARY EDUC")
GateKeeperMathematicsforGPA <- dplyr::filter(newMATH,LAST.MAJOR == "MATHEMATICS" | LAST.MAJOR == "MATHEMATICS, SECONDARY EDUC",GRADE %in% c("A","B","C","D","F","NF","WF"))
FinalGateKeeperMathematics <- GateKeeperGPAfun(GateKeeperMathematicsforGPA,GateKeeperMathematicsforAttempts,3 )
#GateKeeperMathematicsforGPA is a subset of GateKeeperMathematicsforAttempts

newPHYS =  data.frame(ID= Test[Test$COURSE == "PHYS2305" | Test$COURSE == "PHYS2310" |Test$COURSE == "PHYS2320" | Test$COURSE == "PHYS3310"    & match(finalDF$ID,Test$ID),]$ID,GRADE=Test[Test$COURSE == "PHYS2305" | Test$COURSE == "PHYS2310" |Test$COURSE == "PHYS2320" | Test$COURSE == "PHYS3310"  & match(finalDF$ID,Test$ID),]$GRADE,LAST.MAJOR=Test[Test$COURSE == "PHYS2305" | Test$COURSE == "PHYS2310" |Test$COURSE == "PHYS2320" | Test$COURSE == "PHYS3310"  & match(finalDF$ID,Test$ID),]$LAST.MAJOR,COURSE = Test[Test$COURSE == "PHYS2305" | Test$COURSE == "PHYS2310" |Test$COURSE == "PHYS2320" | Test$COURSE == "PHYS3310"   & match(finalDF$ID,Test$ID),]$COURSE ,COURSE.HRS.EARNED = Test[Test$COURSE == "PHYS2305" | Test$COURSE == "PHYS2310" |Test$COURSE == "PHYS2320" | Test$COURSE == "PHYS3310"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.EARNED ,COURSE.HRS.ATTEMPTED = Test[Test$COURSE == "PHYS2305" | Test$COURSE == "PHYS2310" |Test$COURSE == "PHYS2320" | Test$COURSE == "PHYS3310"  & match(finalDF$ID,Test$ID),]$COURSE.HRS.ATTEMPTED )
GateKeeperPhysicsforGPA <- dplyr::filter(newPHYS,LAST.MAJOR=="PHYSICS, GENERAL",GRADE %in% c("A","B","C","D","F","NF","WF"))
GateKeeperPhysicsforAttempts <- dplyr::filter(newPHYS,LAST.MAJOR=="PHYSICS, GENERAL")
FinalGateKeeperPhysics<- GateKeeperGPAfun(GateKeeperPhysicsforGPA,GateKeeperPhysicsforAttempts ,3 )


newCHEM =  data.frame(ID= Test[Test$COURSE == "CHEM1100" | Test$COURSE == "CHEM1200"  & match(finalDF$ID,Test$ID),]$ID,GRADE=Test[Test$COURSE == "CHEM1100" | Test$COURSE == "CHEM1200"   & match(finalDF$ID,Test$ID),]$GRADE,LAST.MAJOR=Test[Test$COURSE == "CHEM1100" | Test$COURSE == "CHEM1200"   & match(finalDF$ID,Test$ID),]$LAST.MAJOR,COURSE = Test[Test$COURSE == "CHEM1100" | Test$COURSE == "CHEM1200"   & match(finalDF$ID,Test$ID),]$COURSE ,COURSE.HRS.EARNED = Test[Test$COURSE == "CHEM1100" | Test$COURSE == "CHEM1200"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.EARNED ,COURSE.HRS.ATTEMPTED = Test[Test$COURSE == "CHEM1100" | Test$COURSE == "CHEM1200"  & match(finalDF$ID,Test$ID),]$COURSE.HRS.ATTEMPTED )
GateKeeperChemistryforGPA <- dplyr::filter(newCHEM,LAST.MAJOR=="CHEMISTRY",GRADE %in% c("A","B","C","D","F","NF","WF"))
GateKeeperChemistryforAttempts <- dplyr::filter(newCHEM,LAST.MAJOR=="CHEMISTRY")
FinalGateKeeperChemistry <- GateKeeperGPAfun(GateKeeperChemistryforGPA,GateKeeperChemistryforAttempts,4 )

newENSC =  data.frame(ID= Test[Test$COURSE == "ENSC3950" & match(finalDF$ID,Test$ID),]$ID,GRADE=Test[Test$COURSE == "ENSC3950" & match(finalDF$ID,Test$ID),]$GRADE,LAST.MAJOR=Test[Test$COURSE == "ENSC3950" & match(finalDF$ID,Test$ID),]$LAST.MAJOR,COURSE = Test[Test$COURSE == "ENSC3950"  & match(finalDF$ID,Test$ID),]$COURSE ,COURSE.HRS.EARNED = Test[Test$COURSE == "ENSC3950"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.EARNED ,COURSE.HRS.ATTEMPTED = Test[Test$COURSE == "ENSC3950"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.ATTEMPTED)
GateKeeperEnvironmentalScienceforGPA <- dplyr::filter(newENSC,LAST.MAJOR== "ENVIRONMENTAL SCIENCE" ,GRADE %in% c("A","B","C","D","F","NF","WF"))
GateKeeperEnvironmentalScienceforAttempts<- dplyr::filter(newENSC,LAST.MAJOR== "ENVIRONMENTAL SCIENCE" )
FinalGateKeeperEnvironmentalScience <- GateKeeperGPAfun(GateKeeperEnvironmentalScienceforGPA,GateKeeperEnvironmentalScienceforAttempts,3 )

newGEOG =  data.frame(ID= Test[Test$COURSE == "GEOG2100" & match(finalDF$ID,Test$ID),]$ID,GRADE=Test[Test$COURSE == "GEOG2100" & match(finalDF$ID,Test$ID),]$GRADE,LAST.MAJOR=Test[Test$COURSE == "GEOG2100" & match(finalDF$ID,Test$ID),]$LAST.MAJOR,COURSE = Test[Test$COURSE == "GEOG2100" & match(finalDF$ID,Test$ID),]$COURSE ,COURSE.HRS.EARNED = Test[Test$COURSE == "GEOG2100"   & match(finalDF$ID,Test$ID),]$COURSE.HRS.EARNED ,COURSE.HRS.ATTEMPTED = Test[Test$COURSE == "GEOG2100"  & match(finalDF$ID,Test$ID),]$COURSE.HRS.ATTEMPTED)
GateKeeperGeographyforGPA <- dplyr::filter(newGEOG, LAST.MAJOR== "GEOGRAPHY", GRADE %in% c("A","B","C","D","F","NF","WF"))
GateKeeperGeographyforAttempts <- dplyr::filter(newGEOG, LAST.MAJOR== "GEOGRAPHY")
FinalGateKeeperGeography <- GateKeeperGPAfun(GateKeeperGeographyforGPA,GateKeeperGeographyforAttempts,3 )

GateKeeperDF<-rbind(FinalGateKeeperBiology,FinalGateKeeperMathematics,FinalGateKeeperComputerScience,FinalGateKeeperPhysics,FinalGateKeeperChemistry,FinalGateKeeperEnvironmentalScience,FinalGateKeeperGeography)
GateKeeperDF$LAST.MAJOR = toupper(Student[match(GateKeeperDF$ID,Student$ID),]$LAST.MAJOR)
library(dplyr)
GateKeeperDF$GateKeeperGPA <- as.numeric(GateKeeperDF$GateKeeperGPA)
GateKeepertoAdd <- select(GateKeeperDF,ID,GateKeeperGPA,numAttempts,AvgAttempts)
STEMstudents <- merge(finalDF,GateKeepertoAdd,by="ID")

#### Appending City column to final data
NCschool<-read.csv("/home/anish/Desktop/NCCUCLASS/Thesis/schools.csv",sep=",", header = TRUE)
finalDF$City<- NCschool[match(finalDF$HS.CEEB.CODE,NCschool$High.School.Code),]$City
schooldist<-read.csv("/home/anish/Desktop/NCCUCLASS/Thesis/schooldist.csv",sep=",", header = TRUE)
schooldist<-schooldist[,c(3,6)]
schooldist$City.Zip<- gsub('[0-9,,,\n," "]+', '', schooldist$City.Zip)
schooldist$City.Zip<-toupper(schooldist$City.Zip)
schooldist[schooldist$City.Zip=="CHAPELHILL",]$City.Zip <- "CHAPEL HILL"
finalDF$School.district<-schooldist[match(finalDF$City,schooldist$City.Zip),]$School.District
finalDF$GRADUATE<-ifelse(finalDF$GRAD.TERM=="","N","Y")
finalDF$GRADUATE<-ifelse(finalDF$GRADUATE=="N",0,1)
finalDF$GRADUATE<- as.factor(finalDF$GRADUATE)
finalDF$prob<- (finalDF$NCCUHoursEarned/124)*finalDF$CUM.GPA
finalDF$prob<- (finalDF$prob- min(finalDF$prob))/(max(finalDF$prob)-min(finalDF$prob))
#finalDF is the Dataframe with all data from the 3 cohorts


####################
#CORRELATION ANALYSIS to see how the  predictors are correlated with each other  
####################
correlationDF <- dplyr::select(finalDF,SATV,SATT,SATM,HS.GPA,CUM.GPA,FreshmanGPA,SophomoreGPA,JuniorGPA,SeniorGPA,MajorRelatedCourse)
#correlationDF <- dplyr::select(STEM,SATV,SATT,SATM,HS.GPA,CUM.GPA,GateKeeperGPA,FreshmanGPA,SophomoreGPA,JuniorGPA,SeniorGPA,MajorRelatedCourseGPA)
correlationDF <-na.omit(correlationDF)
correlationMatrix <-cor(correlationDF )
head(round(correlationMatrix ,2))
library(corrplot)
corrplot(correlationMatrix , method="number")


## filtering to get the dataframe with variables of interest
df_new <- dplyr::select(finalDF,ID,SATV,SATT,SATM,HS.GPA,CUM.GPA,FST_ENG,FST_MATH,City,School.district,GRADUATE,ENTRY.TYPE,PELL.ELIGIBILE,MAJOR,ETHNIC,GENDER,FreshmanGPA,SophomoreGPA,JuniorGPA,SeniorGPA)
df_new$MAJOR<-as.factor(df_new$MAJOR)
df_new<-na.omit(df_new)
sample.df <- df_new
ID.VAR <- "ID"
TARGET.VAR <- "CUM.GPA"
CLASS.VAR <- "GRADUATE"
### Feature selection for Boruta
candidate.features <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR,CLASS.VAR))
# pull out the response variable
response <- as.factor(sample.df$GRADUATE)

# remove identifier and response variables
sample.df <- sample.df[candidate.features]
set.seed(13)
bor.results <- Boruta(sample.df,response,
                      maxRuns=101,
                      doTrace=0)
#bor.results$finalDecision
plot(bor.results)
attStats(bor.results)
CONFIRMED_ATTR <- c("FreshmanGPA","FST_MATH","HS.GPA","JuniorGPA","SATM","SATV","SophomoreGPA","SeniorGPA")
TENTATIVE_ATTR <- c("FST_ENG","GENDER" )
REJECTED_ATTR <-  c("City","ENTRY.TYPE","ETHNIC","MAJOR","PELL.ELIGIBILE","School.district")

df_new <- dplyr::select(finalDF,SATV,SATT,SATM,HS.GPA,CUM.GPA,GRADUATE,GENDER,ENTRY.TYPE,FST_MATH,FST_ENG,FreshmanGPA,SophomoreGPA,JuniorGPA,SeniorGPA)
#df_new <- dplyr::select(STEMstudents,SATV,SATT,SATM,HS.GPA,CUM.GPA,FST_MATH,FST_ENG,GRADUATE,FreshmanGPA,SophomoreGPA,JuniorGPA,SeniorGPA,GateKeeperGPA,AvgAttempts)
df_new<-na.omit(df_new)# GBM doesn't handle NA
df_new$GRADUATE <- as.factor(ifelse(df_new$GRADUATE=="1","Y","N"))
smp_size <- floor(0.8 * nrow(df_new))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_new)), size = smp_size)
train_test <- df_new[train_ind, ]
cross_validation <-df_new[-train_ind, ]
smp_size <- floor(0.75 * nrow(train_test))
set.seed(123)
train_ind <- sample(seq_len(nrow(train_test)), size = smp_size)
train<- train_test[train_ind, ]
test <- train_test[-train_ind, ]

# 10 Fold crossvalidation to fit the model 
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final', # To save out of fold predictions for best parameter combinantions
  classProbs = T # To save the class probabilities of the out of fold predictions
)

#Defining the predictors and outcome
predictors<- c("FreshmanGPA","HS.GPA","JuniorGPA","SATT","SophomoreGPA","SeniorGPA","ENTRY.TYPE","FST_MATH","FST_ENG","GENDER")
predictors2<-c("SATT","HS.GPA","FreshmanGPA","SophomoreGPA","JuniorGPA","SeniorGPA")
outcomeName<- "GRADUATE"

###################################
#STACKED ENSEMBLE MODEL
###################################
set.seed(1)
#Training the random forest model
model_rf<-train (train[,predictors],train[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)
#Training the logistic regression model
model_lr<-train(train[,predictors],train[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)                
#Training the knn model
model_knn<-train(train[,predictors2],train[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)
#Training the GBM model 
model_gbm<- train(train[,predictors],train[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)

### Evaluating Indvidual Models

test$pred_rfOnly<-predict(model_rf,test[predictors],type='prob')$Y
test$pred_lrOnly<-predict(model_lr,test[predictors],type='prob')$Y   
test$pred_knnOnly<-predict(model_knn,test[predictors2],type='prob')$Y  
test$pred_gbmOnly<-predict(model_gbm,test[predictors],type='prob')$Y   



thresh  <- 0.9     # threshold for categorizing predicted probabilities
predFac <- cut(test$pred_rfOnly, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
cTab_RFonly    <- table( predFac, test$GRADUATE,dnn=c( "predicted","actual"))
addmargins(cTab_RFonly )
(Accuracy_RFonly<-sum(diag(cTab_RFonly))/sum(cTab_RFonly))
Sensitivity_RFonly_ClassY <- cTab_RFonly[2,2]/(cTab_RFonly[2,2]+ cTab_RFonly[1,2])
Sensitivity_RFonly_ClassN <- cTab_RFonly[1,1]/(cTab_RFonly[1,1]+ cTab_RFonly[2,1])

thresh  <- 0.9     # threshold for categorizing predicted probabilities
predFac <- cut(test$pred_lrOnly, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
cTab_LRonly    <- table( predFac, test$GRADUATE,dnn=c( "predicted","actual"))
addmargins(cTab_LRonly )
(Accuracy_LRonly<-sum(diag(cTab_LRonly))/sum(cTab_LRonly))
Sensitivity_LRonly_ClassY <- cTab_LRonly[2,2]/(cTab_LRonly[2,2]+ cTab_LRonly[1,2])
Sensitivity_LRonly_ClassN <- cTab_LRonly[1,1]/(cTab_LRonly[1,1]+ cTab_LRonly[2,1])

thresh  <- 0.9     # threshold for categorizing predicted probabilities
predFac <- cut(test$pred_knnOnly, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
cTab_KNNonly    <- table( predFac, test$GRADUATE,dnn=c( "predicted","actual"))
addmargins(cTab_KNNonly )
(Accuracy_KNNonly<-sum(diag(cTab_KNNonly))/sum(cTab_KNNonly))
Sensitivity_KNNonly_ClassY <- cTab_KNNonly[2,2]/(cTab_KNNonly[2,2]+ cTab_KNNonly[1,2])
Sensitivity_KNNonly_ClassN <- cTab_KNNonly[1,1]/(cTab_KNNonly[1,1]+ cTab_KNNonly[2,1])

thresh  <- 0.9     # threshold for categorizing predicted probabilities
predFac <- cut(test$pred_gbmOnly, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
cTab_GBMonly    <- table( predFac, test$GRADUATE,dnn=c( "predicted","actual"))
addmargins(cTab_GBMonly )
(Accuracy_GBMonly<-sum(diag(cTab_GBMonly))/sum(cTab_GBMonly))
Sensitivity_GBMonly_ClassY <- cTab_GBMonly[2,2]/(cTab_GBMonly[2,2]+ cTab_GBMonly[1,2])
Sensitivity_GBMonly_ClassN <- cTab_GBMonly[1,1]/(cTab_GBMonly[1,1]+ cTab_GBMonly[2,1])

#### Correlation of the vectors into second level 
library(caret)
results <- resamples(list(mod1 = model_rf, mod2 = model_lr, mod3 = model_knn, mod4 = model_gbm)) 
modelCor(results) 

#Predicting the out of fold prediction probabilities for training data
train$OOF_pred_rf<-model_rf$pred$Y[order(model_rf$pred$rowIndex)]
train$OOF_pred_lr<-model_lr$pred$Y[order(model_lr$pred$rowIndex)]
train$OOF_pred_knn<-model_knn$pred$Y[order(model_knn$pred$rowIndex)]
train$OOF_pred_gbm<-model_gbm$pred$Y[order(model_gbm$pred$rowIndex)]

#Predicting probabilities for the test data
test$OOF_pred_rf<-predict(model_rf,test[predictors],type='prob')$Y
test$OOF_pred_lr<-predict(model_lr,test[predictors],type='prob')$Y   
test$OOF_pred_knn<-predict(model_knn,test[predictors2],type='prob')$Y 
test$OOF_pred_gbm<-predict(model_gbm,test[predictors],type='prob')$Y 

#Predictors for top layer models for GBM stacked
predictors_top<-c('OOF_pred_rf','OOF_pred_lr','OOF_pred_knn') 
outcomeName<- "GRADUATE"

#GBM as top layer model 
model_gbmStacked<- train(train[,predictors_top],train[,outcomeName],method='gbm',trControl=fitControl,tuneLength=2)              

#predict using GBM top layer model
#test$gbm_stacked<-predict(model_gbm,testSet[,predictors_top])
test$pred_gbmStacked<-predict(model_gbmStacked,test[predictors_top],type='prob')$Y  

thresh  <- 0.9     # threshold for categorizing predicted probabilities
predFac <- cut(test$pred_gbmStacked, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
cTab_gbmStacked    <- table( predFac, test$GRADUATE,dnn=c( "predicted","actual"))
addmargins(cTab_gbmStacked)
(Accuracy_gbmStacked<-sum(diag(cTab_gbmStacked))/sum(cTab_gbmStacked))
Sensitivity_gbmStacked_ClassY <- cTab_gbmStacked[2,2]/(cTab_gbmStacked[2,2]+ cTab_gbmStacked[1,2])
Sensitivity_gbmStacked_ClassN <- cTab_gbmStacked[1,1]/(cTab_gbmStacked[1,1]+ cTab_gbmStacked[2,1])


#Predictors for top layer models for GLM stacked
predictors_top<-c('OOF_pred_rf','OOF_pred_gbm','OOF_pred_knn') 
outcomeName<- "GRADUATE"

#GLM as top layer model 
model_lrStacked<- train(train[,predictors_top],train[,outcomeName],method='glm',trControl=fitControl,tuneLength=2)              

#predict using GBM top layer model
#test$gbm_stacked<-predict(model_gbm,testSet[,predictors_top])
test$pred_lrStacked<-predict(model_lrStacked,test[predictors_top],type='prob')$Y  

thresh  <- 0.9     # threshold for categorizing predicted probabilities
predFac <- cut(test$pred_lrStacked, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
cTab_lrStacked    <- table( predFac, test$GRADUATE,dnn=c( "predicted","actual"))
addmargins(cTab_lrStacked)
(Accuracy_lrStacked<-sum(diag(cTab_lrStacked))/sum(cTab_lrStacked))
Sensitivity_lrStacked_ClassY <- cTab_lrStacked[2,2]/(cTab_lrStacked[2,2]+ cTab_lrStacked[1,2])
Sensitivity_lrStacked_ClassN <- cTab_lrStacked[1,1]/(cTab_lrStacked[1,1]+ cTab_lrStacked[2,1])

# Precision<- cTab[2,2]/(cTab[2,2]+ cTab[2,1])
# Recall<-cTab[2,2]/(cTab[2,2]+ cTab[1,2])
AccuracyTable<- rbind(Accuracy_RFonly,Accuracy_LRonly,Accuracy_KNNonly,Accuracy_GBMonly,Accuracy_gbmStacked,Accuracy_lrStacked)
Sensitivity_ClassY <- rbind(Sensitivity_RFonly_ClassY,Sensitivity_LRonly_ClassY,Sensitivity_KNNonly_ClassY,Sensitivity_GBMonly_ClassY,Sensitivity_gbmStacked_ClassY,Sensitivity_lrStacked_ClassY)
Sensitivity_ClassN <- rbind(Sensitivity_RFonly_ClassN,Sensitivity_LRonly_ClassN,Sensitivity_KNNonly_ClassN,Sensitivity_GBMonly_ClassN,Sensitivity_gbmStacked_ClassN,Sensitivity_lrStacked_ClassN)
# Final table summarisig the model performance
ComparisonTable <- cbind(AccuracyTable,Sensitivity_ClassY,Sensitivity_ClassN)








