student_name=c("A", "B" ,"C", "D", "E", "F", "G", "H", "I", "J", "H", "K", "L", "M", "N", "O", "P", "Q", "R", "S")
student_seatno=c("12270750", "12270751", "12270752", "12270753", "12270754", "12270755", "12270756", "12270757", "12270758", "12270759", "12270760", "12270761", "12270762", "12270763", "12270764", "12270765", "12270766", "12270767", "12270769", "12270770")
sub_END=c(84, 40, 54, 61, 20, 36, 52, 62, 72, 46, 47, 22, 37, 87, 65, 95, 65, 75, 86, 93)
sub_IS=c(80, 30, 61, 64, 56, 54, 71, 58, 69, 80, 60, 14, 48, 71, 70, 78, 63, 72, 82, 37)
sub_AI=c(83, 34, 61, 67, 31, 66, 52, 60, 68, 88, 77, 78, 35, 34, 84, 95, 58, 88, 88, 79)
sub_DL=c(85, 31, 56, 50, 44, 30, 53, 28, 66, 39, 49, 54, 45, 82, 30, 73, 50, 68, 72, 57)
sub_IL=c(88, 20, 47, 45, 48, 43, 62, 60, 32, 96, 33, 68, 30, 82, 74, 79, 53, 49, 75, 33)
sub_Project1=c(83, 35, 83, 78, 63, 76, 75, 83, 73, 84, 80, 70, 71, 95, 83, 82, 76, 79, 90, 81)
students.data<-data.frame(student_name,student_seatno,sub_END,sub_IS,sub_AI,sub_DL,sub_IL,sub_Project1)
Total_Marks<-rowSums(students.data[,3:8])
Percentage<-(Total_Marks/600)*100
#print(Percentage)
students.data<-cbind(students.data,Total_Marks)
students.data<-cbind(students.data,Percentage)
df<-students.data[order(students.data$Percentage),]
print("Top 5")
apply(tail(df,5),2,rev)
print("Last 5")
head(df,5)

median1<-median(students.data$Percentage)

#for (i in students.data$Percentage){
 # if (i<median1){
  #  print(i)
   # print(students.data)
  #}
#}
#install.packages("dplyr")
library(dplyr)
print("Percentage below Median")
filter(students.data, students.data$Percentage<median1)

Rank1<-c()
count1=1
for (i in Percentage){
  if (i>=80){
    Rank1[count1]='A'
    count1=count1+1
  }
  else if (i<80 && i>=75){
    Rank1[count1]='B'
    count1=count1+1
  }
  else if (i<75 && i>=55){
    Rank1[count1]='C'
    count1=count1+1
  }
  else if (i<55 && i>=40){
    Rank1[count1]='D'
    count1=count1+1
  }
  else if (i<40 ){
    Rank1[count1]='E'
    count1=count1+1
  }
}
print(Rank1)
students.data<-cbind(students.data,Rank1)
print(students.data)

filter(students.data, students.data$Rank1=='A')
print("------------------------------------------------")
filter(students.data, students.data$Rank1=='B')
print("------------------------------------------------")
filter(students.data, students.data$Rank1=='C')
print("------------------------------------------------")
filter(students.data, students.data$Rank1=='D')
print("------------------------------------------------")
filter(students.data, students.data$Rank1=='E')

print(students.data)

subjects=c(sub_AI,sub_DL,sub_END,sub_IL,sub_IS,sub_Project1)

filter(students.data, students.data$sub_END==35)
Total_Score=c()
sub_AI1<-list(students.data$sub_AI)
sub_DL1<-list(students.data$sub_DL)
sub_END1<-list(students.data$sub_END)
sub_IL1<-list(students.data$sub_IL)
sub_IS1<-list(students.data$sub_IS)
sub_Project1_1<-list(students.data$sub_Project1)
print(sub_AI1)
subject_failures<-list(end=0,ai=0,is=0,dl=0,il=0,project1=0)
subject_grace<-list(end=0,ai=0,is=0,dl=0,il=0,project1=0)

#grace_marks<-function(name,count3){
 # subject_grace_1<-list(name,count3)
  #append(subject_grace,subject_grace_1)
#}

h<-hash()
abc<-function (list1,name){
  count3=0
  for (i in 1:length(list1[[1]])){
    if(list1[[1]][[i]]<40 && list1[[1]][[i]]>=35){
      if(list1[[1]][[i]]==39){
        list1[[1]][[i]]=list1[[1]][[i]]+1
        count3=count3+1
      }
      else if (list1[[1]][[i]]==38){
        list1[[1]][[i]]=list1[[1]][[i]]+2
        count3=count3+1
        }
      else if (list1[[1]][[i]]==37){
        list1[[1]][[i]]=list1[[1]][[i]]+3
        count3=count3+1
      }
      else if (list1[[1]][[i]]==36){
        list1[[1]][[i]]=list1[[1]][[i]]+4
        count3=count3+1
      }
      else if (list1[[1]][[i]]==35){
        list1[[1]][[i]]=list1[[1]][[i]]+5
        count3=count3+1
      }
    }
  }
  h[[name]]<-count3
  print(name)
  print(count3)
  
  return(list1)
}


abc1<-function (list1){
  count2=0
  for (i in 1:length(list1[[1]])){
    if(list1[[1]][[i]]<35){
      count2=count2+1
    }
  }
  return(count2)
}


Grace_Marks=c(subject_name,grace_marks)
sub_AI_new<-data.frame(unlist(abc(sub_AI1,"AI")),stringsAsFactors = FALSE)
sub_DL_new<-data.frame(unlist(abc(sub_DL1,"DL")),stringsAsFactors = FALSE)
sub_END_new<-data.frame(unlist(abc(sub_END1,"END")),stringsAsFactors = FALSE)
sub_IL_new<-data.frame(unlist(abc(sub_IL1,"IL")),stringsAsFactors = FALSE)
sub_IS_new<-data.frame(unlist(abc(sub_IS1,"IS")),stringsAsFactors = FALSE)
sub_Project1_new<-data.frame(unlist(abc(sub_Project1_1,"Project 1")),stringsAsFactors = FALSE)
students.data<-data.frame(student_name,student_seatno,sub_END_new,sub_IS_new,sub_AI_new,sub_DL_new,sub_IL_new,sub_Project1_new,Percentage,Rank1,Total_Marks)
names(students.data)[names(students.data) == "unlist.abc.sub_END1...END..."] <- "END"
names(students.data)[names(students.data) == "unlist.abc.sub_IS1...IS..."] <- "IS"
names(students.data)[names(students.data) == "unlist.abc.sub_AI1...AI..."] <- "AI"
names(students.data)[names(students.data) == "unlist.abc.sub_DL1...DL..."] <- "DL"
names(students.data)[names(students.data) == "unlist.abc.sub_IL1...IL..."] <- "IL"
names(students.data)[names(students.data) == "unlist.abc.sub_Project1_1...Project.1..."] <- "Project 1"
Total_Score<-rowSums(students.data[,3:8])
students.data<-cbind(students.data,Total_Score)
print(students.data)
subject_failures<-c(end=abc1(sub_END1),ai=abc1(sub_AI1),is=abc1(sub_IS1),dl=abc1(sub_DL1),Il=abc1(sub_IL1),project1=abc1(sub_Project1_1))
print(subject_failures)
print(max(subject_failures))
print(min(subject_failures))
print(which.max(subject_failures))
print(which.min(subject_failures),min(subject_failures))
print(subject_failures)
print(values(h))
