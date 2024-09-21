#you can create new attributes (columns) for a data frame. 
#These new attributes are new vectors which are defined as functions
#of existing attributes, these are derived attributes

#below we will define new attribute pass/fail where students who got A, 
#B, or C passed and students who got F failed

moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2022_new.csv")
moody$PF<-'Pass'
moody[moody$GRADE == 'F', ]$PF<-'Fail'
moody #$replaces Pass with Fail for students who failed

#Making categorical attribute from numerical attribute using function Cut()
