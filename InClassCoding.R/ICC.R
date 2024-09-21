#Low_Score
moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2022_new.csv")
head(moody)
moody$LOW_SCORE<-TRUE
min_score <- tapply(moody$SCORE, moody$GRADE, min)
min_score_B <- min_score["B"]
moody$LOW_SCORE <- ifelse(moody$SCORE <= min_score_B, "Low", "High")
moody
#if lower than lowest score for letter grade B, then low

#Bumped_Up
#this is true or false given student s
moody$BUMPED_UP<-FALSE
#if there is student t who got higher grade than s with lower score than s, then bump up
a_students<-subset(moody, GRADE == "A")
a_students
b_students<-subset(moody, GRADE == "B")
b_students
b_higher_than_a <- moody$GRADE == "B" & moody$SCORE >= min(a_students$SCORE)
moody[b_higher_than_a, "GRADE"] <- "A"
moody[b_higher_than_a, "BUMPED_UP"] <- TRUE
moody

#New A for Computer Science: Different cutoff points for computer science
moody$newGrade<-0
#95+ for A, 80-95 for B, 65-80 for C, and 60-65 for D, <60 is F
newCutoffs<-cut(moody$SCORE, breaks=c(0, 60, 65, 80, 95, 100), labels=c("F", "D,", "C", "B", "A"))
table(newCutoffs)

