#3.1 (Vectors)
#Categorical vectors
#use c function to define a vector (makes it a column)
#below is a vector of text
color <- c('Red', 'Blue', 'Yellow', 'Green')
color #categorical vectors of colors

#Numerical vectors (numerical sequence)
year <- 2018:2022
year #numerical sequence starting from 2018 to 2022 
#rows in data frame are top to bottom 
#columns are left to right (categorical vectors)



#Vector data structure in R:
#Vectors are collection of elements of same type (numeric, character, logical)

#3.2 (Data Frames)
#A data frame in R is 2-D data structure used to store values of any data type
#has rows and columns (columns are categories), rows are list of items for each category
moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/moody2022.csv")
head(moody) #this will print data frame 
#columns consist of Major, Score, Seniority, GPA, and Grade
#rows consist of different majors, scores, seniority, GPA, and grade
#Subsetting queries below 
moody[1, ] #returns first row 
moody[, 5] #prints fifth column which is letter grade
moody[1:5, 2] #prints column 2 which is score of row 1-5
moody[1:3, c(2:4)] #prints column 2, 3 and 4 for rows 1-3
moody[1:5, 1:3] #prints columns 1, 2 and 3 for rows 1-5

#3.3 Table (helps you calculate the mode/frequency of entries)
#You want to know how many students failed a class, this will help make those type
#of queries
#The table() function in R counts how often each value appears in dataset 
#and shows the results in a table 
grades <- table(moody$Grade) #grade distribution of each letter grade 
head(grades) #make a table for the letter grades of students and counts 
grades#same line as above 
#of students for each letter grade
table(moody$Grade, moody$Major) #grade and major distribution
#moody$Grade shows different possible grades, and then you will see it for each 
#major with moody$major
#above query creates table showing grades distribution of grades across differennt majors,
#counting how many students in each major received each grade
#46 got an A in CS, 54 in Econ, 69 in psych, and 42 in stats, total of 211

#3.4 Basic Functions
mean(moody$Score) #mean of the score column 

length(moody$Grade) #length of the grade column (1000)

max(moody$Score) #max value of the score column 

min(moody$Score) #min value of score in the score column

sd(moody$Score) #standard deviation of score column

moody$GPA #entire GPA column
mean(moody$GPA) #mean of gpa column 
max(moody$GPA) #max GPA
min(moody$GPA) #min GPA
#max, min, mean, and sd can only be applied on numeric columns 

length(unique(moody$Major)) #how many majors in dataset, 
#there are 4 (CS, econ, psych, stats), think of it like how many 
#unique columns are there, there's 4 with CS, Econ, psych, and stats

#3.5 Subset/Filtering
#subset() function in R selects and returns specific rows from
#a database based on given conditions 
#basically, subset() is for filtering data
#used to obtain specific rows and columns from the data frame
moody_psychology <- subset(moody, Major== 'Psychology') #new dataset with only psychology students
moody_psychology 
nrow(moody_psychology) #counts and adds number in each row (students) in psychology grouo
nrow(moody) #counts how many rows (students) are in oriignal moody dataset
#229 out of 1000 students are psychology major
#Alternate way to subset below
moody[moody$Major=="Psychology", ] #major, score, seniority, GPA and grade of every student who is a Psychology major
moody[moody$Major!="Psychology", ] #major, score, seniority, GPA and grade of every student who is not a Psychology major 
moody[moody$Score > 80, ] #major, score, seniority, GPA and grade of every student who got a score above an 80
moody[moody$Score > 80 & moody$Grade == 'B', ] #major, score, seniority, GPA and grade of every student who got a score above an 80 and a letter grade of B
#Subsetting rows and columns 
colnames(moody) #column names of moody DataFrame
moody3<-subset(moody, select = -c(1)) #new DataFrame that removes first column ("Major")
ncol(moody3) #should print 4 instead of 5 since 1 got removed
colnames(moody3) #no more "Major" which was first column since it has been removed

moody1 <- subset(moody, select = c(2:4), Major=="Psychology")
#dataframe which will show cols 2-4 (score, seniority, and gpa) of Psychology major
colnames(moody1) #will print score, seniority and gpa since those are columns 2-4
dim(moody1) #returns dimensions of moody1 (showing number of rows, 229, and columns, 3, it contains)
#subsetting allows data scientists to filer out what is not being studied/important
#they can see patterns of what they want to see easily
#through subsetting, you can uncover unusual patterns, identify disproportionate outcomes, and compare subsets within a dataset

#3.6 Tapply
#first vlue of tapply is numerical, second categorical, third is function you want to perform 
#distribution of grades for seniors who major in Econ
tapply(moody[moody$Seniority == 'Junior',]$Score, moody[moody$Seniority=='Junior',]$Grade, mean)
#above line will return code with mean score for each letter grade
#91.85714 for A, 89.09091 for B, 71.38235 for C, 54.50000 for D, and 25.76190 for F
tapply(moody$Score, moody$Grade, mean) #average score for each letter grade
tapply(moody$GPA, moody$Major, max) #max GPA for each major
tapply(moody$Score, list(moody$Grade, moody$Major), sum) #sums score for each combination of grade and major
#tapply() in R applies function like mean, sum or max to groups of data within a column.
#splits data for groups and calculates result for each group based on function 
table(moody[moody$Seniority == 'Junior',]$Grade) #distribution of grades for juniors
#above line is how many juniors got A, B, C, D, and F
table(moody[moody$Seniority == 'Senior' & moody$Major == 'Economics', ,]$Grade)#distribution of grades for seniors who major in economics 

#3.7 String operations
istring <-'Abqkd123&hhsD'
cat('This checks for capital letters  in the string')
contains_capital <- grepl("[A-Z]", istring)
contains_capital #returns true since string contains capital letters
contains_number <- grepl("[0-9]", istring)
contains_number #returns true since string contains numbers
cat('This checks for special characters in the string')
contains_special_char <- grepl("[[:punct:]]", istring)
contains_special_char #returns true since string has special characters
nchar(istring)#number of characters in the string

uppercase_only <- gsub("[^A-Z]", "", istring)
uppercase_only #counts only the uppercase letters in string and returns it (returns "AD")
num_uppercase_letters <- nchar(uppercase_only)
num_uppercase_letters #returns number of uppercase letters (returns 2)

#3.8 basic queries in R
colnames(moody) #names of each columns
summary(moody) #full summary of data frame and each part of it
unique(moody$Seniority) #gives name of each UNIQUE column in data frame
min(moody[moody$Grade=='A',]$Score) #minimum score to get an A in the data frame
max(moody[moody$Grade=='B',]$Score) #maximum score to get a B in the data frame
tapply(moody[moody$Grade=='A',]$Score, moody[moody$Grade=='A',]$Major, min) 
#above line returns the minimum score to get an A for each major
tapply(moody[moody$Grade=='B',]$Score, moody[moody$Grade=='B',]$Major, max)
#above line returns the maximum score to get a B for each major 
moody$ScoreIntervals<-cut(moody$Score,breaks=c(0,60,80,90,100),labels=c("Low","Medium",'Good', "Excellent"))
#0-60 is low, 60-80 is medium, 80-90 is good, 90-100 is high
moody$ScoreIntervals #this prints every single low, medium, high and excellent
table(moody$ScoreIntervals) #this just returns table with count of low, medium, good and excellent, way more efficient 
moody$ScoreIntervals<-cut(moody$Score,breaks=c(0,60,80,90,100),labels=c('Low','Medium','Good', 'Excellent'))
#breaks establishes breaks between the interval
table(moody$ScoreIntervals, moody$Grade) #gives table of low-excellent distribution for each letter grade
table(moody$Major, moody$Grade) #table with count of how many students got a specific letter grade in each major
tapply(moody[moody$Major=='Economics',]$Score, moody[moody$Major=='Economics',]$Seniority, mean) #mean SCORE for each seniority level

#SUbset takes a data frame and returns a data frame
#table takes two columns and returns (1d array of integer values)
#tapply returns array of dataframe after performing indicated function

#cat is important, helps convert numerical into categorical

#tapply is used to apply function like mean or sum to group of data(find average score for each grade in subset)
#table counts frequency of values in a column or across multiple columns (how many students got each grade)
#subset filters rows based on specific conditions, like selecting only the students from a particular major
#c function combines values into vector.