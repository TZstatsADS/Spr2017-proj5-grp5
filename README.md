# ADS Project 5: 

Term: Spring 2017

+ Team 5
+ Projec title: The Fragile Families Challenge
+ Team members
	+ team member 1 ([`Kai Chen`](https://github.com/KaiChenColumbia))
	+ team member 2 ([`Shan Yu`](https://github.com/yshany))
	+ team member 3 ([`Song Wang`](https://github.com/SongWang2017))
	+ team member 4 ([`Zhilin Fan`](https://github.com/zf2169))
	+ team member 5 ([`Zijun Nie`](https://github.com/zn2146))
	
+ Project summary:In this project, we use data based on the Fragile Families and Child Wellbeing Study, which has followed thousands of American families for more than 15 years. During this time, the Fragile Families study collected information about the children, their parents, their schools, and their larger environments.Given all the background data from birth to year 9 and some training data from year 15,we infer six key outcomes(gpa,grit,material hardship,eviction,layoff,jobtraining) in the year 15 test data.  In the data cleaning process, we deal with categorical variable and continuous variables seperately, for continuous variable, we replace the NA with the median value of that variable, and create a new categrocial variable to indicate the NAs (where the NAs may contain information to some degree) ,attach the new indicating categorical variable to the oringinal categorical features.For categorical features, we replace the NAs with a number that doesn't exist in original dataset and transform every categorical to a dummy matrix, for every dummy matrix whose elements are either 0 or 1, we choose the 2nd to last column to avoid collinearity. Given the cleaned data, our team work on different directions, one team work on different features and one team work on different machine learning tools, since we got to know the xgboost apparently outperforms other methods, we together work on features selected from various angles. For case 1: data obtained when children are at age 9 and only consider the continuous variables.Case 2: data obtained when children are at age 9, use categorical variables. Case 3:for predicting the GPA, intuitionally we believe the data from teacher should be informative,so we choose categorical features related to teacher.Case 4:we manually choose features we believe relavant to the academic competence of children as well as the features in case 3. After evaluating the performance of the the four feature selection strategies. Choose three predicitons that perform well and bag them by using the weighted average. We use the same strategy to other continuous outcomes( grit, material hardship).
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
