## ABSTRACT

This project aims to develop successful intervention programs to help undergraduate students graduate on time.
The prediction model is build  for identifying and prioritizing students who need those
interventions the most. This study employs a machine learning framework to identify such stu-
dents, describes features that are useful for this task, applies
several classification algorithms, and evaluates them using standard
metrics.

The academic performance of students (SAT score, highSchool GPA, undergraduate level test scores) and demographic factors (economic Status, ethnicity,  age, gender, entry type) are the predictor variables used to build the model. For improved accuracy, a stacked ensemble model is developed wherein the first level employs standard machine learning algorithms like RandomForest, Logistic regression and k-nearest neighbor algorithm. The predictions from the first level are fed into the second level which is a Gradient Boosting Machine.
Finally, the stacked ensemble model is compared to the individual base learner algorithms using standard metrics like AUC and overall Accuracy.


####References

1. https://blogs.sas.com/content/subconsciousmusings/2017/05/18/stacked-ensemble-models-win-data-science-competitions/
2. http://support.sas.com/resources/papers/proceedings17/SAS0437-2017.pdf
3. https://dl.acm.org/citation.cfm?id=2788620
# test
# test
