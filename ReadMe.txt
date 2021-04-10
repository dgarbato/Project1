The data for this project is from kaggle.  It is data from a Telecommunications Company which contains the variable Churn and several vaibles that may be impacting Churn.
I calculated the Churn rate for all of the customers in the file which was around 26.5%.  

To see how various variables are impacting Churn, I broke the numeric variables out into deciles and calculated the churn rate within each decile.  I then created an index
by performing this calculation (Churn Rate Per Decile)/(Overall Churn Rate) * 100.  I then created graphs for each numeric variable with decile on the x-axis and Index on the y-axis.

For the character variables I broke them out by category and created an index by performing this calculation.  (Churn Rate Per Category)/(Overall Churn Rate) * 100
I then created a graphs for  character variables to see which categories impacted churn.

The app I created is in the  Churn2 folder of my Project1 repository on github.

Users can interact with this graph by seeing how things change by gender.  I hope to add to that functionality.

That link is https://github.com/dgarbato/Project1

My app can be viewed on  https://deniseg.shinyapps.io/Churn2/
 
  