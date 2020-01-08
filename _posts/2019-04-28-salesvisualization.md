---
title: "How to increase the profit of Shoe Shop?"
date: 2019-04-28
tags: [data analysis, data visualization, data science in R
excerpt: "Data Visualization, sales, Statistics"
---
## 1 Introduction

  <p>It's my first project, where our instructor gave me a dataset of 'XYZ'
shoe company. This company's sale have been decreasing since 2018, and they
want to figure out what kind of factors positively affect to sale and
what kind factors on the other hand, decrease company's profit.</p>
  <p>Source of code linked [here](https://github.com/yerlandana/sales/blob/master/unilever.Rmd). </p>

## 2 Loading and Exploring Data

### 2.1 Loading libraries required and reading the data into R

  <p>Loading R packages used besides base R.</p>

![libraries](/photos_sale/39.png)

![dataframes](/photos_sale/40.png)

  <p> Reading the table.csv. Also, I found that date stored as a string
  and I converted it to date format by using 'datetime', and disabled scientific
  notation. This date frame has 809 rows and 11 columns.</p>
![evidence](/photos_sale/0.png)

  <p> By viewing the type of data in each column, I check the number of 'NA'.
  I have only one 'NA'  in 'Cost of sales' column, therefore I found a mean of
  the 'Cost of sales' column and replaced with it. Because, 'NA' is located on the
  last day(row) of sale and they didn't finished to record that data.
  Then I mutated a new column 'Profit' which equals to the 'Fact..KZT..with.discount.- 'Cost.of.sales'.</p>

  ![evidence](/photos_sale/41.png)
  Last 10 rows of 'Cost.of.sale' column, and 'NA' have been replaced by 604679.7.



  ```r
  df_year <- df%>% group_by(year)%>%summarize(meanProfit=mean(Profit), total=sum(Profit), planProfit= sum(Plan..KZT), difference = planProfit-total)
  datatable(df_year)
  options(scipen=100000)
  mydata <- data.frame(Real_with_discount=df_year$total, Predicted=df_year$planProfit)
  barplot(as.matrix(mydata), main="Real & Predicted income", ylab="Total", beside=TRUE,
          col=terrain.colors(3))
  legend("topright", c("2016", "2017","2018"), col=terrain.colors(3), lwd=15);
  ```
