# Newsroom Textual Analysis and Visualization Tools Built With R Shiny

While word cloud visualizations and similar types of simple tools are widely available on the web, the more sophisticated textual analysis software and code unfortunately remain the domain of experts and users of languages like R and Python. To address this, we created three prototypes that would allow newsrooms to harness the power of R’s textual and sentiment analysis packages in a simple drag-and-drop format through public-facing R Shiny apps. These three apps show how powerful data wrangling, analysis and visualization functions may be increasingly democratized for time-pressed journalists. We demonstrate use cases through: 1) exploratory analysis of public speeches; 2) exploration of sentiment analysis around large bodies of political advertising, such as the new archive on the Facebook platform; and 3) comparative analysis of one outlet’s coverage on a specific topic as compared to that of the competition.

R is an open-source statistical software environment with a robust community of developers that collaborate and share knowledge, tools and code. Newsrooms are increasingly adopting R. Currently, R has statistical and textual analysis packages that allow for rich and generative forms of interpretation. In addition, R Shiny, which deploys code and datasets to a server, is easily customizable and user-friendly; it allows for a text-and-numerical dataset to be dropped in and a set of questions to be asked of it, facilitated by a pre-coded operational workflow.

These apps would allow reporters to drop in spreadsheets or text documents of interest and ask a series of cross-cutting questions. In this project, we have customized R’s “tidytext” and “plotly” packages and UVM’s “LabMTsimple” sentiment dictionary for textual and sentiment analysis and added several intervention points for users to subset the data, ask it guided questions and produce exploratory data visualizations.

We consider the possibilities enabled by our applications by looking at three use cases. Our first use case with a prototype Shiny app involves looking at a corpus of texts such as the public speeches of a politician or business leader to understand their rhetorical patterns, popular formulations and sentiment. Techniques such as descriptive statistics, sentiment analysis and TF-IDF are employed. Our second use case with a prototype Shiny app involves discerning patterns in political advertisements on Facebook, allowing for unique insights into the strategies employed in political messaging. Techniques such as n-gram and sentiment analysis are employed. Our third prototype serves as a comparative audit tool juxtaposing one reporter’s or outlet’s coverage with that of competing outlets leveraging news coverage-related data from Media Cloud.

These prototypes point to the possibility of a broader ecosystem of similar deadline-friendly apps for newsrooms that could provide them with greater analytical power and higher-level insights.

## Shiny app for inspecting TXT files

[Click here](https://storybench.shinyapps.io/textanalysis/) for the TXT Shiny app.

## Shiny app for inspecting CSV files

[Click here](https://storybench.shinyapps.io/csvanalysis/) for the CSV Shiny app.



## Shiny app for inspecting Facebook political ads

[Click here](https://storybench.shinyapps.io/facebook/) for the CSV Shiny app.

![img](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/shiny/fbads-1.png)
![img](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/shiny/fbads-2.png)

## Datasets for these Shiny apps

Google Sheet of 20,000+ [r/politics Reddit posts](https://docs.google.com/spreadsheets/d/1fYFpJuyR8neCHh8NAkr90n_HSU08bjca6xW531WFMYU/edit?usp=sharing) from 7/21/17 to 10/12/18. 

CSV of [Trump tweets through Nov. 2016](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/trumptweets-nov16.csv).

CSV of women running in [House and Senate races 2018](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/women_running.csv).

CSV of 1 year of [Washington Post Politics headlines](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/wapo-articles.csv). 

CSV of 1 year of [New York Times Politics headlines](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/nyt-articles.csv).

CSV of three months of [Heidi Heitkamp articles](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/Heitkamp-articles-3-months.csv).

CSV of six months of [Lisa Murkowski articles](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/murkowski.csv).

TXT of [Trump campaign speeches](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/trumpspeeches.txt).

TXT of [2018 Obama rally in Illinois](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/obama-rally-2018.txt).

TXT of [Trump State of the Union speech](https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/trump_state_union_2018.txt).

ProPublica's [database of Facebook political ads](https://projects.propublica.org/facebook-ads/). And find the [full dataset here](https://www.propublica.org/datastore/dataset/political-advertisements-from-facebook). 




