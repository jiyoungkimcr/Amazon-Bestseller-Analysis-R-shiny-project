# Amazon-Bestseller-Analysis-R-shiny-project-
**Project Description:**

This project is to analyze Bestseller Books of 2010~2020 which listed on Amazon.com Bestseller list. I crawled Bestseller data from Amazon page using crawler code that I wrote. Through this project, I wanted to see if variables such as book price, ratings, number of reviews, year, etc...have any affect or relationship with books being bestseller. (Link to: https://www.amazon.com/gp/bestsellers/2020/books)

**About Data Preparation - Crawling using BeautifulSoup:**

For the process and code of how I did my data scraping from Amazon webpage, please click the tab 'Data Crawling' above. In there, you can find my jupyter notebook (Python 3) code for crawling.

**About Dataset:**

Our dataset, 'amazon_bs', consists of 1094 obs of 7 variables (Year, Rank, Book.Title, Author, Rating, Num_Customers_Rated, Price). This data is crawled from Amazon Best Sellers list of 2010~2020. It was originally 1100 obs, but through data preprocessing, I dropped 6 obs with NA and empty values
