# Final Project (Amazon Bestseller 2010-2020 analysis Code)
# Jiyoung Kim

################ R code Part ################
########## Shiny Part is at below ###########



library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(lattice)
library(lubridate)
library(stringr)


# 0. Load Merged Dataset
setwd("/Users/jiyoungkim/Desktop/SGH 1 sem/Querrying data/homework/DATA PROJECT/Crawled Data")
amazon_bs <- read.csv("amazon_bestsellers_2010~2020.csv")

# Data Preprocessing (last time before analysis)

#sum(is.na(amazon_bs)) # In data preprocessing process using Python, I already dropped all 3 rows of NA data.(from 2012, 2016)
# But, I was afraid if there is any blank value such as '0',' ','' since there are some columns with character type of data. 
# So, I decided to find any blank values like that from columns with 'chr' type(Book.Title and Author column) by using %in% as below
#sum(amazon_bs$Book.Title %in% c("0","", " ")) 
#sum(amazon_bs$Author %in% c("0","", " ")) # we have 3 blank values in Author column. I'll drop those 3 rows.
#amazon_bs %>% filter(amazon_bs$Author %in% c("0","", " ")) # These are 3 rows with blank values
#amazon_bs <- amazon_bs[!(amazon_bs$Author %in% c("0","", " ")), ] # Drop those 3 rows, 1097 obs -> 1094 obs)

#names(amazon_bs) # we need to edit Price column name since it included '$'
#names(amazon_bs)[7] <- c("Price")

#amazon_bs <- as.data.frame(amazon_bs)

#write.csv(amazon_bs, file="Amazon_2010~2020.csv", row.names=FALSE)

# 1. [Trend of Number of Book Reviews]

count_reviews <- amazon_bs %>% 
  group_by(Year) %>%
  summarize(total = sum(Num_Customers_Rated)) 

as.numeric(count_reviews$Year)


# 2. [Top Authors with over 5 bestseller books] 

sum(duplicated(amazon_bs$Book.Title)) 
n_distinct(amazon_bs$Book.Title) # As you can see, there are some books who keep remaining in Bestseller list for several years. 
# Considering this, as we count number of bestseller books by authors, I would like to count books distinctly. 

books_by_aut <- amazon_bs %>% 
  as.data.frame %>% 
  group_by(Author) %>%
  summarize(Number_of_books = n_distinct(Book.Title))

books_by_aut$Author <- as.factor(books_by_aut$Author)

# Extracting only Authors who have more than(or equal) 2 bestseller books
# but i will make this as interactive using 'number_of_books' variable
aut_over2 <- books_by_aut %>% filter(Number_of_books>=2)
aut_over2 <- aut_over2[order(-aut_over2$Number_of_books),] 

# Change the order of Levels of 'Author' variable to show it on plot by order

aut_over2$Author <- with(aut_over2, reorder(Author,-Number_of_books))
levels(aut_over2$Author)

cols <- colorRampPalette(brewer.pal(12, "Set3"))
pal <- cols(length(aut_over2$Author))


# 3. [Average Book Price of Top Bestseller Authors] 
# - Does the book price of Top authors who have several bestsellers expensive than average price of all bestseller books?

new_books_by_aut <- amazon_bs %>% 
  as.data.frame %>% 
  group_by(Author) %>%
  summarize(Number_of_books = n_distinct(Book.Title), avg_price = round(mean(Price),digits=2))

new_books_by_aut$Author <- as.factor(new_books_by_aut$Author)

# Extracting only Authors who have more than(or equal) 5 bestseller books
new_books_by_aut <- new_books_by_aut %>% filter(Number_of_books>=5)
new_books_by_aut <- new_books_by_aut[order(-new_books_by_aut$Number_of_books),] 

# Change the order of Levels of 'Author' variable to show it on plot by order
new_books_by_aut$Author<-with(new_books_by_aut,reorder(Author,-Number_of_books))
levels(new_books_by_aut$Author)

cols1 <- colorRampPalette(brewer.pal(12, "Set3"))
pal1 <- cols1(length(new_books_by_aut$Author))

# Checking average price of all books (Let's Remember)
mean(amazon_bs$Price) # $ 10.85095


# 4. [Price of Books] 

# 4-1. Trend of Reviews and Price by Year
# 4-2. Top expensive Bestseller books 

exp_books <- amazon_bs %>% 
  as.data.frame %>% 
  group_by(Book.Title) %>%
  summarize(Price = round(mean(Price),digits=2))


# since the average Price of all bestsellers was $ 10.85095 as we seen above,
# I would like to filter data with books price only above this $10.85095.
exp_books <- exp_books %>% filter(Price>=10.85095)

# Since we still have too much obs to put on graph, I decided to filter data once again considering average price
mean(exp_books$Price) # 19.18634
exp_books_f <- exp_books %>% filter(Price>=19.18634)


# 4-3. Only Top 10 expensive Bestseller books 

exp_10_books <- amazon_bs %>% 
  as.data.frame %>% 
  group_by(Book.Title) %>%
  summarize(Price = round(mean(Price),digits=2)) %>%
  arrange(desc(Price)) %>%
  head(10)


# 5. [Top 10 Books by Average Number of reviews & Average Ratings] 

Reviews_by_books <- amazon_bs %>%
  as.data.frame %>% 
  group_by(Book.Title) %>%
  summarize(avg_reviews = as.integer(mean(Num_Customers_Rated))) %>%
  arrange(desc(avg_reviews)) %>%
  head(10)

Rating_by_books <- amazon_bs %>%
  as.data.frame %>% 
  group_by(Book.Title) %>%
  summarize(avg_ratings = round(mean(Rating), digits=1)) %>%
  arrange(desc(avg_ratings)) %>%
  head(10)


# 6. [Book Rating]

# Distribution of Ratings

table(amazon_bs$Rating)


# 7. [Length of book title] 

title_length <- as.data.frame(nchar(amazon_bs$Book.Title))
title_length <- rename(title_length, "title_length" = "nchar(amazon_bs$Book.Title)")


# 8. Correlation between each variables

# Create Data table with only numeric variables
amazon_bs_only_num <- amazon_bs[,c(1,2,5,6,7)]

# PairPlot with Scatterplot, Correlation coefficient, Histogram
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
} 

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
} 

panel.lm <- function(x, y, col=par("col"), bg=NA, pch=par("pch"),
                     cex=1, col.smooth="black", ...) {
  points(x, y, pch=pch, col=col, bg=bg, cex=cex)
  abline(stats::lm(y~x), col=col.smooth, ...)
} 


# Correlation Plot
library(corrplot)
b<-cor(amazon_bs_only_num)


####################################################################


# Shiny Report Part

library(shiny)
library(shinythemes)
library(DT)


shinyApp(
  ui = fluidPage(
    theme = shinytheme("flatly"),
    navbarPage(theme = "flatly","Amazon Bestseller Analysis📚",
               tabPanel("Analysis (EDA)",
                        sidebarPanel(
                          conditionalPanel("input.tabselected==1",
                                           h5(strong("Project info")), br("Author: Jiyoung Kim"), br("email: jk110075@student.sgh.waw.pl"), br("Last Update: 2020/01/03")),
                          conditionalPanel("input.tabselected==2",
                                           checkboxGroupInput("show_vars", "Columns in our dataset 'amazon_bs' to show:",
                                                              names(amazon_bs), selected = names(amazon_bs))),
                          conditionalPanel("input.tabselected==4",
                                           sliderInput("slider1", "Number of Books:", min=2, max=17, value=c(5,17), sep="")),
                          conditionalPanel("input.tabselected==6",
                                           sliderInput("slider2", "Price Range:", min=19.19, max=152, value=c(40,152), sep="")),
                          conditionalPanel("input.tabselected==8",
                                           sliderInput("slider3", "Range of Rating:", min=3.3, max=4.9, value=c(3.3,4.9), sep="")),
                          conditionalPanel("input.tabselected==9",
                                           sliderInput("slider4", "Range of Book Title Length:", min=4, max=121, value=c(4,121), sep=""))
                          ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("About Project", value=1, 
                              h4(strong("Project Description:")), 
                              "This project is to analyze Bestseller Books of 2010~2020 which listed on Amazon.com Bestseller list. 
                               I crawled Bestseller data from Amazon page using crawler code that I wrote. Through this project, 
                               I wanted to see if variables such as book price, ratings, number of reviews, year, etc...have any affect or relationship with books being bestseller.",
                              a(href="https://www.amazon.com/gp/bestsellers/2020/books", "Link to: Amazon Best Sellers of 2010-2020"),
                              br(),br(),h4(strong("About Data Preparation - Crawling using BeautifulSoup:")),
                              "For the process and code of how I did my data scraping from Amazon webpage, please click the tab 'Data Crawling' above. In there, you can find my jupyter notebook (Python 3) code for crawling.",
                              br(),br(),h4(strong("About Dataset:")),
                              "Our dataset, 'amazon_bs', consists of 1094 obs of 7 variables (Year, Rank, Book.Title, Author, Rating, Num_Customers_Rated, Price). This data is crawled from Amazon Best Sellers list of 2010~2020. 
                              It was originally 1100 obs, but through data preprocessing, I dropped 6 obs with NA and empty values"),
                            tabPanel(id='dataset',"Dataset", value=2, DT::dataTableOutput("mytable1")),
                            tabPanel("Num_Book Reviews", value=3,
                                     h4(strong("1. Trend of Number of Book Reviews")),
                                     plotOutput("plot1"), br(),
                                     "As we can see in the plot, people(readers) who review the books they read have increased by years. This also can be interpreted that customers have more actively participated in reviewing books.
                                     Accordingly, we can guess that the list of bestsellers has been more trustworthy based on people's active reviews and ratings by time."),
                            tabPanel("Top Authors", value=4, 
                                     h4(strong("2. Top Authors according to the number of Bestseller Books")),
                                     em("- Is there any common points or pattern of being an Author of Bestseller books?"),
                                     br(em("- Are there specific authors who have several bestseller books?")),
                                     em("- Is the author whose book was already been in bestseller list in the past has high possiblity to be on bestseller again?"),
                                     br(),plotOutput("plot2"),
                                     h5(strong("Key points I can make in here:")),
                                     "By looking into Book Titles based on this plot's rank, all Top 6 Authors only except John Grisham have several bestseller books because they wrote 'Series Novel Books'.",
                                     br("For example,"),
                                     br("Rick Riordan - Percy Jackson series / Jeff Kinney - Diary of a Wimpy Kid series /"),
                                     br("Dav Pilkey - Dog Man series / J.K.Rowling - Harry Potter series /"), 
                                     br("Bill O'Reilly - Killing series."), br(),
                                     "Actually, even in case of John Grisham, even though his books are not continuous series, but most of his 'thriller crime novel' books were ranked as bestsellers. 
                                     So we can also somehow regard his books as series.",
                                     h5(em(strong("Conclusion: Authors who write Novel(fiction) & Series Books have high possiblity to have more bestseller books. And it seems that authors who already been listed on bestseller more tend to be on bestseller list again.")))),
                           tabPanel("Top Authors with Price", value=5,
                                     h4(strong("3. Average Book Price of Top Bestseller Authors")),
                                     em("Does the book price of Top authors who have several bestsellers expensive than average price of all bestseller books?"),
                                     br(),plotOutput("plot3"),br(),
                                     em(strong("Average price of all books (Let's Remember!)")),
                                     br("mean(amazon_bs$Price) # $10.85095"),
                                     h5(strong("Key points I can make in here:")),
                                     "Since the average price of all bestseller books is '$10.85095', we cannot say all the books from top authors have higher average price than other books. 
                                     But, we can still see that over half of top authors' books have higher average price($11.04~18.15) than the one of all other books."),
                           tabPanel("Book Price", value=6,
                                    h4(strong("4. Price of Bestseller Books")),
                                    h4(strong("4-1. Top expensive Bestseller books")),
                                    plotOutput("plot6"),br(),
                                    h4(strong("4-2. Book Price by Year")),
                                    em("Does Price of books increase by time?"),
                                    "As you can see in the plot below,", 
                                    strong("Book Price doesn't change or increase by time"),
                                    "Except for some outliers, the average price is almost similar every year. So, it seems that Price doesn't have strong relation with bestseller books.",
                                    br(),plotOutput("plot4"),br(),
                                    h4(strong("4-3. Trend of Reviews and Price by Year")),
                                    "Comparing to the trend of Number of Reviews, Price has really not changed by years.",
                                    plotOutput("plot5")),
                           tabPanel("Top Reviews&Ratings", value=7,
                                    h4(strong("5. Top 10 Books by Average Number of reviews & Average Ratings")),
                                    "I need to group by books to have exact average value since there are some books who show up in bestseller list for multiple years. 
                                    So, to prevent counting duplicate number of books, I used group_by function",
                                    br(), plotOutput("plot7"), br(),
                                    h5(strong("Key points I can make in here:")),
                                    "There is no difference in Average Ratings by Books' Number of reviews.",
                                    br("This actually means 2 things:"),
                                    br(em(strong("1) Since these books are all Bestsellers in their own genre(maybe), most ratings are high in average like over 4.8."))),
                                    br(em(strong("2) There are some bestseller books who have same high ratings like 4.9 but only from smaller number of reviewers.")))
                           ),
                           tabPanel("Ratings", value=8,
                                    h4(strong("6. Book Ratings")),
                                    "Distribution of Ratings",
                                    br(), plotOutput("plot8"), br(),
                                    h5(strong("Key points I can make in here:")),
                                    "As we asseume in #5-2), Ratings are mostly densed on over 4.5~5.0. 
                                    So, I would say, in the future, if we further do detailed data analysis, we may need to create weighted Ratings considering Number of Customers Rated."
                           ),
                           tabPanel("Length of Title", value=9,
                                    h4(strong("7. Length of book title")),
                                    em("Does length of book title affect the books being bestsellers?"),
                                    "I created new column called 'title_length' using nchar function",
                                    br(), plotOutput("plot9"), br(),
                                    h5(strong("Key points I can make in here:")),
                                    "At first, I was assuming the bestseller books may have short title length in average since people usually get more attracted to books with simple but engaging title. 
                                     As you can see in the plot, I cannot say my assumption was 100% right, but there are a bit more bestseller books with shorter titles rather than longer ones."
                           ),
                           tabPanel("Correlation", value=10,
                                    h4(strong("8. Correlation between each variables")),
                                    "I extracted columns with only numeric variables and drew pairplot and Correlation Plot",
                                    br(), plotOutput("plot10"), br(), plotOutput("plot11"), plotOutput("plot12"),
                                    h5(strong("Key points I can make in here:")),
                                    "The only slightly meaningful correlations we can take into account are as follows:",
                                    br(strong(em("1) Number of Customers Rated (number of reviews) AND Year : Positive relation (0.32)"))),
                                    br(strong(em("2) Rating AND Year : Positive (0.3)"))),
                                    br(strong(em("3) Number of Customers Rated AND Rank : Negative (-0.23)"))),
                                    "But, even those 3 correlation are also very weak. So, I think it's hard to find that much strong correlation between each variables.", 
                                    br()
                           ),
                           tabPanel("Conclusion", value=11,
                                    h4(strong("Food for Thoughts after analysis")),
                                    "It was such a long but fun journey to deep dive into diverse variables related to Bestseller books. As you could see in analysis on Rating & number of customers
                                    Rated, not all the bestseller books are always have good ratings considering number of reviews. If I have more chance to analyze further on this topic, 
                                    I would definitely try to create new Weighted Ratings to find out the 'key' of being the bestsellers.",
                                    br("As I was dealing with data crawling to prepare my own dataset, I struggled a lot and wanted to crawl more variables such as 
                                    number of book pages, category, duration of being on bestseller list, etc...Unfortunately, due to limit of time and skill, I couldn't achieve all I wished to crawl
                                    from the webpage. Nevertheless, through this project, I could become more familiar to 'Web Crawling using BeautifulSoup' and also realized
                                    preprocessing the data is such an important step which may affect through whole analysis journey.")
                                    ),
                          id="tabselected"
                            )
                          )
                        ),
               tabPanel("Data Crawling", 
                        br(), 
                        "I used BeautifulSoup with Python and Jupyter notebook to crawl data from Amazon.com. I conducted scraping data of Amazon Top 100 Bestseller of 2010~2020. Code I wrote for Crawler is as below and I used same code to crawl all other years' bestseller too. 
                        After the crawling process, I had each 11 scraped result csv file from 2010 to 2020. So, I gathered all those files into one csv file using R code.",
                        br(),
                        br(),
                        mainPanel(htmlOutput("showfile"), br(), br(),
                                  br("As below, these are list of my crawler codes used for crawling each years and list of crawled result files saved."), br(), br(), br(),
                                  img(src='Jupyter_crawlers.png', height=480, width = 1200, align="left"), br(), br(), br(), 
                                  img(src='Jupyter_files.png', height=480, width = 1200, align="left"), br(), br(), br(), 
                                  br("After getting all the result files, I gathered all those files into one and did last touch of pre-processing using R code as below."), br(), br(), br(),
                                  img(src='R_preprocess.png', height=524, width = 1022, align="left"))
                        ))
    ),

  server = function(input, output) {
    # choose columns to display
    amazon_bs_2 = amazon_bs[sample(nrow(amazon_bs), 1094), ]
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(amazon_bs_2[, input$show_vars, drop = FALSE])
    })
    output$plot1 <- renderPlot({
      ggplot(count_reviews, aes(Year, total, fill=Year)) + 
        geom_bar(stat='identity') + geom_text(aes(label=total), vjust=2, size=3) + 
        geom_line() + geom_point() + scale_x_continuous("Year", labels = amazon_bs$Year, breaks = amazon_bs$Year) + 
        labs(title = "Number of Book Reviews by Year", x= "Year", y="Num of Reviews") +
        scale_fill_continuous(low="lightyellow", high="orange") + theme(legend.position='none')
    })
    d <- reactive({
      filtered <-
        aut_over2 %>%
        filter(Number_of_books >= input$slider1[1],
               Number_of_books <= input$slider1[2])
    })
    output$plot2 <- renderPlot({
      ggplot(d(), aes(x=reorder(Author,-Number_of_books), y=Number_of_books, fill=Author)) + geom_bar(stat='identity') +
        geom_text(aes(label=Number_of_books), position=position_dodge(width=0.8), vjust=-0.5, size=3) + 
        labs(title = "Top Authors by number of Bestseller Books", x = "Author", y = "Num of Bestseller Books") +
        theme(legend.position="none") + scale_fill_manual(values=pal) + coord_flip()
    })
    output$plot3 <- renderPlot({
      
      q1<-ggplot(new_books_by_aut, aes(x=reorder(Author,-Number_of_books), y=Number_of_books, fill=Author)) + geom_bar(stat='identity') +
        geom_text(aes(label=Number_of_books), position=position_dodge(width=0.8), vjust=-0.5, size=3) + 
        labs(title = "Top Authors with more than 5 Bestseller Books", x = "Author", y = "Num of Bestseller Books") + 
        theme(plot.title = element_text(hjust = 0.5, size=13, face="bold"), legend.position="none") +
        scale_fill_manual(values=pal1) + coord_flip()
      
      q2<-ggplot(new_books_by_aut, aes(x=reorder(Author,-Number_of_books), y=avg_price, fill=Author)) + geom_bar(stat='identity') +
        geom_text(aes(label=avg_price), position=position_dodge(width=0.8), vjust=-0.5, size=3) + 
        labs(title = "Average Price of Book by Top Authors", x = "Author", y = "Average of book price") + 
        theme(plot.title = element_text(hjust = 0.5, size=13, face="bold"), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
        scale_fill_manual(values=pal1) + coord_flip()
      
      grid.arrange(q1, q2, ncol=2)
      
      })
    
    output$plot4 <- renderPlot({
      boxplot(Price~Year, data = amazon_bs, boxwex=0.5, main="Book Price by Year")
    })
    output$plot5 <- renderPlot({
      xyplot(Num_Customers_Rated+Price~Year, data=amazon_bs, pch=19, lwd=1.5, col=c('black', 'blue'), type = c("p", "r"),
             main="Number of Reviews and Price by Year", key=list(text = list(c("Number of Reviews", "Price")),
                                                                  lines = list(lwd=1.5, col=c("black", "blue")), x=0,y=0.97,cex=0.75))
    })
    d2 <- reactive({
      filtered <-
        exp_books_f %>%
        filter(Price >= input$slider2[1],
               Price <= input$slider2[2])
    })
    output$plot6 <- renderPlot({
      ggplot(d2(), aes(x=Book.Title, y=Price, fill=Book.Title)) + geom_bar(stat='identity') +
        geom_text(aes(label=Price), position=position_dodge(width=0.8), vjust=-0.5, size=3) + 
        aes(stringr::str_wrap(Book.Title, 30), Price) + 
        labs(title = "Top expensive Bestseller Books", x = "Book.Title", y = "Price") + coord_flip() + 
        theme(plot.title = element_text(hjust = 0.5, size=10, face="bold"), legend.position="none")
    })
    output$plot7 <- renderPlot({
      
      p2 <-ggplot(Reviews_by_books, aes(x=reorder(Book.Title,-avg_reviews), y=avg_reviews, fill=Book.Title)) + geom_bar(stat='identity') +
        geom_text(aes(label=avg_reviews), position=position_dodge(width=0.8), vjust=-0.5, size=3) + 
        aes(stringr::str_wrap(Book.Title, 30), avg_reviews) +
        labs(title="Top 10 Books with Most Reviews", x = "Books", y = "Num of Reviews") + 
        theme(plot.title = element_text(hjust = 0.5, size=9.5, face="bold"), legend.position="none") +
        coord_flip()
      
      p3 <-ggplot(Rating_by_books, aes(x=reorder(Book.Title,-avg_ratings), y=avg_ratings, fill=Book.Title)) + geom_boxplot(color="red") +
        aes(stringr::str_wrap(Book.Title, 30), avg_ratings) +
        labs(title = "Top 10 Books with highest Average Ratings", y = "Rating") + 
        theme(plot.title = element_text(hjust = 0.5, size=9.5, face="bold"), legend.position="none", axis.title.y=element_blank()) + 
        coord_flip()
      
      grid.arrange(p2, p3, ncol=2)
      
    })
    d3 <- reactive({
      filtered <-
        amazon_bs %>%
        filter(Rating >= input$slider3[1],
               Rating <= input$slider3[2])
    })
    output$plot8 <- renderPlot({
      r_bar <- ggplot(d3(), aes(x = Rating)) + geom_bar() + labs(title = "Distribution of Ratings")
      r_box <- ggplot(d3(), aes(x = Rating)) + geom_boxplot(fill="lightblue") + labs(title = "Boxplot of Ratings")
      grid.arrange(r_bar, r_box, ncol=2)
    })
    d4 <- reactive({
      filtered <-
        title_length %>%
        filter(title_length >= input$slider4[1],
               title_length <= input$slider4[2])
    })
    output$plot9 <- renderPlot({
      ggplot(d4(), aes(x = title_length))+
        geom_histogram(binwidth=1, fill="#00AFBB",alpha=0.1,colour="#00AFBB") + 
        geom_density(aes(y=stat(count)), fill="orange", alpha=0.4, colour="orange", size=1) +
        labs(title = "Distribution of Length of Book Title", x= "Length")
    })
    output$plot10 <- renderPlot({
      pairs(amazon_bs_only_num,
            lower.panel = panel.lm, 
            upper.panel = panel.cor, 
            diag.panel = panel.hist, 
            pch="*",
            main ="Amazon BestSellers - ScatterPlot, Correlation coefficient, Histogram")
    })
    output$plot11 <- renderPlot({
      corrplot(b)
    })
    output$plot12 <- renderPlot({
      corrplot(b, method="number")
    })
    output$showfile <- renderUI({
      includeHTML("project - Amazon Book Bestsellers Crawling - 2020.html")
      # HTML(readLines(file_to_show))
    })
    }
  )


