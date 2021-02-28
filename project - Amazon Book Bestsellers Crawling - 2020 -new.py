#!/usr/bin/env python
# coding: utf-8

# ### Crawler for Amazon Bestsellers of 2020
# ##### written by Jiyoung Kim

# #### Import libraries required for scraping

# In[1]:


import pandas as pd
import numpy as np
from urllib.request import urlopen
from bs4 import BeautifulSoup
import requests


# #### Scraping Amazon Bestsellers list of 2020 from website

# In[3]:


no_pages = 2

def get_data(pageNo):  
    headers = {"User-Agent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:66.0) Gecko/20100101 Firefox/66.0", "Accept-Encoding":"gzip, deflate", "Accept":"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", "DNT":"1","Connection":"close", "Upgrade-Insecure-Requests":"1"}

    r = requests.get('https://www.amazon.com/gp/bestsellers/2020/books/ref=zg_bsar_cal_ye'+str(pageNo)+'?ie=UTF8&pg='+str(pageNo), headers=headers)#, proxies=proxies)
    content = r.content
    soup = BeautifulSoup(content)
    #print(soup)

    alls = []
    for d in soup.findAll('div', attrs={'class':'a-section a-spacing-none aok-relative'}):
        #print(d)
        name = d.find('span', attrs={'class':'zg-text-center-align'})
        n = name.find_all('img', alt=True)
        #print(n[0]['alt'])
        author = d.find('a', attrs={'class':'a-size-small a-link-child'})
        rating = d.find('span', attrs={'class':'a-icon-alt'})
        users_rated = d.find('a', attrs={'class':'a-size-small a-link-normal'})
        price = d.find('span', attrs={'class':'p13n-sc-price'})

        all1=[]

        if name is not None:
            #print(n[0]['alt'])
            all1.append(n[0]['alt'])
        else:
            all1.append("unknown-product")

        if author is not None:
            #print(author.text)
            all1.append(author.text)
        elif author is None:
            author = d.find('span', attrs={'class':'a-size-small a-color-base'})
            if author is not None:
                all1.append(author.text)
            else:    
                all1.append('0')

        if rating is not None:
            #print(rating.text)
            all1.append(rating.text)
        else:
            all1.append('-1')

        if users_rated is not None:
            #print(price.text)
            all1.append(users_rated.text)
        else:
            all1.append('0')     

        if price is not None:
            #print(price.text)
            all1.append(price.text)
        else:
            all1.append('0')
        alls.append(all1)    
    return alls


# #### Save the scraped result as csv

# In[4]:


results = []
for i in range(1, no_pages+1):
    results.append(get_data(i))
flatten = lambda l: [item for sublist in l for item in sublist]
df2 = pd.DataFrame(flatten(results),columns=['Book Title','Author','Rating','Num_Customers_Rated', 'Price($)'])


# In[5]:


df2.to_csv('amazon_bestsellers_2020.csv', index=False, encoding='utf-8')


# #### Open csv file

# In[6]:


df2 = pd.read_csv("amazon_bestsellers_2020.csv")


# In[7]:


df2.shape


# In[8]:


df2.head(100)


# #### Data Preprocessing 

# In[9]:


df2.insert(0, 'Rank', df2.index + 1) #Adding the Best seller rank using index


# In[10]:


df2.insert(0, 'Year', '2020') #Adding Year column


# In[11]:


#Getting rid of all the 'out of 5 stars' phrase from Rating column values
df2['Rating'] = df2['Rating'].apply(lambda x: x.split()[0]) 


# In[12]:


df2['Rating'] = pd.to_numeric(df2['Rating']) #change Rating's data type into numeric


# In[13]:


#Getting rid of all the dollar sign '$' from Price column values
df2['Price($)'] = df2['Price($)'].str.replace('$', '')
df2['Price($)'] = df2['Price($)'].astype(float)


# In[14]:


#Getting rid of comma from Customers_Rated column value
df2["Num_Customers_Rated"] = df2["Num_Customers_Rated"].str.replace(',', '')
df2['Num_Customers_Rated'] = pd.to_numeric(df2['Num_Customers_Rated'])


# In[15]:


df2.head()


# In[16]:


df2.to_csv("amazon_bestsellers_2020.csv", sep=",", index = False)

