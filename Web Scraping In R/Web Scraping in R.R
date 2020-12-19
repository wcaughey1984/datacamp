#
# Title:    Web Scraping In R
# Purpose:  (Knowledge Development) Learning to webscrape with R
# Author:   Billy Caughey
# Date:     2020.11.19 - Initial build
#

##### Introduction to HTML #####

library(rvest)

# <h[1-5]> are header tacks
# <p> are paragraph tacks
# <ul> <li> is how to open a bulleted list
# <a href = 'site.com'>link</a> is how to set up a link in R, <a> is the tack and href is the attribute

# library(rvest) is the work horse from Tidyverse
# xml_structure(html) look at the html structure/basic outline of the document
# read_html(html) helps r read HTML in an understable way

##### Web Scraping is Cool #####

## Part 1: Read in the excerpt into R

html_excerpt_raw <- '
<html>
  <body>
    <h1>Web scraping is cool</h1>
    <p>It involves writing code – be it R or Python.</p>
    <p>DataCamp has courses on it.</p>
  </body>
</html>'
# Turn the raw excerpt into an HTML document R understands
html_excerpt <- read_html(html_excerpt_raw)
html_excerpt

## Part 2: Alter the html so it points to https://datacamp.com

html_excerpt_raw <- '
<html>
  <body>
    <h1>Web scraping is cool</h1>
    <p>It involves writing code – be it R or Python.</p>
    <p><a href="https://datacamp.com">DataCamp</a>
		has courses on it.</p>
  </body>
</html>'
# Turn the raw excerpt into an HTML document R understands
html_excerpt <- read_html(html_excerpt_raw)
html_excerpt

## Part 3: Use the xml_structure() function to get a better overview of the tag hierarchy of the HTML excerpt.

html_excerpt_raw <- '
<html>
  <body>
    <h1>Web scraping is cool</h1>
    <p>It involves writing code – be it R or Python.</p>
    <p><a href="https://datacamp.com">DataCamp</a>
		has courses on it.</p>
  </body>
</html>'
# Turn the raw excerpt into an HTML document R understands
html_excerpt <- read_html(html_excerpt_raw)
html_excerpt
# Print the HTML excerpt with the xml_structure() function
xml_structure(html_excerpt)

##### Beware of syntax errors #####

broken_html_raw <- "\n
                        <html> \n
                        <h1Web scraping is cool</h1>\n
                        <p>It involves writing code – be it R or Python.</p>\n
                        <p><a href=\"https://datacamp.com\">DataCamp</a> \n
                        \t\thas courses on it.</p>\n
                        </html>"

# missing ">" on the <h1 tack

##### Navigating HTML #####

## Tree Structure ##
#   <html> <--- Root Node, the child is <body>
#       <body>
#           <div> <--- Child of <body> tack
#               <p>The first paragraph.</p>
#           </div>
#           <div> <--- Child of <body> tack
#               Not an actual paragraph, but with a <a ref = "#">link</a>.
#           </div>
#           <p> A paragraph without an enclosing div. </p>
#       </body>
#   </html>

# html <- read_html(html_document):
# html_children(html): Takes an xml structure and returns the nodeset (or children) of the root node
# html_text(): extracts all the text from the children of the root node
# html_node(): Returns the first node that matches my selection
# html_nodes(): can take an html document or an html selector
# html selector: Defines a bath through the html tree
# html_nodes("div p"): returns the paragraphs of a div node
# html_nodes("p"): selects all the paragraphs in the html tree
# html_nodes("div p") == html %>% html_nodes("div") %>% html_nodes("p")
# html_attr: Extracts attributes from the html tree
# html %>% html_node("a") %>% html_attr("href"): returns the href attribute from the "a" node
# html_attrs(): returns all the attributes of a node as a named vector

##### Select all children of a list #####

# Part 1: Read in the corresponding HTML string
list_html <- read_html(list_html_raw)

# Part 2: # Extract the ol node
ol_node <- list_html %>% html_node("ol")

# Part 3: Extract and print the nodeset of all the children of ol_node
html_children(ol_node)

##### Parse hyperlinks into a data frame #####

# Part 1:

# Extract all the a nodes from the bulleted list
links <- hyperlink_html %>%
  read_html() %>%
  html_nodes("ul a") # Remember, I am looking for the bullet list!

# Parse the nodes into a data frame
link_df <- tibble(
  domain = links %>% html_attr("href"),
  name = links %>% html_text()
)

link_df

##### Scrape your first table #####

## Basic Outline of a table ##
# <table> <-- Designates table
#   <tr> <-- Designates a table row
#       <th>Name</td><td>Profession</td><td>Age</td><td>Country</th> <-- Table header tacks
#   </tr>
#   <tr>
#       <td>Dillon Arroyo</td><td>Carpenter</td><td>54</td><td>UK</td>
#   </tr>
#   <tr>
#       <td>Rebecca Douglas></td><td>Developer</td><td>32</td><td>USA</td>
#   </td>
# </table>

# html <- read_html(table_html) # table with <th> header cells
# html %>% html_table() : reads in table, converts the table for R to read

# html <- read_html(table_html) # table without <th> header cells
# html %>% html_table(header = TRUE) : reads in table, converts the table for R to read, first row is header
# html %>% html_table(header = TRUE, fill = TRUE) : reads in table, converts the table for R to read, first row is header, fills in missing entries with NA

##### Turn a table into a data frame with html_table() #####

# Part 1: Extract the "clean" table into a data frame
mountains <- mountains_html %>%
  html_node("table#clean") %>%
  html_table()

mountains

# Part 2: Extract the "dirty" table into a data frame
mountains <- mountains_html %>%
  html_node("table#dirty") %>%
  html_table(header = T, fill = T)

mountains

##### Introduction to CSS #####

# CSS = Cascading Style Sheets
# Selectors I have seen:
# Type Selector:

type {
     key:value;
}

html %>% html_nodes('type') # e.g., 'h1', 'a', or 'span'

type1, type2 {
    key: value;
}

html %>% html_nodes("type1, type2")

# Universal Selector

* {
    key: value;
}

html %>% html_nodes("*")

##### Select multiple HTML types #####

languages_raw_html <- "
<html>
  <body>
    <div>Python is perfect for programming.</div>
    <p>Still, R might be better suited for data analysis.</p>
    <small>(And has prettier charts, too.)</small>
  </body>
</html>"

# Read in the HTML
languages_html <- read_html(languages_raw_html)
# Select the div and p tags and print their text
languages_html %>%
	html_nodes("div, p") %>%
	html_text()

##### CSS Classes and IDs #####

# Classes can categorize HTML elements into certain style groups
# Class is just another HTML attribute - similar to the 'href' attribute of the 'a' element
# Classes are specified with a '.' at the front of it

# If I looking for a specific class node, I write:
html %>% html_nodes(".alert")

# If I am looking for multiple classes, I can write:
html %>% html_nodes(".alert.emph") # Notice the lack of spaces

# If I write:
html %>% html_nodes(".alert, .emph")
# Then I am selecting elements with the ".alert", ".emph", and ".alert.emph" classes

# IDs are special class of classes.
# IDs are UNIQUE! This means only ONE element should have an ID
# In CSS, IDs have a '#' at the beginning like '#special'

# In R, this is done in the following way:
html %>% html_nodes("#special")

# If I want a specific element-class type, I use the following:
html %>% html_nodes("a.alert")

# If I want a specific element-ID type, I use the following: (I can do this with use the ID as they are unique)
html %>% html_nodes("div#special") # Remember this from scraping tables???

# There are pseudo classes as well. There are three basic classes:
# first-child
# nth-child
# last-child

# Consider the following HTML code:

<ol>
    <li>First element.</li> # html %>% html_nodes("li:first-child")
    <li>Second element.</li> # html %>% html_nodes("li:nth-child")
    <li>Third element.</li> # html %>% html_nodes("li:last-child") OR html %>% html_nodes("li:nth-child(3)")
</oi>

Selector Type                           HTML                                CSS Selector
Type                                    <p>...</p>                          p
Multiple Types                          <p>...</p><div>...</div>            p, div
Class                                   <p class = 'x'>...<p>               .x
Multiple Classes                        <p class = 'x y'>...<p>             .x.y
Type + Class                            <p class = 'x'>...</p>              x.p
ID                                      <p id = 'x'>...</p>                 #d
Type + Pseudo Class                     <p>...</p><p>...</p>                p:first-child

##### Leverage the uniqueness of IDs #####

structured_html_raw <- "
<html>
  <body>
    <div id = 'first'>
      <h1 class = 'big'>Joe Biden</h1>
      <p class = 'first blue'>Democrat</p>
      <p class = 'second blue'>Male</p>
    </div>
    <div id = 'second'>...</div>
    <div id = 'third'>
      <h1 class = 'big'>Donald Trump</h1>
      <p class = 'first red'>Republican</p>
      <p class = 'second red'>Male</p>
    </div>
  </body>
</html>"

structured_html <- read_html(structured_html_raw)

# Select the first div
structured_html %>%
  html_nodes("div#first")

# Select the last child of each p group

nested_html_raw <- "
<html>
  <body>
    <div>
      <p class = 'text'>A sophisticated text [...]</p>
      <p class = 'text'>Another paragraph following [...]</p>
      <p class = 'text'>Author: T.G.</p>
    </div>
    <p>Copyright: DC</p>
  </body>
</html>"

nested_html <- read_html(nested_html_raw)

nested_html %>%
    html_nodes("p:last-child")

# This time for real: Select only the last node of the p's wrapped by the div
nested_html %>%
	html_nodes("div p.text:last-child")

##### CSS combinators #####

# There are four common combinators: [space], >, +, ~
# Basic structure: h2#someid {space | > | + | ~ } .someclass
# [space]: descendent combinator (parent to child)
html %>% html_nodes("div.first a")
# '>': child combinator - only selects DIRECT decendents of the parent
html %>% html_ndoes("div.first > a")
# '+': adjacent sibling combinator - selects THE NEXT direct sibling (oldest to youngest)
html %>% html_nodes("div.first + div")
# '~': general sibling combinator - selects all the siblings of the first which match the element specified
html %>% html_nodes("div.first ~ div")
# I can use the wildcard to select ALL siblings of the first no matter the element type
html %>% html_nodes("div.first ~ *")

# So why use combinators? Because there are websites without classes and ids!

##### Select direct descendents with the child combinator #####

# Extract the text of all list elements
languages_html %>%
	html_nodes("li") %>%
	html_text()

# Extract only the text of the computer languages (without the sub lists)
languages_html %>%
	html_nodes('ul#languages li') %>%
	html_text()

# Extract only the text of the computer languages (without the sub lists)
languages_html %>%
	html_nodes('ul > li') %>%
	html_text()

# Extract only the text of the computer languages (without the sub lists)
languages_html %>%
	html_nodes('ul#languages li') %>%
	html_text()

# Extract only the text of the computer languages (without the sub lists)
languages_html %>%
	html_nodes('ul#languages > li') %>%
	html_text()

##### Simply the best #####

complicated_html_raw <- '<html>
  <body>
    <div class="first section">
      A text with a <a href="#">link</a>.
    </div>
    <div class="second section">
      Some text with <a href="#">another link</a>.
      <div class="first paragraph">Some text.</div>
      <div class="second paragraph">Some more text.
        <div>...</div>
      </div>
    </div>
  </body>
</html>'

complicated_html <- read_html(complicated_html_raw)

# Select the three divs with a simple selector
complicated_html %>%
	html_nodes("div div")

##### Not every sibling is the same #####

code_html_raw <- "
<html>
<body>
  <h2 class = 'first'>First example:</h2>
  <code>some = code(2)</code>
  <span>will compile to...</span>
  <code>some = more_code()</code>
  <h2 class = 'second'>Second example:</h2>
  <code>another = code(3)</code>
  <span>will compile to...</span>
  <code>another = more_code()</code>
</body>
</html>"

code_html <- read_html(code_html_raw)

# Select only the first code element in the second example
code_html %>%
	html_nodes("h2.second + code")

# Select all code elements in the second example
code_html %>%
	html_nodes("h2.second ~ code")

##### Introduction to XPATH #####

# XPATH = XML path language
# This is a different pathway than the CSS pathway
# Can go down AND up the HTML tree

# Suppose I want to select the element 'p' nodes...
# CSS: html %>% html_nodes('p')
# XPATH: html %>% html_nodes(xpath = "//p")

# Using the same example... the path would be...
# CSS: html %>% html_nodes("body p") OR html %>% html_nodes("html > body p")
# XPATH: html %>% html_nodes("//body//p") OR html %>% html_nodes("/html/body//p")

# Suppose I want to select 'p' tacks that direct children of 'div' tacks...
# CSS: html %>% html_nodes("div > p")
# XPATH: html %>% html_nodes("//div/p")

# Suppose I want to select 'div' tacks that only have an 'a' tack child...
# CSS: There is not a way to do this...
# XPATH: html %>% html_nodes("div[a]")

# Syntax of XPATH
# Axes: '/' or '//'; '/' is direct child; '//' general descendent relationship
# Steps: HTML types like 'span' and 'a'
# Predicates: [...]

##### Select by class and ID with XPATH #####

weather_html_raw <- "
<html>
  <body>
    <div id = 'first'>
      <h1 class = 'big'>Berlin Weather Station</h1>
      <p class = 'first'>Temperature: 20°C</p>
      <p class = 'second'>Humidity: 45%</p>
    </div>
    <div id = 'second'>...</div>
    <div id = 'third'>
      <p class = 'first'>Sunshine: 5hrs</p>
      <p class = 'second'>Precipitation: 0mm</p>
    </div>
  </body>
</html>"

weather_html <- read_html(weather_html_raw)

# Select all p elements
weather_html %>%
	html_nodes(xpath = '//p')

# Select p elements with the second class
weather_html %>%
	html_nodes(xpath = "//p[@class = 'second']")

# Select p elements that are children of "#third"
weather_html %>%
	html_nodes(xpath = "//div[@id = 'third']")

# Select p elements with class "second" that are children of "#third"
weather_html %>%
	html_nodes(xpath = "//div[@id = 'third']//p[@class = 'second']")

##### Use predicates to select nodes based on their chidlren #####

# Select all divs
weather_html %>%
    html_nodes(xpath = "//div")

# Select all divs with p descendants
weather_html %>%
    html_nodes(xpath = '//div[p]')

# Select all divs with p descendants having the "third" class
weather_html %>%
    html_nodes(xpath = "//div[p[@class = 'third']]") # This reaches down TWO layers

##### XPATH functions and advanced predicates #####

# position() : selects the nth element
# CSS: html %>% html_nodes(css = "ol > li:nth-child(2)")
# XPATH: html %>% html_nodes(xpath = "//ol/li[position() = 2]")

# Now suppose I want to get the first two elements... This can't be done in CSS, but with XPATH...
# html %>% html_nodes(xpath = "//ol/li[position() < 3]")

# Now suppose i want all the elements except for the third... again, CSS can't do this... but with XPATH...
# html %>% html_nodes(xpath = "//ol/li[position() != 3]")

# Now suppose I want all the elements except for the third BUT they need to have class blue...
# html %>% html_nodes(xpath = "//ol/li[position() != 3 and @class = 'blue']")

# count() : we can select the nodes with a specific number of children
# html %>% html_nodes(xpath = "//ol[count(li) = 2]")

##### Get to know the position() function #####

rules_html_raw <- "
<body>
<div>
    <h2>Today's rules</h2>
    <p>Wear a mask</p>
    <p>Wash your hands</p>
</div>
<div>
    <h2>Tomorrow's rules</h2>
    <p>Wear a mask</p>
    <p>Wash your hands</p>
    <p>Bring hand sanitizer with you</p>
</div>
</body>"

rules_html <- read_html(rules_html_raw)

# Select the text of the second p in every div
rules_html %>%
    html_nodes(xpath = "//div/p[position() = 2]") %>%
    html_text()

# Select every p except the second from every div
rules_html %>%
    html_nodes(xpath = "//div/p[position() != 2]") %>%
    html_text()

# Select the text of the last three nodes of the second div
rules_html %>%
    html_nodes(xpath = "//div[count(p) = 3]/p") %>%
    html_text()

##### Extract nodes based on the number of their children #####

forecast_html_raw <- "
<body>
<div>
  <h1>Tomorrow</h1>
</div>
<div>
  <h2>Berlin</h2>
  <p>Temperature: 20°C</p>
  <p>Humidity: 50%</p>
</div>
<div>
  <h2>London</h2>
  <p>Temperature: 15°C</p>
</div>
<div>
  <h2>Zurich</h2>
  <p>Temperature: 22°C</p>
  <p>Humidity: 60%</p>
</div>
</body>"

forecast_html <- read_html(forecast_html_raw)

# Select only divs with one header and at least one paragraph
forecast_html %>%
	html_nodes(xpath = "//div[count(h2) = 1 and count(p) > 1]")

##### The XPATH text() function #####

actor_html_raw <-'
<html>
<body>
    <table id = "cast">
        <tr><td class = "actor">Arnold S.</td><td class = "role"><em>1</em> (Voice)</td></tr>
        <tr><td class = "actor">Burt R.</td><td class = "role"><em>2</em> (Choreo)</td></tr>
        <tr><td class = "actor">Charlize T.</td><td class = "role"><em>3</em> (Voice)</td></tr>
    </table>
</body>
</html>
'

actor_html <- read_html(actor_html_raw)

# suppose I want to access the emphasis elements and not the role. This is impossible in CSS.
# The closest I can get is:

actor_html %>%
    html_nodes("#cast td.role") %>%
    html_text()

# this can be done in XPATH

actor_html %>%
    html_nodes(xpath = '//*[@id = "cast"]//td[@class = "role"]') %>% # equal to '#cast td.role'
    html_nodes(xpath = "./text()") %>% # This only selects text from the "td" element pulled in the line previous
    html_text(trim = T)

# The text() function can do something else: act as a selector by text
# Suppose I am only interest in pulling the rows where the actor gave the character it's voice

actor_html %>%
    html_nodes(xpath = '//*[@id = "cast"]//td[@class = "role" and text() = " (Voice)"]')

# Now, I want to select the entire row... I can use the 'parent' filter to pull the entire 'tr' element
actor_html %>%
    html_nodes(xpath = '//*[@id = "cast"]//td[@class = "role" and text() = " (Voice)"]') %>%
    html_nodes(xpath = '..')

##### The shortcomings of html_table() with badly structured tables #####

roles_html_raw <- '
<table>
 <tr>
  <th>Actor</th>
  <th>Role</th>
 </tr>
 <tr>
  <td class = "actor">Jayden Carpenter</td>
  <td class = "role"><em>Mickey Mouse</em> (Voice)</td>
 </tr>
 ...
</table>'

roles_html <- read_html(roles_html_raw)

# Extract the data frame from the table using a known function from rvest
roles <- roles_html %>%
  html_node(xpath = "//table") %>%
  html_table()
# Print the contents of the role data frame
print(roles)

##### Select directly from a parent element with XPATHs text() #####

# Extract the actors in the cells having class "actor"
actors <- roles_html %>%
  html_nodes(xpath = '//table//td[@class = "actor"]') %>%
  html_text()
actors

# Extract the roles in the cells having class "role"
roles <- roles_html %>%
  html_nodes(xpath = '//table//td[@class = "role"]/em') %>%
  html_text()
roles

# Extract the functions using the appropriate XPATH function
functions <- roles_html %>%
  html_nodes(xpath = '//table//td[@class = "role"]/text()') %>%
  html_text(trim = TRUE)
functions

##### Combine extracted data into a data frame #####

# Create a new data frame from the extracted vectors
cast <- tibble(
  Actor = actors,
  Role = roles,
  Function = functions)

cast

##### Scrape an element based on its text ####

programming_html_raw <- "
<body>
<h3>The rules of programming</h3>
<ol>
  <li>Have <em>fun</em>.</li>
  <li><strong>Don't</strong> repeat yourself.</li>
  <li>Think <em>twice</em> when naming variables.</li>
</ol>
</body>"

programming_html <- read_html(programming_html_raw)

# Select all li elements
programming_html %>%
	html_nodes(xpath = '//li')

# Select all li elements
programming_html %>%
	html_nodes(xpath = '//li') %>%
	# Select all em elements within li elements that have "twice" as text
	html_nodes(xpath = '//em[text() = "twice"]')

# Select all li elements
programming_html %>%
	html_nodes(xpath = '//li') %>%
	# Select all em elements within li elements that have "twice" as text
	html_nodes(xpath = 'em[text() = "twice"]') %>%
	# Wander up the tree to select the parent of the em
    html_nodes(xpath = "..")

##### The nature of HTTP requests #####

# GET() : used to fetch a resource without submitting data
# POST() : Used to send data to a server, e.g. after filling out a form on a page

# With the httr library, I can send HTTP requests from my R session
library(httr)
GET('https://httpbin.org')

# What I got from the website was the HTML code from the website. I can save this in a variable 'response'.
# Using the 'content' function, I can get the HTML code of the website
response <- GET('https://httpbin.org')
content(response)

##### Do it the httr way #####

# Get the HTML document from Wikipedia using httr
wikipedia_response <- GET('https://en.wikipedia.org/wiki/Varigotti')
# Check the status code of the response
status_code(wikipedia_response)
# Parse the response into an HTML doc
wikipedia_page <- content(wikipedia_response)
wikipedia_page
# Extract the altitude with XPATH
wikipedia_page %>%
	html_nodes(xpath = '//table//tr[count(preceding-sibling::*)=8]/td') %>%
	html_text()

wikipedia_page %>%
	html_nodes(xpath = '//table//tr[position()=9]/td') %>%
	html_text()

##### Houston, we got a 404 #####

response <- GET('https://en.wikipedia.org/wiki/Varigott')
# Print status code of inexistent page
status_code(response)

##### Telling who you are with custom user agents #####

# When scraping the web, use my email address so they know who scrapes it

# Modify headers wtih httr
response <- GET('http://example.com',
                user_agent = "Hey, its me, Timo! Reach me at timo@timogrossenbacher.ch.")

# I can make the user_agent 'standard'
set_config(add_headers(`User Agent` = "Hey, it's me, Timo! Reach me at timo@timogrossenbacher.chc."))
response <- GET("http://example.com")

##### Check out your user agent #####

# Access https://httpbin.org/headers with httr
response <- GET("https://httpbin.org/headers")
# Print its content
print(content(response))

##### Add a customer user agent #####

# Pass a custom user agent to a GET query to the mentioned URL
response <- GET("https://httpbin.org/user-agent",
                config = user_agent("A request form a DataCamp course on scraping"))
# Print the response content
print(content(response))

# Globally set the user agent to "A request from a DataCamp course on scraping"
set_config(add_headers(`User-Agent` = "A request form a DataCamp course on scraping"))
# Pass a custom user agent to a GET query to the mentioned URL
response <- GET("https://httpbin.org/user-agent")
# Print the response content
content(response)

##### How to be gentle and slow down your requests #####

# Throttling: scraping multiple websites right after one another after a lag

# while(T){
#     print(Sys.time())
#     response <- GET("https://httpbin.org")
#     print(status_code(response))
# }

# a nicer way of requesting data from websites

# user the 'purrr' library to slow down requests
library(purrr)
throttled_GET <- slowly(~ GET("https://httpbin.org"),
                        rate = rate_delay(3)) # Delays each call by 3 seconds

# This call is hard coded and can't adjust very well. So, to adjust... use this:
throttled_GET <- slowly(GET(.),
                        rate = rate_delay(3))

# The dot will allow me to do this: throttled_GET(website).
# I can now interchange websites without hardcoding

##### Custom arguments for throttled functions #####

# Why is wikipedia printed instead of Google?

throttled_read_html <- slowly(~ read_html("https://wikipedia.org"),
                    rate = rate_delay(0.5))

for(i in c(1, 2, 3)){
  throttled_read_html("https://google.com") %>%
      html_node("title") %>%
      html_text() %>%
    print()
}

##### Apply throttling to a multipage crawler #####

# Define a throttled read_html() function with a delay of 0.5s
read_html_delayed <- slowly(~ read_html("https://en.wikipedia.org/w/index.php?title=K2&oldid=956671989"),
                            rate = rate_delay(0.5))

# Construct a loop that goes over all page urls
for(page_url in mountain_wiki_pages){
  # Read in the html of each URL with a delay of 0.5s
  html <- read_html_delayed(page_url)
}

# Construct a loop that goes over all page urls
for(page_url in mountain_wiki_pages){
   # Read in the html of each URL with a delay of 0.5s
  html <- read_html_delayed(page_url)
  # Extract the name of the peak and its coordinates
  peak <- html %>%
  	html_nodes("h1.firstHeading") %>% html_text()
  coords <- html %>%
    html_nodes("#coordinates .geo-dms") %>% html_text()
  print(paste(peak, coords, sep = ": "))
}
