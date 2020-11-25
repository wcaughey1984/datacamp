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
