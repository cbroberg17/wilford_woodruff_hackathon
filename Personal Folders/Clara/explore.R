library(tidyverse)
library(readr)

journals <- read_csv("data/journals.csv")
scriptures <- read_csv("data/lds-scriptures.csv")
journal_dates <- read_csv("data/WW_by_date_with_may.csv")


extract <- journal_dates %>%
  mutate(book = str_extract(text, "Book of \\w+"))




####### getting chapters ########
book_of <- journal_dates %>%
  mutate(book = str_extract(text, "[B|b]ook of \\w+"))


books_table <- book_of %>%
  drop_na(book)


chapters <- books_table %>%
  mutate(book_ch = str_extract(text, '(((\\d+|\\w+)(st|rd|th)\\b)|\\d+)([A-Z a-z])+?[B|b]ook of \\w+'))

chapters$book <- sub("Book of Doctrine\\b", "Book of Doctrine and Covenants", chapters$book)
chapters$book <- sub("Book of D\\b", "Book of Doctrine and Covenants", chapters$book)
chapters$book <- sub("Book of Covenents\\b", "Book of Doctrine and Covenants", chapters$book)
chapters$book <- sub("Book of Doctrins\\b", "Book of Doctrine and Covenants", chapters$book)
chapters$book <- sub("Book of Doctrines\\b", "Book of Doctrine and Covenants", chapters$book)
chapters$book <- sub("Book of Morm\\b", "Book of Mormon", chapters$book)
chapters$book <- sub("Book of mormon\\b", "Book of Mormon", chapters$book)
chapters$book <- sub("book of mormon\\b", "Book of Mormon", chapters$book)
chapters$book <- sub("book of Mormon\\b", "Book of Mormon", chapters$book)
chapters$book <- sub("Book of Mormoon\\b", "Book of Mormon", chapters$book)
chapters$book <- sub("Book of Jer\\b", "Book of Jeremiah", chapters$book)
chapters$book <- sub("Book of Mathew\\b", "Book of Matthew", chapters$book)
chapters$book <- sub("Book of Mormonto\\b", "Book of Mormon", chapters$book)


chapters2 <- chapters[-c(87, 99, 100, 101, 106, 123, 140, 142, 143, 146, 148, 152, 167, 175, 183, 184),]


#### building plots
chapters3 <- chapters2 %>%
  mutate(date = mdy(date))


ggplot(data=chapters3, mapping=aes(y=book, x=date, color=book)) +
  geom_point() +
  theme(legend.position = "none")


ggplot(data=chapters2, mapping = aes(x=book, stat='count', fill=book)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "none", axis.title.y=element_blank())

not_morm <- chapters2 %>%
  filter(book != "Book of Mormon")

ggplot(data=not_morm, mapping = aes(x=book, stat='count', fill=book)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "none", axis.title.y=element_blank())




##### new stuff

books <- scriptures$book_title
# creating one string
book_string <- paste(books, collapse = "|")
# Splitting it by one book
franklin <-str_split(book_string,'\\|')
# This unlisted it though im not sure it did anything
carl <- franklin %>% unlist()
# This creates unique list
new <-unique(carl)
new <- new %>% c("Nephi", 'John', 'Peter', 'Corinthians','Thessalonians')

new_word= "Book of"

for (i in seq_along(new)) {
  new[i] <- paste(new_word,new[i])
}



# This extracts from the text all things that match any names in the list and put it into a new column
chapters$book <- sapply(str_extract_all(chapters$text, paste(new, collapse = "|")), 
                       paste, collapse = ", ")



######################### prophet data 

papers = journal_dates %>% mutate(prophet = str_extract(text, pattern = '\\w+ \\((OT|NT)\\)')) %>%
  drop_na(prophet)


papers$prophet <- sub("the (NT)", "John the Baptist (NT)", papers$prophet)


###### prophet graphs 
ggplot(data=papers, mapping = aes(x=prophet, stat='count', fill=prophet))  +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "none", axis.title.y=element_blank())


papers <- papers %>%
  mutate(date = mdy(date))
ggplot(data=papers, mapping=aes(y=prophet, x=date, color=prophet)) +
  geom_point() +
  theme(legend.position = "none")




