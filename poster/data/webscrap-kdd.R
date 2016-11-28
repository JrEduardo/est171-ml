## ----setup, eval=TRUE, include=FALSE-------------------------------------

## See the details in post: web address
## <https://jreduardo.github.io/kddClub/post1-webscrap-kdd.html>

opts_chunk$set(eval = FALSE)

## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## Load the packages
library("rvest")
library("plyr")

## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## Read the topics with title, organizer and abstract

## Save the html page
url <- "http://www.kdd.org/kdd2016/topics"
topicshtml <- url %>% read_html

## Get interesting nodes
titles <- topicshtml %>% html_nodes("h4 strong") %>% html_text
paragraphs <- topicshtml %>% html_nodes("blockquote p") %>% html_text
chairs <- paragraphs[seq(1, by = 4, length.out = length(titles))] %>%
    gsub(pattern = ".+: (.+)$", replacement = "\\1")
abstracts <- paragraphs[seq(3, by = 4, length.out = length(titles))]

## Organize the text topics into data.frame
topics <- data.frame(
    topics = titles,
    chairs = chairs,
    abstracts = abstracts,
    stringsAsFactors = FALSE
)

## Export to txt file
write.table(topics, "./data/topics.txt", sep = ";",
            row.names = FALSE)


## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## Read the abstracts subtopics in each topics

## Links of the topics page
links <- topicshtml %>%
    html_nodes("blockquote a") %>%
    html_attr("href") %>%
    unique

## Attribute names to link
names(links) <- sapply(links, function(x) {
    gsub("^.*view/(.*)$", "\\1", x)
}, USE.NAMES = FALSE)

## Extract the abstracts (this part is a little slow)
abstracts <- lapply(links, function(page_topics) {
    page_topics_html <- page_topics %>% read_html
    links <- page_topics_html %>%
        html_nodes("strong a") %>%
        html_attr("href")
    title <- page_topics_html %>%
        html_nodes("strong a span") %>%
        html_text
    texto <- sapply(links, function(page_paper) {
        ps <- page_paper %>%
            read_html %>%
            html_nodes("section p") %>%
            html_text
        ps[3] ## The abstract is in third position
    })
    ma <- cbind(title, texto)
    dimnames(ma) <- NULL
    ma
})

## Organize texts into data.frame
papers <- ldply(abstracts, .id = "topic")
names(papers) <- c("topic", "title", "abstract")
papers <- transform(
    papers,
    title = as.character(title),
    abstract = as.character(abstract)
)

## Export to txt file
write.table(papers, "./data/papers.txt", sep = ";",
            row.names = FALSE)


## ---- eval=TRUE, echo=FALSE----------------------------------------------

## Load the data
topics <- read.table("./data/topics.txt", header = TRUE, sep = ";",
                     stringsAsFactors = FALSE)
papers <- read.table("./data/papers.txt", header = TRUE, sep = ";",
                     stringsAsFactors = FALSE)

str(topics)
str(papers)
