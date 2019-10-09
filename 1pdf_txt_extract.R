setwd("D:/Shared/Backed Up/Caitlin/NewConsDebate")
# setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/Backed Up/Caitlin/NewConsDebate")

# Load packages
# install.packages("pdftools")
# install.packages("tesseract")
library(pdftools)
library(tesseract)


### BACKGROUND
# Here's the list of 30+30+30 orgs from Paul we'll be using
sample <- read.csv("Larson_etal_EcoEvo_990s.csv")
sample <- sample %>%
  select(NGO.name, Zip, State, Range) %>%
  group_by(NGO.name) %>%
  filter(row_number()==1)
write.csv(sample, "90_orgs.csv")


# Pulled all pdfs off Global Star page for The Wilderness Society (https://www.guidestar.org/profile/53-0167933)
# Used Chrome extension called Batch Link Downloader to scrape all pdfs from that page at once.
# N.b., guidestar may have longer running dataset than propublica?
# N.b., TWS ammended their 2012 990, so there are two pdfs for 2012...
# ... we'll have to add some code that uses any amended forms. Maybe finds check-box re: amended on pg. 1.
# Refs:
# https://dss.iq.harvard.edu/blog/extracting-content-pdf-files
# https://diging.atlassian.net/wiki/spaces/DCH/pages/5275668/Tutorial+Text+Extraction+and+OCR+with+Tesseract+and+ImageMagick
# Chttps://cran.r-project.org/web/packages/tesseract/vignettes/intro.html


# Read in PDF text.
# Read all pdf names stored in my folder; maintain full path name.
list990s <- list.files("D:/Shared/Backed Up/Caitlin/NewConsDebate/990s", pattern = ".pdf$", full.names = TRUE)
# Try reading text off a random pdf -- here 14th.
pdfs <- pdf_text(list990s[14])

# ARGH! Only get text from the header page. Info in 990 PDFs aren't stored as text but as images >:(
# So, clearly this won't work for the 990s, but maybe for the annual reports or op-eds 
# With a good text pdf, this exports raw text into a character vector...
# with spaces to show the white space and \n to show the line breaks.



# Convert pdfs to images then extract text.
###############################################################################################
# # FIXME:
# For 990s, convert pdfs to pngs.
# This pdf_convert function may work if dpi of orig pdf is OK and you specify high dpi in coversion.
# This pulls page 1 of 14th pdf from list, then saves as png with "_pg1.png" added to filename).

pdf_convert(list990s[14], format = "png", pages = 1, dpi=600, filenames = paste0(list990s[14],"_pg1.png"),verbose = TRUE)
###############################################################################################


# Read in all png names. N.b., this folder of pngs I converted at pdf2png.com, but we should automate above
pngs <- list.files("D:/Shared/Backed Up/Caitlin/NewConsDebate/990s/2008-530167933-050eb8ea-9", pattern = ".png$", full.names = TRUE)

# Use OCR (Optical Character Recognition) from package tesseract to extract text from png. Here, pg. 1.
text <- ocr(pngs[1]) 
cat(text) # Prints concatenated text, based on line breaks of \n

# # Generate word list/confidence. I don't think this will be helpful for 990s but maybe for annual reports and op-eds?
# # Alt: just find word counter and operate on text itself, whereas this still looks at image.
# results <- ocr_data(pngs[1])
# print(results, n=20)

# Pull the year out of this massive string. Hopefully yr always shows up right before the words "calendar year"
# but this will depend on quality of png to text conversion (both position and whether "calendar year" is there).
# Searches for words "calendar year" then steps back 5 characters to starting position.
# Captures text from that starting position to the stopping position, which is 1 character back.
yr <- substring(text, regexpr("calendar year", text) - 5, regexpr("calendar year", text) - 2)
# tada! yr is 2007.

##############################################################################################
# Options going forward:
# 1) could rely on line breaks if some value is consistently on same line \n (though I don't like this)
# 2) could find key text like this ("calendar year") and specify where to pull text (in sequence, between, etc.).
# Doing so would likely require a spell check. Here are some potential refs:
# https://www.r-bloggers.com/automatic-cleaning-of-messy-text-data/
# https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html
# Potential tool: AspellCheck(input, output = c("eval", "sugg", "fix"), sep = FALSE, cap.flag = c("none", "first", "all"), ignore=NULL, split.missing = FALSE)
