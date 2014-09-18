import requests
import lxml.html as lh

url = 'http://www.nytimes.com/reuters/2013/01/25/world/americas/25reuters-venezuela-prison.html?partner=rss&emc=rss'
page = requests.get(url)
doc = lh.fromstring(page.content)
text = doc.xpath('//p[@itemprop="articleBody"]')
finalText = str()
for par in text:
    finalText += par.text_content()

print finalText


import pattern.web

url = 'http://rss.nytimes.com/services/xml/rss/nyt/World.xml'
results = pattern.web.Newsfeed().search(url, count=5)
results

print '%s \n\n %s \n\n %s \n\n' % (results[0].url, results[0].title, results[0].description)
print '%s \n\n %s \n\n %s \n\n' % (results[0].url, results[0].title, pattern.web.plaintext(results[0].description))

import codecs

outputFile = codecs.open('~/tutorialOutput.txt', encoding='utf-8', mode='a')

def scrape(url):
    page = requests.get(url)
    doc = lh.fromstring(page.content)
    text = doc.xpath('//p[@itemprop="articleBody"]')
    finalText = str()
    for par in text:
        finalText += par.text_content()
    return finalText

for result in results:
    outputText = scrape(result.url)
    outputFile.write(outputText)

outputFile.close()


url = 'http://164.100.47.132/LssNew/psearch/Result13.aspx?dbsl='

for i in xrange(5175,5973):
    newUrl = url + str(i)
    print 'Scraping: %s' % newUrl


from scrapy.contrib.spiders import CrawlSpider, Rule
from scrapy.contrib.linkextractors.sgml import SgmlLinkExtractor
from scrapy.selector import HtmlXPathSelector
from scrapy.item import Item
from BeautifulSoup import BeautifulSoup
import re
import codecs

class MySpider(CrawlSpider):
    name = 'statespider' #name is a name
    start_urls = ['http://www.state.gov/r/pa/prs/dpb/2010/index.htm',
    ] #defines the URL that the spider should start on. adjust the year.

        #defines the rules for the spider
    rules = (Rule(SgmlLinkExtractor(allow=('/2010/'), restrict_xpaths=('//*[@id="local-nav"]'),)), #allows only links within the navigation panel that have /year/ in them.

    Rule(SgmlLinkExtractor(restrict_xpaths=('//*[@id="dpb-calendar"]',), deny=('/video/')), callback='parse_item'), #follows links within the caldendar on the index page for the individuals years, while denying any links with /video/ in them

    )

    def parse_item(self, response):
        self.log('Hi, this is an item page! %s' % response.url) #prints the response.url out in the terminal to help with debugging
        
        #Insert code to scrape page content

        #opens the file defined above and writes 'texts' using utf-8
        with codecs.open(filename, 'w', encoding='utf-8') as output:
            output.write(texts)
