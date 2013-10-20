#!/usr/bin/env python 

from BeautifulSoup import BeautifulSoup 
import urllib2
from pprint import pprint 
from contextlib import closing, nested

url='http://www.python.org/community/jobs/'
jobs='/tmp/index.html'

with nested(closing(urllib2.urlopen(url)), open (jobs,'w')) as (stream,myout): 
    html=stream.read()
    soup=BeautifulSoup(html)

    print >>myout,"<html><body>"
    sections=soup.findAll('div',{'class':'section'})
    for section in sections : 
        if section.findAll(lambda tag : tag.findAll('li',text='Telecommuting OK')) :
            print  >>myout,section
    print >>myout, "</body></html>"
