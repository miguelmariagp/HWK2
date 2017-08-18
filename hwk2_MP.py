import collections
import csv
import os, sys
import re
import nltk
from nltk import word_tokenize
from nltk.stem import PorterStemmer
from nltk import trigrams
from nltk.corpus import stopwords
from urllib import urlopen

#Load press releases
path_1 = 'C:/Users/ststest/Dropbox/TextAnalysis Grimmer/press/raw/Shelby'
dShelby = os.listdir(path_1)
path_2 = 'C:/Users/ststest/Dropbox/TextAnalysis Grimmer/press/raw/Sessions'
dSessions = os.listdir(path_2)

# # #EXERCISE 1

#Nested dictionaries with basic data from press releases
press={}
press['month']=[]
press['year']=[]
press['day']=[]
press['author']=[]

ds=dShelby+dSessions
for file in ds:
	press['day'].append(file[:2]) #Day
	press['month'].append(file[2:5]) #Month
	#seauth.append('sessions') #Author
	press['year'].append(file[5:9]) #year
press['author']=['shelby']*len(dShelby)+['sessions']*len(dSessions)

#Stemmer
pt = PorterStemmer()
#STOP WORDS
#stop_words = urlopen('http://jmlr.org/papers/volume5/lewis04a/a11-smart-stop-list/english.stop').read().split('\n')
#Adding to stop_words list
added_sw=['shelby','sessions','richard','jeff','email','press','room','member','senate']
stops = stopwords.words('english')+added_sw
#Stemming stop words
stops_stem=map(pt.stem,stops)


#Preprocessing and adding text
press['tokenized']=[]
press['trigrams']=[]
for i in range(0,len(ds)):
	#This if statement is to distinguish between the paths for each guy's press releases
	if press['author'][i]=='shelby':
		f=open('C:/Users/ststest/Dropbox/TextAnalysis Grimmer/press/raw/Shelby/'+ds[i],'r')
	else:
		f=open('C:/Users/ststest/Dropbox/TextAnalysis Grimmer/press/raw/Sessions/'+ds[i],'r')
	st=str(f.readlines())
	text_1 = st.lower() #no capitalization
	text_2 = re.sub('\W', ' ', text_1) #no punctuation
	text_3 = word_tokenize(text_2) #bag of words
	text_4 = map(pt.stem,text_3) #stemmed bag of words
	text_5 = [w for w in text_4 if not w in stops_stem] #removing stop words
	
	#Adding unigrams to dictionary
	press['tokenized'].append(text_5)
	#Adding trigrams to dictionary
	text_5_tri = trigrams(text_5)
	tgms=list() #Creating list of trigrams
	for i in text_5_tri:
		tgms.append(i)
	press['trigrams'].append(tgms)

	
#Counting UNIGRAMS
all_tokens=[]
for p in press['tokenized']:
	all_tokens=all_tokens+p

#Number of times each unigram is used
count_unigrams=collections.Counter(all_tokens)
#Hottest 1000 unigrams
mc1000=count_unigrams.most_common(1000)
tokens_mc1000=[] #Just the list of 1000 most common unigrams
for tup in mc1000:
	tokens_mc1000.append(tup[0])

press['tokenized'][0].count(mc1000[0])

#Creating document-term matrix for unigrams and dropping it in a csv
hdr=tokens_mc1000
hdr.insert(0,'Speaker')
with open('dtm_1.csv', 'wb') as f:
	my_writer = csv.writer(f)
	my_writer.writerow(hdr)
	for i in range(0,len(ds)): #
		cts=[] #Counts instances of each unigram on press release i
		for w in tokens_mc1000:
			cts.append(press['tokenized'][i].count(w)) 
		cts.insert(0,press['author'][i]) #Add author name to list of counts
		my_writer.writerow(cts)

f.close()

#Counting TRIGRAMS
all_trig=[]
for p in press['trigrams']:
	all_trig=all_trig+p
#Number of times each trigram is used
count_trigrams=collections.Counter(all_trig)
#Hottest 500 trigrams
mc500=count_trigrams.most_common(500)

#Creating document-term matrix for trigrams in a csv
trig_mc500=[] #Just the list of 500 most common trigrams
hdrt=[]
for tup in mc500:
	hdrt.append(str('.'.join(tup[0])))
	trig_mc500.append(tup[0])
hdrt.insert(0,'Speaker') #For the header

with open('dtm_2.csv', 'wb') as f:
	my_writer = csv.writer(f)
	my_writer.writerow(hdrt) #Header
	for i in range(0, len(ds)): #len(ds)
		cts=[] #Counts instances of each trigram on press release i
		for w in trig_mc500:
			cts.append(press['trigrams'][i].count(w))
		cts.insert(0,press['author'][i]) #Add author name to list of counts
		my_writer.writerow(cts)

f.close()
	
	
# # #EXERCISE 2 and 3 in R