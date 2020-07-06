import numpy as np
import pandas as pd
import re
import requests
from bs4 import BeautifulSoup as Soup
import csv


baseurl = "https://sofifa.com/players?offset="

column = ['ID','Player Name','picture','Flag','Position','Team_Image','Overall']
#,'Age','Position','Overall','Potential',
#		  'Team_Image','Team','Value','Wage','Total_Point']

FIFAdata = pd.DataFrame(columns = column)

offset = 0 ######PAGE NUMBER FOR SOFIFA
count = 1
flag = 0

#thresh = 1

#for offset in range(0,2):
while(1):
	print(count-1)
	url = baseurl + str(offset*60)	
	try:
		p_html = requests.get(url, timeout = 100)
	except:
		break
	p_soup = p_html.text
	data = Soup(p_soup,'html.parser')
	table = data.find('tbody')
	for i in table.findAll('tr'):	
		td = i.findAll('td')
		try:
			picture = td[0].find('img').get('data-src')
		except:
			picture = ""

		try:
			ID = td[0].find('img').get('id')
		except:
			ID = ""


		if(count==1):
			n = ID
		elif(count!=1):
			print(ID, n)
			if(ID == n):
				flag = 1

		if(flag==1):
			break


		try:
			flag = td[1].find('img').get('data-src')
		except:
			flag = ""

		#Name = td[1].findAll('a')[1].text

		try:
			player_name = td[1].find('div').text
		except:
			player_name = ""
		#print(player_name)

		try:
			Position = td[1].findAll('a')[1].text
		except:
			Position = ""

		try:
			Overall = td[3].find('span').text
		except:
			Overall = ""

		try:
			Team_image = td[5].find('img').get('data-src')
		except:
			Team_image = ""

		player_data = pd.DataFrame([[ID,player_name,picture,flag,Position,Team_image,Overall]])
		#	,Age,Position,Overall,Potential,
		#						 Team_image,Team,Value,Wage,Total_Point]])

		player_data.columns = column

		FIFAdata = FIFAdata.append(player_data, ignore_index = True)
		#print(count, player_data)

		count+=1

	if(flag==1):
		break

	offset+=1

	#print(table.prettify)
print("HERE")

FIFAdata.to_csv('sofifa.csv', index = False)
'''with open('sofifa.csv', mode='w') as csvfile:
	csv_writer = csv.writer(csvfile)
		#, quotechar='"', quoting=csv.QUOTE_MINIMAL)
	csv_writer.writerows(FIFAdata)
csvfile.close()
'''
