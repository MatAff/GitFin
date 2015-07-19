import mysql.connector

cnx = mysql.connector.connect(user='finance', password='nederland', host='localhost', database='finance')

cnx.close()