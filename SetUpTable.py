import mysql.connector

cnx = mysql.connector.connect(user='finance', password='nederland', host='localhost', database='finance')

cursor = cnx.cursor()
cursor.execute("SHOW DATABASES;")
cursor.execute("USE finance;")
cursor.execute("CREATE TABLE Persons (PersonID int);")

cnx.close()