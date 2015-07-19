import mysql.connector

cnx = mysql.connector.connect(user='finance', password='nederland', host='localhost', database='finance')

cursor = cnx.cursor()
cursor.execute("SHOW DATABASES;")
cursor.execute("USE finance;")
cursor.execute(" "CREATE TABLE Prices (
	Ticker varchar(255),
	Price DECIMAL(8,3)
	);")

cnx.close()