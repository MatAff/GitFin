USE  finance ;

/* Create table to save notifications */

DROP TABLE IF EXISTS notification ;
CREATE TABLE  notification  (
   noticeID  Integer(11) NOT NULL AUTO_INCREMENT,
   timestamp  DATETIME NOT NULL,
   notice  varchar(255) NOT NULL,
   importance  Integer(11) NOT NULL,
  PRIMARY KEY ( noticeID )
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

/* Create table to save news */

DROP TABLE IF EXISTS  basicnews ;
CREATE TABLE  basicnews  (
   newsID  Integer(11) NOT NULL AUTO_INCREMENT,
   timestamp  DATETIME NOT NULL,
   source  varchar(255) NOT NULL, 
   title  varchar(1000) NOT NULL, 
   description  varchar(1000),
   url  varchar(255),
   tickerTags  varchar(255), 
  PRIMARY KEY ( newsID ),
  UNIQUE KEY uniq_TimeStamp_Title (timestamp, title),
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

