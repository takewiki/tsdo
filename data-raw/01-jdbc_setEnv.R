jdbc_file <-'https://cran.r-project.org/web/packages/RJDBC/RJDBC.pdf';
download.file(jdbc_file,'rjdbc_file.pdf')

#check jgc
library(RJDBC);
# .libPaths()
# install.packages('RJDBC',.libPaths()[3]);

# 已经完成安装

# connect to sql server using RJDBC


# https://www.microsoft.com/en-us/download/details.aspx?id=57782
# 
# I would try RJDBC http://cran.r-project.org/web/packages/RJDBC/RJDBC.pdf
# 
# with these drivers https://msdn.microsoft.com/en-us/sqlserver/aa937724.aspx
# 
# library(RJDBC)
# drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","/sqljdbc4.jar") 
# con <- dbConnect(drv, "jdbc:sqlserver://server.location", "username", "password")
# dbGetQuery(con, "select column_name from table")

# https://www.microsoft.com/en-us/sql-server/developer-get-started/java/ubuntu/


# require(RJDBC)
# drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
#             "/etc/sqljdbc_2.0/sqljdbc4.jar") 
# conn <- dbConnect(drv, "jdbc:sqlserver://serverName", "userID", "password")
# #then build a query and run it
# sqlText <- paste("
#    SELECT * FROM myTable
#   ", sep="")
# queryResults <- dbGetQuery(conn, sqlText)

# 
# Download sqljdbc_<version>_<language>.tar.gz to a temporary directory. 
# 
# 2. To unpack the zipped tar file, navigate to the directory where you want the driver unpacked and type gzip -d sqljdbc_<version>_<language>.tar.gz.  
# 
# 3. To unpack the tar file, move it to the directory where you want the driver installed and type tar –xf sqljdbc_<version>_<language>.tar.  
# 


# CLASSPATH =.:/home/usr1/mssqlserverjdbc/Driver/sqljdbc_7.2/enu/mssql-jdbc-7.2.2.jre11.jar


library(RJDBC)
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","/opt/sqljdbc_7.2/enu/mssql-jdbc-7.2.2.jre8.jar")
con <- dbConnect(drv, "jdbc:sqlserver://115.159.201.178:1433;databaseName=AIS20190427230019", "sa", "Hoolilay889@")
db <-dbGetQuery(con, "select * from T_AP_PAYBILL;");
View(db);


voucher <- dbGetQuery(con,'select * from T_GL_VOUCHER;')

View(voucher);

