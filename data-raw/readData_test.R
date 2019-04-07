library(tsda);
file <-'./data-raw/acct_revenue_structure.xlsx';
class(file) <-'excel';
rs <- readData(file);
rs;
class(rs);


file2 <- './data-raw/acct_revenue_structure_csv.csv';
class(file2) <-'csv';
rs2 <-readData(file2);
rs2;
