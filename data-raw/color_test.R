library(readxl)
takewiki_color_mapping_table_v1_0 <- read_excel("~/pkgs/tsda/data-raw/takewiki_color_mapping_table_v1.0.xlsx")
takewiki_color_mapping_table_v1_0;
C <- takewiki_color_mapping_table_v1_0$C;
M <- takewiki_color_mapping_table_v1_0$M;
Y <- takewiki_color_mapping_table_v1_0$Y;
K <- takewiki_color_mapping_table_v1_0$K;
library(tsda);
CMYK2RGB(C,M,Y,K);
