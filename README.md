sqlr
===
Flexibly generate SQL statements and queries from R to run against a DBMS, using [DBI](https://cran.r-project.org/web/packages/DBI) back ends. Currently the focus is on MySQL/MariaDB SQL and the [RMariaDB](https://cran.r-project.org/web/packages/RMariaDB) back end.

[![Travis-CI Build Status](https://travis-ci.org/nbenn/sqlr.svg?branch=master)](https://travis-ci.org/nbenn/sqlr)
[![Coverage status](https://codecov.io/gh/nbenn/sqlr/branch/master/graph/badge.svg)](https://codecov.io/github/nbenn/sqlr?branch=master)

## Installation
The easiest way of getting up and running is by means of the `devtools` package as
```r
install.packages("devtools")
devtools::install_github("nbenn/sqlr")
```
Alternatively the package can be [downloaded](https://github.com/nbenn/sqlr/archive/master.zip) and installed manually:
```bash
unzip ~/Downloads/sqlr-master.zip
R CMD INSTALL --no-multiarch --with-keep.source ~/Downloads/sqlr-master
```
## Database connection
Database credentials and access configuration is yaml-based. The default location that is searched for a .yml file is the current working directory of the R session. An example configuration is provided as
```yaml
db_setup:
  dbtype: mysql
  dbname: test
  username: test
  password: test123
```
The default top level node that is used is `db_setup`, but any other top level node can be used via the `section` argument of `set_con()`. All entries except `dbtype` are passed as named arguments to `DBI::dbConnect()`, while `dbtype` specifies the DBI back end. This means that if a `~/.my.cnf` file with a `[my-db]` group is present, this can be used by having a `group: my-db` entry in the loaded yaml node. For interactive sessions and if access credentials are not to be stored in plain text files, the yaml file can also parse R expressions. A field such as `password: !expr getPass::getPass("enter password ")`, will prompt the user for the password when the config file is loaded.
## Hello World
In order to write the data that is attached to this package (based on [nycflights13](https://CRAN.R-project.org/package=nycflights13)), one could proceed as follows.
```r
write_db_tbl("airline", airlines, keys = pk_spec("carrier"),
             char_set = "ascii")
```
This will create a table as
```sql
CREATE TABLE `airport` (
  `faa` char(3) NOT NULL,
  `name` varchar(51) DEFAULT NULL,
  `lat` double DEFAULT NULL,
  `lon` double DEFAULT NULL,
  `alt` smallint(6) DEFAULT NULL,
  `tz` tinyint(4) DEFAULT NULL,
  `dst` char(1) DEFAULT NULL,
  `tzone` enum('America/Anchorage','America/Chicago','America/Denver',
               'America/Los_Angeles','America/New_York','America/Phoenix',
               'America/Vancouver','Asia/Chongqing','Pacific/Honolulu') 
               DEFAULT NULL,
  PRIMARY KEY (`faa`)
) ENGINE=InnoDB DEFAULT CHARSET=ascii;
```
Analogously, the tables `airport` and `plane` could be created and written to as:
```r
write_db_tbl("airport", airports, keys = pk_spec("faa"), char_set = "ascii")
write_db_tbl("plane", planes, keys = pk_spec("tailnum"), char_set = "ascii")
```
For the weather table we would like to introduce a foreign key constraint, indicating at which airports the weather measurements were performed. Furthermore we would like to use a compound primary key, as only the combination of data and place is unique. This can be achieved by running:
```r
write_db_tbl("weather", weather,
             keys = list(pk_spec(c("origin", "ts")),
                         fk_spec("origin", "airport", "faa",
                                 on_del = "set_null")),
             char_set = "ascii")
```
which creates and writes to a table like
```sql
CREATE TABLE `weather` (
  `origin` char(3) NOT NULL,
  `ts` datetime NOT NULL,
  `temp` float DEFAULT NULL,
  `dewp` float DEFAULT NULL,
  `humid` float DEFAULT NULL,
  `wind_dir` int(11) DEFAULT NULL,
  `wind_gust` double DEFAULT NULL,
  `wind_speed` double DEFAULT NULL,
  `precip` float DEFAULT NULL,
  `pressure` float DEFAULT NULL,
  `visib` enum('0','1','2','3','4','5','6','7','8','9','10') DEFAULT NULL,
  PRIMARY KEY (`origin`,`ts`),
  CONSTRAINT `weather_ibfk_1`
    FOREIGN KEY (`origin`) REFERENCES `airport` (`faa`)
    ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=ascii;
```
Finally the table holding all flight information can be created with foreign key constraints for airports, airlines and planes. Cases where no corresponding entries in parent tables exist, have to be NULL'ed, otherwise the fk constraints will fail on import.
```r
flights$dest[!flights$dest %in% airports$faa] <- NA
flights$tailnum[!flights$tailnum %in% planes$tailnum] <- NA

write_db_tbl("flight", flights,
             cols = c(col_id(as_lst = TRUE),
                      get_col_spec(flights)),
             keys = list(fk_spec("origin", "airport", "faa",
                                 on_del = "set_null"),
                         fk_spec("dest", "airport", "faa",
                                 on_del = "set_null"),
                         fk_spec("carrier", "airline", "carrier",
                                 on_del = "set_null"),
                         fk_spec("tailnum", "plane", "tailnum",
                                 on_del = "set_null")),
             char_set = "ascii")
```
The corresponding `CREATE TABLE` statement is given as
```sql
CREATE TABLE `flight` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `departure` datetime DEFAULT NULL,
  `dep_delay` time DEFAULT NULL,
  `arr_delay` time DEFAULT NULL,
  `air_time` time DEFAULT NULL,
  `carrier` char(2) DEFAULT NULL,
  `flight` smallint(5) unsigned DEFAULT NULL,
  `tailnum` varchar(6) DEFAULT NULL,
  `origin` char(3) DEFAULT NULL,
  `dest` char(3) DEFAULT NULL,
  `distance` double DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `origin` (`origin`),
  KEY `dest` (`dest`),
  KEY `carrier` (`carrier`),
  KEY `tailnum` (`tailnum`),
  CONSTRAINT `flight_ibfk_1` FOREIGN KEY (`origin`) 
    REFERENCES `airport` (`faa`) 
    ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `flight_ibfk_2` FOREIGN KEY (`dest`)
    REFERENCES `airport` (`faa`)
    ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `flight_ibfk_3` FOREIGN KEY (`carrier`)
    REFERENCES `airline` (`carrier`)
    ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `flight_ibfk_4` FOREIGN KEY (`tailnum`)
    REFERENCES `plane` (`tailnum`)
    ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=80790 DEFAULT CHARSET=ascii;
```
