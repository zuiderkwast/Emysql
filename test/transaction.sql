%% DDL for transaction test suite.
%% Author: Bart van Deenen
%% 04/18/12

USE hello_database;

DROP TABLE IF EXISTS `uniq`;
CREATE TABLE `uniq` (
  `key` int(11) NOT NULL,
  `value` int(11) NOT NULL,
  PRIMARY KEY (`key`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `nonuniq`;
CREATE TABLE `nonuniq` (
  `key` int(11) NOT NULL,
  `value` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
