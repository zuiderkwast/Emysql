
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
