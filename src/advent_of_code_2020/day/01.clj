(ns advent-of-code-2020.day.01)

;; Before you leave, the Elves in accounting just need you to fix your expense
;; report (your puzzle input); apparently, something isn't quite adding up.

;;;;;;;;;; Part 1 ;;;;;;;;;;

;; Specifically, they need you to find the two entries that sum to 2020 and then
;; multiply those two numbers together.

;; For example, suppose your expense report contained the following:

(def example [1721 979 366 299 675 1456])

;; In this list, the two entries that sum to 2020 are 1721 and 299.
;; Multiplying them together produces 1721 * 299 = 514579.

(def example-solution 514579)

;; Of course, your expense report is much larger. Find the two entries that sum to
;; 2020; what do you get if you multiply them together?

(def expense-report
  [1597 1857 1703 1956 1809 1683 1629 230 1699 1875 1564 1700 1911 1776 1955 1585
   1858 1725 1813 1568 1962 1535 487 1621 1620 1573 1918 1794 2003 1957 1840 1936
   285 1630 1753 1649 1951 1968 1916 1694 1593 1980 1806 1779 1637 1674 1842 1659
   1553 1846 1677 1944 1811 1645 1784 1791 1988 1864 1596 1773 1132 1715 1938 1901
   1617 1892 1708 1788 1765 1684 1595 1971 1798 1543 507 1772 1757 1950 1844 1898
   1671 1602 1599 1524 2005 1855 1624 1884 1990 1604 1873 1736 1747 1900 1534 1713
   1690 1525 1838 587 74 1979 1635 1896 1580 1727 1994 1940 1819 1758 1852 1639
   1754 1559 1919 1879 1874 1921 1575 1693 1710 1949 1719 1933 1673 1909 1989 1897
   909 1889 1641 1658 1530 1541 1952 1627 1839 1803 833 1527 1756 2009 1605 1548
   1660 1966 1770 1552 1939 1726 382 1665 1960 1733 1983 1544 1974 1985 1625 609
   1931 1749 1975 1562 1563 1922 2008 2010 1704 1545 1636 1762 1841 1717 622 2007
   1670 1771 1910 1978 1615 1805 1999 1643 1748 1531 1539 1787 1722 1111 1774 1540
   1607 1902 1991 1763 1691 1812 1783 1579])

(defn find-two
  "Find the two entries that sum to a target value."
  [report target]
  (let [report-set (set report)]
    (when-let [num-1 (some #(report-set (- target %1)) report)]
      [(- target num-1) num-1])))

(defn solution-part-1
  [report]
  (->> (find-two report 2020)
       (#(do (println "Numbers:"%)%))
       (apply *)))


;;;;;;;;;; Part 2 ;;;;;;;;;;

;; They offer you a second one if you can find three numbers in your expense report
;; that meet the same criteria.

(defn find-three
  "Find the three entries that sum to a target value."
  [report target]
  (let [report-set (set report)
        num-1 (first (some #(seq (find-two report (- target %1))) report))]
    (conj (find-two report (- target num-1)) num-1)))

(defn solution-part-2
  [report]
  (->> (find-three report 2020)
       (#(do (println "Numbers:"%)%))
       (apply *)))

(defn print-answers []
  (println "---- Part 1 ----")
  (->> (solution-part-1 expense-report)
       (println "Answer: "))

  (println "---- Part 2 ----")
  (->> (solution-part-2 expense-report)
       (println "Answer: ")))
