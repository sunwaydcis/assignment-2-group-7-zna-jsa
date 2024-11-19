import java.io //To read the hospital.csv file
import java.time.LocalDateTime //To get DateTime Type

/*
This Assignment is from:
- 21074380 : Zar Nie Aung
- ID Here : Joey Simon Abrahams
 */

/* PSEUDOCODE:
- Code is to read all records of CSV and store them into a list of TYPE: T - can use case class
- Code is to process / order them according to given criteria
- Q1: Scan the entire table, print out the state with the most beds
- Q2: Scan the entire table, calculate total beds used for covid vs total bed available and get the ratio
- Q3: Scan the entire table. For each state, calculate all admissions of each type, and average it against the total records scanned for it.
-- Ex: If Malacca had 183 total covid cases over 30 records (30 days) that means they had 6.1 covid cases a day.
 */

/* CLASS HIERARCHY / RELATIONSHIPS
- traits
-- Displayable: Any information that can be diplsayed --Don't exactly know why we need it - Zn
-- AnalysisOperations - Contains the methods to fulfill the question requirements.
-- Classes:
-- Abstract class (parent to case classes): Hospital Data, is Displayable.
---- Why? All data has a state.
-- Case Class 1 - Hospital Capacity Data - to get the capacity of each hospital and state
-- Case Class 2 - Hospital Admissions Data - to get the total admissions for each hospital
-- Object: Hospital Data Analysis, uses trait: Analysis Operations. -- Actually, I think we can go straight to it with an object - Zn
 */

object Runner extends App:
  println("Hello World")


