import java.io //To read the hospital.csv file
import java.time.LocalDateTime //To get DateTime Type

/* This Assignment is from:
- 21074380 : Zar Nie Aung
- ID Here : Joey Simon Abrahams

PSEUDOCODE:
- Code is to read all records of CSV and store them into a list of TYPE: T - can use case class
- Code is to process / order them according to given criteria
- Q1: Scan the entire table, print out the state with the most beds
- Q2: Scan the entire table, calculate total beds used for covid vs total bed available and get the ratio
- Q3: Scan the entire table. For each state, calculate all admissions of each type, and average it against the total records scanned for it.
-- Ex: If Malacca had 183 total covid cases over 30 records (30 days) that means they had 6.1 covid cases a day.

CLASS HIERARCHY / RELATIONSHIPS
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

trait Displayable: //Don't think we need this - ZN.
  def showData(): Unit

//Trait to conduct all analysis operations. By right a trait should better carry abstract methods.
trait AnalysisOperations //--Still think it should be an object directly.
  //Moved all methods elsewhere

//Parent class of hospital data, to remove redundancies (State name)
case class HospitalData2(val state: String, val )

abstract class HospitalData(val state: String) extends Displayable

case class HospitalCapacityData(_state: String, val totalBeds: Int, val covidBeds: Int, val nonCovidBeds: Int)
  extends HospitalData(_state):
  override def showData() =
    println(s"State: $state, Total Beds: $totalBeds, COVID Beds: $covidBeds, Non-COIVD Beds: $nonCovidBeds")

case class HospitalAdmissionsData(_state: String, val totalAdmittedPui: Int, val totalAdmittedCovid: Int)
  extends HospitalData(_state):
  override def showData() =
    println(s"State: $state, Total PUI Admissions: $totalAdmittedPui, Total Covid Admissions: $totalAdmittedCovid")


object HospitalDataAnalysis extends AnalysisOperations: //I have an idea how to make the code more scalable for other analysis operations
  def getStateWithHighestBedCount(data: List[HospitalCapacityData]): String =
    if (data.isEmpty)
      println("Warning:The dataset is empty.")
      return ""
    data.maxBy(_.totalBeds).state

  def getCovidBedRatio(data: List[HospitalCapacityData]): Double =
    if (data.isEmpty)
      println("Warning: The dataset is empty.")
      return 0.0
    val totalCovidBeds = data.map(_.covidBeds).sum
    val totalBeds = data.map(_.totalBeds).sum
    totalCovidBeds.toDouble / totalBeds

  def getAvgDailyAdmissions(data: List[HospitalAdmissionsData], category: String): Map[String, Double] =
    ???


object Runner extends App:
  //Test Data for HospitalCapacityAnalysis
  val datasetCapacity = List(
    HospitalCapacityData("Johor", 500, 150, 350),
    HospitalCapacityData("Kedah", 400, 100, 300),
    HospitalCapacityData("Kelantan", 600, 200, 400),
    HospitalCapacityData("Melaka", 450, 120, 330),
    HospitalCapacityData("NegeriSembilan", 450, 120, 330),
    HospitalCapacityData("Pahang", 450, 120, 330),
    HospitalCapacityData("Perak", 450, 120, 330),
    HospitalCapacityData("Perlis", 450, 120, 330),
    HospitalCapacityData("Pulau Pinang", 450, 120, 330),
    HospitalCapacityData("Sabah", 450, 120, 330),
    HospitalCapacityData("Sarawak", 450, 120, 330),
    HospitalCapacityData("Selangor", 450, 120, 330),
    HospitalCapacityData("Terengganu", 450, 120, 330),
    HospitalCapacityData("W.P.Kuala Lumpur", 450, 120, 330),
    HospitalCapacityData("W.P.Labuan", 450, 120, 330)
  )

  // Testing analysis functions through trait
  val highestBedsState = HospitalDataAnalysis.getStateWithHighestBedCount(datasetCapacity)
  println(s"State with the highest total hospital beds: $highestBedsState")

  val covidBedToTotalRatio = HospitalDataAnalysis.getCovidBedRatio(datasetCapacity)
  println(f"Ratio of COVID-19 dedicated beds to total available hospital beds: $covidBedToTotalRatio%.2f")

//GOING TO IMPROVE CODE HIERARCHY