import java.io
import java.time.LocalDateTime
import scala.io.Source

//Reading the file. (TESTING)
val hopsitalDataset = Source.fromFile("C:/Users/User/Downloads/hospital.csv").getLines().toList

trait CSVFileReader[T]:
  /** Reads the CSVFile and converts it into a list of type T.
   *  Why? I thought it was the best approach to take.
   */
  def processFile(source: String): List[T]
  

//Data Model to be operated on
//Why keep all data? - Under the assumption that the DataAnalysis operations MAY increase to other fields.
case class HospitalData( val date: LocalDateTime,
                         val state: String,
                         val beds: Int,
                         val covidBeds: Int,
                         val nonCritBeds: Int,
                         val covidAdmissions: Int,
                         val puiAdmissions: Int,
                         val totalAdmissions: Int,
                         val puiDischarged: Int,
                         val covidDischarged: Int,
                         val totalDischarged: Int,
                         val hospPui: Int,
                         val hospCovid: Int,
                         val hospNonCovid: Int):
  //Get the method to get total beds, removes the need to constantly run (record => record.beds + ...)
  def getTotalBeds = beds+covidBeds+nonCritBeds


//Responsible for all DataAnalysis Operations for the Hospital
object HospitalDataAnalysis:
  def getStateWithHighestBedCount(data: List[HospitalData]) : String =
    /** Calculates total bed count for each record and retrieves the name of the state with the highest total.
     * How:
     * Runs through the list once
     * Returns whichever record has the maximum sum of beds (beds + covid beds + non-critical beds)
     */
    data.maxBy(record => record.getTotalBeds).state

  def getCovidBedRatio(data: List[HospitalData]): Double =
    val (totalBeds, totalCovidBeds) = data.foldLeft((0, 0)) { (acc, record) =>
      (acc._1 + record.getTotalBeds, acc._2 + record.covidBeds)
    }

    if (totalBeds == 0) 0.0
    else totalCovidBeds.toDouble / totalBeds

  def averageAdmissionsByCategory(data: List[HospitalData]): Double = ???

object Runner extends App:
  //Test data
  val dataset = List(
    HospitalData(LocalDateTime.now(), "Johor", 500, 300, 350, 30, 20, 50, 15, 10, 25, 5, 10, 15),
    HospitalData(LocalDateTime.now(), "Johor", 500, 150, 350, 40, 25, 65, 20, 15, 35, 10, 15, 20),
    HospitalData(LocalDateTime.now(), "Kedah", 400, 100, 300, 20, 15, 35, 10, 5, 15, 3, 7, 10),
    HospitalData(LocalDateTime.now(), "Kedah", 400, 100, 300, 25, 20, 45, 12, 8, 20, 4, 9, 12)
  )

  val higheststate = HospitalDataAnalysis.getStateWithHighestBedCount(dataset)
  val ratio = HospitalDataAnalysis.getCovidBedRatio(dataset)

  println(s"$higheststate, \n$ratio")

  //Effective running
  hopsitalDataset.foreach(line => println(line))
