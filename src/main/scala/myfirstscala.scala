import java.io
import java.time.LocalDateTime
import scala.io.Source

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
                         val hospNonCovid: Int)

//Responsible for all DataAnalysis Operations for the Hospital
object HospitalDataAnalysis:
  def getStateWithHighestBedCount(data: List[HospitalData]) : String =
    /** Calculates total bed count for each record and retrieves the name of the state with the highest total.
     * How:
     * Runs through the list once
     * Returns whichever record has the maximum sum of beds (beds + covid beds + non-critical beds)
     */
    data.maxBy(record => record.beds + record.covidBeds + record.nonCritBeds).state

  def getCovidBedRatio(data: List[HospitalData]): Double =
    //Calculates total covid beds in the entire table and divided by the sum of beds across all records (converted to double to retain decimal value after division)
    data.map(_.covidBeds).sum.toDouble / data.map(record => record.beds + record.covidBeds + record.nonCritBeds).sum.toDouble

  def averageAdmissionsByCategory(data: List[HospitalData]): Double = ???

object Runner extends App:
  val dataset = List(
    HospitalData(LocalDateTime.now(), "Johor", 500, 300, 350, 30, 20, 50, 15, 10, 25, 5, 10, 15),
    HospitalData(LocalDateTime.now(), "Johor", 500, 150, 350, 40, 25, 65, 20, 15, 35, 10, 15, 20),
    HospitalData(LocalDateTime.now(), "Kedah", 400, 100, 300, 20, 15, 35, 10, 5, 15, 3, 7, 10),
    HospitalData(LocalDateTime.now(), "Kedah", 400, 100, 300, 25, 20, 45, 12, 8, 20, 4, 9, 12)
  )

  val higheststate = HospitalDataAnalysis.getStateWithHighestBedCount(dataset)
  val ratio = HospitalDataAnalysis.getCovidBedRatio(dataset)

  println(s"$higheststate, \n$ratio")