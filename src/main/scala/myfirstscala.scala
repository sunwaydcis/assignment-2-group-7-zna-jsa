import scala.io.Source
import scala.util.Using
import scala.collection.mutable.ListBuffer

case class HospitalData( date: String, //Much easier to read this way
                         state: String,
                         beds: Int,
                         covidBeds: Int,
                         nonCritBeds: Int,
                         covidAdmissions: Int,
                         puiAdmissions: Int,
                         totalAdmissions: Int,
                         puiDischarged: Int,
                         covidDischarged: Int,
                         totalDischarged: Int,
                         hospPui: Int,
                         hospCovid: Int,
                         hospNonCovid: Int)

object HospitalCSVReader: //Processes File Hospital.csv ONLY.
  def processFile(source: String): List[HospitalData] =
    Using(Source.fromFile(source)) { source =>
      val records = source.getLines()
      val headerRow = records.next().split(',').map(_.trim).zipWithIndex.toMap // Will process header accordingly
      records.map { row =>
        val fields = row.split(',').map(_.trim)
        HospitalData(
          date = fields(headerRow("date")),
          state = fields(headerRow("state")),
          beds = fields(headerRow("beds")).toInt,
          covidBeds = fields(headerRow("beds_covid")).toInt,
          nonCritBeds = fields(headerRow("beds_noncrit")).toInt,
          covidAdmissions = fields(headerRow("admitted_covid")).toInt,
          puiAdmissions = fields(headerRow("admitted_pui")).toInt,
          totalAdmissions = fields(headerRow("admitted_total")).toInt,
          puiDischarged = fields(headerRow("discharged_pui")).toInt,
          covidDischarged = fields(headerRow("discharged_covid")).toInt,
          totalDischarged = fields(headerRow("discharged_total")).toInt,
          hospCovid = fields(headerRow("hosp_covid")).toInt,
          hospPui = fields(headerRow("hosp_pui")).toInt,
          hospNonCovid = fields(headerRow("hosp_noncovid")).toInt
        )
      }.toList
    }.getOrElse {
      println(s"Error: Unable to read the file at $source.")
      List.empty
    }

object HospitalDataAnalysis: //Responsible for all DataAnalysis Operations for the Hospital
  def stateWithHighestBedCount(data: List[HospitalData]) : String =
    if(data.isEmpty)
      println("Warning: List is empty")
      "Undefined"
    else data.maxBy(_.beds).state

  def overallCovidBedRatio(data: List[HospitalData]): Double =
    if(data.isEmpty)
      println("Warning: List is Empty")
      0.0
    val (totalBeds, totalCovidBeds) = data.foldLeft((0, 0)) { (cumulative, record) =>
      (cumulative._1 + record.beds, cumulative._2 + record.covidBeds)
    }
    if (totalBeds == 0)
      println("Warning: no beds recorded (HospitalData.beds). Returning 0.0 as default")
      0.0 //Divide-By-Zero error prevention
    else totalCovidBeds.toDouble / totalBeds

  def averageAdmissionsByCategory(data: List[HospitalData]): Map[String, List[Double]] =
    if(data.isEmpty)
      Map("Undefined" -> List(0.0, 0.0))
    else
      data.groupBy(_.state).map { (state, records) =>
        val totalSize = records.size
        state -> List(
          records.map(_.covidAdmissions).sum.toDouble / totalSize,
          records.map(_.puiAdmissions).sum.toDouble / totalSize
      )
    }

@main def main(): Unit =
  val startTime = System.currentTimeMillis()

  val startReadTime = System.currentTimeMillis()  //For time testing purposes: Start time
  val dataset = HospitalCSVReader.processFile("C:/Users/User/Downloads/hospital.csv")
  val endReadTime = System.currentTimeMillis()

  val startMaxBedTime = System.currentTimeMillis()
  val maxBed = HospitalDataAnalysis.stateWithHighestBedCount(dataset)
  val endMaxBedTime = System.currentTimeMillis()

  val startAverageCovidTime = System.currentTimeMillis()
  val averageCovid = HospitalDataAnalysis.overallCovidBedRatio(dataset)
  val endAverageCovidTime = System.currentTimeMillis()

  val startStateAverageAdmissions = System.currentTimeMillis()
  val averageAdmissions = HospitalDataAnalysis.averageAdmissionsByCategory(dataset)
  val endStateAverageAdmissions = System.currentTimeMillis()

  //Why this approach? Wanted to try going for Functional Programming paradigms.
  val startPrintTime = System.currentTimeMillis()
  println(f"State with the highest bed count: ${HospitalDataAnalysis.stateWithHighestBedCount(dataset)}\n" +
    f"Average Covid to Bed Ratio overall: ${HospitalDataAnalysis.overallCovidBedRatio(dataset)}%.2f\n" +
    "===========================================================================================\n" +
    "Average Admissions for Each State\n" +
    "===========================================================================================\n" +
    f"${"STATE"}%20s \t| AVERAGE COVID ADMISSIONS \t| AVERAGE PUI ADMISSIONS\n"+
    "===========================================================================================")

  HospitalDataAnalysis.averageAdmissionsByCategory(dataset).foreach{ //Averages-println block
    case (state, values) =>
      println(f"$state%20s \t| ${values.head}%24.2f \t| ${values(1)}%22.2f\n"
        + "___________________________________________________________________________________________")
  }
  val endPrint = System.currentTimeMillis()

  val endTime = System.currentTimeMillis() //For time testing purposes: End time
  println(s"Total time taken: ${(endTime - startTime) / 1000.0}") //Checks the time taken in seconds
  println(s"Total time taken: ${(endReadTime - startReadTime) / 1000.0}") //Checks the time taken in seconds
  println(s"Total time taken: ${(endMaxBedTime - startMaxBedTime) / 1000.0}") //Checks the time taken in seconds
  println(s"Total time taken: ${(endAverageCovidTime - startAverageCovidTime) / 1000.0}") //Checks the time taken in seconds
  println(s"Total time taken: ${(endStateAverageAdmissions - startStateAverageAdmissions) / 1000.0}") //Checks the time taken in seconds
  println(s"Total time taken: ${(endPrint - startPrintTime) / 1000.0}") //Checks the time taken in seconds




