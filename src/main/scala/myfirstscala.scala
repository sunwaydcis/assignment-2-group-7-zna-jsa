import scala.io.Source
import scala.util.Using
import scala.collection.mutable
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
      val headerRow = records.next().split(',').map(_.trim).zipWithIndex.toMap
      val recordsBuffer = ListBuffer.empty[HospitalData]

      records.foreach { row =>
        val fields = row.split(',').map(_.trim)

        recordsBuffer += HospitalData(
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
      }
      recordsBuffer.toList
    }.getOrElse{
      //Error Handling - Return empty list if the source just cannot be used
      println(s"Error: Unable to read the file at $source.")
      List.empty
    }

//    //Didn't get to explain last commit. Why use Using. Want to close file after automatically finished
//    Using(Source.fromFile(source)) { source =>
//      val records = source.getLines()
//      val headerRow = records.next().split(',').map(_.trim).zipWithIndex.toMap // Will process header accordingly
//      records.map { row =>
//        val fields = row.split(',').map(_.trim)
//        HospitalData(
//          date = fields(headerRow("date")),
//          state = fields(headerRow("state")),
//          beds = fields(headerRow("beds")).toInt,
//          covidBeds = fields(headerRow("beds_covid")).toInt,
//          nonCritBeds = fields(headerRow("beds_noncrit")).toInt,
//          covidAdmissions = fields(headerRow("admitted_covid")).toInt,
//          puiAdmissions = fields(headerRow("admitted_pui")).toInt,
//          totalAdmissions = fields(headerRow("admitted_total")).toInt,
//          puiDischarged = fields(headerRow("discharged_pui")).toInt,
//          covidDischarged = fields(headerRow("discharged_covid")).toInt,
//          totalDischarged = fields(headerRow("discharged_total")).toInt,
//          hospCovid = fields(headerRow("hosp_covid")).toInt,
//          hospPui = fields(headerRow("hosp_pui")).toInt,
//          hospNonCovid = fields(headerRow("hosp_noncovid")).toInt
//        )
//      }.toList
//    }.getOrElse { //Error Handling - Return empty list if the source just cannot be used
//      println(s"Error: Unable to read the file at $source.")
//      List.empty
//    }

object HospitalDataAnalysis: //Responsible for all DataAnalysis Operations for the Hospital
  def calculateStateWithHighestBedCount(data: List[HospitalData]) : Unit =
    if(data.isEmpty) println(s"Warning: List is empty\n${"Undefined"}") //Prevent operating on an empty list
    else println(s"State with largest bed count: ${data.maxBy(_.beds).state}")

  def overallCovidBedRatio(data: List[HospitalData]): Unit =
    if(data.isEmpty) println("Warning: List is Empty\n0.0") //Prevent operating on an empty list
    else
      val (totalBeds, totalCovidBeds) = data.foldLeft((0, 0)) { (cumulative, record) =>
        (cumulative._1 + record.beds, cumulative._2 + record.covidBeds)
      }
      if (totalBeds == 0) println("Warning: no beds recorded (HospitalData.beds). \n0.0") //Prevent divide by 0 error
      else println(s"Average Covid beds to bed ratio overall: ${totalCovidBeds.toDouble / totalBeds}")

  def averageAdmissionsByCategory(data: List[HospitalData]): Map[String, List[Double]] =
    if(data.isEmpty) Map("Undefined" -> List(0.0, 0.0)) //Prevent operating on an empty list
    else
      //Use mutable map. Since each time the map is being updated, using an immutable map means creating a new collection, which is slower.
      //Source - chatGPT
      val cumulator = mutable.Map.empty[String, (Double, Double, Int)]
      data.foreach { record =>
        val state = record.state
        val covidAdmissions = record.covidAdmissions
        val puiAdmissions = record.puiAdmissions

        cumulator.updateWith(state) {
          case Some((covidSum, puiSum, count)) =>
            Some((covidSum + covidAdmissions, puiSum + puiAdmissions, count + 1))
          case None =>
            Some((covidAdmissions, puiAdmissions, 1))
        }
      }
      cumulator.map {
        case (state, (covidSum, puiSum, count)) =>
          state -> List(covidSum / count, puiSum / count)
      }.toMap
//      This is our own.
//      data.groupBy(_.state).map { (state, records) =>
//        val totalSize = records.size
//        state -> List(
//          records.map(_.covidAdmissions).sum.toDouble / totalSize,
//          records.map(_.puiAdmissions).sum.toDouble / totalSize
//      )
//    }

@main def main(): Unit =
  val dataset = HospitalCSVReader.processFile("C:/Users/User/Downloads/hospital.csv")
  val averageAdmissions = HospitalDataAnalysis.averageAdmissionsByCategory(dataset)
  HospitalDataAnalysis.stateWithHighestBedCount(dataset)
  HospitalDataAnalysis.overallCovidBedRatio(dataset)

  println("===========================================================================================\n" +
    "Average Admissions for Each State\n" +
    "===========================================================================================\n" +
    f"${"STATE"}%20s \t| AVERAGE COVID ADMISSIONS \t| AVERAGE PUI ADMISSIONS\n"+
    "===========================================================================================")

  averageAdmissions.foreach{ //Averages-println block
    case (state, values) =>
      println(f"$state%20s \t| ${values.head}%24.2f \t| ${values(1)}%22.2f\n"
        + "___________________________________________________________________________________________")
  }




