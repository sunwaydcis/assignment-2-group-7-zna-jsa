import scala.io.Source
import scala.util.Using
import scala.collection.mutable.ListBuffer

/**
 *  This branch is the combined efforts taken from Simplifying-The-OOP structure > Fixing2 > Fixing3, and then all of them were compiled onto the main branch.
 *  Meaning all the commits for this file is the total for those respective branches.
 */

/** Why use a HospitalData case class?
 *  It makes the code slightly more understandable and manageable.
 */

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

/** Processes File Hospital.csv ONLY into List[HospitalData] from List[Array[String]].*/
object HospitalCSVReader:
  /** To process the CSVFile into a usable (immutable) List[HospitalData]
   * Why use Using? Auto-closes the entire file once done (Source: https://alvinalexander.com/scala/how-to-open-read-text-files-in-scala-cookbook-examples/)
   *
   * As noticeable, we used a try-catch block and a .getOrElse. Why?
   *
   * Try-Catch Block: covers the critical area of the code, malformed data will be caught and simply skipped.
   * .getOrElse: In the case the file itself cannot be read, we still return an emptyList as the output MUST be a List[HospitalData]
   * This way, we still fulfill the necessary requirements, and the list outputted is an empty one, leading to error handling in the children methods much easier.
   */
  def processFile(source: String): List[HospitalData] =
    Using(Source.fromFile(source)) { source =>
      val records = source.getLines() //Does not turn them into a list yet
      val headerRow = records.next().split(',').map(_.trim).zipWithIndex.toMap

      //ListBuffer for efficiency: Mutable lists allows us to freely append things easily instead of creating a new list each time (Better for larger datasets)
      val recordsBuffer = ListBuffer.empty[HospitalData]

      records.foreach { row =>
        val fields = row.split(',').map(_.trim)
        try
          //Try catch. This is a critical section, if anything fails here, we must catch it so that the code remains running. (What's the issue? When a row has malformed data)
          // Append to ListBuffer
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
        catch //Catch any row with misaligned data and skip it.
          case e: Exception =>
            println(s"Error processing row: $row. Exception: $e")
      }
      recordsBuffer.toList // Convert the ListBuffer to a List for final result
    }.getOrElse {
      // Error Handling - Return empty list if the source just cannot be used
      println(s"Error: Unable to read the file at $source.")
      List.empty
    }


object HospitalDataAnalysis: //Responsible for all DataAnalysis Operations for the Hospital (as presented on Teams)
  /**Finds the state with the highest bed count
   * using data.maxBy(._beds).state is sufficient. (Just run through the list and return the state who has the most beds)
   */
  def highestBedCountState(data: List[HospitalData]) : String =
    if(data.isEmpty) //Prevent operating on an empty list, returns a default value (I think I can use a helper function)
      println(s"Warning: List is empty")
      "Undefined"
    else data.maxBy(_.beds).state

  /**
   * Calculates the overall ratio of covidBeds to normal beds (Source: https://www.baeldung.com/scala/folding-lists)
   * Makes use of foldLeft.
   *
   * Our initial method used .map twice, which is not efficient.
   * Using foldLeft, we could make the function iterate through the list ONCE and also execute an operation to it (accumulate)
   * Everytime the list runs through, it just adds to the accumulator (cumulative) with the current records .beds and .covidBeds values.
   * */
  def overallCovidToBedRatio(data: List[HospitalData]): Double =
    if(data.isEmpty) //Prevent operating on an empty list
      println("Warning: List is Empty")
      0.0
    else
      val (totalBeds, totalCovidBeds) = data.foldLeft((0, 0)) { (cumulative, record) =>
        (cumulative._1 + record.beds, cumulative._2 + record.covidBeds)
      }
      if (totalBeds == 0) //Prevent divide by 0 error
        println("Warning: no beds recorded (HospitalData.beds)")
        0.0
      else totalCovidBeds.toDouble / totalBeds

  /**calculates the average admissions (covid and person under investigation) for each state. (Renamed to ByState)
   *
   * How?
   * Use of GroupMapReduce (Source: ChatGPT)
   * Combines the power of foldLeft in overallCovidRatio and groupBy together resulting in an accumulator function (reduction) for a single state,
   * leading to iterating the list once. Results in collection of type Map containing Types: String and Tuple[Double, Double].
   *
   *  Why not use groupMap directly?
   * Yes it'll be efficient in grouping the data by state, but we still need to tally out the covid and pui admissions, calling for another function,
   * leading to another list iteration, making the code slower.
   */
  def averageAdmissionsByState(data: List[HospitalData]): Map[String, (Double, Double)] =
    if(data.isEmpty) Map("Undefined" -> (0.0, 0.0)) //Prevent operating on an empty list
    else
      data.groupMapReduce(_.state)(record => (record.covidAdmissions.toDouble, record.puiAdmissions.toDouble, 1)){
        case ((covidAdmission1, puiAdmission1, recordCount1), (covidAdmission2, puiAdmission2, countAdmission2)) =>
          (covidAdmission1 + covidAdmission2, puiAdmission1 + puiAdmission2, recordCount1 + countAdmission2)
      }.map { case (state, (totalCovid, totalPui, count)) =>
        state -> (totalCovid / count, totalPui / count)
      }

@main def main(): Unit =
  //  val startRunTime = System.currentTimeMillis()
  //  val startReadTime = System.currentTimeMillis()

  val dataset = HospitalCSVReader.processFile("C:/Users/User/Downloads/hospital.csv")

  //  val endReadTime = System.currentTimeMillis()
  //  val startAverageTime = System.currentTimeMillis()

  val averageAdmissions = HospitalDataAnalysis.averageAdmissionsByState(dataset)

  //  val endAverageTime = System.currentTimeMillis()
  //  val startMaxBedTime = System.currentTimeMillis()

  println(s"State with the highest bed count: ${HospitalDataAnalysis.highestBedCountState(dataset)}")

  val endMaxBedTime = System.currentTimeMillis()
  val startRatioTime = System.currentTimeMillis()

  println(f"State with the highest bed count: ${HospitalDataAnalysis.overallCovidToBedRatio(dataset)}%.2f")

  //  val endRatioTime = System.currentTimeMillis()

  /** Printing the averages of each state
   * Since the outcome is of Collection Map[String, Tuple(Double, Double)], we need to iterate for each element and sub elements.
   * Why use Map, was easier to process in the function compared to a List, can handle more than 1 kind of Type and uses Type String as Key (crucial for groupMapReduce).
   */
  //  val startPrintTime1 = System.currentTimeMillis()

  println("===========================================================================================\n" +
    "Average Admissions for Each State\n" +
    "===========================================================================================\n" +
    f"${"STATE"}%20s \t| AVERAGE COVID ADMISSIONS \t| AVERAGE PUI ADMISSIONS\n"+
    "===========================================================================================")

  //  val endPrintTime1 = System.currentTimeMillis()
  //  val startPrintTime2 = System.currentTimeMillis()

  averageAdmissions.foreach{ //Averages-println block
    case (state, values) =>
      println(f"$state%20s \t| ${values.head}%24.2f \t| ${values(1)}%22.2f\n"
        + "___________________________________________________________________________________________")
  } //yes, using %24.2f is not elegant, but we needed something to use as a padding

//  val endPrintTime2 = System.currentTimeMillis()
//  val endRunTime = System.currentTimeMillis()
//  println(s"${(endRunTime - startRunTime)/1000.0} Run\n${(endReadTime - startReadTime)/1000.0} Read\n${(endAverageTime - startAverageTime)/1000.0} Averages\n${(endMaxBedTime - startMaxBedTime)/1000.0} MaxBed\n${(endRatioTime - startRatioTime)/1000.0} CovidRatio\n${(endPrintTime1 - startPrintTime1)/1000.0} Print1\n${(endPrintTime2 - startPrintTime2)/1000.0} Print2")




