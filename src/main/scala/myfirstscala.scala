import scala.io.Source

trait CSVFileReader[T]:
  /** Reads the CSVFile and converts it into a list of type T.
   *  Why? I thought it was the best approach to take. It demonstrates Parametric Polymorphism
   */
  def processFile(source: String): List[T]
/** Data Model to be operated on
* Why keep all data? - Under the assumption that the DataAnalysis operations MAY increase to other fields.
* Starting to not see a need for it
 */
case class HospitalData( date: String,
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

//Processes File Hospital.csv ONLY.
object HospitalCSVReader extends CSVFileReader[HospitalData]:
  //Will process header accordingly.
  override def processFile(source: String): List[HospitalData] =
    val records = Source.fromFile(source).getLines().toList
    val headerRow = records.head.split(',').map(_.trim)  //Get the header and prepare data parsing //Need buffered lists
    val recordRows = records.tail

    //Parses each line into a data field of the HospitalData
    recordRows.map {
      row => val fields = row.split(',').map(_.trim)
        HospitalData(
          /** Drawbacks: THE COLUMN MUST EXIST. Cons: Not Robust.
           * Use getOrElse()
           */
          // date = LocalDateTime.parse(fields(headerRow.indexOf("date")), DateTimeFormatter.ofPattern("MM-dd-yyyy")),
          date = fields(headerRow.indexOf("date")),
          state = fields(headerRow.indexOf("state")),
          beds = fields(headerRow.indexOf("beds")).toInt,
          covidBeds = fields(headerRow.indexOf("beds_covid")).toInt,
          nonCritBeds = fields(headerRow.indexOf("beds_noncrit")).toInt,
          puiAdmissions = fields(headerRow.indexOf("admitted_pui")).toInt,
          covidAdmissions = fields(headerRow.indexOf("admitted_covid")).toInt,
          totalAdmissions = fields(headerRow.indexOf("admitted_total")).toInt,
          puiDischarged = fields(headerRow.indexOf("discharged_pui")).toInt,
          covidDischarged = fields(headerRow.indexOf("discharged_covid")).toInt,
          totalDischarged = fields(headerRow.indexOf("discharged_total")).toInt,
          hospCovid = fields(headerRow.indexOf("hosp_covid")).toInt,
          hospPui = fields(headerRow.indexOf("hosp_pui")).toInt,
          hospNonCovid = fields(headerRow.indexOf("hosp_noncovid")).toInt
        )
    }

//Responsible for all DataAnalysis Operations for the Hospital
object HospitalDataAnalysis:
  def stateWithHighestBedCount(data: List[HospitalData]) : String =
    data.maxBy(_.beds).state
  /** Calculates total bed count for each record and retrieves the name of the state with the highest total.
   * How:
   * Runs through the list once
   * Returns whichever record has the maximum sum of beds (beds + covid beds + non-critical beds)
   * No try/catch/fail-safe needed
   */
  def covidBedRatio(data: List[HospitalData]): Double =
    val (totalBeds, totalCovidBeds) = data.foldLeft((0, 0)) { (cumulative, record) =>
      (cumulative._1 + record.beds, cumulative._2 + record.covidBeds)
    }
    if (totalBeds == 0) 0.0 //Divide-By-Zero error prevention
    else totalCovidBeds.toDouble / totalBeds
  /** Use of FoldLeft
   * Why? Runs through list once, updates the accumulator tuple.
   */
  def averageAdmissionsByCategory(data: List[HospitalData]): Map[String, List[Double]] =
    data.groupBy(_.state).map { (state, records) =>
      val totalSize = records.size
      state -> List(
        //Rounds to 2 decimal places
        records.map(_.covidAdmissions).sum.toDouble / totalSize,
        records.map(_.puiAdmissions).sum.toDouble / totalSize
      )
    }

//  def averageAdmissionsByCategory2(data: List[HospitalData]): Map[String, List[Double]] =
//    data.groupBy(_.state).foldLeft(0.0, 0.0, 0.0){
//
//    }
/** How?
 * Group by all records by their state name
 * So far, similar to old CovidBedRatio, it uses .map (making it run through the list 4 times)
 * It is easier to manage, but poorer performance (if the record gets any larger)
 */

//object Runner extends App:
@main def main(): Unit =
  //To remove WARNING - made private (as it is only owned by the runner object - not needed but nice)
  val dataset = HospitalCSVReader.processFile("C:/Users/User/Downloads/hospital.csv")

//Why this approach? Wanted to try going for Functional Programming paradigms to improve efficiency and memory usage.
  println(f"State with the highest bed count: ${HospitalDataAnalysis.stateWithHighestBedCount(dataset)}\n" +
    f"Average Covid to Bed Ratio overall: ${HospitalDataAnalysis.covidBedRatio(dataset)}%.2f\n" +
    "===========================================================================================\n" +
    "Average Admissions for Each State\n" +
    "===========================================================================================\n" +
    f"${"STATE"}%20s \t| AVERAGE COVID ADMISSIONS \t| AVERAGE PUI ADMISSIONS\n"+
    "===========================================================================================")

  //Averages block
  HospitalDataAnalysis.averageAdmissionsByCategory(dataset).foreach{
    case (state, values) =>
      println(f"$state%20s \t| ${values.head}%24.2f \t| ${values(1)}%22.2f\n"
      + "___________________________________________________________________________________________")
  }

/* Problems causing poor marks:
- Naming convention
- records.Tail
- A lot of get methods.
- Possibly might need a new data structure.
 */

/*
Parametric Poly - with maps, change Trait based to Map based transformation
Naming Convention change
records.tail
 */

