import scala.io.Source

////Reading the file. (TESTING)
//val hospitalDataset = Source.fromFile("C:/Users/User/Downloads/hospital.csv").getLines().toList

trait CSVFileReader[T]:
  /** Reads the CSVFile and converts it into a list of type T.
   *  Why? I thought it was the best approach to take. It demonstrates Parametric Polymorphism
   */
  def processFile(source: String): List[T]
  

//Data Model to be operated on
//Why keep all data? - Under the assumption that the DataAnalysis operations MAY increase to other fields.
//Starting to not see a need for it
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
                         hospNonCovid: Int):
  //Get the method to get total beds, removes the need to constantly run (record => record.beds + ...)
  def getTotalBeds: Int = beds + covidBeds + nonCritBeds

//Processes File Hospital.csv ONLY.
object HospitalCSVReader extends CSVFileReader[HospitalData]:
  //Will process header accordingly.
  override def processFile(source: String): List[HospitalData] =
    val records = Source.fromFile(source).getLines().toList
    val headerRow = records.head.split(',').map(_.trim)  //Get the header and prepare data parsing
    val recordRows = records.tail

    //Parses each line into a data field of the HospitalData
    recordRows.map {
      row => val fields = row.split(',').map(_.trim)
        HospitalData(
          //Drawbacks: THE COLUMN MUST EXIST. Cons: Not Robust.
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
  def getStateWithHighestBedCount(data: List[HospitalData]) : String =
    /** Calculates total bed count for each record and retrieves the name of the state with the highest total.
     * How:
     * Runs through the list once
     * Returns whichever record has the maximum sum of beds (beds + covid beds + non-critical beds)
     */
    data.maxBy(record => record.getTotalBeds).state
    //No try/catch/fail-safe needed

  def getCovidBedRatio(data: List[HospitalData]): Double =
    /** Use of FoldLeft
     * Why? Runs through list once, updates the accumulator tuple.
     */
    val (totalBeds, totalCovidBeds) = data.foldLeft((0, 0)) { (cumulative, record) =>
      (cumulative._1 + record.getTotalBeds, cumulative._2 + record.covidBeds)
    }
    if (totalBeds == 0) 0.0 //Divide-By-Zero error prevention
    else totalCovidBeds.toDouble / totalBeds

  def getAverageAdmissionsByCategory(data: List[HospitalData]): Map[String, List[Double]] =
    /** How?
     * Group by all records by their state name
     * So far, similar to old CovidBedRatio, it uses .map (making it run through the list 4 times)
     * It is easier to manage, but poorer performance (if the record gets any larger)
     */
    data.groupBy(_.state).map { (state, records) =>
      state -> List(
        //Rounds to 2 decimal places
        BigDecimal(records.map(_.covidAdmissions).sum.toDouble / records.size).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble,
        BigDecimal(records.map(_.puiAdmissions).sum.toDouble / records.size).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble,
        BigDecimal(records.map(_.totalAdmissions).sum.toDouble / records.size).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      )
    }

object Runner extends App:
  //Test data
//  val dataset = List(
//    HospitalData(LocalDateTime.now(), "Johor", 500, 300, 350, 30, 20, 50, 15, 10, 25, 5, 10, 15),
//    HospitalData(LocalDateTime.now(), "Johor", 500, 150, 350, 40, 25, 65, 20, 15, 35, 10, 15, 20),
//    HospitalData(LocalDateTime.now(), "Kedah", 400, 100, 300, 20, 15, 35, 10, 5, 15, 3, 7, 10),
//    HospitalData(LocalDateTime.now(), "Kedah", 400, 100, 300, 25, 20, 45, 12, 8, 20, 4, 9, 12)
//  )

  //To remove WARNING - made private (as it is only owned by the runner object - not needed but nice)
  private val dataset = HospitalCSVReader.processFile("C:/Users/User/Downloads/hospital.csv")

  //This approach can still be used - Better readability, uses memory
  val highestState = HospitalDataAnalysis.getStateWithHighestBedCount(dataset)
  val ratio = HospitalDataAnalysis.getCovidBedRatio(dataset)
  val averages = HospitalDataAnalysis.getAverageAdmissionsByCategory(dataset)

  //test printing
//  println(s"$highestState, \n$ratio \n$averages")

//Why this approach? Wanted to try going for Functional Programming paradigms to improve efficiency and memory usage.
  println(s"State with the highest bed count: ${HospitalDataAnalysis.getStateWithHighestBedCount(dataset)}\n" +
    s"Average Covid to Bed Ratio overall: ${HospitalDataAnalysis.getCovidBedRatio(dataset)}\n" +
    "===========================================================================================\n" +
    s"Average Admissions for Each State\n" +
    "------------------------------------------------------------------------------------------\n")

  //Averages block
  HospitalDataAnalysis.getAverageAdmissionsByCategory(dataset).foreach{
    case (state, values) =>
      println(s"$state \nAverage Covid: ${values.head} \nAverage Pui: ${values(1)}  \nAverage Total: ${values(2)}\n"
      + "------------------------------------------------------------------------------------------")
  }

//Effective running
//  hospitalDataset.foreach(line => println(line))
