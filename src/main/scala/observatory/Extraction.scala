package observatory

import java.time.LocalDate
import Math.round

import org.apache.spark.sql.{DataFrame, Dataset, Row, SparkSession}

import scala.io.Source


/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .master("local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  private def filePath(fileName: String): Dataset[String] = {
    val fileStream = Source.getClass.getResourceAsStream(fileName)
    spark.sparkContext.makeRDD(Source.fromInputStream(fileStream).getLines().toList).toDS
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
   val temperatureDF = spark.read.csv(filePath(temperaturesFile))
   val stationDF = spark.read.csv(filePath(stationsFile))

    val temperature = getTemperature(temperatureDF)
    val station = getStation(stationDF)
    val joined = castItems(temperature, station)


    joined.map(x => (LocalDate.of(year, x.getInt(0), x.getInt(1)),
      Location(x.getDouble(2), x.getDouble(3)), toCelsius(x.getDouble(4))))

  }

  /**
    *
    * @param temperature temperature selected columns
    * @param station  station selected columns
    * @return  casted selected columns
    */
  def castItems(temperature: Dataset[Row], station: Dataset[Row]): Array[Row] = {
    temperature.join(station, "station")
      .where('station.eqNullSafe('station))
      .select('month.cast("Int"), 'day.cast("Int")
        , 'lat.cast("Double"), 'lon.cast("Double")
        , 'temperature.cast("Double")).collect()
  }

  /**
    *
    * @param temperatureDF temperature file in DataFrame format
    * @return selected columns
    */
    def getTemperature(temperatureDF: DataFrame): Dataset[Row] = {
    temperatureDF.select('_c0.as("station"), '_c2.as("month"),
      '_c3.as("day"), '_c4.as("temperature"))
      .where('station.isNotNull && 'month.isNotNull && 'day.isNotNull && 'temperature.isNotNull)
  }

  /**
    *
    * @param stationDF station file in DataFrame format
    * @return selected columns
    */
  def getStation(stationDF: DataFrame): Dataset[Row] = {
    stationDF.select('_c0.as("station"), '_c3.as("lat"), '_c3.as("lon"))
      .where('station.isNotNull && 'lat.isNotNull && 'lon.isNotNull)
  }


  /**
    *
    * @param fahrenheit temperature in fahrenheit
    * @return temperature in celsius
    */
  def toCelsius(fahrenheit: Double): Double = {
    round((fahrenheit - 32) * 5 / 9)
  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
      records.groupBy(_._2).mapValues(x => {
        val temperature = x.map(_._3).seq
        getAverageTemperature(temperature)
      })
  }

  /**
    *
    * @param temperature sequence of temperature
    * @return average temperature
    */
  def getAverageTemperature(temperature: Iterable[Temperature]): Double = temperature.sum / temperature.size

}