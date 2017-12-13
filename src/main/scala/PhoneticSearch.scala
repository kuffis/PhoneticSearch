import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import weka.core.Instances
import weka.core.SerializationHelper
import java.io.FileReader
import java.io.BufferedReader
import java.io.PrintWriter
import java.io.File

/**
 * Offers interactive command-line interface for searching phonetically similar words.
 */
object PhoneticSearchApp {

  def loadTrainingItems(manualDataFile : String, enrichedDataFile : String) : TrainingData = {
    if (Util.isNewer(manualDataFile, enrichedDataFile)) {
        // To be useful ML training data, the (word, pronunciation) pairs must match
        // individual letters to phonemes. Since manually annotating 44000 words is too much work,
        // we enrich the data by guessing most likely letter-to-phoneme matchings in the training data. 
        val trainingItems = TrainingItemReader.read(manualDataFile)
        val plainData = new TrainingData(trainingItems)

        println("Enriching training data (this might take a minute) ...")
        val enrichedData = Util.time{ new PronunMatchGuesser(plainData).guess() }
        println(" enriched.")

        val splitPercentage = 100.0 * enrichedData.trainingItems.length / trainingItems.length
        println("Split percentage: " + splitPercentage)
        println("All items:" + trainingItems.length)
        enrichedData.write(enrichedDataFile)
        return enrichedData
    } else {
        val items = TrainingItemReader.read(enrichedDataFile)
        return new TrainingData(items)
    }
  }

  def isSearchable(word : String) : Boolean = {
    val regex = "[a-z ]+".r
    return (regex.replaceAllIn(word, "").isEmpty)
  }

  def main(args: Array[String]) {
    print("Loading data ... ")
    val trainingData = loadTrainingItems("data/ipa.txt", "models/ipa.split.txt")
    println("loaded.")

    if (args.length > 0 && args(0) == "test") {
      new TrainingParamTester(trainingData).test()
      return
    }

    val trainer = new WekaTrainer(trainingData)
    trainer.writeToArff()

    if (args.length > 0) {
      val word = args(0).toLowerCase
      if (!isSearchable(word)) {
        println("Please search words only contains letters a-z.")
        return
      }

      print("Creating pronunciation for " + args(0) + " ... ")
      val pronun = trainer.createIpaPronun(args(0)).replace("_","")
      println(" created.")
      println(args(0) + ":/" + pronun + "/")

      
      val segmenter = PronunSegmenterFactory.create()
      val filterer = Util.time{ (new PhoneticDistanceFilter(trainingData, segmenter)) }

      println("Searching phonetically similar words ...")
      val result = Util.time{ filterer.filter(pronun, 15) }
      for (r <- result) {
        println("Word: " + r._1.unsplitWord + ", pronun:/" + r._1.unsplitPronun + "/, distance:" + r._2)
      }
    }
  }
}
