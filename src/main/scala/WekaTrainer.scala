import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import weka.classifiers.trees.J48
import weka.classifiers.trees.HoeffdingTree
import weka.classifiers.bayes.NaiveBayesMultinomialText
import weka.core.Instances
import weka.core.SerializationHelper
import java.io.FileReader
import java.io.BufferedReader
import java.io.PrintWriter
import java.io.File
import java.io.Serializable
import scala.collection.mutable.StringBuilder

/**
 * Trains a machine learning model about English pronunciation.
 * Creates IPA pronunciations for new words based on the machine learning model.
 *
 * The model is based on letter-to-phoneme mapping.
 * For example in the word kissa:/ˈkɪsæ/, the letter-to-phoneme mapping is
 * k -> ˈk   i -> ɪ   ss -> s   a -> æ
 *
 * The classification unit in the model is letter and its surroundings.
 * We surround the word with start and end marks. F.ex. "kissa" would
 * split into the following classification instances:
 * [START,START,k,i] -> ˈk
 * [START,k,i,ss]    -> ɪ
 * [k,i,ss,a]        -> s
 * [i,ss,a,END]      -> æ
 *
 * MAIN LETTER means the letter, which maps to a phoneme.
 * In instance [START,START,k,i] -> ˈk the main letter is k.
 *
 * Since Weka ML models are too big, we generate a different classifier
 * for each main letter and load them into memory only as needed.
 *
 * @param data Training data for training the model.
 */
class WekaTrainer(val data : TrainingData) {
  // Possible letters, longest first
  val letters = data.letters.toArray.sortWith( (a,b) => a.length>b.length )
  val START = "S"
  val END   = "E"

  var anInstance : weka.core.Instance = null
  
  /** Returns the file where the letter's arff file is stored. */
  def arffName(letter : String) = {
    "models/pronunciation." + letter + ".arff"
  }

  /** Returns the file where the letter's trained binary model is stored. */
  def modelName(letter : String) = {
    "models/pronunciation." + letter + ".bin"
  }

  /** Trains the model for a letter, unless it is already up to date. */
  def maybeTrainModel(letter : String) {
    if (anInstance == null || Util.isNewer(arffName(letter), modelName(letter))) {
      val model = trainLetter(letter)
      SerializationHelper.write(modelName(letter), model)
    }
  }
  
  /** Generates the ML model for a main letter. */
  def trainLetter(letter : String) : J48 = {
    println("Training letter " + letter)
    val file = new BufferedReader(new FileReader(arffName(letter)))
    val instances = new Instances(file)
    file.close()
    instances.setClassIndex(instances.numAttributes() - 1);

    val classifier = new J48()
    classifier.buildClassifier(instances)

    anInstance = instances.instance(0)
    return classifier
  }

  /**
   * Split the word into letters, which can map to phonemes.
   * For example "kissa" splits into [k,i,ss,a] because when preprocessing
   * training data we decided that ss is a better phoneme-mapping unit than s,s.
   */ 
  def recSplitWord(word : String) : List[String] = {
    for (letter <- letters) {
      if (word.startsWith(letter)) {
        return letter :: recSplitWord(word.substring(letter.length))
      }
    }
    return List[String]()
  }
 
  /** Nice name for a 4-tuple */
  class InternalInstance(val prevPrev : String, val prev : String, val current : String, val next : String) {
    def join() = { prevPrev + prev + current + next }
  }

  /** Groups the 4-letter instances by the main letter. */ 
  class LettersToInstances(val map : HashMap[String, Set[InternalInstance]]) {
    def addInstance(instance : InternalInstance) : LettersToInstances = {
      val instancesForCurrentLetter = map.get(instance.current).getOrElse(Set[InternalInstance]())
      val newInstanceSet = instancesForCurrentLetter + instance
      val newMap = (map += (instance.current -> newInstanceSet))
      new LettersToInstances(newMap)
    }
  }

  /** 
   * Splits a word into 4-letter instances. Add the instances to the catalog of all instances.
   * @param acc Catalog of all relevant instances, grouped by main letter.
   * @param letters A word split into phoneme-mapping units.
   **/
  def listInstances(acc : LettersToInstances, letters : List[String]) : LettersToInstances = {
    Range(2, letters.length - 1).foldLeft(acc) { (acc, index) => {
      val instance : InternalInstance = new InternalInstance( letters(index - 2), letters(index - 1), letters(index + 0), letters(index + 1) )
      acc.addInstance(instance)
    }}
  }
  
  /** Creates IPA pronunciations for a list of words. */
  def createIpaPronuns(words : List[String]) : List[String] = {
    //println("Creating pronunciations for " + words.length + " words.")

    val splitWords = words.map({ word => 
      recSplitWord(START + START + word + END)
    })

    // Instance means a letter (to be mapped into a phoneme) and its surroundings.
    // For example word "kissa", split into letters as "START|START|k|i|ss|a|END"
    // becomes 4 instances, the first of which is [START|START|k|i] and the last [i|ss|a|END].
    // First we list all instances in all words, grouped by the main letter.
    val emptyModelsToInstances = new LettersToInstances(new HashMap[String, Set[InternalInstance]]())
    val modelsToInstances = splitWords.foldLeft(emptyModelsToInstances) { listInstances }
    
    // Next we map instances like "SSki" or "issaEND" to phonemes.
    val modelsAndSE = modelsToInstances.map.keys // Different ML model for each main chactacter.
    val models = modelsAndSE.filter( model => model != "S" && model != "E" )
    val emptyInstancesToPhonemes = new HashMap[String, String]()
 
    // Contains entries like "SSki -> k" or "issaEND -> ɒ"
    val instancesToPhonemes = 
      models.foldLeft(emptyInstancesToPhonemes) { (instancesToPhonemes, letter) => {
        //println("Model " + letter + " is needed to map " + modelsToInstances.map(letter).size + " instances.")
        maybeTrainModel(letter)
        val classifier : J48 = SerializationHelper.read(modelName(letter)).asInstanceOf[J48]
        val instances = modelsToInstances.map(letter)
        instances.foldLeft(instancesToPhonemes) { (instancesToPhonemes, instance) => {
          anInstance.setValue(0, instance.prevPrev)
          anInstance.setValue(1, instance.prev)
          anInstance.setValue(2, instance.next)
          anInstance.setClassMissing()
          val classification = classifier.classifyInstance(anInstance).toInt
          val phoneme = anInstance.classAttribute.value(classification)
          // Add a pronunciation mapping like "issaEND -> ɒ"
          val instanceLetters = instance.join()
          instancesToPhonemes += (instanceLetters -> phoneme) 
        }}
      }}
    
    // Creates pronunciation for each word.
    val pronunciations = splitWords.map(word => {
      // Finds a phoneme for each instance (letter & surroundings) in the word.
      val phonemeList = 
        Range(2, word.length-1).map( index => {
          val letters = word(index-2) + word(index-1) + word(index) + word(index + 1)
          instancesToPhonemes(letters)
        })
      Util.join(phonemeList, "")
    })
    return pronunciations
  }

  /** Creates IPA pronunciation for a single word. */
  def createIpaPronun(word : String) : String = {
    createIpaPronuns(List[String](word))(0)
  }

  /** Writes the training data to Weka training files. */
  def writeToArff() {
    data.letters.map(writeLetterToArffIfNeeded)
  }

  /**
   * Writes the Weka training file (ARFF file)
   * if source data has changed or the file hasn't been generated yet.
   */
  def writeLetterToArffIfNeeded(letter : String) {
    if (Util.isNewer("models/ipa.split.txt", arffName(letter))) {
      writeLetterToArff(letter)
    }
  }

  /**
   * Writes the Weka training file for a single main letter.
   */
  def writeLetterToArff(letter : String) {
    val arff = new PrintWriter(new File(arffName(letter)))
    arff.println("@RELATION pronunciation")
    arff.println("")

    val splitTrainingItems = data.trainingItems.filter(_.isSplit)
    val trainingLines = 
      splitTrainingItems.map( trainingItem => {
        val SSsplitting = (START,START) :: (START,START) :: trainingItem.splitting.toList
        Range(2, SSsplitting.length).map(i => {
          val thisChar = SSsplitting(i)._1
          if (thisChar == letter) {
            val prevPrevChar = SSsplitting(i-2)._1
            val prevChar = SSsplitting(i-1)._1
            val nextChar = if (i+1 >= SSsplitting.length) END else SSsplitting(i+1)._1
            val _classPho = SSsplitting(i)._2
            val classifiedPhoneme = (if (_classPho == "") "_" else _classPho)

            val trainingLine = prevPrevChar + "," + prevChar + "," + nextChar + "," + classifiedPhoneme + " % " + trainingItem.unsplitWord
            Some(trainingLine)
          } else
            None
        }).flatten
      }).flatten

    val letterCategory = "{" + Util.join(data.letters, ",") + "}"
    arff.println("@ATTRIBUTE prevprevletter " + letterCategory)
    arff.println("@ATTRIBUTE prevletter " + letterCategory)
    //arff.println("@ATTRIBUTE thisletter " + letterCategory)
    arff.println("@ATTRIBUTE nextletter " + letterCategory)

    val phonemesWithEmptyAs_ = data.phonemes.map(a => if (a == "") "_" else a)
    val phonemeCategory = "{" + Util.join(phonemesWithEmptyAs_, ",") + "}"
    arff.println("@ATTRIBUTE class      " + phonemeCategory)
    arff.println("")

    arff.println("@DATA")
    for (trainingLine <- trainingLines) {
      arff.println(trainingLine)
    }

    arff.close()
  }
}
