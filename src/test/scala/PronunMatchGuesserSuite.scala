import org.scalatest.FunSuite

class PronunMatchGuesserSuite extends FunSuite {
  test("it guesses the letter-phoneme mapping based on already splat items") {
    val t1 = new TrainingItem(TrainingDataSuite.toSplitting("kissa"), "kissa", "KISSA", true)
    val t2 = new TrainingItem(TrainingDataSuite.toSplitting("pentu"), "pentu", "PENTU", true)
    val t3 = new TrainingItem(null, "pint", "PINT", false)  // Will be split
    val t4 = new TrainingItem(null, "musti", "MUSTI", false) // Can't be split, since "M" is not in example data.
    
    val data = new TrainingData(List[TrainingItem](t1, t2, t3, t4))
    val _data = new PronunMatchGuesser(data).guess()
    // Assert it dropped the unsplittable "musti"
    assert(_data.trainingItems.length == 3)
    // Assert it splat pint:/PINT/
    assert(_data.trainingItems(2).splitting.toList == TrainingDataSuite.toSplitting("pint").toList)
  }
}
