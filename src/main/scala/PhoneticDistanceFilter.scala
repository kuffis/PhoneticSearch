import scala.collection.immutable.HashMap

/**
 * Finds out n phonetically closest matches to the given search word from the Wikipedia
 * list of English words and pronunciations.
 * @data Vocabulary to be searched.
 * @segmenter Segments pronunciation into units for which we know the phonetic features.
 */
class PhoneticDistanceFilter(
  val data : TrainingData,
  val segmenter : PronunSegmenter
) {
  val wholeTree = PronunTreeFactory.create(data, segmenter)
  val weigher = PhonemeDistanceFactory.create()
  // Pruning threshold. When phonetic distance is greater in some branch, no need to look further.
  var threshold = 2f

  /** Filters the n closest matches by pronunciation distance. */
  def filter(pronun : String, n : Int) : List[(TrainingItem, Float)] = {
    val results = List[PronunTreeNode]()
    val segments = segmenter.segment(pronun)
    val m = new IncrementalEditDistance(weigher, segments)
    
    threshold = 2f
    val result = filter(segments, n, wholeTree, m)
    result
  }
  
  /**
   * Recursively filters the n closest matches by pronunciation distance,
   * improving pruning threshold and abandoning branches as we find better
   * and better matches.
   */
  def filter(segments : List[String], n : Int, tree : PronunTreeNode, m : IncrementalEditDistance) : List[(TrainingItem, Float)] = {
    // baseLine: lists words, where this node is the last phoneme.
    val baseLine = {
      val currentWords = evaluateBaseline(segments, m, tree.outputs)
      if (currentWords.isEmpty)
        emptyBaseline
      else {
        // Searches multi-word matches
        val nextWords = filter(segments, n, wholeTree, m)
        merge(currentWords, concatenate(currentWords, nextWords), n)
      }
    }.filter(_._2 <= threshold)

    if (tree.subnodes.isEmpty) { // No subnodes.
      baseLine
    } else if (m.minDistance() > threshold) { // Subnodes phonetically far, we already have better matches.
      baseLine
    } else {
      // Inner node with several options, prune based on minimal possible distance

      // First search the phonetically closest subtree.
      // Speeds up search as we quickly find matches to lower the pruning threshold.
      val closestSubnodesFirst = tree.subnodes.toArray.map( key => {
        m.push(key._1)
        val minDis = m.minDistance()
        m.pop()
        (key._1, key._2, minDis)
      }).sortBy(_._3)

      // Traverse subtree depth-first.
      closestSubnodesFirst.foldLeft(baseLine) { (result, subnode) => {
        val (phoneme, subtree, distance) = subnode
        m.push(phoneme)
        val subtreeResult = filter(segments, n, subtree, m)
        m.pop()
        merge(result, subtreeResult, n)
      }}
    }
  }
  
  val emptyBaseline = List[(TrainingItem, Float)]()

  /** 
   * Returns the matches, which are phonetically close enough
   * to be solutions or solution prefixes.
   */
  def evaluateBaseline(segments : List[String], m : IncrementalEditDistance, items : List[TrainingItem]) : List[(TrainingItem, Float)] = {
    if (items == null) {
      return emptyBaseline
    }
    val _minDistance = m.minDistance()
    if (_minDistance > threshold) {
      return emptyBaseline
    }

    val distance = m.distance()
    val itemsAndDistances = items.map(item => {
      (item, distance)
    })
    itemsAndDistances
  }

  /**
   * Merges two lists of result candidates.
   * Improves pruning threshold, as we find better and better matches.
   */
  def merge(items1 : List[(TrainingItem, Float)], items2 : List[(TrainingItem, Float)], n : Int) : List[(TrainingItem, Float)] = {
    // More than enough matches? Drop worst matches.
    val newList = (items1 ++ items2)
    val keep =
      if (newList.length < n) newList
      else newList.sortBy(_._2).take(n)

    // Improve pruning threshold.
    if (keep.length == n) {
      threshold = Math.min(threshold, keep(n-1)._2)
    }

    keep
  }

  /** Concatenates two vocabulary items. */
  def join(item1 : TrainingItem, item2 : TrainingItem) : TrainingItem = {
    new TrainingItem(
      null, //item1.splitting     ++      item2.splitting,
      item1.unsplitWord + " " + item2.unsplitWord,
      item1.unsplitPronun + " " + item2.unsplitPronun,
      true
    )
  }

  /**
   * Concatenates results when two concatenated words match better
   * than any single word in the vocabulary.
   */
  def concatenate(firstWords : List[(TrainingItem, Float)], secondWords : List[(TrainingItem, Float)]) : List[(TrainingItem, Float)] = {
    firstWords.map( itemAndDistance1 => {
      secondWords.map( itemAndDistance2 => {
        val concatenatedItem = join(itemAndDistance1._1, itemAndDistance2._1)
        (concatenatedItem, itemAndDistance2._2)
      })
    }).flatten
  }
}
