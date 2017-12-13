import scala.annotation.tailrec

/** Creates a pronunciation tree from training data. */
object PronunTreeFactory
{
   // 2-letter non-words like "ah" and "ur" dominate multi-word search results
   // unless filtered away.
   val allowedShorts = Set("is","or","at","oh","hi","ho","be","to","on","in","of","pi","my","as","so","he","go","me","if","id","up")

   /** Removes particles which would dominate the search result without even being valid English words. */
   def isFitForTree(item : TrainingItem) : Boolean = {
     item.unsplitWord.length > 2 || allowedShorts.contains(item.unsplitWord)
   }

   /** Creates a pronunciation tree. */
   def create(data : TrainingData, segmenter : PronunSegmenter) : PronunTreeNode = {
     val longEnough = data.trainingItems.filter(isFitForTree)
     recCreateTree(PronunTreeNode.empty, longEnough, segmenter)  
   }

   /** Creates a pronunciation tree. */
   @tailrec   
   def recCreateTree(root : PronunTreeNode, data : List[TrainingItem], segmenter : PronunSegmenter) : PronunTreeNode = {
     if (data.isEmpty) {
       root
     } else {
       val nextTree = try {
           val segments = segmenter.segment(data.head.unsplitPronun.replace("_",""))
           root.addItem(data.head, segments)
         } catch {
           case e:IllegalArgumentException => {
             //println("ERROR WITH " + data.head.unsplitPronun)
             root
           }
         }
       recCreateTree(nextTree, data.tail, segmenter)
     }
   }
}
