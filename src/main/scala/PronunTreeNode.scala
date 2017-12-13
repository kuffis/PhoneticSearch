import scala.collection.immutable.HashMap;

/**
 * Represents a pronunucation tree, where words which start with the same
 * phonemes are represented by a single path. The path branches only when
 * the pronunciations differ.
 *
 * @param subnodes Mapping from next phonemes to subtrees.
 * @param outputs Words for which this tree node is the last phoneme.
 *
 * Note that the node does not store the phoneme which it represents.
 * The parent node stores the phoneme as a key in subnodes map.
 */
class PronunTreeNode(
    val subnodes : HashMap[String, PronunTreeNode],
    val outputs : List[TrainingItem]
){
    /**
     * Adds a training item to the subtree. Walks down the tree as long
     * as it contains similar pronunciation prefix and branches only when
     * previously unseen pronunciation starts.
     */
    def addItem(item : TrainingItem, segments : List[String]) : PronunTreeNode = {
        if (segments.length == 0) {
            return new PronunTreeNode(subnodes, item :: outputs)
        } else {
            val tailNode =
              if (subnodes.contains(segments(0))) { subnodes(segments(0)) }
              else { PronunTreeNode.empty }
            val newSubnodes = subnodes + (segments(0) -> tailNode.addItem(item, segments.tail))
            return new PronunTreeNode(newSubnodes, outputs)
        }
    }
    
    /**
     * Searches the tree node with the given pronunciation.
     */
    def traverse(segments : List[String]) : List[TrainingItem] = {
      if (segments.isEmpty) { outputs }
      else {
        subnodes(segments.head).traverse(segments.tail)
      }
    }
}

/** Fatory for an empty tree */
object PronunTreeNode
{
    val empty = new PronunTreeNode(HashMap[String, PronunTreeNode](), List[TrainingItem]())
}
