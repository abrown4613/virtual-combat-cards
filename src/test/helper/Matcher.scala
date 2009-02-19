//$Id$
package test.helper

/**
 * This create an object with two extractors to be used in matching
 * and converting data form arbitraty sequences
 * @param matcher A matcher partial function, it should return the entry it matches 
 * by the isDefinedAt method, converted to the type of the Matcher
 * 
 */
class Matcher [T](matcher:PartialFunction[Any,T]) {
  
  /**
   * Find and return the first matching pattern, this is
   * done using a for and will return just the first case.
   */
  object findFirst {
    def unapply(s:Seq[Any]):Option[T] = {
      for(x<-s) if(matcher.isDefinedAt(x)) return Some(matcher(x))
      None
    }
  }
  /**
   * Extractor for a sequence of matching elements, 
   * the extrator will return empty sequences if no 
   * match is found
   */
  object findAll {
    def unapplySeq(s:Seq[Any]):Option[Seq[T]]= {
      Some(s.filter(x=>{matcher.isDefinedAt(x)}).map(matcher))
    }
  }
}
