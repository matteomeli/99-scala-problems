import org.scalatest._

import nnsp.P10._

class P10Test extends FlatSpec with Matchers {
  "Function encode" should "return the run-length encoding of a list" in {
    encode((List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
}
