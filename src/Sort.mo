import Order "mo:base/Order";
import Iter "mo:base/Iter";
import Sequence "Sequence";
import Stream "Stream";
import Cell "StreamCell";

/// Demand-driven (lazy) sort.
///
/// Input is a sequence, and output is an iterator.
///
/// Sorting only occurs to the extent that the iterator is used.
///
/// Common cases (are all optimal, in terms total comparisons):
///
///  - Demand zero things (edge case): No comparisons.
///  - Demand 1 thing: O(n) comparisons.
///  - Demand 2 things: More comparisons, but still O(n) total.
///  - Demand all things: O(log n) comparisons.
///
module {

  public type Iter<X> = Iter.Iter<X>;
  public type Seq<X> = Sequence.Sequence<X>;
  public type Cell<X> = Cell.Cell<X>;

  public class Sort<X>(compare : (X, X) -> Order.Order) {

    private func merge(s1 : Cell<X>, s2 : Cell<X>) : Cell<X> {
      switch (s1, s2) {
      case (_, null) s2;
      case (null, _) s1;
      case (?c1, ?c2) {
             switch (compare(c1.0, c2.0)) {
               case (#less or #equal) {
                      ?(c1.0, ?(func () : Cell<X> {
                                  merge(
                                    Cell.laterNow(s1),
                                    s2)
                                })
                      )
                    };
               case (#greater) {
                      ?(c2.0, ?(func () : Cell<X> {
                                  merge(
                                    Cell.laterNow(s2),
                                    s1)
                                })
                      )
                    };
             }
           }
      }
    };

    public func iter<X>(s : Seq<X>) : Iter<X> {
      // let sort = foldUp merge ;
      // StreamCell.toIter(sort)
      assert false ; loop { }
    };

  }

}
