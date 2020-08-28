import Order "mo:base/Order";
import Iter "mo:base/Iter";
import Sequence "Sequence";
import Stream "Stream";
import Cell "StreamCell";
import Debug "mo:base/Debug";

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

  public class Sort<X>(toText : X -> Text,
                       compare : (X, X) -> Order.Order) {

    /// compiler-issue: Type error if I inline this definition.
    func cellMake(x : X) : Cell<X> = Cell.make(x);

    /// Create a "merge cell" from two cells
    public func merge(s1 : Cell<X>, s2 : Cell<X>) : Cell<X> {
      switch (s1, s2) {
      case (_, null) s1;
      case (null, _) s2;
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

    /// Create a "sort cell" from a sequence.
    public func sort(s : Seq<X>) : Cell<X> {
      Sequence.monoid<X, Cell<X>>(
        s,
        null,
        cellMake,
        merge
      );
    };

    /// Iterate the sorted sequence.
    public func iter(s : Seq<X>) : Iter<X> {
      object {
        // no sorting until first invocation of next
        var iter : ?Iter<X> = null;
        public func next() : ?X {
          switch iter {
            case null {
                   let i = Cell.toIter(sort(s));
                   iter := ?i;
                   i.next()
                 };
            case (?i) { i.next() };
          }
        }
      }
    };

  };

}
