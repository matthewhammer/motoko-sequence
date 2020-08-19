import Iter "mo:base/Iter";

module {
  public type Stream<X> = { next : () -> X };

  public func fromIter<X>(iter : Iter.Iter<X>) : Stream<X> {
    object {
      public func next() : X {
        switch (iter.next()) {
        case null { assert false; loop { } };
        case (?x) { x }
        }
      }
    }
  };
}
