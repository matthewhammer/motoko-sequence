import Iter "mo:base/Iter";

module {
  public func max<X>(i : Iter.Iter<X>, maxCount : Nat) : Iter.Iter<X> {
    object {
      var c = 0;
      public func next() : ?X {
        if (c < maxCount) {
          c += 1;
          i.next()
        } else { null }
      }
    }
  }
}
