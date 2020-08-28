import Iter "mo:base/Iter";
import Debug "mo:base/Debug";

module {
  /// Partially-inspected, finite stream representation.
  ///
  /// Unlike Stream and Iter, represents an _inspected_ stream head/tail.
  public type Cell<X> = ?(X, ?(() -> Cell<X>));

  /// Construct Cell from existing head and tail
  public func cell<X>(head : X, tail : Cell<X>) : Cell<X> =
    ?(head, ?(func () : Cell<X> { tail }));

  /// Construct one-value Cell from head (only)
  public func make<X>(head : X) : Cell<X> = ?(head, null);

  /// Construct Cell from existing data "now" and data "later"
  public func nowLater<X>(now : X, later : () -> Cell<X>) : Cell<X> =
    ?(now, ?later);

  /// Advance a cell's "later" half, getting it "now"
  public func now<X>(later : ?(() -> Cell<X>)) : Cell<X> {
    switch later {
      case null null;
      case (?f) { f() };
    }
  };

  /// Get the later part of the cell, and advance to "now"
  public func laterNow<X>(c : Cell<X>) : Cell<X> {
    switch c {
      case null null;
      case (?c) { now(c.1) }
    }
  };

  /// Cell from (more lazy) Iter type.
  ///
  /// Necessarily, tries to advance iterator, but (initially) just once.
  public func fromIter<X>(i : Iter.Iter<X>) : Cell<X> {
    func nextCell() : Cell<X> {
      switch (i.next()) {
      case null null;
      case (?now) nowLater(now, nextCell);
    }};
    nextCell()
  };

  /// Iter from (less lazy) Cell type.
  public func toIter<X>(c : Cell<X>) : Iter.Iter<X> {
    object {
      var cell = c;
      public func next() : ?X {
        switch cell {
          case null null;
          case (?c) {
                 cell := now(c.1);
                 ?c.0
               };
        }
      };
    }
  };
}
