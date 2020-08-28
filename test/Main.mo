import Sequence "../src/Sequence";
import Stream "../src/Stream";

import Sort "../src/Sort";

import Buffer "mo:base/Buffer";
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";

actor {

  public type Sequence<X> = Sequence.Sequence<X>;
  public type Buffer<X> = Buffer.Buffer<X>;

  let append = Sequence.defaultAppend();

  func build(nats : [Nat]) : (Sequence<Nat>, Buffer<Nat>) {
    let b = Buffer.Buffer<Nat>(0);
    for (n in nats.vals()) {
      b.add(n);
    };
    let levels = Stream.Bernoulli.seedFrom(0);
    let s = Sequence.fromArray(nats, levels);
    (s, b)
  };

  func sum(x : Sequence<Nat>) : Nat {
    Sequence.binaryOp(x, 0, Nat.add);
  };

  func min(x : Sequence<Nat>) : Nat {
    Sequence.binaryOp(x, 0, Nat.min);
  };

  /// Returns the maximum value, or null if:
  /// - The sequence is empty.
  //  - The sequence contains a null value.
  ///
  /// E.g., Consider `?Nat` to be encoding `Nat` with special element (`null`), representing "bottom".
  func max(x : Sequence<?Nat>) : ?Nat {
    Sequence.monoid(
      x,
      null,
      func(x : ?Nat) : ?Nat { x },
      func (x : ?Nat, y : ?Nat) : ?Nat {
        switch (x, y) {
        case (null, _) null;
        case (_, null) null;
        case (?x, ?y) ?Nat.max(x, y)
        }
      }
    )
  };

  func equalIter(i : Iter.Iter<Nat>, j : Iter.Iter<Nat>) : Bool {
    Debug.print "test equality:";
    loop {
      switch (i.next(), j.next()) {
        case (null, null) {
               Debug.print "  EQUAL.";
               return true
             };
        case (?x, ?y) {
               if (x != y) {
                 Debug.print "  NOT equal: distinct vals."; // to do: more info?
                 return false;
               };
             };
        case (?_, _) {
               Debug.print "  NOT equal: first too long, or second too short";
               return false
             };
        case (_, ?_) {
               Debug.print "  NOT equal: first too short, or second too long";
               return false
             };
      }
    }
  };

  func equal(x : Sequence<Nat>, y : Buffer<Nat>) : Bool {
    let i = Sequence.iter(x, #fwd);
    let j = y.vals();
    equalIter(i, j)
  };

  func bisimulationTest(x : Sequence<Nat>, y : Buffer<Nat>) {
    assert equal(x, y);

    Debug.print "sequence append";
    let xx = append(x, x);
    Debug.print "buffer append";
    y.append(y.clone());
    assert equal(xx, y);

    Debug.print "sequence append";
    let x4 = append(xx, xx);
    Debug.print "buffer append";
    y.append(y.clone());
    assert equal(x4, y);

    Debug.print "DUMP sequence and array";
    Debug.print (debug_show x4);
    Debug.print (debug_show y.toArray());
  };

  func testSlice() {
    Debug.print "sequence slice";
    let (s, _) = build([1, 2, 3, 4, 5, 6, 7, 8,
                        9, 10, 11, 12, 13, 14, 15, 16]);
    let (s1, s2, s3) = Sequence.slice(s, 5, 5);
    let (_, b2) = build([6, 7, 8, 9, 10]);
    assert equal(s2, b2);
  };

  func testSort(x : Sequence<Nat>, y : Buffer<Nat>) {
    Debug.print "sequence sort";
    let (s, _) = build([15, 16, 9, 10, 10,
                        1, 2, 3, 3, 4, 5,
                        0, 1, 6, 7, 8, 6,
                        11, 12, 0, 13, 1, 2,
                        14, 15, 16, 1, 2, 4]);
    let (_, b) = build([
                         0,
                         0,
                         1,
                         1,
                         1,
                         1,
                         2,
                         2,
                         2,
                         3,
                         3,
                         4,
                         4,
                         5,
                         6,
                         6,
                         7,
                         8,
                         9,
                         10,
                         10,
                         11,
                         12,
                         13,
                         14,
                         15,
                         15,
                         16,
                         16,
                       ]);
    let sort = Sort.Sort(Nat.compare);
    assert equalIter(sort.iter(s), b.vals())
  };

  public func selfTest() {
    Debug.print "BEGIN bi-simulation of Sequence versus Buffer modules";
    let (s0, b0) = build([1, 2, 3, 4, 5, 6, 7, 8,
                          9, 10, 11, 12, 13, 14, 15, 16]);
    bisimulationTest(s0, b0);
    testSlice();
    Debug.print "SUCCESS";
  };

  public func doNextCall() : async Bool {
    false
  };
}
