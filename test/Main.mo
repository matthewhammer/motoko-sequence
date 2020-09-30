import Sequence "../src/Sequence";
import Stream "../src/Stream";

import Sort "../src/Sort";

import TextSeq "../src/Text";

import Search "../src/Search";

import Buffer "mo:base/Buffer";
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Char "mo:base/Char";
import Text "mo:base/Text";
import Iter "mo:base/Iter";

actor {

  public type Sequence<X> = Sequence.Sequence<X>;
  public type Buffer<X> = Buffer.Buffer<X>;

  let append = Sequence.defaultAppend();

  func build<X>(arr : [X]) : (Sequence<X>, Buffer<X>) {
    let b = Buffer.Buffer<X>(0);
    for (x in arr.vals()) {
      b.add(x);
    };
    let levels = Stream.Bernoulli.seedFrom(0);
    let s = Sequence.fromArray(arr, levels);
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

  func equalIter<X>(i : Iter.Iter<X>, j : Iter.Iter<X>,
                    text : X -> Text, equal : (X, X) -> Bool) : Bool {
    Debug.print "test equality:";
    loop {
      let (x, y) = (i.next(), j.next());
      switch (x, y) {
        case (null, null) {
               Debug.print "  EQUAL.";
               return true
             };
        case (?x, ?y) {
               if (not (equal(x, y))) {
                 Debug.print "  NOT equal: distinct vals."; // to do: more info?
                 Debug.print (text x);
                 Debug.print (text y);
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
    equalIter(i, j, Nat.toText, Nat.equal)
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

  func testSort() {
    Debug.print "sequence sort";
    let (s, _) = build([10, 4, 2, 0, 1, 8, 0, 2, 3, 1, 0, 2]);
    let (_, b) = build([0, 0, 0, 1, 1, 2, 2, 2, 3, 4, 8, 10]);
    let sort = Sort.Sort(Nat.toText, Nat.compare);
    assert equalIter(sort.iter(s), b.vals(), Nat.toText, Nat.equal)
  };

  func testTokens() {
    Debug.print "sequence tokens";
    let (s1, _) = build(["A", " ", "c", "a", "t", " ", "a", "n", "d", " ", "h", "a", "t", "."]);
    let (s2, _) = build(["A", "cat", "and", "hat."]);
    let levels = Stream.Bernoulli.seedFrom(0);
    let s12 = Sequence.tokens(s1, func (t : Text) : Bool { t == " " }, levels);
    let s3 = TextSeq.flatten(s12);
    Debug.print (debug_show s1);
    Debug.print (debug_show s2);
    Debug.print (debug_show s12);
    Debug.print (debug_show s3);
    assert equalIter(Sequence.iter<Text>(s2, #fwd), Sequence.iter<Text>(s3, #fwd), func (t: Text) : Text { t }, Text.equal)
  };

  public func selfTest() {
    Debug.print "BEGIN bi-simulation of Sequence versus Buffer modules";
    let (s0, b0) = build([1, 2, 3, 4, 5, 6, 7, 8,
                          9, 10, 11, 12, 13, 14, 15, 16]);
    bisimulationTest(s0, b0);
    testSlice();
    testSort();
    testTokens();
    Debug.print "SUCCESS";
  };

  public func doNextCall() : async Bool {
    false
  };
}
