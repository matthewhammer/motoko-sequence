import Sequence "../src/Sequence";
import Stream "../src/Stream";

import Buffer "mo:base/Buffer";
import Debug "mo:base/Debug";

actor {

  /// seed append function with a deterministic Bernoulli distribution
  let append = Sequence.makeAppend<Nat>(Stream.Bernoulli.seedFrom(0));

  public type Sequence<X> = Sequence.Sequence<X>;
  public type Buffer<X> = Buffer.Buffer<X>;

  func build(x : Nat, y : Nat, z : Nat) : (Sequence<Nat>, Buffer<Nat>) {
    let b = Buffer.Buffer<Nat>(0);
    b.add(x);
    b.add(y);
    b.add(z);
    let s = append(
      append(Sequence.make(x), Sequence.make(y)),
      Sequence.make(z)
    );
    (s, b)
  };

  func equal(x : Sequence<Nat>, y : Buffer<Nat>) : Bool {
    Debug.print "test equality:";
    let i = Sequence.iter(x, #fwd);
    let j = y.vals();
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
  };

  public func selfTest() {
    Debug.print "BEGIN bi-simulation of Sequence versus Buffer modules";
    let (s0, b0) = build(1, 2, 3);
    bisimulationTest(s0, b0);
    Debug.print "SUCCESS";
  };

  public func doNextCall() : async Bool {
    false
  };
}
