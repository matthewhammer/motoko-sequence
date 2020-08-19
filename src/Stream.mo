import Hash "mo:base/Hash";
import Word32 "mo:base/Word32";
import Iter "mo:base/Iter";
import Nat32 "mo:base/Nat32";
import Debug "mo:base/Debug";

module {
  public type Stream<X> = { next : () -> X };

  /// Transform infinite iterator
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

  /// Stream of numbers drawn from a [Bernoulli_distribution](https://en.wikipedia.org/wiki/Bernoulli_distribution)
  public module Bernoulli {
    public type Value = Nat32;
    public func seedFrom(seed : Nat) : Stream<Value> {
      object {
        func hash() : Word32 {
          Hash.hash(nextNum + 1); // avoid zero (hash is also zero)
        };
        var nextNum : Nat = seed;
        var nextHash : Word32 = hash();
        public func next() : Value {
          let level = Word32.bitcountTrailingZero(nextHash);
          nextNum := nextNum + 1;
          nextHash := hash();
          Nat32.fromNat(Word32.toNat(level));
        };
      }
    }
  };
}
