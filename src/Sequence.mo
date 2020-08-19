import Trie "mo:base/Trie";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Stream "Stream";

/// Sequences with efficient, balanced representation
module {

  // Sequences with efficient, balanced representation
  //
  // #### Summary:
  //
  // - Pure/persistent representation
  // - O(log n) sequence operations (get element, split, append)
  // - Supports Deque operations in worst-case O(log n) time (base library currently uses a Deque with worst-case linear time)
  //
  // ### Algorithm details:
  //
  // See chunky list representation from [Section 5 of this POPL 1989 paper](https://dl.acm.org/doi/10.1145/75277.75305).
  //

  // Iter<Level> vs Stream<Level>
  //
  // to do -- would be nice to have Stream<Level>, and avoid unwrapping the ? for each level


  public type Sequence<X> = {
    #branch : Branch<X>;
    #leaf : X;
    #empty;
  };

  public type Branch<X> = {
    left : Sequence<X>;
    right : Sequence<X>;
    level : Level;
    size : Nat;
  };

  /// identify positions uniquely
  ///
  /// (generally not sequential numbers)
  public type Pos = Nat;

  /// see POPL paper
  public type Level = Nat32;

  public type Stream<X> = Stream.Stream<X>;

  public func empty<X>() : Sequence<X> {
    #empty
  };

  public func make<X>(data : X) : Sequence<X> {
    #leaf(data)
  };

  public func branch<X>(l : Sequence<X>, midLev : Level, r : Sequence<X>) : Sequence<X> {
    let s = size(l) + size(r);
    #branch({ left = l ; level = midLev ; right = r ; size = s })
  };

  // given an infinite stream of levels, we can append pairs of sequences forever : )
  public func makeAppend<X>(levels : Stream<Level>) : <X>(Sequence<X>, Sequence<X>) -> Sequence<X> {
    func append<X>(s1 : Sequence<X>, s2 : Sequence<X>) : Sequence<X> {
      appendLevel(s1, levels.next(), s2)
    };
    append
  };

  public func appendLevel<X>(s1 : Sequence<X>, midLev : Nat32, s2 : Sequence<X>) : Sequence<X> {
    switch (s1, s2) {
      case (#empty, s2) s2;
      case (s1, #empty) s1;
      case (#leaf(x), #leaf(y)) {
             branch(s1, midLev, s2)
           };
      case (#branch(b), #leaf(x)) {
             if (b.level < midLev) {
               branch(s1, midLev, s2)
             } else {
               branch(b.left, b.level, appendLevel(b.right, midLev, s2))
             }
           };
      case (#leaf(x), #branch(b)) {
             if (midLev > b.level) {
               branch(s1, midLev, s2)
             } else {
               branch(appendLevel(s1, midLev, b.left), b.level, b.right)
             }
           };
      case (#branch(b1), #branch(b2)) {
             if (midLev > b1.level and midLev > b2.level) {
               // midLevel is the max; no further recursion into s1 or s2
               branch(s1, midLev, s2)
             } else {
               // b1 or b2's level is the max level of the three; descend into the "middle child"
               if (b1.level > b2.level) {
                 branch(b1.left,
                        b1.level,
                        appendLevel(b1.right, midLev, s2)
                 )
               } else {
                 branch(appendLevel(s1, midLev, b2.left),
                        b2.level,
                        b2.right
                 )
               }
             };
           };
    }
  };


  public func size<X>(s : Sequence<X>) : Nat {
    switch s {
      case (#empty) 0;
      case (#leaf(x)) 1;
      case (#branch(b)) b.size;
    }
  };

  public func get<X>(s : Sequence<X>, pos : Nat) : ?X {
    switch s {
      case (#empty) null;
      case (#leaf(x)) if (pos == 0) ?x else null;
      case (#branch(b)) {
             if (pos < size(b.left)) {
               get(b.left, pos)
             } else {
               get(b.right, pos - b.size)
             }
           };
    }
  };

  /// split sequence into a pair where the first has the given size
  ///
  /// for insufficient sequence values, the first result is the full input, and the second result is empty
  public func split<X>(s : Sequence<X>, size1 : Nat) : (Sequence<X>, Sequence<X>) {
    if (size1 > size(s)) {
      (s, #empty)
    } else {
      switch s {
        case (#empty) { (#empty, #empty) };
        case (#leaf(x)) { (#leaf(x), #empty) };
        case (#branch(b)) {
               if (size1 == size(b.left)) { // perfect sized match on left
                 (b.left, b.right)

               } else if (size1 < size(b.left)) { // left size is too big; split it
                 let (s1, s2) = split(b.left, size1);
                 let size1Diff = size1 - size(s1);
                 // append, re-using old branch node's level; return extra level (if any)
                 (s1, appendLevel(s2, b.level, b.right))

               } else { // left side too small; split right and append
                 let size1Diff = size1 - size(b.left);
                 let (s1, s2) = split(b.right, size1Diff);
                 (appendLevel(b.left, b.level, s1), s2)
               }
             }
      }
    }
  };

  public func pushBack<X>(seq : Sequence<X>, level : Level, data : X) : Sequence<X> {
    appendLevel(seq, level, make(data))
  };

  public func peekBack<X>(seq : Sequence<X>) : ?X {
    get(seq, size(seq) - 1)
  };

  public func peekFront<X>(seq : Sequence<X>) : ?X {
    get(seq, 0)
  };

  public func pushFront<X>(data : X, level : Level, seq : Sequence<X>) : Sequence<X> {
    appendLevel(make(data), level, seq)
  };


  type IterRep<X> = List.List<Sequence<X>>;
  
  public func iter<X>(s : Sequence<X>, dir : {#fwd; #bwd}) : Iter.Iter<X> {
    object {
      var seqs : IterRep<X> = ?(s, null);
      public func next() : ?X {
        switch (dir, seqs) {
        case (_, null) { null };
        case (_, ?(#empty, ts))        { seqs := ts; next() };
        case (_, ?(#leaf(x), ts))      { seqs := ts; ?x };
        case (#fwd, ?(#branch(b), ts)) { seqs := ?(b.left, ?(b.right, ts)); next() };
        case (#bwd, ?(#branch(b), ts)) { seqs := ?(b.right, ?(b.left, ts)); next() };
        }
      }
    }
  };

}
