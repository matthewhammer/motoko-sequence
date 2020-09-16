import Text "mo:base/Text";
import Seq "Sequence";
import Stream "Stream";

module {
  public type LevelStream = Stream.Stream<Nat32>;
  public type TextSeq = Seq.Sequence<Text>;
  public type TextSeqSeq = Seq.Sequence<TextSeq>;

  public type Pattern = Text.Pattern;

  public func toText(s : TextSeq) : Text {
    Seq.binaryOp(s, "", Text.concat)
  };

  public func flatten(s2 : TextSeqSeq) : TextSeq {
    Seq.foldUp<TextSeq, TextSeq>(
      s2,
      Seq.empty<Text>(),
      func (t : TextSeq) : TextSeq { #leaf(toText(t)) },
      func (b, l, r) : TextSeq { Seq.branch(l, b.level, r) }
    )
  };

  public func removeSlice(s : TextSeq, pos : Nat, size : Nat) : ?(TextSeq, TextSeq, TextSeq) {
    let (s1, s23) = Seq.split(s, pos + size);
    let (s2, s3) = Seq.split(s23, size);
    ?(s1, s2, s3)
  };

  /// separate text by whitespace, and include all sub-sequences
  /// (whitespace and non-whitespace)
  /// in the output (a sequence of text sequences).
  public func tokens(s : TextSeq, p : Pattern) : TextSeqSeq {
    switch s {
      case (#empty) #empty;
      case (#leaf(text)) {
             var seq : TextSeq = Seq.empty();
             var siz = 0;
             for (token in Text.tokens(text, p)) {
               siz += 1;
               // (compiler-question(dfx 6.7): why cannot infer type arg here?)
               seq := Seq.branch<Text>(Seq.empty() : TextSeq, 0, seq);
             };
             #leaf(seq)
           };
      case (#branch(b)) {
             Seq.branch<TextSeq>(tokens(b.left, p),
                                 b.level,
                                 tokens(b.right, p))
           };
    }
  };

  // optimized variant fuses flatten and tokens into one recursive tree walk.
  // (same as tokens(), except in #leaf case, we do not introduce another level of nested sequence.)
  public func flatTokens(s : TextSeq, p : Pattern) : TextSeq {
    switch s {
      case (#empty) #empty;
      case (#leaf(text)) {
             var seq : TextSeq = Seq.empty();
             var siz = 0;
             for (token in Text.tokens(text, p)) {
               siz += 1;
               seq := Seq.branch<Text>(Seq.empty() : TextSeq, 0, seq);
             };
             seq
           };
      case (#branch(b)) {
             Seq.branch<Text>(flatTokens(b.left, p),
                              b.level,
                              flatTokens(b.right, p))
           };
    }
  };

  /// Operations that use Sequence.append (via a stateful stream of levels)
  public class WithLevels(levels : LevelStream) {

    let append = Seq.makeAppend(levels);


    public func putSlice(s1 : TextSeq, pos : Nat, s2 : TextSeq) : TextSeq {
      // to do
      #empty
    };

    public func deleteSlice(s : TextSeq, pos : Nat, size : Nat) : TextSeq {
      switch (removeSlice(s, pos, size)) {
      case null s;
      case (?(s1, _, s3)) append(s1, s3);
      };
    };

  };

  // to do:
  // slice (insertSlice, removeSlice)
  // tokens
  // flatten
}
