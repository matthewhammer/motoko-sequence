import Text "mo:base/Text";
import Seq "Sequence";
import Stream "Stream";

module {
  public type LevelStream = Stream.Stream<Nat32>;

  public type TextSeq = Seq.Sequence<Text>;
  public type TextSeqSeq = Seq.Sequence<TextSeq>;

  public type Token = { pos : Nat; text : Text }; // (original raw position, token text)
  public type TokenSeq = Seq.Sequence<Token>;

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
  public func tokens(s : TextSeq, p : Pattern) : TokenSeq {
    tokensRec(s, p, 0)
  };

  func tokensRec(s : TextSeq, p : Pattern, pos : Nat) : TokenSeq {
    switch s {
      case (#empty) #empty;
      case (#leaf(text)) {
             var seq : TokenSeq = Seq.empty();
             var pos_ = pos;
             for (token in Text.tokens(text, p)) {
               // (compiler-question(dfx 6.7): why cannot infer type arg here?)
               seq := Seq.branch<Token>(Seq.make<Token>({pos=pos_; text=token}), 0, seq);
               pos_ += Text.size(token);
             };
             seq
           };
      case (#branch(b)) {
             Seq.branch<Token>(tokensRec(b.left, p, pos),
                               b.level,
                               tokensRec(b.right, p, pos + Seq.size(b.left)))
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
}
