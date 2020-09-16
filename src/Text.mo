import Text "mo:base/Text";
import Seq "Sequence";
import Stream "Stream";

module {
  public type LevelStream = Stream.Stream<Nat32>;
  public type TextSeq = Seq.Sequence<Text>;
  public type TextSeqSeq = Seq.Sequence<TextSeq>;

  public func toText(levels : LevelStream, s : TextSeq) : Text {
    Seq.binaryOp(s, "", Text.concat)
  };

  public func removeSlice(s : TextSeq, pos : Nat, size : Nat) : ?(TextSeq, TextSeq, TextSeq) {
    let (s1, s23) = Seq.split(s, pos + size);
    let (s2, s3) = Seq.split(s23, size);
    ?(s1, s2, s3)
  };

  /// separate text by whitespace, and include all sub-sequences
  /// (whitespace and non-whitespace) 
  /// in the output (a sequence of text sequences).
  public func tokens(s : TextSeq) : TextSeqSeq {
    #empty
    // to do
  };

  /// Operations that use Sequence.append (via a stateful stream of levels)
  public class WithLevels(levels : LevelStream) {
    
    let append = Seq.makeAppend(levels);

    public func flatten(s2 : TextSeqSeq) : TextSeq {
      Seq.monoid( // to do
        s2,
        loop { },
        loop { },
        loop { })
    };

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
