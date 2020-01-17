package hw3

object Main {
  def standardDeviation(vector: List[Double]): Double =
    {
      if(vector.isEmpty) return 0
      val sum = vector.sum
      val avg = sum/vector.length
      val sum2 = vector.foldLeft(0d)((acc,x)=>(acc + (x-avg)*(x-avg)))
      val avg2 = sum2/vector.length
      math.sqrt(avg2)
    }

  def letterFrequencyRanking(corpus: String): String =
    {
      val sortCorp = corpus.filter((c:Char) => c.isLetter && !List('ˇ','´').contains(c)).toLowerCase.toCharArray.sorted
      //there is a bit problem with accents, since they shoudn't be letters, but are counted as one by .isLetter
      //and if I count only a-z then letters like 'ž' are omited completely, so that's why I had to do this stupid roughpatch
      //if you would want only english letters, man, that would be easy since I could just convert to lowercase and then take characters between 'a' and 'z'
      //please, next time think of the czech students... ('_')
      if(sortCorp.length == 0) return ""
      var char = sortCorp(0)
      var count = 1;
      var map = Map[Char,Int]()
      for(i <- 1 until sortCorp.length) {
        if (sortCorp(i) == char) {
          count += 1;
        }
        else {
          map = map + (char -> count)
          char = sortCorp(i)
          count = 1;
        }
      }
      map = map + (char -> count)
      var out : StringBuilder = new StringBuilder("")
      while(map.nonEmpty)
        {
          val c = firstMax(map)
          out = (out += c)
          map = map.removed(c)
        }
      out.mkString
    }
  def firstMax(map: Map[Char,Int]):Char =
  {
    var max:Int = 0
    var char:Char = ' '
    for(i <- map)
    {
      if(i._2>max)
        {
          char = i._1
          max = i._2
        }
    }
    char
  }

  def romanji(katakana: String): String =
    {
      var builder:StringBuilder = new StringBuilder("")
      var init = true
      var lastChar = ' '
      var currChar = ' '
      for(c <- katakana)
        {
          if (init) {
            currChar = c
            init = false
          }
          else {
            lastChar = currChar
            currChar = c
            builder = builder ++= processChars(lastChar, currChar)
          }
        }
        builder = builder ++= processChars(currChar,' ')
      builder.mkString
    }
  //transforms previous character based on current character (in some exceptions it also transforms the current character)
  def processChars(prev: Char, curr: Char):String =
    {
      def ichanger(prev: Char, curr: Char, c: Char): String =
      {
        if (!Katakana.symbols(prev).contains('i') || Katakana.symbols(prev).length != 2) throw new IllegalArgumentException(curr + " must come after i")
        val out = new StringBuilder(Katakana.symbols(prev).mkString)
        ((out.setCharAt(1,'y')) += c).mkString
      }

      curr match{
        case 'ー' =>
        {
          if(Katakana.special.contains(prev) || prev == 'ン') throw new IllegalArgumentException("ー must come after lengthenable character")
          val out = new StringBuilder(Katakana.symbols(prev).mkString)
          out.setCharAt(out.length()-1,Katakana.longVowels(out.last))
          out.mkString
        }
        case 'ャ' => ichanger(prev,curr,'a')
        case 'ュ' => ichanger(prev,curr,'u')
        case 'ョ' => ichanger(prev,curr,'o')
        case _:Char => if (curr == 'ッ' || !Katakana.special.contains(curr))
        {
          if(prev == ' ') return " "
          if(prev == 'ッ')
          {
            if (!Katakana.symbols.contains(curr) || !Katakana.symbols(curr).contains('k')) throw new IllegalArgumentException("ッ must come before 'k' consonant")
            return "k"
          }
          if(Katakana.special.contains(prev)) return ""
          if(!Katakana.symbols.contains(prev)) throw new IllegalArgumentException("wrong '" + prev + "' character! only enter katakana characters and ' '")
          Katakana.symbols(prev).mkString
        } else  ""
      }
    }

  def gray(bits: Int): List[String] =
    {
      if (bits < 0) throw new IllegalArgumentException("must not be negative")
      if (bits == 0) return List()

      var out = List[String]()

      val max = 1 << bits;
      for(n <- 0 until max){
        val i = n ^ (n >> 1)
        out = fullBits(i,bits) :: out
      }
      out.reverse
    }
  def fullBits(int: Int,length:Int): String =
  {
    val bit = int.toBinaryString
    var out : StringBuilder = new StringBuilder("")
    for(i <- bit.length() until length) {
      out = (out+='0')
    }
    out = (out ++= bit)
    out.mkString
  }
}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'),  'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k','a'), 'キ' -> List('k','i'), 'ク' -> List('k','u'), 'ケ' -> List('k','e'), 'コ' -> List('k','o'),
    'ガ' -> List('g','a'), 'ギ' -> List('g','i'), 'グ' -> List('g','u'), 'ゲ' -> List('g','e'), 'ゴ' -> List('g','o'),
    'サ' -> List('s','a'), 'シ' -> List('s','i'), 'ス' -> List('s','u'), 'セ' -> List('s','e'), 'ソ' -> List('s','o'),
    'ザ' -> List('z','a'), 'ジ' -> List('z','i'), 'ズ' -> List('z','u'), 'ゼ' -> List('z','e'), 'ゾ' -> List('z','o'),
    'タ' -> List('t','a'), 'チ' -> List('t','i'), 'ツ' -> List('t','u'), 'テ' -> List('t','e'), 'ト' -> List('t','o'),
    'ダ' -> List('d','a'), 'ヂ' -> List('d','i'), 'ヅ' -> List('d','u'), 'デ' -> List('d','e'), 'ド' -> List('d','o'),
    'ナ' -> List('n','a'), 'ニ' -> List('n','i'), 'ヌ' -> List('n','u'), 'ネ' -> List('n','e'), 'ノ' -> List('n','o'),
    'ハ' -> List('h','a'), 'ヒ' -> List('h','i'), 'フ' -> List('h','u'), 'ヘ' -> List('h','e'), 'ホ' -> List('h','o'),
    'バ' -> List('b','a'), 'ビ' -> List('b','i'), 'ブ' -> List('b','u'), 'ベ' -> List('b','e'), 'ボ' -> List('b','o'),
    'パ' -> List('p','a'), 'ピ' -> List('p','i'), 'プ' -> List('p','u'), 'ペ' -> List('p','e'), 'ポ' -> List('p','o'),
    'マ' -> List('m','a'), 'ミ' -> List('m','i'), 'ム' -> List('m','u'), 'メ' -> List('m','e'), 'モ' -> List('m','o'),
    'ヤ' -> List('y','a'),                        'ユ' -> List('y','u'),                        'ヨ' -> List('y','o'),
    'ラ' -> List('r','a'), 'リ' -> List('r','i'), 'ル' -> List('r','u'), 'レ' -> List('r','e'), 'ロ' -> List('r','o'),
    'ワ' -> List('w','a'), 'ヰ' -> List('w','i'),                        'ヱ' -> List('w','e'), 'ヲ' -> List('w','o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
  val special = List('ー','ャ','ュ','ョ','ッ')
}