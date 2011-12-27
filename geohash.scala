//http://en.wikipedia.org/wiki/Geohash
object geohash {
	val LAT_RANGE = (-90.0, 90.0)
	val LON_RANGE = (-180.0, 180.0)

	private def mid(pair:(Double,Double)) = (pair._1+pair._2) / 2
	
	private def decodePart(range:(Double,Double), bin:String) = bin.map(_.toInt - '0'.toInt).foldLeft(range)( (acc,bit) => if (bit == 0) (acc._1, mid(acc)) else (mid(acc), acc._2) )

	import base32._

	private def odd(bits:String) = bits.drop(1).sliding(1,2).toList.reduce(_+_)
	private def even(bits:String) = bits.sliding(1,2).toList.reduce(_+_)

	//decode geohash into (lat, lon) tuple
	def decode(hash:String) = (lat(hash), lon(hash))

	def lat(hash:String) = mid(decodePart(LAT_RANGE, odd(toBits(hash))))

	def lon(hash:String) = mid(decodePart(LON_RANGE, even(toBits(hash))))

	def encode(lat:Double, lon:Double, precision:Int=5) = {
		val remainderBit = precision * 5 % 2
		val latEncoded = encodePart(lat, LAT_RANGE, precision*5 / 2)
		val lonEncoded = encodePart(lon, LON_RANGE, precision*5 / 2 + remainderBit)
		//add extra "0" to lat bits in case lat bits were shorter than long bits
		//then drop 0 or 1 last bit from the "zipped" bits:
		//0 - no need to drop since lat/lon bits were even and the added extra "0" is dropped by zip
		//1 - lat < lon, so drop the added "0"
		val lonBits = lonEncoded.map(_.toString)
		val latBits = (latEncoded + "0").map(_.toString)
		val bits = (lonBits zip latBits).map( x=> x._1 + x._2 ).reduceLeft(_+_).dropRight(remainderBit)
		bits.sliding(5,5).map(toBase32).toList.map(_.toString).reduceLeft(_+_)
	}

	def encodePart(part:Double, range:(Double,Double), i:Int):String = {
    	if (i == 0) ""
    	else {
     		if (part >= mid(range)) 
     			"1" + encodePart(part, (mid(range), range._2), i-1)
     		else 
     			"0" + encodePart(part, (range._1, mid(range)), i-1)
    	}
    }
}

object base32 {
	val BASE32 = ('0' to '9') ++ ("bcdefghjkmnpqrstuvwxyz")
	val BITS = Array(16,8,4,2,1)

	def base32toBin(s:String) = s.map( BASE32.indexOf(_) )
	
	def bin32(i:Int) = BITS.map( c => if ((i & c) > 0) "1" else "0" ).reduceLeft(_+_)

	def toBits(s:String) = base32toBin(s).map(bin32).reduce(_+_)

	//return base32-encoding of a binary string
	def toBase32(bin:String) = BASE32((BITS zip bin).collect{ case(x,'1') => x }.sum)
}
