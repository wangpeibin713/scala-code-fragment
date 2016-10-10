package peibin.wang.nowaterscala

import scala.annotation.tailrec

/**
  * Created by admin on 2016/10/10.
  */
object Mixin {
  type Definitions = Seq[(String, Seq[(String, String)])]
  def existMixin(d: Definitions): Boolean = d.exists(_._2.exists(_._1=="mixin"))
  @tailrec
  def expand(d: Definitions): Definitions = {
    if(!existMixin(d)) d
    else {
      val dMap = d.toMap[String,Seq[(String, String)]]
      val newD : Definitions = d.map(pair => {
        val newValue = pair._2.flatMap(x=> {
          x._1 match {
            case "mixin" => dMap(x._2)
            case _ => Some(x)
          }
        })
        (pair._1, newValue)
      })
      expand(newD)
    }
  }

  def main(args: Array[String]): Unit = {
    val d: Definitions = Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
      "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang"))
    assert(expand(d) == Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang"),
      "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang")))

    val d2: Definitions = Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
      "Address" -> Seq("Province" -> "Shanghai", "mixin" -> "City"), "City"-> Seq("District" -> "Minhang"))
    assert(expand(d2) == Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang"),
      "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang"),"City"-> Seq("District" -> "Minhang")))
  }
}
