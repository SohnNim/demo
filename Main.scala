package com.sparrow.gittest

import com.sparrow.gittest.option.{MyOption, NoOpt, OptionA, OptionB}
import com.sparrow.gittest.works.{Work1, Work2}

object Main {
  def main(args: Array[String]): Unit = {
    val option: MyOption = {
      args.headOption.map {
        case "a" => OptionA
        case "b" => OptionB
        case opt => NoOpt(opt)
      }.getOrElse(NoOpt(""))
    }

    val work1 = Work1(List(1, 2, 3, 4, 5), option)
    val work2 = Work2(List("a", "b", "c", "d", "e"), option)
    assert(work1.data.length == work2.data.length)

    val filteredWork1 = work1.filterHalf
    val filteredWork2 = work2.filterHalf
    assert(filteredWork1.data.length == filteredWork2.data.length)

    val transedWork1 = filteredWork1.transData
    val transedWork2 = filteredWork2.transData
    assert(transedWork1.data.length == transedWork2.data.length)
  }
}
