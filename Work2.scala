package com.sparrow.gittest.works

import com.sparrow.gittest.option.{MyOption, OptionA, OptionB}

case class Work2(data: List[String], option: MyOption) {
  def filterHalf: Work2 = {
    option match {
      case OptionA => {
        val oddFilteredData = data.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
        this.copy(data = oddFilteredData)
      }
      case OptionB => {
        val evenFilteredData = data.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
        this.copy(data = evenFilteredData)
      }
      case _ => this
    }
  }
}
