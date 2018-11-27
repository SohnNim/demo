package com.sparrow.gittest.works

import com.sparrow.gittest.option.{MyOption, OptionA, OptionB}

case class Work2(data: List[String], option: MyOption) {
  def filterHalf: Work2 = {
    option match {
      case OptionA => {
        val secondFiltered = data.zipWithIndex.filterNot(_._2 % 4 == 1)
        val filteredData = secondFiltered.filter(_._2 % 4 == 2).map(_._1)
        this.copy(data = filteredData)
      }
      case OptionB => {
        val evenFilteredData = data.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
        this.copy(data = evenFilteredData)
      }
      case _ => this
    }
  }

  def transData: Work2 = {
    option match {
      case OptionA => {
        val multiplyDupData = data.map(alpha => alpha + alpha + alpha + alpha)
        this.copy(data = multiplyDupData)
      }
      case OptionB => {
        val dupWithDelimiterData = data.map(alpha => alpha + ":" + alpha)
        this.copy(data = dupWithDelimiterData)
      }
      case _ => this
    }
  }
}
