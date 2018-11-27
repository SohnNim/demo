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
        val filteredData = data.zipWithIndex.filterNot(_._2 % 4 == 0).map(_._1)
        this.copy(data = filteredData)
      }
      case _ => this
    }
  }

  def transData: Work2 = {
    option match {
      case OptionA => {
        val dupData = data.map(alpha => alpha + alpha)
        this.copy(data = dupData)
      }
      case OptionB => {
        val dupWithDelimiterData = data.map(alpha => alpha + ":" + alpha)
        this.copy(data = dupWithDelimiterData)
      }
      case _ => this
    }
  }
}
