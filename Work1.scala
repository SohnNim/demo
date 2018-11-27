package com.sparrow.gittest.works

import com.sparrow.gittest.option.{MyOption, OptionA, OptionB}

case class Work1(data: List[Int], option: MyOption) {

  def filterHalf: Work1 = {
    option match {
      case OptionA => {
        val secondFiltered = data.zipWithIndex.filterNot(_._2 % 4 == 1)
        val lastFiltered = secondFiltered.filterNot(_._2 % 4 == 3)
        val filteredData = lastFiltered.filter(_._2 % 4 == 2).map(_._1)
        this.copy(data = filteredData)
      }
      case OptionB => {
        val thirdFiltered = data.zipWithIndex.filterNot(_._2 % 4 == 2)
        val fourthFilteredData = thirdFiltered.filterNot(_._2 % 4 == 3).map(_._1)
        this.copy(data = fourthFilteredData)
      }
      case _ => this
    }
  }

  def transData: Work1 = {
    option match {
      case OptionA => {
        val oneAddedData = data.map(_ + 1)
        this.copy(data = oneAddedData)
      }
      case OptionB => {
        val threeAddedData = data.map(_ + 3)
        this.copy(data = threeAddedData)
      }
      case _ => this
    }
  }

}
