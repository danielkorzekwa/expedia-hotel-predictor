package expedia.data

import java.util.Date

case class Click(userRegion:Int,userLoc:Int, dist:Double, userId:Int, isPackage:Int,destId:Int, isBooking:Int,continentId:Int,countryId:Int,marketId:Int,
    stayDays:Int,dateTime:Date,checkinDate:Date,cluster:Int) {
  
  
}