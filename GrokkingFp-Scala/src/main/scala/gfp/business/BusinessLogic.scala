package gfp.business

import cats.effect.IO
import cats.implicits._
import gfp.dal.DataAccess
import gfp.model
import gfp.model.AttractionOrdering.ByLocationPopulation
import gfp.model.PopCultureSubject.{Artist, Movie}
import gfp.model.{PopCultureSubject, TravelGuide}

object BusinessLogic {
  def travelGuide(data: DataAccess, attractionName: String): IO[Option[TravelGuide]] = {
    for {
      attractions <- data.findAttractions(attractionName, ByLocationPopulation, 1)
      guide <- attractions.parTraverse(
        attraction => for {
          artists <- data.findArtistsFromLocation(attraction.location.id, 2)
          movies <- data.findMoviesAboutLocation(attraction.location.id, 2)
        } yield model.TravelGuide(attraction, artists.appendedAll(movies))
      )
    } yield guide.sortBy(guideScore).reverse.headOption
  }

  def guideScore(guide: TravelGuide): Int = {
    val descriptionScore = guide.attraction.description.map(_ => 30).getOrElse(0)
    val quantityScore = Math.min(40, guide.subjects.size * 10)
    val totalFollowers = guide.subjects
      .map {
        case Artist(_, followers) => followers.toLong
        case _ => 0
      }.sum

    val totalBoxOffice = guide.subjects
      .map {
        case Movie(_, boxOffice) => boxOffice.toLong
        case _ => 0
      }.sum

    val followersScore = Math.min(15, totalFollowers / 100_000).toInt
    val boxOfficeScore = Math.min(15, totalBoxOffice / 10_000_000).toInt

    descriptionScore + quantityScore + followersScore + boxOfficeScore
  }
}
