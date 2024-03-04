package gfp.business

import cats.effect.IO
import cats.implicits.*
import gfp.dal.DataAccess
import gfp.model
import gfp.model.AttractionOrdering.ByLocationPopulation
import gfp.model.PopCultureSubject.{Artist, Movie}
import gfp.model.{Attraction, PopCultureSubject, SearchReport, TravelGuide}

object BusinessLogic {

  private def travelGuideForAttraction(dataAccess: DataAccess, attraction: Attraction): IO[TravelGuide] = {
    List(
      dataAccess.findArtistsFromLocation(attraction.location.id, 2),
      dataAccess.findMoviesAboutLocation(attraction.location.id, 2)
    ).parSequence.map(_.flatten).map(subjects => TravelGuide(attraction, subjects))
  }

  private def findGoodGuide(errorsOrGuides: List[Either[Throwable, TravelGuide]]): Either[SearchReport, TravelGuide] = {
    val (errors, guides) = errorsOrGuides.separate
    val errorMessages = errors.map(_.getMessage)

    guides.sortBy(guideScore).reverse.headOption match {
      case Some(bestGuide) =>
        if (guideScore(bestGuide) > 55) Right(bestGuide)
        else Left(SearchReport(guides, errorMessages))
      case None => Left(SearchReport(List.empty, errorMessages))
    }
  }

  def travelGuide(data: DataAccess, attractionName: String): IO[Either[SearchReport, TravelGuide]] = {
    data
      .findAttractions(attractionName, ByLocationPopulation, 3)
      .attempt
      .flatMap(_ match {
        case Left(exception) => IO.pure(Left(SearchReport(List.empty, List(exception.getMessage))))
        case Right(attractions) => attractions
          .map(a => travelGuideForAttraction(data, a))
          .map(_.attempt)
          .parSequence
          .map(findGoodGuide)
      })
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
