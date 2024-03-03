package gfp.dal

import cats.effect.IO
import gfp.model.PopCultureSubject.{Artist, Movie}
import gfp.model.{Attraction, AttractionOrdering, LocationId}

trait DataAccess {
  def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]]
  def findMoviesAboutLocation(locationId: LocationId, limit: Int): IO[List[Movie]]
  def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]]
}

