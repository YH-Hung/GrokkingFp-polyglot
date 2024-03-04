import gfp.business.BusinessLogic.{guideScore, travelGuide}
import gfp.model.{Attraction, AttractionOrdering, Location, LocationId, PopCultureSubject, TravelGuide}
import gfp.model.PopCultureSubject.{Artist, Movie}
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.*
import Arbitrary.*
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import gfp.dal.DataAccess
import org.scalatestplus.scalacheck.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class TravelGuideTest extends AnyFunSuite {
  implicit val runtime: IORuntime = IORuntime.global

  test("score of a guide with a description, 0 artists, and 2 popular movies should be 65") {
    val guide = TravelGuide(
      Attraction(
        "Yellowstone National Park",
        Some("first national park in the world"),
        Location(LocationId("Q1214"), "Wyoming", 586107)
      ),
      List(Movie("The Hateful Eight", 155760117), Movie("Heaven's Gate", 3484331))
    )

    // 30 (description) + 0 (0 artists) + 20 (2 movies) + 15 (159 million box office)
    assert(guideScore(guide) == 65)
  }

  test("score of a guide with no description, 0 artists, and 0 movies should be 0") {
    val guide = TravelGuide(
      Attraction(
        "Yellowstone National Park",
        None,
        Location(LocationId("Q1214"), "Wyoming", 586107)
      ),
      List.empty
    )

    // 0 (description) + 0 (0 artists) + 0 (0 movies)
    assert(guideScore(guide) == 0)
  }

  test("score of a guide with no description, 0 artists, and 2 movies with no box office earnings should be 20") {
    val guide = TravelGuide(
      Attraction(
        "Yellowstone National Park",
        None,
        Location(LocationId("Q1214"), "Wyoming", 586107)
      ),
      List(Movie("The Hateful Eight", 0), Movie("Heaven's Gate", 0))
    )

    // 0 (description) + 0 (0 artists) + 20 (2 movies) + 0 (0 million box office)
    assert(guideScore(guide) == 20)
  }

  test("guide score should not depend on its attraction's name and description strings") {
    forAll((name: String, description: String) => {
      val guide = TravelGuide(
        Attraction(
          name, // introduce: empty strings and shorter/longer sizes with different characters
          Some(description),
          Location(LocationId("Q1214"), "Wyoming", 586107)
        ),
        List(Movie("The Hateful Eight", 155760117), Movie("Heaven's Gate", 3484331))
      )

      // 30 (description) + 0 (0 artists) + 20 (2 movies) + 15 (159 million box office)
      assert(guideScore(guide) == 65)
    })
  }

  test("guide score should always be between 30 and 70 if it has a description and some bad movies") {
    forAll((amountOfMovies: Byte) => {
      val guide = TravelGuide(
        Attraction(
          "Yellowstone National Park",
          Some("first national park in the world"),
          Location(LocationId("Q1214"), "Wyoming", 586107)
        ),
        if (amountOfMovies > 0) List.fill(amountOfMovies)(Movie("Random Movie", 0))
        else List.empty
      )
      val score = guideScore(guide)
      // min. 30 (description) and no more than 70 (upper limit with no artists and 0 box office)
      assert(score >= 30 && score <= 70)
    })
  }

  val nonNegativeInt: Gen[Int] = Gen.chooseNum(0, Int.MaxValue)

  test("guide score should always be between 20 and 50 if there is an artist and a movie but no description") {
    forAll(nonNegativeInt, nonNegativeInt)((followers: Int, boxOffice: Int) => {
      val guide = TravelGuide(
        Attraction("Yellowstone National Park", None,
          Location(LocationId("Q1214"), "Wyoming", 586107)
        ),
        List(Artist("Chris LeDoux", followers), Movie("The Hateful Eight", boxOffice))
      )
      val score = guideScore(guide)
      // the score needs to be at least: 20 = 0 (no description) + 10 (1 artist) + 10 (10 movie)
      // but maximum of 50 in a case when there are lots of followers and high box office earnings
      assert(score >= 20 && score <= 50)
    })
  }

  val randomArtist: Gen[Artist] = for {
    name <- Gen.identifier
    followers <- nonNegativeInt
  } yield Artist(name, followers)

  test("guide score should always be between 10 and 25 if there is just a single artist") {
    forAll(randomArtist)((artist: Artist) => {
      val guide = TravelGuide(
        Attraction("Yellowstone National Park", None,
          Location(LocationId("Q1214"), "Wyoming", 586107)),
        List(artist)
      )
      val score = guideScore(guide)
      // no description (0), just a single artist (10) with random number of followers (0-15)
      assert(score >= 10 && score <= 25)
    })
  }

  val randomArtists: Gen[List[Artist]] = for {
    numberOfArtists <- Gen.chooseNum(0, 100)
    artists <- Gen.listOfN(numberOfArtists, randomArtist)
  } yield artists

  test("guide score should always be between 0 and 55 if there is no description and no movies") {
    forAll(randomArtists)((artists: List[Artist]) => {
      val guide = TravelGuide(
        Attraction("Yellowstone National Park", None,
          Location(LocationId("Q1214"), "Wyoming", 586107)),
        artists
      )
      // 40 points if 4 artists or more + 15 if 1_500_000 followers or more
      val score = guideScore(guide)
      assert(score >= 0 && score <= 55)
    })
  }

  val randomMovie: Gen[Movie] = for {
    name <- Gen.identifier
    boxOffice <- nonNegativeInt
  } yield Movie(name, boxOffice)

  val randomMovies: Gen[List[Movie]] = for {
    numberOfMovies <- Gen.chooseNum(0, 100)
    movies <- Gen.listOfN(numberOfMovies, randomMovie)
  } yield movies

  val randomPopCultureSubjects: Gen[List[PopCultureSubject]] = for {
    movies <- randomMovies
    artists <- randomArtists
  } yield movies.appendedAll(artists)

  test("guide score should always be between 0 and 70 if it only contains pop culture subjects") {
    forAll(randomPopCultureSubjects)((popCultureSubjects: List[PopCultureSubject]) => {
      val guide = TravelGuide(
        Attraction("Yellowstone National Park", None,
          Location(LocationId("Q1214"), "Wyoming", 586107)),
        popCultureSubjects
      )
      // min. 0 if the list of pop culture subjects is empty (there is never any description)
      // max. 70 if there are more than four subjects with big followings
      val score = guideScore(guide)
      assert(score >= 0 && score <= 70)
    })
  }

  test("travel guide should include artists originating from the attraction's location") {
    // given an external data source with an attraction named "Tower Bridge"
    // at a location that brought us "Queen"
    val attractionName = "Tower Bridge"
    val london: Location = Location(LocationId("Q84"), "London", 8_908_081)
    val queen: Artist = Artist("Queen", 2_050_559)
    val dataAccess = new DataAccess {
      def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] =
        IO.pure(List(Attraction(attractionName, None, london)))

      def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]] =
        if (locationId == london.id) IO.pure(List(queen)) else IO.pure(List.empty)

      def findMoviesAboutLocation(locationId: LocationId, limit: Int): IO[List[Movie]] = IO.pure(List.empty)
    }

    // when we want to get a travel guide for this attraction
    val guide: Option[TravelGuide] = travelGuide(dataAccess, attractionName).unsafeRunSync()

    // then we get a travel guide with "Queen"
    assert(guide.exists(_.subjects == List(queen)))
  }

  test("travel guide should include movies set in the attraction's location") {
    // given an external data source with an attraction named "Golden Gate Bridge"
    // at a location where "Inside Out" was taking place in
    val attractionName = "Golden Gate Bridge"
    val sanFrancisco: Location = Location(LocationId("Q62"), "San Francisco", 883_963)
    val insideOut: Movie = Movie("Inside Out", 857_611_174)
    val dataAccess = new DataAccess {
      def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] =
        IO.pure(List(Attraction(attractionName, None, sanFrancisco)))

      def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]] = IO.pure(List.empty)

      def findMoviesAboutLocation(locationId: LocationId, limit: Int): IO[List[Movie]] =
        if (locationId == sanFrancisco.id) IO.pure(List(insideOut)) else IO.pure(List.empty)
    }

    // when we want to get a travel guide for this attraction
    val guide: Option[TravelGuide] = travelGuide(dataAccess, attractionName).unsafeRunSync()

    // then we get a travel guide that includes the "Inside Out" movie
    assert(guide.exists(_.subjects == List(insideOut)))
  }


}
