import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import gfp.dal.DataAccessRdfImpl
import gfp.model.{Attraction, LocationId}
import gfp.model.PopCultureSubject.{Artist, Movie}
import org.apache.jena.fuseki.main.FusekiServer
import org.apache.jena.query.DatasetFactory
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.riot.RDFDataMgr
import org.scalatest.funsuite.AnyFunSuite
import gfp.model.AttractionOrdering.{ByLocationPopulation, ByName}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class DalTest extends AnyFunSuite{
  implicit val runtime: IORuntime = IORuntime.global

  def localSparqlServer: Resource[IO, FusekiServer] = {
    val start: IO[FusekiServer] = IO.blocking {
      val model = RDFDataMgr.loadModel(getClass.getResource("testdata.ttl").toString)
      val ds = DatasetFactory.create(model)
      val server = FusekiServer.create.add("/test", ds).build
      server.start()
      server
    }

    Resource.make(start)(server => IO.blocking(server.stop()))
  }

  val testServerConnection: Resource[IO, RDFConnection] =
    localSparqlServer.flatMap(ls => DataAccessRdfImpl.connectionResource(ls.serverURL(), "test"))

  test("data access layer should fetch attractions from a real SPARQL server") {
    val result: List[Attraction] = testServerConnection
      .use(connection => {
        // given a real external data source with attractions in Venice
        val dataAccess = DataAccessRdfImpl.getSparqlDataAccess(DataAccessRdfImpl.execQuery(connection))
        // when we use it to find attractions named "Bridge of Sighs"
        dataAccess.findAttractions("Bridge of Sighs", ByLocationPopulation, 5)
      })
      .unsafeRunSync()
    // then we get a list of results with Bridge of Sighs in it
    assert(result.exists(_.name == "Bridge of Sighs") && result.size <= 5)
  }

  test("data access layer should fetch attractions sorted by name") {
    val attractions: List[Attraction] = testServerConnection
      .use(connection => {
        // given a real external data source with national parks in the US
        val dataAccess = DataAccessRdfImpl.getSparqlDataAccess(DataAccessRdfImpl.execQuery(connection))
        // when we use it to find five attractions named "National Park"
        dataAccess.findAttractions("National Park", ByName, 5)
      })
      .unsafeRunSync()
    // then we get a list of five attractions sorted properly by their name
    assert(attractions.size == 5 &&
      attractions.map(_.name) == attractions.sortBy(_.name).map(_.name))
  }

  val veniceId: LocationId = LocationId("Q641")

  test("data access layer should fetch artists from a real SPARQL server") {
    val artists: List[Artist] = testServerConnection
      .use(connection => {
        // given a real external data source with attractions in Venice
        val dataAccess = DataAccessRdfImpl.getSparqlDataAccess(DataAccessRdfImpl.execQuery(connection))
        // when we use it to find an artist from Venice
        dataAccess.findArtistsFromLocation(veniceId, 1)
      })
      .unsafeRunSync()
    // then we get a list of a single artist named "Talco"
    assert(artists.map(_.name) == List("Talco"))
  }

  test("data access layer should fetch movies from a real SPARQL server") {
    val movies: List[Movie] = testServerConnection
      .use(connection => {
        // given a real external data source with attractions in Venice
        val dataAccess = DataAccessRdfImpl.getSparqlDataAccess(DataAccessRdfImpl.execQuery(connection))
        // when we use it to find max two movies set in Venice
        dataAccess.findMoviesAboutLocation(veniceId, 2)
      })
      .unsafeRunSync()
    // then we get a list of a two movies: "Spider-Man: Far from Home" and "Casino Royale"
    assert(movies.map(_.name) == List("Spider-Man: Far from Home", "Casino Royale"))
  }

  val nonNegativeInt: Gen[Int] = Gen.chooseNum(0, Int.MaxValue)

  test("data access layer should accept and relay limit values to a real SPARQL server") {
    forAll(nonNegativeInt)((limit: Int) => {
      val movies: List[Movie] = testServerConnection
        .use(connection => {
          val dataAccess = DataAccessRdfImpl.getSparqlDataAccess(DataAccessRdfImpl.execQuery(connection))
          dataAccess.findMoviesAboutLocation(veniceId, limit)
        })
        .unsafeRunSync()

      assert(movies.size <= limit)
    })
  }
}
