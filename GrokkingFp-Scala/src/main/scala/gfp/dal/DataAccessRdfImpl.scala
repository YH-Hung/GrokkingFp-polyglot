package gfp.dal

import cats.effect.{IO, Ref, Resource}
import cats.implicits.*
import gfp.model.AttractionOrdering.{ByLocationPopulation, ByName}
import gfp.model.*
import gfp.model.PopCultureSubject.{Artist, Movie}
import org.apache.jena.query.{QueryFactory, QuerySolution}
import org.apache.jena.rdfconnection.{RDFConnection, RDFConnectionRemote}

import scala.jdk.CollectionConverters.*
import scala.util.chaining.scalaUtilChainingOps

object DataAccessRdfImpl {
  private val prefixes =
    """
      |PREFIX wd: <http://www.wikidata.org/entity/>
      |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX schema: <http://schema.org/>
      |""".stripMargin

  def connectionResource(address: String, endpoint: String): Resource[IO, RDFConnection] = Resource.make(
    IO.blocking(
      RDFConnectionRemote.create
        .destination(address)
        .queryEndpoint(endpoint)
        .build
    )
  )(connection => IO.blocking(connection.close()))
  
  def execQuery(connection: RDFConnection)(query: String): IO[List[QuerySolution]] = {
    query
      .pipe(QueryFactory.create)
      .pipe(connection.query)
      .execSelect()
      .asScala.toList // scala.Collection.convert.asScala was deprecated. Use scala.jdk.CollectionConverter instead.
      .pipe(IO.blocking)
  }

  def cachedExecQuery(connection: RDFConnection, cache: Ref[IO, Map[String, List[QuerySolution]]])(query: String): IO[List[QuerySolution]] = {
    for {
      cachedQueries <- cache.get
      solutions <- cachedQueries.get(query) match
        case Some(cachedSolutions) => IO.pure(cachedSolutions)
        case None => for {
          realSolutions <- execQuery(connection)(query)
          _ <- cache.update(_.updated(query, realSolutions))
        } yield realSolutions
    } yield solutions
  }

  def getSparqlDataAccess(execQuery: String => IO[List[QuerySolution]]): DataAccess =
    new DataAccess:
      override def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[PopCultureSubject.Artist]] = {
        val query =
          s"""
             |$prefixes
             |SELECT DISTINCT ?artist ?artistLabel ?followers WHERE {
             |  ?artist wdt:P136 ?genre;
             |          wdt:P8687 ?followers;
             |          rdfs:label ?artistLabel.
             |  FILTER(LANG(?artistLabel) = "en").
             |
             |  ?artist wdt:P740 wd:${locationId.value}
             |
             |} ORDER BY DESC(?followers) LIMIT $limit
             |""".stripMargin

        for {
          solutions <- execQuery(query)
          artists <- IO.delay(
            solutions.map[Artist](s =>
              Artist(name = s.getLiteral("artistLabel").getString, followers = s.getLiteral("followers").getInt))
          )
        } yield artists
      }

      override def findMoviesAboutLocation(locationId: LocationId, limit: Int): IO[List[PopCultureSubject.Movie]] = {
        val query =
          s"""
             |$prefixes
             |SELECT DISTINCT ?subject ?subjectLabel ?boxOffice WHERE {
             |  ?subject wdt:P31 wd:Q11424;
             |           wdt:P2142 ?boxOffice;
             |           rdfs:label ?subjectLabel.
             |
             |  ?subject wdt:P840 wd:${locationId.value}
             |
             |  FILTER(LANG(?subjectLabel) = "en").
             |
             |} ORDER BY DESC(?boxOffice) LIMIT $limit
             |""".stripMargin

        for {
          solutions <- execQuery(query)
          movies <- IO.delay(
            solutions.map[Movie](s =>
              Movie(name = s.getLiteral("subjectLabel").getString, boxOffice = s.getLiteral("boxOffice").getInt)
            )
          )
        } yield movies
      }

      override def findAttractions(name: _root_.java.lang.String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
        val orderBy = ordering match
          case ByName => "?attractionLabel"
          case ByLocationPopulation => "?DESC(?population)"

        val query =
          s"""
             |$prefixes
             |SELECT DISTINCT ?attraction ?attractionLabel ?description ?location ?locationLabel ?population WHERE {
             |  ?attraction wdt:P31 wd:Q570116;
             |              rdfs:label ?attractionLabel;
             |              wdt:P131 ?location.
             |  FILTER(LANG(?attractionLabel) = "en").
             |
             |  OPTIONAL {
             |    ?attraction schema:description ?description.
             |    FILTER(LANG(?description) = "en").
             |  }
             |
             |  ?location wdt:P1082 ?population;
             |            rdfs:label ?locationLabel;
             |  FILTER(LANG(?locationLabel) = "en").
             |
             |  FILTER(CONTAINS(?attractionLabel, "$name")).
             |} ORDER BY $orderBy LIMIT $limit""".stripMargin

        for {
          solutions <- execQuery(query)
          attractions <- solutions.traverse(parseAttraction)
        } yield attractions
      }

      private def parseAttraction(s: QuerySolution): IO[Attraction] = {
        IO.delay(
          Attraction(
            name = s.getLiteral("attractionLabel").getString,
            description =
              if (s.contains("description"))
                Some(s.getLiteral("description").getString)
              else
                None,
            location = Location(
              id = LocationId(s.getResource("location").getLocalName),
              name = s.getLiteral("locationLabel").getString,
              population = s.getLiteral("population").getInt
            )
          )
        )
      }
}
