import cats.effect.IO
import cats.effect.kernel.Resource
import gfp.business.BusinessLogic
import gfp.dal.DataAccessRdfImpl
import org.apache.jena.rdfconnection.{RDFConnection, RDFConnectionRemote}
import cats.effect.unsafe.IORuntime
import gfp.model.{SearchReport, TravelGuide}

@main
def main(): Unit = {
  // run IO required a implicit IO Runtime
  implicit val runtime: IORuntime = IORuntime.global

  // Or Resource.make with provided release procedure
  val connectionResource: Resource[IO, RDFConnection] = Resource.fromAutoCloseable(
    IO.blocking(
      RDFConnectionRemote.create
        .destination("https://query.wikidata.org/")
        .queryEndpoint("sparql")
        .build
    )
  )

  val program: IO[Either[SearchReport, TravelGuide]] =
    connectionResource
      .map(connection => DataAccessRdfImpl.getSparqlDataAccess(DataAccessRdfImpl.execQuery(connection)))
      .use(dataAccess => BusinessLogic.travelGuide(dataAccess, "Yosemite"))

  program.unsafeRunSync()
}