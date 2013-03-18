package spray.metrics

import spray.json._
import spray.http.HttpBody
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.http.MediaTypes._
import spray.routing.directives._
import spray.routing._
import spray.http._
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import spray.json._
 
import scala.concurrent.ExecutionContext.Implicits._
import scala.collection.JavaConverters._
 
import java.util.concurrent._
import com.yammer.metrics._
 
case class MetricsSummary(label:String, timers: Map[String, String], counters: Map[String, String])
 
// Container for metrics, currently only timers and counters
class MetricsContainer(s: String) {
 
  import BasicDirectives._
   
  val timers: ConcurrentMap[String, core.Histogram] = new ConcurrentHashMap[String, core.Histogram]
  val counters: ConcurrentMap[String, core.Meter] = new ConcurrentHashMap[String, core.Meter]
   
  def getHist(label: String):core.Histogram = timers.get(label)
  def getMeter(label: String):core.Meter = counters.get(label)
   
  def meter(label: String) {
    counters.putIfAbsent(label, Metrics.newMeter(new core.MetricName("org","testing","metrics"), label, java.util.concurrent.TimeUnit.SECONDS))
    getMeter(label).mark
  }
   
  def meterAndGet(label: String) = {
    meter(label)
    getMeter(label)
  }
   
  // Directives
  def count(label: String): Directive0 =
    mapRequest { request => meter(label); request }
   
  def time(label: String): Directive0 =
    mapRequestContext { ctx =>
      timers.putIfAbsent(label, Metrics.newHistogram(new core.MetricName("a","b","c"), true))
      val startTime = System.nanoTime
      ctx.mapRouteResponse { response => getHist(label).update((System.nanoTime - startTime) / 1000) ; response }
    }
   
  def showAsJson: MetricsSummary =
    MetricsSummary(s, timers.entrySet().asScala.map{k => (k.getKey(), k.getValue().mean.toString())}.toMap,
    counters.entrySet().asScala.map{k => (k.getKey(), k.getValue().count.toString())}.toMap)
}

// Factory for MetricsContainer:s and container for marshallers
object SampleMetrics extends DefaultJsonProtocol {
 
  implicit val metricsSummaryFormat = jsonFormat3(MetricsSummary)
   
  implicit val MetricsContainerMarshaller =
    Marshaller.of[MetricsSummary](`application/json`) { (value, contentType, ctx) =>
    ctx.marshalTo(HttpBody(contentType, value.toJson.prettyPrint))
  }
 
  implicit val MetricsContainerUnmarshaller =
    Unmarshaller[MetricsSummary](`application/json`) {
    case HttpBody(contentType, buffer) =>
      (new String(buffer)).asJson.convertTo[MetricsSummary]
    }
   
  def apply(s: String):MetricsContainer = {
    new MetricsContainer(s)
  }
}
