trait ActorRef[-T]
trait Listing[T]

object LocalReceptionist { // extends ReceptionistBehaviorProvider {
  private type Service[K <: AbstractServiceKey] = Aux.Service[K]
  private type Subscriber[K <: AbstractServiceKey] = Aux.Subscriber[K]

  private object Aux {
    type Aux[P] = AbstractServiceKey { type Protocol = P }

    type Service[K <: Aux[?]] = K match {
      case Aux[t] => ActorRef[t]
    }
    type Subscriber[K <: Aux[?]] = K match {
      case Aux[t] => ActorRef[Listing[t]]
    }
  }

  private final case class State(
    services: TypedMultiMap[AbstractServiceKey, Service],
    subscriptions: TypedMultiMap[AbstractServiceKey, Subscriber]
  ) {
    def serviceInstanceAdded[Key <: AbstractServiceKey](key: Key)(serviceInstance: ActorRef[key.Protocol]): State = {
      val newServices = services.inserted(key)(serviceInstance) // error
      ???
    }

    def subscriberAdded[Key <: AbstractServiceKey](key: Key)(subscriber: Subscriber[key.type]): State = {
      val newSubscriptions = subscriptions.inserted(key)(subscriber)  // error
      ???
    }
  }
}

class TypedMultiMap[T <: AnyRef, K[_ <: T]] {
  def inserted(key: T)(value: K[key.type]): TypedMultiMap[T, K] = ???
}

abstract class AbstractServiceKey {
  type Protocol
}
