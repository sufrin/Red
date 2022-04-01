package Red

trait Session extends Notifier[DocumentEvent] {
  val document: DocumentInterface
}
