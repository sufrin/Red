package Red

trait DocumentProvider extends Notifier[DocumentEvent] {
  val document: DocumentInterface
}
