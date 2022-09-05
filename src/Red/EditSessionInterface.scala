package Red

trait EditSessionInterface extends Notifier[DocumentEvent] {
  val document: DocumentInterface
}
