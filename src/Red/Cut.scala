package Red

/** Representation of text that has been cut/deleted/copied in an editing
  * session. The `extent` is negative iff the cursor was to the right of the
  * mark when this `Cut` was made. The intention is that when pasteing a `Cut`
  * the cursor and mark end up at the correct ends of the pasted characters,
  * thereby allowing a cut action to be undone immediately by a paste of what
  * was cut, and a paste action to be undone immediately by a cut of the
  * selection.
  *
  * '''Incidentals'''
  *
  * The `generation` is the value of the generation of the document before the
  * cut was taken.
  *
  * The `cursor` is value of the session cursor at the moment the `Cut` was
  * constructed. Some cuts can safely be merged, namely adjacent cuts made from
  * successive generations of the document.
  *
  * '''Invariant''' {{{extent.abs == text.length}}}
  */
case class Cut(text: String, extent: Int, cursor: Int, generation: Long) {

  /** Merge the `next: Cut` with this if they are one generation apart and are
    * adjacent (a sufficient condition for them arising from a sequence of
    * successive deletes). Otherwise return `next`. In some cases (a mixed
    * sequence of left and right deletes) the question of whether the merged cut
    * should be treated as a left or a right cut is moot (ie a matter of taste).
    */

  def merge(next: Cut): Cut = {
    val merged: Cut =
      if (next.generation != this.generation + 1) next
      else {
        val (nc, ng) = (next.cursor, next.generation)

        // There have been no other insertions or deletions
        // between `this` and `next`.
        //
        // The guards below could probably be simplified, but each of them is
        // explainable in its own right.
        //
        // The ''soundness'' criterion for each is that the document
        // text after pasting the merged result, should be identical
        // to the document text after pasting `this`, followed by
        // (if necessary) moving the  cursor to the "appropriate end"
        // of the pasted text, and pasting `next`.
        if (next.extent == -1 && this.cursor == next.cursor + 1)
          // successive single deletes leftwards; cursor stays at the right
          Cut(next.text + this.text, this.extent + next.extent, nc, ng)
        else if (next.extent == 1 && this.cursor == next.cursor)
          // successive single deletes rightwards; cursor stays at the left
          Cut(this.text + next.text, this.extent + next.extent, nc, ng)
        else if (this.left == next.right)
          // a cut of text that was to the left of this; force cursor to the right
          Cut(
            next.text + this.text,
            -this.text.length - next.text.length,
            nc,
            ng
          )
        else if (this.right - 1 == next.left)
          // a cut of text that was to the right of this; force cursor to the right
          Cut(
            this.text + next.text,
            -this.text.length - next.text.length,
            nc,
            ng
          )
        else if (this.left == next.left)
          // a cut of text that was to the left of this; force cursor to the right
          Cut(
            this.text + next.text,
            -this.text.length - next.text.length,
            nc,
            ng
          )
        else
          next
      }
    merged
  }

  private def left  = cursor min (cursor + extent)
  private def right = cursor max (cursor + extent)

}
