

package Commands

  /**    === Command: the big ideas
   *
   *     Every `command: Command[T]` represents a state-changing operation that is
   *     applied to a `target` of type T` by invoking
   *     {{{ command.DO(target}}}
   *
   *     Suppose a (mutable) object `target: T` is in state `s`.
   *
   *     If `DO(target)` succeeds, thereby changing the state of `target` to
   *     `s'`, then it must yield `Some(change)` such that henceforth,
   *     whenever target is in state `s'`, `change.undo()` will drive
   *     it back to state `s`; and whenever target is in state `s`,
   *     `change.redo()` will drive target back to state `s'`.
   *
   *     If `DO(target)` fails, or is otherwise non-applicable, then it
   *     must yield `None`, and must not have changed the state of `target`.
   *
   *     `c1 &&& c2` is the ''sequential composition'' of the commands `c1`
   *     and `c2`.
   *
   *      === Composition and Alternation
   *
   *      When `target` is in state `s`, `(c1 &&& c2).DO(target)` succeeds only if
   *      {{{c1.DO(target)}}} succeeds (leaving `target` in state `s'`), and
   *      {{{c2.DO(target)}}} then succeeds (leaving `target` in
   *      state `s''`). The result is a `StateChange` whose `undo`,
   *      is guaranteed to restore the original state `s` whenever executed
   *      in state `s''`
   *
   *      If either `DO` fails, then `target` is left in its original state, `s`.
   *
   *     `c1 ||| c2` is the ''alternation'' of the commands `c1`
   *     and `c2`.
   *
   *     When `target` is in state `s`, `(c1 ||| c2).DO(target)` succeeds
   *     if `c1.DO(target)` succeeds, or if it does not succeed and
   *     `c2.DO(target)` succeeds. The result is a `StateChange` whose `undo`,
   *     is guaranteed to restore the original state `s` whenever executed
   *     in the target state `s'` that is the result of the successful
   *     `DO`.
   *
   *     `(c1>>>c2).DO(target)` succeeds only if `c1.DO(target)` succeeds.
   *     In that case it yields the resulting `StateChange`
   *     but only after executng  `c2.DO(target)`.
   */
  trait Command[T] {
    def DO(target: T): Option[StateChange]
    def &&&(that: Command[T]): Command[T] = Command.andThen(this, that)
    def |||(that: Command[T]): Command[T] = Command.orElse(this, that)
    def >>>(that: Command[T]): Command[T] = Command.andThenSilently(this, that)
    def when(condition: T => Boolean): Command[T] = Command.when(condition, this)
    def guarded(condition: T => Boolean): Command[T] = Command.guarded(condition, this)
  }

  /**     === StateChange: the big ideas
   *
   *      A `StateChange` captures the state of a target on which a
   *      `Command` has just been `DO`d successfully. Invoking `undo()`
   *      restores the target's state to what it was just before the
   *      `DO`; whilst `redo()` restores the target's state to what it
   *      was just before the `undo`
   *
   */
   
  trait StateChange  {
    def undo(): Unit
    def redo(): Unit
    /**
     *      === Merging
     *
     *          In some applications it makes sense to 'merge' two
     *          adjoining successful `Command`s for the purposes of
     *          undoing and redoing, and this is handled by offering
     *          the `StateChange` of the second command, `next` to the
     *          `merge` of the first.
     *
     *          If `merge(next)` yields `None`, then `next` cannot
     *          be merged with this `StateChange`.  If `merge(next)`
     *          yields `Some(merged)`, then `merged` is the sequential
     *          composition of this `StateChange` and next, and this
     *          `StateChange` should be replaced by it.
     *
     *          The default implementation of `merge` is such that two
     *          `StateChange`s with the the same (non-default) `kind`
     *          string  are merged; and this may be enough to achieve the
     *          sort of functionality that appears in editors, 'etc',
     *          though in our experience some degree of dynamic choice
     *          by the user can be beneficial in some kinds of system.
     *          We have made no attempt to make the default implementation
     *          space-efficient. The default `kind` of a `StateChange`
     *          is `"Nothing"`.
     *
     *          The type of `kind` is `Any` in order to facilitate a
     *          redefinition (by overriding) of `merge` in more complex
     *          applications. But see the definition  (in `EditSessionCommands`)
     *          of the `InsChange` subclass of `StateChange` as an example of a
     *          straightforward overriding of `merge` that yields space-efficient
     *          representations for merges of sequences of `StatChanges`
     *          arising from single-character insertions.
     *
     */
    def merge(next: StateChange): Option[StateChange] =
      if (this.kind!="Nothing" && this.kind==next.kind )
        Some(StateChange.compose(this, next))
      else
        None

    val kind: Any = "Nothing"
  }

  object StateChange {
    /**
     * The (forward) composition of `StateChange`s `u1` and `u2`
     * composes their `undo`s in reverse order, and their `redo`s in
     * forward order. Its `kind` is the same as that of `u2`.
     */
    def compose(u1: StateChange, u2: StateChange): StateChange = new StateChange {
      def undo(): Unit = {
        u2.undo(); u1.undo()
      }
      def redo(): Unit = {
        u1.redo(); u2.redo()
      }

      override
      val kind: Any = u2.kind

      override
      def toString: String= s"StateChange.compose($u1,  $u2)"
    }
  }

  object Command {
    /** Forward composition */
    private def andThen[T](c1: Command[T], c2: Command[T]): Command[T] = new Command[T] {
      def DO(target: T): Option[StateChange] =
        c1.DO(target) match {
          case Some(u1) =>
            c2.DO(target) match {
              case Some(u2) => Some(StateChange.compose(u1, u2))
              case None     => u1.undo(); None
            }
          case None => None
        }
    }

    private def andThenSilently[T](c1: Command[T], c2: Command[T]): Command[T] = new Command[T] {
      def DO(target: T): Option[StateChange] =
        c1.DO(target) match {
          case None => None
          case some =>
            c2.DO(target)
            some
        }
    }

    /** Alternation */
    private def orElse[T](c1: Command[T], c2: Command[T]): Command[T] = new Command[T] {
      def DO(target: T): Option[StateChange] =
        c1.DO(target) match {
          case None     => c2.DO(target)
          case c1Change => c1Change
        }
    }

    /**  */
    object undoNothing extends StateChange {
      def redo(): Unit = ()
      def undo(): Unit = ()

      override
      def toString: String = s"()"
    }

    /**
     * Execution of `doNothing` on a target succeeds, with no effect on the
     * target, and yields a state change whose `undo` and `redo` have
     * no effect.
     */
    def doNothing[T]: Command[T] = new Command[T] {
        def DO(target: T): Option[StateChange] = Some(undoNothing)
    }

    /**
     *  All the component commands of a transaction must succeed
     *  in order for the transaction to succeed.
     */
    def transaction[T](cmds: Iterable[Command[T]]): Command[T] = {
      if (cmds.isEmpty)
        doNothing[T]
      else {
        var tx = cmds.head
        for (c <- cmds.tail) tx = tx &&& c
        tx
      }
    }

    /**
     *  A command that behaves as `command(t)` when `condition(t)` holds,
     *  but otherwise just succeeds, having done nothing to the target `t`.
     */
    def when[T](condition: T => Boolean, command: Command[T]): Command[T] = new Command[T] {
      def DO(target: T): Option[StateChange] = {
        if (condition(target))
           command.DO(target)
        else
           Some(undoNothing)
      }
    }

    /**
     *  A command that behaves as `command(t)` when `condition(t)` holds,
     *  but otherwise fails.
     */
    def guarded[T](condition: T => Boolean, command: Command[T]): Command[T] = new Command[T] {
      def DO(target: T): Option[StateChange] = {
        if (condition(target)) command.DO(target) else None
      }
    }


    /**
     * This class provides facilities for executing, undoing, and
     * redoing commands that operate on the object `target: T`. For
     * simplicity `UNDO` and `REDO` are implemented as non-undoable
     * user-invoked commands.  All other commands are executed by
     * `DO`.
     */
    class StateChangeHistory[T](target: T) extends Red.Notifier[(Int, Int)] {

      import scala.collection._

      val done, undone = new mutable.Stack[StateChange]

      override def toString: String = s"""DONE: ${done.mkString(", ")}\nUNDONE: ${undone.mkString(", ")}"""

      def notifyHandlers(): Unit = notify((done.length, undone.length))

      /**
       * Pop the topmost (most recent) entry of `done`, redo it, and push
       * it onto `undone`
       */
      val UNDO: Command[T] = new Command[T] {
        def DO(t: T): Option[StateChange] = {
          if (done.nonEmpty) {
            undone.push(done.top); done.pop().undo(); notifyHandlers()
          }
          None
        }

        override def toString: String = "UNDO"
      }

      /**
       * Redo the topmost (most recent) entry of `undone`, undo it, and
       * push it onto to `done`
       */
      val REDO: Command[T] = new Command[T] {
        def DO(t: T): Option[StateChange] = {
          if (undone.nonEmpty) {
            done.push(undone.top); undone.pop().redo(); notifyHandlers()
          }
          None
        }

        override def toString: String = "REDO"

      }

      /**
       * Execute the command, `c`, and if it yields a `StateChange`
       * (`change`, say) then clear the `undone` stack, and push
       * `change` onto the `done` stack. `UNDO` and `REDO` are both
       * executable by `DO`, but neither yields an
       * `StateChange`.
       */
      def DO(c: Command[T]): Unit =
        c.DO(target) match {
            case None => ()
            case Some(change) =>
              undone.clear()
              if (done.isEmpty)
                done.push(change)
              else {
                done.top.merge(change) match {
                  case None         => done.push(change)
                  case Some(merged) => done.pop(); done.push(merged)

                }
              }
              notifyHandlers()
            }
        }
  }


