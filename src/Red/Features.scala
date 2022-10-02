package Red

import Red.Utils.DynamicMenu

import scala.collection.mutable
import scala.swing.{Action, Component, Label, MenuItem}

object Features {
  /**
   * Every feature has a kind -- akin to a type
   */
   trait Feature {
     val name:     String
     def mkString: String
     def process(parameters: List[String], action: (Boolean, List[String]) => Unit): Option[String]
   }

  /**
   *   A feature has a name and zero or more possible attributes,
   *   depending on its kind.
   *
   *   === invariant
   *      {{{
   *           noDuplicates(attributes)
   *      }}}
   *   The feature `kind` determines how it is shown in a menu.
   *    -  A `Bool`  feature can be true or false.
   *    -  A `OneOf`   feature can have exactly one of the attributes.
   *    -  An `AnyOf`  feature can have zero or more of the attributes.
   */
    case class Bool(name: String, attributes: Seq[String] = List("true", "false")) extends Feature  {
      var value: Boolean = false
      def withValue(v: Boolean): this.type = { value=v; this }
      def mkString: String = if (value) attributes(0) else attributes(1)
      def process(parameters: List[String], action: (Boolean, List[String])=>Unit): Option[String] =
        { action(value, parameters)
          None
        }
    }

    case class OneOf(name: String, attributes: Seq[String])   extends Feature
    { var value: String = ""
      def withValue(v: String): this.type = { value=v; this }
      def mkString: String = value
      def process(parameters: List[String], action: (Boolean, List[String])=>Unit): Option[String] = {
        parameters match {
          case "==" :: pat :: rest => action(value==pat, rest); None
          case "!=" :: pat :: rest => action(value!=pat, rest); None
          case "??" :: pat :: rest => action(value.matches(pat), rest); None
          case other => Some(s"invalid operator for a OneOf: $other")
        }
      }
    }

    case class AnyOf(name: String, attributes: Seq[String]) extends Feature {
      var value: List[String] = Nil
      def withValue(v: List[String]): this.type = { value=v; this }
      def mkString: String = value.mkString("("," ",")")
      def process(parameters: List[String], action: (Boolean, List[String])=>Unit): Option[String] = {
        parameters match {
          case "??" :: pat :: rest => action((value.mkString(""," ", "").matches(pat)), rest); None
          case other => Some(s"invalid operator for an AnyOf: $other")
        }
      }
    }



    /**
     *  ===invariant: no two features have the same name
     *    {{{ noDuplicates(_features map _.name) }}}
     */
     val features: collection.mutable.Map[String,Feature]= new mutable.LinkedHashMap[String,Feature]

      /** Construct a feature, `f`, from a declaration, and return `Left(f)` otherwise return `Right(errorDescription)` if the declaration is unsound  */
      def makeFeature(_name: String, _kind: String, _attributes: Seq[String]): Either[Feature, String] = {
        var error: Option[String] = None
        val feature: Feature = _kind.toLowerCase() match {
          case "eval" =>
            _attributes match {
              case s"$${$variable-$default}" :: values =>
                val value = sys.env.getOrElse(variable, default)
                OneOf(_name, values ++ (if (values.contains(value)) Nil else List(value))).withValue(value)

              case s"$$$variable" :: values =>
                sys.env.get(variable) match {
                  case Some(value) => OneOf (_name, values ++ (if (values.contains(value)) Nil else List(value))) . withValue (value)
                  case None => AnyOf (_name, values)
                }

              case _ =>
                error = Some(s"Eval feature must have one name, and possibly some alternate value")
                null
            }
          case "bool" | "boolean" =>
            _attributes match {
              case Nil => Bool(_name)
              case List("=", v) if v.toLowerCase().matches("true|false") => Bool(_name).withValue(v.toBoolean)
              case List(on, off, "=", v) if v.toLowerCase().matches("true|false") => Bool(_name, List(on, off)).withValue(v.toBoolean)
              case _ =>
                error = Some(s"Boolean feature notation: Bool or Bool = True | False or Bool onvalue offvalue = True | False")
                null
            }
          case "oneof" | "one" =>
            _attributes match {
              case attrs if attrs.contains("=") =>
                val (pre, _ :: post) = attrs.span(s => s != "=")
                if (post.exists(attr => !pre.contains(attr))) {
                  error = Some(s"OneOf initial feature value must be one of those declared")
                  null
                }
                else
                OneOf(_name, pre.toList.reverse).withValue(if (post.nonEmpty) post(0) else "")
              case Nil => error = Some(s"OneOf feature must have some potential values"); null
              case attrs => OneOf(_name, attrs.toList.reverse)
            }
          case "anyof" | "any" =>
            _attributes match {
              case attrs if attrs.contains("=") =>
                val (pre, _ :: post) = attrs.span(s => s != "=")
                if (post.exists(attr => !pre.contains(attr))) {
                  error = Some(s"AnyOf initial feature values must all be among those declared")
                  null
                }
                else
                   AnyOf(_name, pre.toList.reverse).withValue(post.toList)
              case Nil => error = Some(s"AnyOf feature must have some potential values"); null
              case attrs => AnyOf(_name, attrs.toList.reverse)
            }
          case other => error = Some(s"Feature type must be bool(ean), one(of), or any(of)"); null
        }
        if (error.isEmpty) return Left(feature) else return Right(error.get)
      }

      def add(_name: String, _kind: String, _attributes: Seq[String]): Option[String] =
        makeFeature(_name: String, _kind: String, _attributes) match {
          case Left(theFeature) =>
            // redefinitions of features are ignored silently
            features.get(_name) match {
              case None                  => features(_name) = theFeature; None
              case Some(originalFeature) => None
            }
          case Right(theError)  => Some(theError)
        }

      def clear(): Unit = features.clear()

      def Lab(name: String): Component = new Label(s"  $name") {
          font = Utils.menuButtonFont
      }

      def makeFeatureMenu(f: Feature): List[scala.swing.Component] =
        f match {
          case f @ Bool(name, _) =>
            val item =
            new Utils.CheckBox(s"$name", name) {
              font = Utils.menuButtonFont
              selected = f.value
              def click(): Unit = f.value = selected
            }
            List(Separator(), Separator(), item)


          case f @ OneOf(name, attrs) =>
          { val group = new Utils.Group() {
                   def select(value: String): Unit = { f.value = value }
                   value = f.value
                }
            val menu = for { attr <- attrs.toList.reverse }  yield group.CheckBox(attr, attr)
                Separator() :: Separator() :: Lab(s"$name:") ::  menu
          }

          case f @ AnyOf(name, attrs) =>
          {
            val menu =
              for { attr <- attrs.toList.reverse } yield
                new Utils.CheckBox(attr, attr) {
                  font = Utils.menuButtonFont
                  selected = f.value.contains(attr)
                  def click(): Unit = {
                      if (selected) f.value = attr :: f.value.toList else f.value = f.value.filter(_.!=(attr))
                  }
                }
            Separator() :: Separator() :: Lab(s"$name::") ::  menu
            }
          }

      def menu: DynamicMenu = new DynamicMenu("Profile") {
        def dynamic: Seq[scala.swing.Component] = {
          val components = new collection.mutable.ListBuffer[scala.swing.Component]
          components += new MenuItem(Action("Bindings"){ Personalised.Bindings.reImportBindings() }) {
            font = Utils.menuButtonFont
            tooltip = "Reimport all bindings"
          }

          for { (_, feature) <- features }
            for { component <- makeFeatureMenu(feature) }
                  components.addOne(component)

          components.toSeq
        }

        var oldProfile = profile

        override def popupMenuWillBecomeVisible(): Unit = {
          oldProfile = profile
          super.popupMenuWillBecomeVisible()
        }

        override def popupMenuWillBecomeInvisible(): Unit = {
          // reimport bindings if the profile has changed
          if (oldProfile != profile) {
            Personalised.Bindings.reImportBindings()
            profileChanged.notify(profile)
          }
          super.popupMenuWillBecomeInvisible()
        }
      }

  /** Evaluate a feature name */
  def eval(feature: String): Feature = features.getOrElse(feature, throw new NoSuchElementException(feature))

  /** Evaluate a feature  */
  def eval(feature: String, default: String): String =
    feature match {
      case s"$${$featureId=$default}" =>
        try eval(featureId).mkString catch { case _ : NoSuchElementException => default}
      case s"$$$featureId" => eval(featureId).mkString
      case other           => default
    }

      val profileChanged: Notifier[String] = new Notifier[String]

      /** The profile is the concatenated representations of the features: pro-tem */
      def profile: String = {
        features.values.map(_.mkString).mkString("", " ", "")
      }

  }
