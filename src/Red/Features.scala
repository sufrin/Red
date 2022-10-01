package Red

import Red.Utils.DynamicMenu

import scala.swing.{Action, Component, Label, MenuItem}

object Features {
  /**
   * Every feature has a kind -- akin to a type
   */
   trait Feature {
     val name:     String
     def mkString: String
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
   *    -  A `BoolFeature`  feature can be true or false.
   *    -  A `OneOfFeature`   feature can have exactly one of the attributes.
   *    -  An `AnyOfFeature`  feature can have zero or more of the attributes.
   */
    case class BoolFeature(name: String) extends Feature  {
      var value: Boolean = false
      def withValue(v: Boolean): this.type = { value=v; this }
      def mkString: String = if (value) name else ""
    }

    case class OneOfFeature(name: String, attributes: Seq[String])   extends Feature
    { var value: String = ""
      def withValue(v: String): this.type = { value=v; this }
      def mkString: String = if (value=="") "" else s"$name=$value"
    }

    case class AnyOfFeature(name: String, attributes: Seq[String]) extends Feature {
      var value: List[String] = Nil
      def withValue(v: List[String]): this.type = { value=v; this }
      def mkString: String = s"$name=${value.mkString("(","/",")")}"
    }



    /**
     *  ===invariant: no two features have the same name
     *    {{{ noDuplicates(_features map _.name) }}}
     */
     private val features: collection.mutable.ListBuffer[Feature]= new collection.mutable.ListBuffer()

      /** Construct a feature, `f`, from a declaration, and return `Left(f)` otherwise return `Right(errorDescription)` if the declaration is unsound  */
      def makeFeature(_name: String, _kind: String, _attributes: Seq[String]): Either[Feature, String] = {
        var error: Option[String] = None
        val feature: Feature = _kind.toLowerCase() match {
          case "env" | "environment" =>
            _attributes match {
              case List(s"$${$variable=$default}") =>
                val value = sys.env.getOrElse(variable, default)
                OneOfFeature(_name, List(value)).withValue(value)

              case variable :: values =>
                sys.env.get(variable) match {
                  case Some(value) => OneOfFeature (_name, values ++ (if (values.contains(value)) Nil else List(value))) . withValue (value)
                  case None => AnyOfFeature (_name, values)
                }

              case _ =>
                error = Some(s"Env feature must have one name, and possibly a default value")
                null
            }
          case "bool" | "boolean" =>
            _attributes match {
              case Nil => BoolFeature(_name)
              case List("=", v) if v.toLowerCase().matches("true|on") => BoolFeature(_name).withValue(true)
              case List("=", v) if v.toLowerCase().matches("false|off") => BoolFeature(_name).withValue(false)
              case _ =>
                error = Some(s"Boolean features must be set = true/on or = false/off")
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
                OneOfFeature(_name, pre.toList.reverse).withValue(if (post.nonEmpty) post(0) else "")
              case Nil => error = Some(s"OneOf feature must have some potential values"); null
              case attrs => OneOfFeature(_name, attrs.toList.reverse)
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
                   AnyOfFeature(_name, pre.toList.reverse).withValue(post.toList)
              case Nil => error = Some(s"AnyOf feature must have some potential values"); null
              case attrs => AnyOfFeature(_name, attrs.toList.reverse)
            }
          case other => error = Some(s"Feature type must be bool(ean), one(of), or any(of)"); null
        }
        if (error.isEmpty) return Left(feature) else return Right(error.get)
      }

      def add(_name: String, _kind: String, _attributes: Seq[String]): Option[String] =
        makeFeature(_name: String, _kind: String, _attributes) match {
          case Left(theFeature) =>
            // redefinitions of features are ignored silently
            if (!features.exists(f => f.name == _name)) features += theFeature
            None
          case Right(theError)  => Some(theError)
        }

      def clear(): Unit = features.clear()

      def Lab(name: String): Component = new Label(s"  $name") {
          font = Utils.menuButtonFont
      }

      def makeFeatureMenu(f: Feature): List[scala.swing.Component] =
        f match {
          case f @ BoolFeature(name) =>
            val item =
            new Utils.CheckBox(s"$name", name) {
              font = Utils.menuButtonFont
              selected = f.value
              def click(): Unit = f.value = selected
            }
            List(Separator(), Separator(), item)


          case f @ OneOfFeature(name, attrs) =>
          { val group = new Utils.Group() {
                   def select(value: String): Unit = { f.value = value }
                   value = f.value
                }
            val menu = for { attr <- attrs.toList.reverse }  yield group.CheckBox(attr, attr)
                Separator() :: Separator() :: Lab(s"$name:") ::  menu
          }

          case f @ AnyOfFeature(name, attrs) =>
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

          for { feature <- features }
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

      val profileChanged: Notifier[String] = new Notifier[String]

      /** The profile is the concatenated representations of the features  */
      def profile: String = {
        features.map(_.mkString).mkString("", " ", "")
      }

  }
