package Red

import Red.Menus.DynamicMenu
import Red.Personalised.Bindings.AbortBindings

import scala.collection.mutable
import scala.swing.{Action, Component, Label, MenuItem}

object Features {

  lazy val persistent = Utils.appleRed.node("Features")

  def sync(): Unit = { persistent.sync(); Utils.appleRed.sync() }

  /** Clear the persistent store and nullify all features.
   *  The effect of this is that when bindings are
   *  read features are given the default values specified
   *  in the bindings file(s).
   */
  def resetFeatures(): Unit = { features.clear(); persistent.clear(); sync() }

  /**
   * Every feature has a kind -- akin to a type
   */
   trait Feature {
     /** The feature's name */
     val name:     String
     /** The feature's value, as a string */
     def valueString: String
     /** The feature's `name=value` string */
     def profileString: String
     /**
      *  Conditionally continue to process the rest of a declaration starting with this feature
      */
     def processConditional(tail: List[String], process: List[String] => Unit, ifSo: Boolean=>Boolean): Unit

    /**
     *  Save the feature in the persistent store
     */
    def save(): Unit

    /**
     *  Restore the feature from the persistent store
     */
    def restore(): Unit
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
      def valueString: String = if (value) attributes(0) else attributes(1)
      def profileString: String = s"$name=$valueString"
      def processConditional(tail: List[String], process: List[String]=>Unit, ifSo: Boolean=>Boolean): Unit =
          if (ifSo(value)) process(tail)

        def save(): Unit = {
         persistent.putBoolean(name, value)
      }

      def restore(): Unit = {
          value = persistent.getBoolean(name, value)
      }
  }

    case class OneOf(name: String, attributes: Seq[String])   extends Feature
    { var value: String = ""
      def withValue(v: String): this.type = { value=v; this }
      def valueString: String = value
      def profileString: String = s"$name=$valueString"
      def processConditional(tail: List[String], process: List[String]=>Unit, ifSo: Boolean=>Boolean): Unit = {
        tail match {
          case "==" :: pat :: rest => if (ifSo(value==pat)) process(rest)
          case "!=" :: pat :: rest => if (ifSo(value!=pat)) process(rest)
          case "??" :: pat :: rest => if (ifSo(value matches pat)) process(rest)
          case other => throw new AbortBindings(s"invalid conditional operator for a OneOf: $other")
        }
      }

      def save(): Unit = {
        persistent.put(name, value)
      }

      def restore(): Unit = {
        value = persistent.get(name, value)
      }
    }

    case class AnyOf(name: String, attributes: Seq[String]) extends Feature {
      var value: List[String] = Nil
      def withValue(v: List[String]): this.type = { value=v; this }
      def valueString: String = value.mkString("","\u00A0","")
      def profileString: String = s"$name=$valueString"
      def processConditional(tail: List[String], process: List[String]=>Unit, ifSo: Boolean=>Boolean): Unit = {
        tail match {
          case "??" :: pat :: rest => if (ifSo(value.mkString(""," ", "").matches(pat))) process(rest)

          case contains :: pat :: rest if contains == "<<=" || contains == "\u2286" =>
            val set    = pat.split(",?\\s+")
            val subset = value.forall ( set.contains(_) )
            if (ifSo(subset)) process(rest)

          case contains :: pat :: rest if contains == "=>>" || contains == "\u2287" =>
            val set    = pat.split(",?\\s+")
            val subset = set.forall ( value.contains(_) )
            if (ifSo(subset)) process(rest)

          case contains :: pat :: rest if contains == "intersects" || contains == "\u2229" =>
            val set    = pat.split(",?\\s+")
            val subset = set.exists ( value.contains(_) )
            if (ifSo(subset)) process(rest)

          case "==" :: pat :: rest =>
            val set    = pat.split(",?\\s+")
            val equals = set.forall ( value.contains(_) ) && value.forall ( set.contains(_) )
            if (ifSo(equals)) process(rest)

          case other => throw new AbortBindings(s"invalid operator for an AnyOf: $other")
        }
      }

      def save(): Unit = {
        persistent.put(name, valueString)
      }

      def restore(): Unit = {
        val rep = persistent.get(name, valueString)
        value = rep.split('\u00A0').toList
      }
    }

    /**
     *  '''Invariant''' {{{
     *     (forall(id, _) <- features) yield features(id).name==id
     *   }}}
     */
     val features: collection.mutable.Map[String,Feature]= new mutable.LinkedHashMap[String,Feature]

      /**
       *  Invoked at a feature declaration of the form
       *
       *    `feature` ''name'' ''kind'' ''attributes''
       *
       *  - If the declaration is sound, then build the corresponding feature `f`, and return `Left(f)`
       *  - If the declaration is unsound, return `Right(errorDescription)`
       */
      def newFeature(_name: String, _kind: String, _attributes: Seq[String]): Either[Feature, String] = {
        def cons(value: String, values: List[String]): List[String] = if (values.contains(value)) values else value::values
        var error: Option[String] = None
        val feature: Feature = _kind.toLowerCase() match {

          case s"$${$variable-$default}" =>
            val value = sys.env.getOrElse(variable, default)
            OneOf(_name, cons(value, _attributes.toList)).withValue(value)

          case s"$${=$default}" =>
            val value =
            _attributes.filter { v => sys.env.get(v).nonEmpty } match {
              case v::_ => sys.env.get(v).get
              case Nil  => default
            }
            OneOf(_name, List(value)).withValue(value)

          case s"$$$variable" =>
            sys.env.get(variable) match {
              case Some(value) => OneOf(_name, cons(value, _attributes.toList)).withValue(value)
              case None        => OneOf(_name, _attributes.toList).withValue("")
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

  /**
   * Add to `features` the feature declared by:
   *
   * `feature` ''name kind attributes''
   *
   * if it is sound, and if it hasn't already been added; then yield `None`.
   * Otherwise yield `Some`''(errorReport)''
   *
   * Declarations of existing features are ignored, and
   * are presumed to come while re-reading preferences.
   *
   * TODO: differentiate between the first and subsequent reading
   *       of preferences.
   */
      def declare(_name: String, _kind: String, _attributes: Seq[String]): Option[String] =
        newFeature(_name: String, _kind: String, _attributes) match {
          case Left(theFeature) =>
            features.get(_name) match {
              case None    => features(_name) = theFeature; theFeature.restore(); None
              case Some(_) => None
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
            new Buttons.CheckBox(s"$name", name) {
              font = Utils.menuButtonFont
              selected = f.value
              def click(): Unit = { f.value = selected; f.save() }
            }
            List(Separator(), Separator(), item)


          case f @ OneOf(name, attrs) =>
          { val group = new Buttons.Group() {
                   def select(value: String): Unit = { f.value = value; f.save() }
                   value = f.value
                }
            val menu = for { attr <- attrs.toList.reverse }  yield group.CheckBox(attr, attr)
                Separator() :: Separator() :: Lab(s"$name:") ::  menu
          }

          case f @ AnyOf(name, attrs) =>
          {
            val menu =
              for { attr <- attrs.toList.reverse } yield
                new Buttons.CheckBox(attr, attr) {
                  font = Utils.menuButtonFont
                  selected = f.value.contains(attr)
                  def click(): Unit = {
                      if (selected) f.value = attr :: f.value.toList else f.value = f.value.filter(_.!=(attr))
                      f.save()
                  }
                }
            Separator() :: Separator() :: Lab(s"$name::") ::  menu
            }
          }

      val purgeFeatures: Component = new MenuItem(Action("Reset"){
          resetFeatures()
          Personalised.Bindings.reImportBindings()
      }) {
        font = Utils.menuButtonFont
        tooltip = "Erase all feature settings then import the profile file, thereby setting all features to their defaults"
      }

      def menu: DynamicMenu = new DynamicMenu("Profile") {
        font = Utils.menuButtonFont

        def dynamicContents: Seq[scala.swing.Component] = {
          val components = new collection.mutable.ListBuffer[scala.swing.Component]
          components += new MenuItem(Action("Reimport Profile"){ Personalised.Bindings.reImportBindings() }) {
            font = Utils.menuButtonFont
            tooltip = "Import the profile file using current feature settings"
          }

          for { (_, feature) <- features }
            for { component <- makeFeatureMenu(feature) }
                  components.addOne(component)

          components.addOne(Separator())
          components.addOne(Separator())
          components.addOne(purgeFeatures)

          components.toSeq
        }

        var oldProfile = profile

        override def popupMenuWillBecomeVisible(): Unit = {
          oldProfile = profile
          super.popupMenuWillBecomeVisible()
        }

        override def popupMenuWillBecomeInvisible(): Unit = {
          // reimport bindings if the profile has changed
          // but wait until the menu has actually gone
          if (oldProfile != profile) Utils.invokeLater {
            Personalised.Bindings.reImportBindings()
            profileChanged.notify(profile)
          }
          super.popupMenuWillBecomeInvisible()
        }

      }

  /** Evaluate a feature name */
  def get(featureId: String): Feature = features.getOrElse(featureId, throw new NoSuchElementException(featureId))

  def getOrElse(featureId: String, default: => Feature): Feature = features.getOrElse(featureId, default)

  /** Evaluate a feature expression of one of the forms:
   *  - `${featureId-default}` yields the value of the id or the specified default
   *  - `$featureId`  yields the value of the id or fails
   *  - `featureId` yields the id itself
   */
  def eval(featureExpr: String): String =
    featureExpr match {
      case s"$${$featureId-$default}" =>
        try get(featureId).valueString catch { case _ : NoSuchElementException => default}
      case s"$$$featureId" => get(featureId).valueString
      case _               => featureExpr
    }

      val profileChanged: Notifier[String] = new Notifier[String]

      /** The profile is the concatenated representations of the features: pro-tem */
      def profile: String = {
        features.values.map(_.profileString).mkString("", " ", "")
      }

  }
