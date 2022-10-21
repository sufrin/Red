package Red

import Red.Menus.DynamicMenu
import Red.Persistent.AnyFeature

import scala.collection.mutable
import scala.swing.{Action, Component, MenuItem}

// Pro-tem, during refactoring
object Features {
  
  def sync(): Unit = { Utils.appleRed.node("Features").sync(); Utils.appleRed.sync() }

  /** Clear the persistent store and nullify all features.
   *  The effect of this is that when bindings are
   *  read features are given the default values specified
   *  in the bindings file(s).
   */
     def resetFeatures(): Unit = { features.clear(); Utils.appleRed.clear(); Utils.appleRed.sync() }

     val features: collection.mutable.Map[String,AnyFeature]= new mutable.LinkedHashMap[String,AnyFeature]
     def add(f: AnyFeature): Unit = features += ((f.key, f))

      def clearAppleRed: Component = new MenuItem(
        Action("Clear AppleRed"){
               Persistent.clearState()
         }) {
        font = Utils.menuButtonFont
        tooltip = "Erase all persistent AppleRed properties"
      }

      def clearAppleRedFeatures: Component = new MenuItem(
        Action("Clear Features"){
          Persistent.clearFeatures()
        }) {
        font = Utils.menuButtonFont
        tooltip = "Erase all persistent AppleRed/Feature properties"
      }

      def showFeatures: Component = new MenuItem(
        Action("Show AppleRed properties"){
          Persistent.showState()
        }) {
        font = Utils.menuButtonFont
        tooltip = "Show the state of the persistent store (on the terminal)"
      }



  def menu: DynamicMenu = new DynamicMenu("Profile") {
        font = Utils.menuButtonFont

        def dynamicContents: Seq[scala.swing.Component] = {
          val components = new collection.mutable.ListBuffer[scala.swing.Component]
          components += new MenuItem(Action("Reimport the profile"){
            Personalised.Bindings.reImportBindings()
          }) {
            font = Utils.menuButtonFont
            tooltip = "Import the profile file using current feature settings"
          }

          components.addOne(Separator())
          components.addOne(Separator())

          for { (_, feature) <- features } { components.addOne(feature.menuItem); components.addOne(Separator()) }

          components.addOne(Separator())
          components.addOne(Separator())
          components.addOne(showFeatures)
          components.addOne(clearAppleRedFeatures)
          components.addOne(clearAppleRed)

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

      val profileChanged: Notifier[String] = new Notifier[String]

      /** The profile is the concatenated representations of the features: pro-tem */
      def profile: String = {
        features.values.map(_.profileString).mkString("", " ", "")
      }

  }
