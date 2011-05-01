import scala.annotation.tailrec

trait Action[-T <: Item] {
  def perform(item: T, gameState: GameState): GameState
}

object NoAction extends Action[Nothing] {
  def perform(item: Nothing, state: GameState): GameState = state
}

trait Item {
  type Concrete <: Item
  
  val name: String
  val weight: Int
  val description: String
  val value: MonetaryValue
  val action: Option[Action[Concrete]]
  
  val pickUpAction: Action[Item] = new Action[Item] {
    
    def perform(item: Item, state: GameState): GameState = {
      val inventory = item +: state.player.inventory
      val player = state.player.copy(inventory = inventory)
      state.copy(player = player)
    }
  }
}


case class MonetaryValue(val value: Int)

case class Weapon(name: String, attack: Int, weight: Int, description: String, value: MonetaryValue)
  extends Item {
  
  type Concrete = Weapon
  
  val action = Some(new Action[Weapon] {
    def perform(item: Weapon, state: GameState): GameState = {
      val chars = state.currentArea.characters map { _.damage(item) }
      val area = state.currentArea.copy(characters = chars)
      state.copy(currentArea = area)
    }
  })
  
  def changeAttackForce(amount: Int): Weapon = this.copy(attack = attack + amount)
}

case class Potion(name: String, potency: Int, weight: Int, description: String, value: MonetaryValue)
  extends Item {
  
  type Concrete = Potion
  
  val action = Some(new Action[Potion] {
  	def perform(item: Potion, state: GameState): GameState = {
  	  val player = state.player.drinkPotion(item)
  	  state.copy(player = player)
  	}
  })
}

case class Armor(name: String, defendsAgainst: List[Weapon], weight: Int, description: String, value: MonetaryValue)
  extends Item {
  val action = None
}

trait Lockable {
  val locked: Boolean
  def unlock: Lockable
}

case class Door(first: Area, second: Area, locked: Boolean = true) extends Lockable {
  
  def unlock = {
    copy(locked = false)
  }
  
  def opensTo(area: Area): Boolean = {
    first == area || second == area
  }
  
  def other(area: Area): Area = {
    if (first == area)
      second
    else
      first
  }
}

case class Key(name: String, opens: Lockable, weight: Int, description: String, value: MonetaryValue)
  extends Item {
  
  type Concrete = Key
  
  val action = Some(new Action[Key] {
    def perform(item: Key, state: GameState): GameState = {
      item.opens match {
        case _: Door => state.copy(map = state.map.unlockDoor(item))
        case _ => state
      }
    }
  })
}

case class Inventory(items: List[Item] = Nil) {
  
  def :+(item: Item) = new Inventory(items :+ item)
  
  def +:(item: Item) = new Inventory(item :: items)
  
  def -(item: Item) = new Inventory(items filter { _ != item })
  
  def contains(item: Item): Boolean = items.contains(item)
}

case class Character(
    val name: String,
    val health: Int = 100,
    val weightCapacity: Int = 0,
    val inventory: Inventory = new Inventory(Nil)) {
  
  def weapons = (inventory.items collect { case weapon: Weapon => weapon })
  
  def armors = (inventory.items collect { case armor: Armor => armor })
  
  private def currentWeight = inventory.items map { _.weight } sum
  
  def pickUpItem(item: Item): Character = {
    if (currentWeight + item.weight > weightCapacity)
      this
    else
      this.copy(inventory = new Inventory(item :: inventory.items))
  }
  
  def drinkPotion(potion: Potion): Character = {
    this.copy(health = health + potion.potency)
  }
  
  def isDead = health <= 0
  
  def damage(weapon: Weapon): Character = {
    this.copy(health = health - weapon.attack)
  }

  def hasKeyFor(door: Door): Boolean = keyFor(door).isDefined
  
  def keyFor(door: Door): Option[Key] = {
    inventory.items collect { case k @ Key(_, door, _, _, _) => k } headOption
  }
  
  def hasArmor = inventory.items exists { case _: Armor => true }
}

class DropAction(character: Character) extends Action[Item] {

  def perform(item: Item, state: GameState): GameState = {
    
    if (!character.inventory.contains(item))
      state.copy(message = Some(character.name +"'s inventory does not contain "+ item))
    else {
      val inventory = character.inventory - item
      val c = character.copy(inventory = inventory)
      state.copy(player = c)
    }
  }
}

class OpenDoorAction(door: Door) extends Action[Item] {
  
  def perform(item: Item, state: GameState): GameState = {
    
    val key = state.player.keyFor(door)
    door match {
      case Door(_, _, locked) if (!key.isDefined)  =>
        state.copy(message = Some("Door is locked"))
      case _ =>
        state.copy(map = state.map.unlockDoor(key.get))
    }
  }
}

case class Area(items: List[Item] = Nil, characters: List[Character] = Nil) {
  
  def ++(area: Area): Door = {
    Door(this, area)
  }
}

case class GameMap(doors: Set[Door] = Set()) {
  
  def areas: Set[Area] = {
    doors flatMap { door => door.first :: door.second :: Nil }
  }
  
  def addDoor(door: Door) : GameMap = {
    copy(doors = doors + door)
  }
  
  def doorsIn(area: Area): Set[Door] = {
    doors filter { door => door.first == area || door.second == area }
  }
  
  def unlockDoor(key: Key): GameMap = {
    copy(doors = doors map {
      door =>
      if (key.opens == door)
        door.unlock
      else
        door
    })
  }
}

class Choice[T <: Item](val action: Action[T], val item: T, val description: String)


case class GameState(player: Character, map: GameMap, currentArea: Area, message: Option[String] = None) {
  
  def currentDoors: Set[Door] = map.doorsIn(currentArea)
  
  def enter(door: Door): GameState = {
    
    if (door.opensTo(currentArea)) {
      val key = player.keyFor(door)
      val newMap = key match {
        case Some(k) if door.locked => map.unlockDoor(k)
        case None => map
      }
      copy(map = newMap, currentArea = door.other(currentArea))
    }
    else
      this
  }
  
  def start() {
    printChoices()
    next()
  }
  
  @tailrec final def next() {
    
    readChoice() match {
      case None => return
      case Some(choice) =>
      
        val state = choice.action.perform(choice.item, this) match {
          case GameState(_, _, _, Some(m)) =>
            println(m)
            copy(message = None)
          case x => x
        }
    }
    printChoices()
    next()
  }
  
  private def readChoice(): Option[Choice[Item]] = {
    val i = readLine.toInt
    if (i == 0)
      None
    else
      Some(choices(i-1))
  }
  
  private def printChoices() {
    println("0) Quit")
    choices.zipWithIndex foreach { case (c, i) => println((i+1) +") "+ c.description) }
  }
  
  def choices: List[Choice[Item]] = {
    (for (door <- currentDoors)
     yield new Choice[Item](new OpenDoorAction(door), null, "Open door")).toList ++
    (for (item <- currentArea.items)
     yield new Choice[Item](item.pickUpAction, item, "Pick up "+ item.name))
  }
}


object Main extends App {
  
  val axe = Weapon("Axe", 5, 10, "Cuts things", new MonetaryValue(10))
  val armor = Armor("chain mail", axe :: Nil, 7, "protects from axes", new MonetaryValue(1))
  val cyanide = Potion("cyanide", 1000, 6, "causes death", new MonetaryValue(3))
  

  var map = new GameMap()

  val monster = Character("ogre", weightCapacity = 10)
  val area1 = Area(items = List(axe))
  val area2 = Area(characters = List(monster))
  val door = area1 ++ area2
  map = map.addDoor(door)
  
  val key = Key("door key", door, 5, "", new MonetaryValue(4))
  
  var player = new Character("Fred")
  val state = GameState(player, map, area1)
  
  state.start()
}