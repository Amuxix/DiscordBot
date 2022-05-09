package me.amuxix.secrethitler

import cats.effect.IO
import cats.instances.list.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import me.amuxix.secrethitler.Game.{PolicyEffects, fascistPoliciesToWin, hitlerElectionWinPoliciesNeeded, liberalPoliciesToWin}
import me.amuxix.secrethitler.Vote as GameVote
import me.amuxix.secrethitler.Vote.Vote
import me.amuxix.secrethitler.commands.*
import me.amuxix.secrethitler.commands.policies.*
import me.amuxix.wrappers.{Channel, Guild, User}

import scala.util.Random

object Game {
  val policyDeck: List[Policy] = List.fill(6)(LiberalPolicy) ++ List.fill(11)(FascistPolicy)
  val minimumPlayers: Int = 5
  val maximumPlayers: Int = 10
  val liberalPoliciesToWin: Int = 5
  val fascistPoliciesToWin: Int = 6
  val fascistPoliciesToUnlockVeto: Int = 5
  val hitlerElectionWinPoliciesNeeded: Int = 3

  def revealTopCards(game: StartedGame): IO[GameBeforeNomination] = {
    val policies = game.policies.drawPolicies()
    for
      _ <- game.president.sendMessage(
        s"The next 3 cards are ${policies.floating.mkString(" -> ")}.",
      )

      _ <- game.channel.sendMessage("The president has seen the top 3 cards.")
      president = game.president.next(game.playerList)
      _ <- game.announceNextPresident(president)
    yield new GameBeforeNomination(
      game.guild,
      game.channel,
      game.policyEffects,
      game.roles,
      president,
      game.lastChancellor,
      game.failedElections,
      policies,
      game.vetoUnlocked,
    )

  }

  def kill(game: StartedGame): IO[WaitingForPolicyResolution] =
    for
      _ <- game.channel.sendMessage(
        s"${game.president.mention} must select the target to kill by writing, `kill <name, id or mention>`.",
      )
    yield new WaitingForPolicyResolution(
      game.guild,
      game.channel,
      game.policyEffects,
      game.roles,
      game.president,
      game.lastChancellor,
      game.policies,
      game.failedElections,
      game.policies.enacted.fascist >= Game.fascistPoliciesToUnlockVeto,
      Kill,
    )

  def investigate(game: StartedGame): IO[WaitingForPolicyResolution] =
    for
      _ <- game.channel.sendMessage(
        s"${game.president.mention} must select the target to investigate by writing, `investigate <name, id or mention>`.",
      )
    yield new WaitingForPolicyResolution(
      game.guild,
      game.channel,
      game.policyEffects,
      game.roles,
      game.president,
      game.lastChancellor,
      game.policies,
      game.failedElections,
      game.vetoUnlocked,
      SelectNextPresident,
    )

  def selectNextPresident(game: StartedGame): IO[WaitingForPolicyResolution] =
    for
      _ <- game.president.sendMessage(
        s"Please select the next president by writing, `investigate <name, id or mention>`. You can choose your self.",
      )

      _ <- game.channel.sendMessage("The president will select the next president.")
    yield new WaitingForPolicyResolution(
      game.guild,
      game.channel,
      game.policyEffects,
      game.roles,
      game.president,
      game.lastChancellor,
      game.policies,
      game.failedElections,
      game.vetoUnlocked,
      SelectNextPresident,
    )

  type PolicyEffects = Map[Int, StartedGame => IO[StartedGame]]

  val lowPolicyEffects: PolicyEffects = Map(
    3 -> revealTopCards,
    4 -> kill,
    5 -> kill,
  )

  val mediumPolicyEffects: PolicyEffects = Map(
    2 -> investigate,
    3 -> selectNextPresident,
    4 -> kill,
    5 -> kill,
  )

  val highPolicyEffects: PolicyEffects = Map(
    1 -> investigate,
    2 -> investigate,
    3 -> selectNextPresident,
    4 -> kill,
    5 -> kill,
  )

  def announcePresident(channel: Channel, president: President): IO[Unit] = channel
    .sendMessage(
      s"${president.mention} is the President, use `nominate @mention` to nominate your chancellor.",
    )
    .as(())
}

sealed trait Game {
  def channel: Channel
  def guild: Guild
  def availableCommands: Map[User, Set[SecretHitlerCommand]]
  def players: Set[User]

  def isPlaying(user: User): Boolean = players.contains(user)
}

class CreatedGame(val players: Set[User], val channel: Channel, val guild: Guild) extends Game {

  override def availableCommands: Map[User, Set[SecretHitlerCommand]] =
    Map.empty.withDefaultValue(Set(StartSecretHitler, LeaveGame, JoinGame))

  lazy val start: IO[Option[Game]] =
    players.size match {
      case i if i < Game.minimumPlayers =>
        channel.sendMessage(s"Not enough players to start at least ${Game.minimumPlayers} required.").as(this.some)
      case i if i > Game.maximumPlayers =>
        channel.sendMessage(s"Too many players to start maximum players is ${Game.minimumPlayers}.").as(this.some)
      case _ => startGame
    }

  private lazy val hitlerKnows: Boolean = players.size < 7

  private lazy val hitlerIsKnowMessage: String = {
    val knows = if hitlerKnows then "knows" else "doesn't know"
    s"Hitler $knows who the fascists are"
  }

  private lazy val policyEffects = players.size match {
    case 9 | 10 => Game.highPolicyEffects
    case 7 | 8  => Game.mediumPolicyEffects
    case 5 | 6  => Game.lowPolicyEffects
    case _      => Game.lowPolicyEffects
  }

  private lazy val startGame: IO[Option[GameBeforeNomination]] = {
    val numberOfFascists = (players.size - 1) / 2
    val shuffled = Random.shuffle(players.toList)
    val (allFascists, liberals) = shuffled.splitAt(numberOfFascists)
    val hitler :: fascists = allFascists: @unchecked
    val playersWithRoles: List[(User, Role)] =
      liberals.map(_ -> Liberal) ++ fascists.map(_ -> Fascist) :+ (hitler -> Hitler)

    val playersWithRolesShuffled = Random.shuffle(playersWithRoles)
    val playersShuffled = playersWithRolesShuffled.map(_._1)
    val president = new President(playersShuffled.head)

    for
      _ <- channel.sendMessage(
        s"Game starting there are ${allFascists.size} fascists, ${liberals.size} liberals and $hitlerIsKnowMessage.",
      )

      _ <- channel.sendMessage(s"Player order is ${playersShuffled.map(_.getNameIn(guild)).mkString(", ")}")

      _ <- Game.announcePresident(channel, president)

      _ <- playersWithRoles.toMap.view
        .mapValues {
          case `Hitler` if hitlerKnows =>
            val isOrAre = if allFascists.size == 1 then "fascist is" else "fascists are"
            s"You are the Secret Hitler this game, the $isOrAre ${fascists.map(_.name).mkString(", ")}."
          case `Hitler` =>
            s"You are the Secret Hitler this game."
          case `Fascist` =>
            val isOrAre = if allFascists.size == 1 then "fascist is" else "fascists are"
            s"You are a Fascist this game. ${hitler.name} is the Secret Hitler, the $isOrAre ${fascists.map(_.name).mkString(", ")}."
          case `Liberal` =>
            s"You are a Liberal this game."
        }
        .toList
        .traverse_ { case (player, message) =>
          player.sendMessage(message)
        }
    yield new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      playersWithRolesShuffled,
      president,
      None,
      0,
      Policies(),
      false,
    ).some
  }
}

sealed abstract class StartedGame(
  val guild: Guild,
  val channel: Channel,
  val policyEffects: PolicyEffects,
  val roles: List[(User, Role)],
  val president: President,
  val lastChancellor: Option[User],
  val failedElections: Int,
  val policies: Policies,
  val vetoUnlocked: Boolean,
) extends Game
    with Finishable {
  lazy val roleMap: Map[User, Role] = roles.toMap
  lazy val playerList: List[User] = roles.map(_._1)
  override lazy val players: Set[User] = roleMap.keySet
  def announceNextPresident(nextPresident: President): IO[Unit] = Game.announcePresident(channel, nextPresident)

  lazy val nextRound: IO[GameBeforeNomination] = {
    val nextPresident = president.next(playerList)
    println(s"fascists ${policies.enacted.fascist}")
    println(s"Veto unlocked? ${policies.enacted.fascist >= Game.fascistPoliciesToUnlockVeto}")
    announceNextPresident(nextPresident).as(
      new GameBeforeNomination(
        guild,
        channel,
        policyEffects,
        roles,
        nextPresident,
        lastChancellor,
        failedElections,
        policies,
        vetoUnlocked,
      ),
    )
  }
}

sealed trait Finishable { this: StartedGame =>

  /** Checks if a given game has finished
    * @return None if game has ended, a game state otherwise
    */
  def attemptToFinishGame(currentChancellor: Option[User]): IO[Option[StartedGame]] = {
    def hitlerWasElected(hitler: Option[User], currentChancellor: Option[User]) =
      (for
        currentChancellor <- currentChancellor
        hitler <- hitler
      yield currentChancellor == hitler).getOrElse(false)

    lazy val hitler: Option[User] = roles.collectFirst { case (user, `Hitler`) =>
      user
    }

    if policies.enacted.fascist >= hitlerElectionWinPoliciesNeeded && hitlerWasElected(hitler, currentChancellor) then {
      channel.sendMessage("Hitler was elected chancellor! Fascists win!").as(None)
    } else if policies.enacted.fascist == fascistPoliciesToWin then {
      channel.sendMessage(s"All $fascistPoliciesToWin fascist policies were enacted! Fascists win!").as(None)
    } else if policies.enacted.liberal == liberalPoliciesToWin then {
      channel.sendMessage(s"All $liberalPoliciesToWin liberal policies were enacted! Liberals win!").as(None)
    } else if hitler.isEmpty then {
      channel.sendMessage(s"Hilter has been killed! Liberals win!").as(None)
    } else {
      IO.pure(Some(this))
    }
  }
}

sealed trait Updateable[+B <: StartedGame] { this: StartedGame =>
  def withPolicies(policies: Policies): B
  def withRoles(roles: List[(User, Role)], president: President): B
  def withPresidentOverride(president: User): B

  protected def enactPolicy(policy: Policy, doEffects: Boolean): IO[StartedGame] = {
    val game = withPolicies(policies.enact(policy))
    for
      _ <- channel.sendMessage(s"A $policy policy has been enacted!")
      _ <- channel.sendMessage(
        s"${game.policies.enacted.liberal} Liberal and ${game.policies.enacted.fascist} Fascist policies have been enacted, in total.",
      )

      gameWithEffects <-
        if doEffects then {
          game.policyEffects.get(game.policies.enacted.fascist).fold[IO[StartedGame]](game.nextRound)(_(game))
        } else {
          game.nextRound
        }
    yield gameWithEffects
  }

  //Set failedElections to 0
  //Check Win condition

}

class GameBeforeNomination(
  guild: Guild,
  channel: Channel,
  policyEffects: PolicyEffects,
  roles: List[(User, Role)],
  president: President,
  lastChancellor: Option[User],
  failedElections: Int,
  policies: Policies,
  vetoUnlocked: Boolean,
) extends StartedGame(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    ) {

  override def availableCommands: Map[User, Set[SecretHitlerCommand]] = Map[User, Set[SecretHitlerCommand]](
    president.current -> Set(NominateChancellor),
  ).withDefaultValue(Set.empty)

  def nominateChancellor(candidate: User): IO[Option[StartedGame]] = candidate match {
    case candidate if president.current == candidate =>
      channel.sendMessage("You cannot nominate yourself for chancellor.").as(this.some)
    case candidate if lastChancellor.contains(candidate) =>
      channel.sendMessage(s"${candidate.name} was the last chancellor and cannot be nominated.").as(this.some)
    case candidate if !players.contains(candidate) =>
      channel.sendMessage(s"${candidate.name} is not playing!").as(this.some)
    case candidate =>
      for
        message <- channel.sendMessage(s"${candidate.name} has been nominated for chancellor cast your votes now!")
        _ <- message.addReactions("?", "?")
      yield withCandidate(candidate).some
  }

  private def withCandidate(candidate: User): GameDuringElection =
    new GameDuringElection(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      candidate,
      failedElections,
      policies,
      vetoUnlocked,
    )
}

class GameDuringElection(
  guild: Guild,
  channel: Channel,
  policyEffects: PolicyEffects,
  roles: List[(User, Role)],
  president: President,
  lastChancellor: Option[User],
  candidate: User,
  failedElections: Int,
  policies: Policies,
  vetoUnlocked: Boolean,
  votes: Map[User, Vote] = Map.empty,
) extends StartedGame(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    )
    with Updateable[GameBeforeNomination] {

  override def availableCommands: Map[User, Set[SecretHitlerCommand]] =
    Map.empty.withDefaultValue(Vote.all.toList.toSet)

  override def withPolicies(policies: Policies): GameBeforeNomination =
    new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    )

  override def withRoles(roles: List[(User, Role)], president: President): GameBeforeNomination =
    new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    )

  override def withPresidentOverride(president: User): GameBeforeNomination =
    new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      new President(president),
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    )

  private def addVote(user: User, vote: Vote): IO[Map[User, Vote]] =
    if votes.contains(user) then {
      user.sendMessage("You already voted, your vote remains unchanged.").as(votes)
    } else {
      IO.pure(votes + (user -> vote))
    }

  private def failedVote(president: President): GameBeforeNomination =
    new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections + 1,
      policies,
      vetoUnlocked,
    )

  private lazy val passVote: IO[Option[StartedGame]] =
    for
      _ <- channel.sendMessage(
        //s"${candidate.mention} has been elected Chancellor. ${president.mention} please check your private messages.",
        s"${candidate.mention} has been elected Chancellor.",
      )

      updatedPolicies = this.policies.drawPolicies()
      gameWithChancellorElected = new GameWithChancellorElected(
        guild,
        channel,
        policyEffects,
        roles,
        president,
        candidate,
        updatedPolicies,
        failedElections,
        vetoUnlocked,
      )
      checkedGame <- gameWithChancellorElected.attemptToFinishGame(Some(candidate))
      _ <- checkedGame.fold(IO.unit) { _ =>
        val message =
          s"You drew the following policies: ${updatedPolicies.floating.mkString(", ")}. Use the reactions below to choose which to **discard**."
        for
          message <- president.sendMessage(message)
          emotes = updatedPolicies.floating.distinct.map(_.emote)
          _ <- message.addReactions(emotes *)
        yield ()
      }
    yield checkedGame

  private lazy val failVote: IO[Option[StartedGame]] = {
    lazy val nextPresident = president.next(playerList)
    val (message, game) = failedElections match {
      case 0 =>
        val message = s"Vote failed!"
        val game = announceNextPresident(nextPresident).as(Some(failedVote(nextPresident)))
        (message, game)
      case 1 =>
        val message =
          s"Vote failed! If the next vote fails the top policy will be enacted!"
        val game = announceNextPresident(nextPresident).as(Some(failedVote(nextPresident)))
        (message, game)
      case _ =>
        val updatedPolicies = policies.drawPolicies(1)
        val policy = updatedPolicies.floating.head
        val message = s"Vote failed for 3rd time in a row. $policy policy was enacted."
        val game = for
          game <- enactPolicy(policy, doEffects = false) //This selects the next president
          beforeNomination = new GameBeforeNomination(
            game.guild,
            game.channel,
            game.policyEffects,
            game.roles,
            game.president,
            game.lastChancellor,
            0,
            game.policies,
            game.vetoUnlocked,
          )
          updatedGame <- beforeNomination.attemptToFinishGame(None)
        yield updatedGame

        (message, game)
    }
    for
      _ <- channel.sendMessage(message)
      game <- game
    yield game
  }

  private def withVotes(votes: Map[User, Vote]): GameDuringElection =
    new GameDuringElection(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      candidate,
      failedElections,
      policies,
      vetoUnlocked,
      votes,
    )

  def vote(user: User, vote: Vote): IO[Option[StartedGame]] =
    for
      votes <- addVote(user, vote)
      votePassed = votes.values.count(_ == GameVote.Ja) > players.size / 2
      voteFailed = votes.values.count(_ == GameVote.Nein) >= players.size / 2f
      game <-
        if votePassed then {
          passVote
        } else if voteFailed then {
          failVote
        } else {
          IO.pure(Some(withVotes(votes)))
        }
    yield game
}

class GameWithChancellorElected(
  guild: Guild,
  channel: Channel,
  policyEffects: PolicyEffects,
  roles: List[(User, Role)],
  president: President,
  chancellor: User,
  policies: Policies,
  failedElections: Int,
  vetoUnlocked: Boolean,
) extends StartedGame(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      Some(chancellor),
      failedElections,
      policies,
      vetoUnlocked,
    ) {

  override def availableCommands: Map[User, Set[SecretHitlerCommand]] = Map[User, Set[SecretHitlerCommand]](
    president.current -> Set(Discard, PickFascist, PickLiberal),
  ).withDefaultValue(Set.empty)

  def discardPolicy(policy: Policy): IO[Option[StartedGame]] =
    if !policies.floating.contains(policy) then {
      president.sendMessage("You cannot discard what you do not have!").as(this.some)
    } else {
      val updatedPolicies = policies.discardPolicy(policy)
      val vetoMessage =
        if vetoUnlocked then " Veto power has been unlocked you can propose a veto by saying `I wish to veto this agenda`."
        else ""
      for
        message <- chancellor.sendMessage(
          s"The president has given you the following policies: ${updatedPolicies.floating.mkString(", ")}. Use the reactions below to choose which to **enact**.$vetoMessage",
        )
        emotes = updatedPolicies.floating.distinct.map(_.emote)
        _ <- message.addReactions(emotes *)
        _ <- emotes.traverse_(e => message.addReaction(e))
      yield new GameWaitingForChancellorDecision(
        guild,
        channel,
        policyEffects,
        roles,
        president,
        chancellor,
        updatedPolicies,
        failedElections,
        vetoUnlocked,
      ).some
    }
}

class GameWaitingForChancellorDecision(
  guild: Guild,
  channel: Channel,
  policyEffects: PolicyEffects,
  roles: List[(User, Role)],
  president: President,
  chancellor: User,
  policies: Policies,
  failedElections: Int,
  vetoUnlocked: Boolean,
  vetoDenied: Boolean = false,
) extends StartedGame(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      Some(chancellor),
      failedElections,
      policies,
      vetoUnlocked,
    )
    with Updateable[GameWaitingForChancellorDecision] {

  override def availableCommands: Map[User, Set[SecretHitlerCommand]] =
    (if vetoUnlocked && !vetoDenied then {
       Map[User, Set[SecretHitlerCommand]](chancellor -> Set(Enact, PickFascist, PickLiberal, Veto))
     } else {
       Map[User, Set[SecretHitlerCommand]](chancellor -> Set(Enact, PickFascist, PickLiberal))
     }).withDefaultValue(Set.empty)

  override def withPolicies(policies: Policies): GameWaitingForChancellorDecision =
    new GameWaitingForChancellorDecision(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      chancellor,
      policies,
      failedElections,
      vetoUnlocked,
      vetoDenied,
    )

  override def withRoles(roles: List[(User, Role)], president: President): GameWaitingForChancellorDecision =
    new GameWaitingForChancellorDecision(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      chancellor,
      policies,
      failedElections,
      vetoUnlocked,
      vetoDenied,
    )

  override def withPresidentOverride(president: User): GameWaitingForChancellorDecision =
    new GameWaitingForChancellorDecision(
      guild,
      channel,
      policyEffects,
      roles,
      new President(president),
      chancellor,
      policies,
      failedElections,
      vetoUnlocked,
      vetoDenied,
    )

  def enactPolicy(policy: Policy): IO[Option[StartedGame]] =
    //Chancellor loses seat as soon as he enacts a policy
    enactPolicy(policy, doEffects = true).flatMap(_.attemptToFinishGame(None))

  lazy val veto: IO[Option[GameWithVetoRequest]] =
    for
      message <- president.sendMessage("The chancellor has requested a veto, do you accept?")
      _ <- message.addReactions("?", "?")
    yield new GameWithVetoRequest(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      chancellor,
      policies,
      failedElections,
      true,
    ).some

}

class GameWithVetoRequest(
  guild: Guild,
  channel: Channel,
  policyEffects: PolicyEffects,
  roles: List[(User, Role)],
  president: President,
  chancellor: User,
  policies: Policies,
  failedElections: Int,
  vetoUnlocked: Boolean,
) extends StartedGame(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      Some(chancellor),
      failedElections,
      policies,
      vetoUnlocked,
    ) {

  override def availableCommands: Map[User, Set[SecretHitlerCommand]] =
    Map[User, Set[SecretHitlerCommand]](president.current -> Vote.all.toList.toSet).withDefaultValue(Set.empty)

  def accept: IO[Option[GameBeforeNomination]] =
    for
      _ <- channel.sendMessage("The current agenda was vetoed.")
      nextPresident = president.next(playerList)
      _ <- announceNextPresident(nextPresident)
    yield new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      nextPresident,
      Some(chancellor),
      failedElections + 1,
      policies.discardAll,
      true,
    ).some

  def deny: IO[Option[GameWaitingForChancellorDecision]] =
    for
      _ <- channel.sendMessage("The veto was refused.")
    yield new GameWaitingForChancellorDecision(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      chancellor,
      policies,
      failedElections,
      true,
      true,
    ).some
}

class WaitingForPolicyResolution(
  guild: Guild,
  channel: Channel,
  policyEffects: PolicyEffects,
  roles: List[(User, Role)],
  president: President,
  lastChancellor: Option[User],
  policies: Policies,
  failedElections: Int,
  vetoUnlocked: Boolean,
  command: SecretHitlerCommand,
) extends StartedGame(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    )
    with Updateable[GameBeforeNomination] {

  override def availableCommands: Map[User, Set[SecretHitlerCommand]] =
    Map[User, Set[SecretHitlerCommand]](president.current -> Set(command)).withDefaultValue(Set.empty)

  override def withPolicies(policies: Policies): GameBeforeNomination =
    new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    )

  override def withRoles(roles: List[(User, Role)], president: President): GameBeforeNomination =
    new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    )

  override def withPresidentOverride(president: User): GameBeforeNomination =
    new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      new President(president),
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    )

  def kill(target: User): IO[Option[StartedGame]] =
    for
      _ <- channel.sendMessage(s"${target.name} was killed!")
      filteredRoles = roles.filter {
        case (user, _) if user == target => false
        case _                           => true
      }
      _ = println(filteredRoles)
      president = this.president.next(filteredRoles.map(_._1))
      game <- withRoles(filteredRoles, president).attemptToFinishGame(None)
      _ <- game.fold(IO.unit)(_.announceNextPresident(president))
    yield game

  def investigate(target: User): IO[Option[GameBeforeNomination]] =
    for
      _ <- president.sendMessage(s"${target.name} is a ${roleMap(target).role}")
      president = this.president.next(this.playerList)
      _ <- announceNextPresident(president)
    yield new GameBeforeNomination(
      guild,
      channel,
      policyEffects,
      roles,
      president,
      lastChancellor,
      failedElections,
      policies,
      vetoUnlocked,
    ).some

  def selectNextPresident(target: User): IO[Option[GameBeforeNomination]] =
    channel
      .sendMessage(s"${target.name} is now the president. Normal order will resume afterwards.")
      .as(withPresidentOverride(target).some)
}
