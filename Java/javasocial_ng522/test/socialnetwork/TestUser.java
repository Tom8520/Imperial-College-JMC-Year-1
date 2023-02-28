package socialnetwork;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import socialnetwork.domain.Board;
import socialnetwork.domain.Message;

/**
 * Pretty much the same as User, but it stores any messages sent locally.
 */
public class TestUser extends User {

  protected static final int SLEEP_INTERVAL = 10;
  protected static int maxActions = 100;
  protected static int maxRecipients = 10;
  private int messageCounter;
  private final Random random;

  private final Multimap<User, Message> sentMessages;
  private final Set<Integer> deletedMessagesId;

  public TestUser(String username, SocialNetwork socialNetwork) {
    super(username, socialNetwork);
    sentMessages = HashMultimap.create();
    this.random = new Random(123);
    this.messageCounter = 0;
    deletedMessagesId = new HashSet<>();
  }

  @Override
  public void run() {
    int performedActions = 0;

    while (performedActions++ < maxActions) {
      Board board = socialNetwork.userBoard(this);
      Set<User> possibleRecipients = socialNetwork.getBoards().keySet();
      pickAFewUsersAndSendThemAMessage(possibleRecipients);

      if (random.nextBoolean()) {
        pickOneMessageToBeDeleted(board);
      }

      try {
        Thread.sleep(SLEEP_INTERVAL);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
      performedActions++;
    }
  }

  public Random getRandom() {
    return random;
  }

  protected void pickAFewUsersAndSendThemAMessage(Set<User> possibleRecipients) {
    Set<User> everyoneElse = new HashSet<>();
    everyoneElse.addAll(possibleRecipients);
    assert everyoneElse.remove(this);

    int totalUsersToContact = Math.min(TestUser.maxRecipients, everyoneElse.size());
    if (totalUsersToContact == 0) {
      return; // no users to contact
    }

    final int numUsersToContact = getRandom().nextInt(totalUsersToContact + 1);
    if (numUsersToContact > 0) {
      List<User> recipients = new ArrayList<>(everyoneElse);
      Collections.shuffle(recipients, getRandom());
      recipients = recipients.subList(0, numUsersToContact);
      String messageText = generateNewMessageFor(recipients);
      Message sent = this.socialNetwork.postMessage(this, recipients, messageText);

      updateMessageStorage(recipients, sent);
    }
  }

  private void updateMessageStorage(List<User> recipients, Message sent) {
    recipients.stream()
        .forEach(
            r -> {
              boolean newMessage = sentMessages.put(r, sent);
              if (SocialNetwork.DEBUG) {
                System.out.println("TS (add):: " + this.getUserId() + " :: " + r + "|" + sent);
              }
              assert newMessage;
            });
    boolean newMessage = sentMessages.put(this, sent);
    if (SocialNetwork.DEBUG) {
      System.out.println("TS (add):: " + this.getUserId() + " :: " + this + "|" + sent);
    }
    assert newMessage;
  }

  protected void pickOneMessageToBeDeleted(Board board) {
    List<Message> messagesOnTheBoard = board.getBoardSnapshot();
    List<Message> myMessages =
        messagesOnTheBoard.stream()
            .filter(m -> m.getSender().getUserId() == this.getUserId())
            .collect(Collectors.toList());
    if (myMessages.isEmpty()) {
      return;
    }

    Collections.shuffle(myMessages, this.getRandom());
    Message chosen = myMessages.get(0);
    assert chosen.getSender().equals(this);
    if (deletedMessagesId.contains(chosen.getMessageId())) {
      return; // already requested to delete this one!
    }
    boolean newDeletionRequest = deletedMessagesId.add(chosen.getMessageId());
    assert newDeletionRequest;

    socialNetwork.deleteMessage(chosen);
    updateMessageStorage(chosen);
  }

  private void updateMessageStorage(Message chosen) {
    // Update tally for assertions
    List<User> recipients = new ArrayList<>(chosen.getRecipients());
    recipients.forEach(
        k -> {
          boolean removedMessage = sentMessages.remove(k, chosen);
          if (SocialNetwork.DEBUG) {
            System.out.println("TS (rem):: " + this.getUserId() + " :: " + k + "|" + chosen);
          }
          assert removedMessage;
        });
    boolean removedMessage = sentMessages.remove(this, chosen);
    if (SocialNetwork.DEBUG) {
      System.out.println("TS (rem):: " + this.getUserId() + " :: " + this + "|" + chosen);
    }
    assert removedMessage;
  }

  protected String generateNewMessageFor(Collection<User> recipients) {
    return "Message "
        + (messageCounter++)
        + " from "
        + this.getName()
        + " ["
        + this.getUserId()
        + "] to "
        + recipients;
  }

  public Multimap<User, Message> getSentMessages() {
    return sentMessages;
  }
}
