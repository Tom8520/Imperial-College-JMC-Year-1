package socialnetwork;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import socialnetwork.domain.Backlog;
import socialnetwork.domain.Board;
import socialnetwork.domain.Message;
import socialnetwork.domain.Task;
import socialnetwork.domain.Task.Command;

public class SocialNetwork {

  public static final boolean DEBUG = false;
  private final Backlog editsBacklog;
  private final Map<User, Board> boards = new ConcurrentHashMap<>();

  public SocialNetwork(Backlog editsBacklog) {
    this.editsBacklog = editsBacklog;
  }

  public void register(User user, Board board) {
    getBoards().put(user, board);
  }

  public Message postMessage(User sender, Collection<User> recipients, String text) {
    checkUserRegistered(sender);
    assert !recipients.contains(sender) : "s:" + sender + " rec" + recipients;
    final Message message = new Message(sender, recipients, text);

    List<User> allRecipients = new ArrayList<>(recipients.size() + 1);
    allRecipients.add(sender);
    allRecipients.addAll(recipients);
    allRecipients.stream()
        .forEach(
            r -> {
              checkUserRegistered(r);
              editsBacklog.add(new Task(Task.Command.POST, message, getBoards().get(r)));
            });
    if (DEBUG) {
      System.out.println(
          "new message " + message.getMessageId() + " from " + sender + " to " + allRecipients);
    }
    return message;
  }

  public void deleteMessage(Message message) {
    getBoards().entrySet().stream()
        .filter(
            entry -> {
              User user = entry.getKey();
              return message.getRecipients().contains(user) || message.getSender().equals(user);
            })
        .forEach(entry -> editsBacklog.add(new Task(Command.DELETE, message, entry.getValue())));
    if (DEBUG) {
      System.out.println(
          "deleting message "
              + message.getMessageId()
              + " to "
              + message.getRecipients()
              + ","
              + message.getSender());
    }
  }

  public Board userBoard(User user) {
    checkUserRegistered(user);
    return getBoards().get(user);
  }

  private void checkUserRegistered(User user) {
    if (!getBoards().containsKey(user)) {
      throw new RuntimeException(
          "Sender " + user + " not (correctly) registerd to the social network");
    }
  }

  public Map<User, Board> getBoards() {
    return boards;
  }

  public Set<User> getAllUsers() {
    return boards.keySet();
  }
}
