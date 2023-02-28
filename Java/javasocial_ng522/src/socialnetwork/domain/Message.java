package socialnetwork.domain;

import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;
import socialnetwork.User;

public class Message implements Comparable<Message> {

  private static final AtomicInteger nextId = new AtomicInteger(0);

  private final User sender;
  private final Collection<User> recipients;
  private final String text;
  private final int id;

  public Message(User sender, Collection<User> recipients, String text) {
    this.sender = sender;
    this.recipients = recipients;
    this.text = text;
    this.id = nextId.getAndIncrement();
  }

  public User getSender() {
    return sender;
  }

  public Collection<User> getRecipients() {
    return recipients;
  }

  public String getText() {
    return text;
  }

  public int getMessageId() {
    return id;
  }

  @Override
  public String toString() {
    return "Message{"
        + "sender="
        + sender
        + ", recipients="
        + recipients
        + ", text='"
        + text
        + '\''
        + ", id="
        + id
        + '}';
  }

  @Override
  public int hashCode() {
    return id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    Message message = (Message) o;
    return id == message.id;
  }

  @Override
  public int compareTo(Message o) {
    return Integer.compare(-hashCode(), -o.hashCode());
  }
}
