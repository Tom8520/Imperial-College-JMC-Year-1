package socialnetwork;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import socialnetwork.domain.Message;

public class User extends Thread {

  private static final AtomicInteger nextId = new AtomicInteger(0);

  protected final SocialNetwork socialNetwork;
  private final int id;
  private final String name;

  public User(String username, SocialNetwork socialNetwork) {
    this.name = username;
    this.id = User.nextId.getAndIncrement();
    this.socialNetwork = socialNetwork;
  }

  public int getUserId() {
    return id;
  }

  @Override
  public void run() {
    while (true) {
      double task = new Random().nextDouble();

      if (task < 0.05) {
        break;
      } else if (task < 0.8) {
        Set<User> users = randomSubset(socialNetwork.getAllUsers());

        socialNetwork.postMessage(this, users, "some random message");
      } else {
        Set<Message> messages = randomSubset(
            new HashSet<>(socialNetwork.userBoard(this).getBoardSnapshot()));

        messages.forEach(socialNetwork::deleteMessage);
      }
    }
  }

  private <T> Set<T> randomSubset(Set<T> set) {
    List<T> list = new ArrayList<>(set);
    Collections.shuffle(list);

    int size = list.size() == 0 ? 0 : new Random().nextInt(list.size());

    return new HashSet<>(list.subList(0, size));
  }

  @Override
  public String toString() {
    return "User{" + "id=" + id + ", name='" + name + '\'' + '}';
  }

  @Override
  public int hashCode() {
    return id;
  }
}
