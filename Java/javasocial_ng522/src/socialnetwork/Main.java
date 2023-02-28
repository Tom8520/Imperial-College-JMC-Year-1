package socialnetwork;

import java.util.ArrayList;
import java.util.List;
import socialnetwork.domain.Backlog;
import socialnetwork.domain.TaskBacklog;
import socialnetwork.domain.UserBoard;
import socialnetwork.domain.Worker;

public class Main {

  public static void main(String[] args) {
    Backlog bl = new TaskBacklog();
    SocialNetwork sn = new SocialNetwork(bl);

    List<Thread> workers = new ArrayList<>();
    for (int i = 0; i < 10; i++) {
      workers.add(new Worker(bl));
    }

    workers.forEach(Thread::start);

    List<Thread> users = new ArrayList<>();
    for (int i = 0; i < 5; i++) {
      users.add(new User("User " + i, sn));
      sn.register((User) users.get(i), new UserBoard());
    }

    users.forEach(Thread::start);

    users.forEach((t) -> {
      try {
        t.join();
      } catch (InterruptedException e) {
        throw new RuntimeException(e);
      }
    });

    workers.forEach(Thread::interrupt);

    workers.forEach((t) -> {
      try {
        t.join();
      } catch (InterruptedException e) {
        throw new RuntimeException(e);
      }
    });
  }
}
