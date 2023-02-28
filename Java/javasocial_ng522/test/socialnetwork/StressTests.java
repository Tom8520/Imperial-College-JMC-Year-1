package socialnetwork;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.google.common.collect.Multimap;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.junit.Test;
import socialnetwork.domain.Backlog;
import socialnetwork.domain.Board;
import socialnetwork.domain.Message;
import socialnetwork.domain.TaskBacklog;
import socialnetwork.domain.UserBoard;
import socialnetwork.domain.Worker;

public class StressTests {

  @Test
  public void testSmallParams() {
    ExperimentSettings settings = new ExperimentSettings(1, 5, 50, 3, 123456);
    runExperiment(settings);
  }

  @Test
  public void testMediumParams() {
    ExperimentSettings settings = new ExperimentSettings(3, 10, 50, 10, 123456);
    runExperiment(settings);
  }

  @Test
  public void testSlightlyLargerParams() {
    ExperimentSettings settings = new ExperimentSettings(5, 40, 75, 15, 123456);
    runExperiment(settings);
  }

  @Test
  public void testLargeParams() {
    ExperimentSettings settings = new ExperimentSettings(10, 100, 100, 20, 123456);
    runExperiment(settings);
  }

  static class ExperimentSettings {

    int nWorkers;
    int nUsers;
    int maxActions;
    int maxRecipients;
    int seed;

    public ExperimentSettings(
        int nWorkers, int nUsers, int maxActions, int maxRecipients, int seed) {
      this.nWorkers = nWorkers;
      this.nUsers = nUsers;
      this.maxActions = maxActions;
      this.maxRecipients = maxRecipients;
      this.seed = seed;
    }
  }

  private void runExperiment(ExperimentSettings settings) {
    // TODO replace by your Backlog implementation
    Backlog backlog = new TaskBacklog();
    SocialNetwork socialNetwork = new SocialNetwork(backlog);

    Worker[] workers = new Worker[settings.nWorkers];
    Arrays.setAll(workers, i -> new Worker(backlog));
    Arrays.stream(workers).forEach(Thread::start);

    TestUser.maxActions = settings.maxActions;
    TestUser.maxRecipients = settings.maxRecipients;

    TestUser[] userThreads = new TestUser[settings.nUsers];
    Arrays.setAll(
        userThreads,
        i -> {
          TestUser user = new TestUser("user" + i, socialNetwork);
          user.getRandom().setSeed(settings.seed++); // use distinct seeds for each user
          return user;
        });
    Arrays.stream(userThreads)
        .forEach(
            u -> {
              // TODO add your own board implementation
              socialNetwork.register(u, new UserBoard());
              u.start();
            });

    Arrays.stream(userThreads)
        .forEach(
            u -> {
              try {
                u.join();
              } catch (InterruptedException e) {
                e.printStackTrace();
              }
            });

    while (backlog.numberOfTasksInTheBacklog() != 0) {
      try {
        Thread.sleep(50);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    }

    Arrays.stream(workers).forEach(w -> w.interrupt());
    Arrays.stream(workers)
        .forEach(
            w -> {
              try {
                w.join();
              } catch (InterruptedException e) {
                e.printStackTrace();
              }
            });

    assertEquals(0, backlog.numberOfTasksInTheBacklog());
    System.out.println("Work's done, checking for consistency...");

    Map<User, Set<Integer>> userToReceivedMessages = new HashMap<>();
    for (Map.Entry<User, Board> entry : socialNetwork.getBoards().entrySet()) {
      Board board = entry.getValue();
      User user = entry.getKey();
      // check if all messages in a board are ordered
      checkBoardOrdering(board);

      Set<Integer> messageIds =
          board.getBoardSnapshot().stream().map(Message::getMessageId).collect(Collectors.toSet());
      userToReceivedMessages.put(user, messageIds);
    }

    Map<User, Integer> expectedMessageCount = new HashMap<>();
    // check that all messages sent by an user were received properly
    for (TestUser tUser : userThreads) {
      Multimap<User, Message> userMessages = tUser.getSentMessages();
      //      System.out.println(tUser + " ---> " + userMessages);

      for (Map.Entry<User, Collection<Message>> entry : userMessages.asMap().entrySet()) {
        User recipient = entry.getKey();
        List<Integer> sentMessageIds =
            entry.getValue().stream().map(Message::getMessageId).collect(Collectors.toList());
        Set<Integer> recipientBoard = userToReceivedMessages.get(recipient);
        assertTrue(recipientBoard.containsAll(sentMessageIds));

        int count = expectedMessageCount.getOrDefault(recipient, 0);
        expectedMessageCount.put(recipient, count + sentMessageIds.size());
      }
    }

    // check that the number of messages delivered and received is consistent
    for (TestUser tUser : userThreads) {
      Board userBoard = socialNetwork.userBoard(tUser);
      int expectedCount = expectedMessageCount.getOrDefault(tUser, 0);
      int actualCount = userBoard.size();
      assertEquals("count for user " + tUser + " doesn't match!", expectedCount, actualCount);
    }
  }

  private void checkBoardOrdering(Board b) {
    List<Message> messages = b.getBoardSnapshot();
    if (messages.isEmpty()) {
      return;
    }

    Iterator<Message> it = messages.iterator();
    Message current = it.next();
    while (it.hasNext()) {
      Message next = it.next();
      int cId = current.getMessageId();
      int nId = next.getMessageId();
      assertTrue(cId > nId);
      current = next;
    }
  }
}
